subroutine get_model_state(cal_uztwc, cal_uzfwc, cal_lztwc, &
                           cal_lzfsc, cal_lzfpc, cal_adimc)

  use nrtype
  use snow17_sac, only: model_out, sim_length

  implicit none

!output variables
  real(dp),intent(out)	:: cal_uztwc, cal_uzfwc, cal_lztwc  !sac model state variables
  real(dp),intent(out)	:: cal_lzfsc, cal_lzfpc, cal_adimc

!local variables
  integer(I4B)		:: i,dum_int

  real(dp)		:: dum_flt

  character(len = 1024)	:: pt_1, pt_2  !file name character arrays


!code

  30 FORMAT(I4.4, 3(1x,I2.2),7(F12.4))

  pt_1 = model_out
  pt_2 = '.model_state'
  pt_1 = trim(pt_1)//trim(pt_2)


  open(UNIT=88,file=pt_1,form='formatted')
  
  do i = 1,sim_length
    read(UNIT=88,30) dum_int,dum_int,dum_int,dum_int, &
                     cal_uztwc,cal_uzfwc,cal_lztwc,cal_lzfsc, &
                     cal_lzfpc,cal_adimc,dum_flt

  enddo
  
  close(UNIT=88)


  return
end subroutine get_model_state

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine get_start_points(obs_offset,obs_val_offset,forcing_offset,forcing_val_offset,val_length)
  use nrtype
  use snow17_sac, only: stream_name, start_month, start_day, sim_length
  use gauge_calib, only: julianday_scalar

  implicit none
 
!output variables
  integer(I4B),	intent(out)	:: obs_offset			!offset from start of obs file (in days) to correct starting point for calibration
  integer(I4B), intent(out)	:: forcing_offset		!offset from start of forcing data file (in days) to starting point for calibration
  integer(I4B),	intent(out)	:: obs_val_offset		!offset from start of obs file (in days) to correct starting point for validation
  integer(I4B), intent(out)	:: forcing_val_offset		!offset from start of forcing data file (in days) to starting point for validation
  integer(I4B), intent(out)	:: val_length			!length of validation period in observed file based on calibration period ending point

!local variables
!this subroutine assumes streamflow is daily data of the following format:
  character(len=64), parameter :: read_format = "(I8.8,1x,I4.4, 2(1x,I2.2),1x,1(F8.2))"

  integer(I4B)	:: cnt			!counter variable for reading through observed streamflow record
  integer(I4B)	:: gauge		!gauge id of current line read in
  integer(I4B)	:: yr			!year of current line read in
  integer(I4B)	:: mn			!month of current line read in
  integer(I4B)	:: dy			!day of current line read in
  integer(I4B)	:: jday_obs		!day of year variable
  integer(I4B)	:: tmp_jday		!day of year variable
  integer(I4B)	:: i			!counter variable
  integer(I4B)	:: forcing_year		!year that forcing data record starts
  integer(I4B)	:: dum_int
  integer(I4B)	:: obs_length		!length of entire observed record
  integer(I4B)  :: ios			!end of file integer

  real(dp)	:: dum_real		!place holder real value
  

!code
  obs_offset = 0
  obs_val_offset = 0
  forcing_offset = 0
  forcing_val_offset = 0

  forcing_year = 1980


!open streamflow file
  open (UNIT=50,file=stream_name,form='formatted',status='old')


!want to find the first time we hit some date that is user specified
!in observed streamflow data

  cnt = 0
  do
    read (UNIT=50,FMT=read_format) gauge,yr,mn,dy,dum_real
    cnt = cnt + 1
    !if we are at the start_month,start_day for calibration, keep the record number
    if(mn .eq. start_month .and. dy .eq. start_day) then
      obs_offset = cnt
      exit
    endif
  enddo
!this gets the doy for the user specifed start date in the year that it occurs
!in the observed record
  call julianday_scalar(yr,start_month,start_day,jday_obs)


!now read entire file quick and keep length of file
  rewind(unit=50)
  cnt = 0

  do while(ios .ge. 0)
    cnt = cnt + 1
    read (UNIT=50,FMT=read_format,IOSTAT=ios) dum_int,dum_int,dum_int,dum_int,dum_real
  enddo
  obs_length = cnt

  close(unit=50)

!calculate how many days that is from jan 1 1980 
!this is the offset in the daymet forcing data to match the start
!of the streamflow record
!loop through the years from 1980 to yr-1
  do i = forcing_year,1,yr-1
    call julianday_scalar(i,12,31,tmp_jday)
    forcing_offset = forcing_offset + tmp_jday
  enddo
!add partial year 
  forcing_offset = forcing_offset + jday_obs


!validation period offsets are calibration offset + sim_length
  obs_val_offset = obs_offset + sim_length
  forcing_val_offset = forcing_offset + sim_length

!also want to calculate validation length based off of observed record & calibration specifications
  val_length = obs_length - obs_val_offset

  return
end subroutine get_start_points

!ccccccccccccccccccccccccccccccccc

subroutine read_cida_areal_forcing(forcing_offset,forcing_val_offset,val_length)
  use nrtype
  use constants,  only: cfs_cms, sec_day
  use snow17_sac, only: forcing_name, sim_length, lat, elev, area_basin, &
                        year, month, day, hour, dayl, precip, swdown, &
                        tmax, tmin, tair, vpd, streamflow, mean_obs, &
			val_period

  implicit none

!input variables
  integer(I4B), intent(in)		:: forcing_offset  !offset for forcing file to get to start of calibration period matching observed record
  integer(I4B), intent(in)		:: forcing_val_offset !offset for focing file to get to start of validation period matching observed record
  integer(I4B), intent(in)		:: val_length		!length of validation period

!local variables
  integer(I4B)				:: i,jday_s,wy_jday,dum_int,i2,end_pt

  character(len=64), parameter		:: read_format = "(I4.4, 3(1x,I2.2),1x,7(F10.2))"
  character(len = 1024)			:: dum_str
  real(DP),dimension(36500)		:: swe
  real(DP)				:: dum_real
  real(DP)				:: sum_obs


!code
  sum_obs = 0.0
!read met file
  open (UNIT=50,file=trim(forcing_name),form='formatted',status='old')

  read (UNIT=50,FMT='(F7.2)') lat
  read (UNIT=50,FMT='(F7.2)') elev
  read (UNIT=50,FMT='(F10.0)') area_basin
  read (UNIT=50,FMT='(63A)') dum_str
  !read the rest of the input file
  !forcing_offset is the first day of the first complete water year in corresponding observed streamflow
  !this is the point at which we want to keep the forcing data
  !keep through the end of the sim_len or end of file (if validation period)
  !need to do this because forcing starts 01 Jan 1980
  !observed streamflow varies in its start date by gauge

  if(val_period .eq. 0) then
    print *,forcing_offset,sim_length,forcing_offset+sim_length

    i2 = 0
    do i = 1,(forcing_offset + sim_length)
      !if we are at or past the forcing offset date, keep data
      if(i .ge. forcing_offset) then
 	i2 = i2 + 1
	read (UNIT=50,FMT=read_format) year(i2),month(i2),day(i2),hour(i2),&
				      dayl(i2),precip(i2),swdown(i2),swe(i2),tmax(i2),tmin(i2),vpd(i2)

      !need to compute tair too
	tair(i2) = (tmax(i2)+tmin(i2))/2.0_dp
      else
      !read in but don't keep
	read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,&
				       dum_real,dum_real,dum_real,dum_real,dum_real,dum_real,dum_real
      endif
    enddo
    !need to convert streamflow to mm/day
    do i = 1,sim_length
      !convert streamflow (cfs) to cms
      streamflow(i) = streamflow(i)*cfs_cms  !now in cubic meters per second

      !need to convert to mm/day
      streamflow(i) = streamflow(i)*sec_day !now in cubic meters per day m^3 day^-1
                                           !m^3/day

      !1 cubic meter per day is 1000 mm per square m -> mm*m^2/day
      streamflow(i) = streamflow(i)*1000./area_basin  !now in mm/day
      sum_obs = sum_obs + streamflow(i)
    enddo
    mean_obs = sum_obs/real(sim_length)
  !print *,'mean: ',mean_obs
  else
  !validation grab
    print *,forcing_val_offset,val_length,forcing_offset+sim_length+val_length
    i2 = 0
    do i = 1,(forcing_val_offset + val_length)
      if(i .ge. forcing_val_offset-1) then
	i2 = i2 + 1

	read (UNIT=50,FMT=read_format) year(i2),month(i2),day(i2),hour(i2),&
				      dayl(i2),precip(i2),swdown(i2),swe(i2),tmax(i2),tmin(i2),vpd(i2)
  !print *,i
      !need to compute tair too
	tair(i2) = (tmax(i2)+tmin(i2))/2.0_dp
      else
	read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,&
				    dum_real,dum_real,dum_real,dum_real,dum_real,dum_real,dum_real
      endif
    enddo
!print *,'cida',year(1),month(1),day(1),precip(1),streamflow(1)
  !need to convert streamflow to mm/day
    do i = 1,val_length
      !convert streamflow (cfs) to cms
      streamflow(i) = streamflow(i)*cfs_cms  !now in cubic meters per second

      !need to convert to mm/day too
      streamflow(i) = streamflow(i)*sec_day !now in cubic meters per day m^3 day^-1

      !1 cubic meter per day is 1000 mm per square m
      streamflow(i) = streamflow(i)*1000./area_basin  !now in mm/day
      sum_obs = sum_obs + streamflow(i)
    enddo
    mean_obs = sum_obs/real(val_length)
  !print *,'mean: ',mean_obs
  endif
  close(UNIT=50)

print *,'cida',year(1),month(1),day(1),precip(1),streamflow(1)

  return

end subroutine read_cida_areal_forcing

!cccccccccccccccccccccccccccccccccccccccccc

subroutine read_streamflow(obs_offset,obs_val_offset,val_length)
  use nrtype
  use snow17_sac, only: stream_name, sim_length, streamflow, val_period, &
                        area_basin
  implicit none

!input variables
  integer(I4B), intent(in)	:: obs_offset
  integer(I4B), intent(in)	:: obs_val_offset
  integer(I4B), intent(in)	:: val_length

!local variables
  integer(I4B) :: i,i2,end_pt,dum_int,jday_s,wy_jday
  integer(I4B) :: yr,mn,dy,gauge,cnt,ios,error
  real(dp)    :: dum_real,st_1

!this subroutine assumes streamflow is daily data of the following format:
  character(len=64), parameter :: read_format = "(I8.8,1x,I4.4, 2(1x,I2.2),1x,1(F8.2))"


!code

! open streamflow file
  open (UNIT=50,file=stream_name,form='formatted',status='old')

  !read in for calibration period
  if(val_period .eq. 0) then
    i2 = 0
    do i = 1,obs_offset+sim_length
      !if withint time period, keep record
      if(i .ge. obs_offset) then
        i2 = i2 + 1

	read (UNIT=50,FMT=read_format) gauge,yr,mn,dy,streamflow(i2)

	if(i2 .eq. 1) then
	  print *,'streamflow calib: ',yr,mn,dy,streamflow(i2),i2,area_basin
	endif

      else
      !don't store record
	read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,dum_real
      endif
    enddo
  !validation period
  else
  !now if val_period .ne. 0 read rest of file
    i2  = 0
    ios = 0
    do while(ios .ge. 0)
      if(cnt .ge. obs_val_offset-1) then
	i2 = i2 + 1
	read (UNIT=50,FMT=read_format,IOSTAT=ios) gauge,yr,mn,dy,streamflow(i2)
	if(i2 .le. 1) then
	  print *,'streamflow val: ',yr,mn,dy,streamflow(i2),i2,area_basin
	endif
      else
      !don't keep record if not in correct time window
	read (UNIT=50,FMT=read_format,IOSTAT=ios) dum_int,dum_int,dum_int,dum_int,dum_real
      endif
    enddo
  endif !end val_period if check

print *,'val',val_length,i2
  close(UNIT=50)

  return
end subroutine read_streamflow

!ccccccccccccccccccccccccccccccccc
!this version isn't setup to read in for validation period, only calibration period
!this is likely an obsolte function at this point, schedule for removal ( AJN 9/9/2013 )
subroutine read_mopex_forcing(forcing_offset)
  use nrtype
  use snow17_sac, only: forcing_name, sim_length, streamflow, &
                        year, month, day, hour, precip, pet, &
                        raim, sneqv_verif, tair, mean_obs,   &
                        lat, elev
  implicit none

!input variables
  integer(I4B), intent(in)		:: forcing_offset  !offset for forcing file to get to start of calibration period matching observed record

!local variables
  integer(I4B)              :: i,dum_int,end_pt,i2
  character(len=1000)  :: dum_string
  real(dp)                 :: est_et,est_q,surf_q,grnd_q,soil_m
  real(dp)                 :: sum_obs,dum_real

  character(len=64), parameter :: read_format = "(I4.4, 3(3x,I2.2),11(F10.2))"

!code
  sum_obs = 0.0

  end_pt = forcing_offset + sim_length

  i2 = 0

!read met file
  open (UNIT=50,file=forcing_name,form='formatted',status='old')

  read (UNIT=50,FMT="(A78)") dum_string
  read (UNIT=50,FMT="(A23)") dum_string
  read (UNIT=50,FMT="(A24)") dum_string
  read (UNIT=50,FMT="(A70)") dum_string
  read (UNIT=50,FMT="(A28)") dum_string
  read (UNIT=50,FMT="(A36)") dum_string
  read (UNIT=50,FMT="(A130)") dum_string
  read (UNIT=50,FMT="(F4.0)") elev
  read (UNIT=50,FMT="(F5.2)") lat 
  do i = 1,forcing_offset+sim_length

    if(i .ge. forcing_offset .and. i .lt. end_pt) then
!      i2 = i-start_calib+1
      i2 = i2 + 1
      read (UNIT=50,FMT=read_format) year(i2),month(i2),day(i2),hour(i2),&
      		                   precip(i2),est_et,pet(i2),est_q,streamflow(i2),surf_q,grnd_q, &
                                   soil_m,raim(i2),sneqv_verif(i2),tair(i2)

      sum_obs = sum_obs + streamflow(i2)
    else
      read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,&
      		                   dum_real,dum_real,dum_real,dum_real,dum_real,dum_real,dum_real, &
                                   dum_real,dum_real,dum_real,dum_real
    endif
  enddo
  close(UNIT=50)
  mean_obs = sum_obs/real(sim_length)

!  print *,'read: ', year(1),month(1),day(1),hour(1),raim(1),streamflow(1)
  return
end subroutine read_mopex_forcing