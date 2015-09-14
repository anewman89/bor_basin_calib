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

subroutine get_start_points(obs_offset,obs_val_offset,forcing_offset, &
                            forcing_val_offset,val_length)
  use nrtype
  use snow17_sac, only: stream_name, forcing_name,start_month, start_day, sim_length
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
  character(len=64), parameter :: read_force = "(I4.4, 3(1x,I2.2),1x,7(F10.2))"

  character(len=5000)     :: dum_str

  integer(I4B)	:: cnt			!counter variable for reading through observed streamflow record
  integer(I4B)	:: gauge		!gauge id of current line read in
  integer(I4B)	:: yr			!year of current line read in
  integer(I4B)  :: obs_yr		!year of obs start point for calibration
  integer(I4B)	:: mn			!month of current line read in
  integer(I4B)	:: dy			!day of current line read in
  integer(I4B)	:: jday_obs		!day of year variable
  integer(I4B)	:: tmp_jday		!day of year variable
  integer(I4B)	:: i			!counter variable
  integer(I4B)	:: forcing_year		!year that forcing data record starts
  integer(I4B)	:: dum_int
  integer(I4B)	:: obs_length		!length of entire observed record
  integer(I4B)  :: force_length		!length of entire forcing record

  integer(I4B)  :: ios			!end of file integer

  real(dp)	:: dum_real		!place holder real value
  

!code
  obs_offset = 0
  obs_val_offset = 0
  forcing_offset = 0
  forcing_val_offset = 0

!
  forcing_year = 1980

! for klemes
!  forcing_year = 1995

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
      obs_yr     = yr
      exit
    endif
  enddo
  close(unit=50)
!this gets the doy for the user specifed start date in the year that it occurs
!in the observed record
  call julianday_scalar(obs_yr,start_month,start_day,jday_obs)


!now read entire file quick and keep length of file
  open (UNIT=50,file=stream_name,form='formatted',status='old')
  cnt = 0
  ios = 0

  do while(ios .ge. 0)
    cnt = cnt + 1
    read (UNIT=50,FMT=read_format,IOSTAT=ios) dum_int,dum_int,dum_int,dum_int,dum_real
  enddo
  obs_length = cnt

  close(unit=50)

  open (UNIT=50,file=forcing_name,form='formatted',status='old')
  cnt = 0
  ios = 0
  read (UNIT=50,FMT='(F7.2)') dum_real
  read (UNIT=50,FMT='(F7.2)') dum_real
  read (UNIT=50,FMT='(F11.0)') dum_real
  read (UNIT=50,FMT='(94A)') dum_str
  do while(ios .ge. 0)

    read (UNIT=50,FMT=read_format,IOSTAT=ios) dum_int,dum_int,dum_int,dum_int,dum_real, &
                                  dum_real,dum_real,dum_real,dum_real,dum_real,dum_real
    cnt = cnt + 1  
  enddo
  force_length = cnt

  close(unit=50)

!!calculate how many days that is from jan 1 1980 
!!this is the offset in the daymet forcing data to match the start
!!of the streamflow record
!!loop through the years from 1980 to yr-1
  do i = forcing_year,obs_yr-1,1
    call julianday_scalar(i,12,31,tmp_jday)
    forcing_offset = forcing_offset + tmp_jday
  enddo
!add partial year 
  forcing_offset = forcing_offset + jday_obs
  
!for klemes
!  forcing_offset = obs_offset

!validation period offsets are calibration offset + sim_length
  obs_val_offset = obs_offset + sim_length
  forcing_val_offset = forcing_offset + sim_length+1

!also want to calculate validation length based off of observed record & calibration specifications
  if(obs_length .gt. force_length) then
    val_length = force_length - obs_val_offset - 1
  else
    val_length = obs_length - obs_val_offset - 1
  endif
  val_length = force_length-forcing_val_offset
!  print *,'VALIDATION LENGTH: ',val_length
!for maurer
!  val_length = force_length-forcing_val_offset

  print *,'get_start_pts:',obs_length,force_length,forcing_offset,obs_offset,obs_val_offset,forcing_val_offset,val_length

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
  integer(I4B)				:: num_valid,cnt,ios,force_length,val_length_cor

  character(len=64), parameter		:: read_format = "(I4.4, 3(1x,I2.2),1x,7(F10.2))"
  character(len = 1024)			:: dum_str
  real(DP),dimension(36500)		:: swe
  real(DP)				:: dum_real
  real(DP)				:: sum_obs
  

!code
  sum_obs = 0.0
  num_valid = 0


!read met file once to get length
  open (UNIT=50,file=forcing_name,form='formatted',status='old')
  cnt = 0
  ios = 0
  read (UNIT=50,FMT='(F7.2)') dum_real
  read (UNIT=50,FMT='(F7.2)') dum_real
  read (UNIT=50,FMT='(F11.0)') dum_real
  read (UNIT=50,FMT='(94A)') dum_str
  do while(ios .ge. 0)
    cnt = cnt + 1
    read (UNIT=50,FMT=read_format,IOSTAT=ios) dum_int,dum_int,dum_int,dum_int,dum_real, &
                                  dum_real,dum_real,dum_real,dum_real,dum_real,dum_real
  enddo
  force_length = cnt-1

  close(unit=50)



!read met file
  open (UNIT=50,file=trim(forcing_name),form='formatted',status='old')

  read (UNIT=50,FMT='(F7.2)') lat
  read (UNIT=50,FMT='(F7.2)') elev
!  read (UNIT=50,FMT='(F10.0)') area_basin
  read (UNIT=50,FMT='(F11.0)') area_basin
  read (UNIT=50,FMT='(94A)') dum_str
!  read (UNIT=50,*) dum_str

  print *,'basin area:',area_basin
  !read the rest of the input file
  !forcing_offset is the first day of the first complete water year in corresponding observed streamflow
  !this is the point at which we want to keep the forcing data
  !keep through the end of the sim_len or end of file (if validation period)
  !need to do this because forcing starts 01 Jan 1980
  !observed streamflow varies in its start date by gauge

  if(val_period .eq. 0) then
    print *,'in cida',forcing_offset,sim_length,forcing_offset+sim_length

    i2 = 0
    do i = 1,(forcing_offset + sim_length)
      !if we are at or past the forcing offset date, keep data
      if(i .ge. forcing_offset) then
 	i2 = i2 + 1
!	read (UNIT=50,FMT=read_format) year(i2),month(i2),day(i2),hour(i2),&
!				      dayl(i2),precip(i2),swdown(i2),swe(i2),tmax(i2),tmin(i2),vpd(i2)
	read (UNIT=50,FMT=*) year(i2),month(i2),day(i2),hour(i2),&
				      dayl(i2),precip(i2),swdown(i2),swe(i2),tmax(i2),tmin(i2),vpd(i2)

      !need to compute tair too
	tair(i2) = ((tmax(i2)+tmin(i2))/2.0_dp)
      else
      !read in but don't keep
!	read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,&
!				       dum_real,dum_real,dum_real,dum_real,dum_real,dum_real,dum_real
	read (UNIT=50,FMT=*) dum_int,dum_int,dum_int,dum_int,&
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
      if(streamflow(i) .ge. 0) then
	sum_obs = sum_obs + streamflow(i)
	num_valid = num_valid + 1
      endif
    enddo
!    mean_obs = sum_obs/real(sim_length)
    mean_obs = sum_obs/real(num_valid,kind(dp))
  !print *,'mean: ',mean_obs
  else
  !validation grab
    !print *,'val forcing grab',forcing_val_offset,val_length,forcing_offset+sim_length+val_length
    i2 = 0
    val_length_cor = force_length-forcing_val_offset
    print *,'force len: ',force_length,val_length_cor
    do i = 1,(force_length)
      if(i .ge. forcing_val_offset) then
	i2 = i2 + 1

!	read (UNIT=50,FMT=read_format) year(i2),month(i2),day(i2),hour(i2),&
!				      dayl(i2),precip(i2),swdown(i2),swe(i2),tmax(i2),tmin(i2),vpd(i2)
	read (UNIT=50,FMT=*) year(i2),month(i2),day(i2),hour(i2),&
				      dayl(i2),precip(i2),swdown(i2),swe(i2),tmax(i2),tmin(i2),vpd(i2)
  !print *,i
      !need to compute tair too
	tair(i2) = (tmax(i2)+tmin(i2))/2.0_dp
      else
!	read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,&
!				    dum_real,dum_real,dum_real,dum_real,dum_real,dum_real,dum_real
	read (UNIT=50,FMT=*) dum_int,dum_int,dum_int,dum_int,&
				    dum_real,dum_real,dum_real,dum_real,dum_real,dum_real,dum_real
      endif
    enddo
!print *,'cida',year(1),month(1),day(1),precip(1),streamflow(1)
  !need to convert streamflow to mm/day
    do i = 1,val_length_cor+1
      !convert streamflow (cfs) to cms
      streamflow(i) = streamflow(i)*cfs_cms  !now in cubic meters per second

      !need to convert to mm/day too
      streamflow(i) = streamflow(i)*sec_day !now in cubic meters per day m^3 day^-1

      !1 cubic meter per day is 1000 mm per square m
      streamflow(i) = streamflow(i)*1000./area_basin  !now in mm/day
      if(streamflow(i) .ge. 0.0) then
	sum_obs = sum_obs + streamflow(i)
	num_valid = num_valid + 1
      endif
    enddo
!    mean_obs = sum_obs/real(val_length)
    mean_obs = sum_obs/real(num_valid,kind(dp))

  !print *,'mean: ',mean_obs
  endif
  close(UNIT=50)

print *,'cida start',year(1),month(1),day(1),precip(1),streamflow(1),mean_obs
print *,'cida end',i2,year(i2),month(i2),day(i2),precip(i2),streamflow(i2)

  return

end subroutine read_cida_areal_forcing

!cccccccccccccccccccccccccccccccccccccccccc

subroutine read_streamflow(obs_offset,obs_val_offset,val_length)
  use nrtype
  use snow17_sac, only: stream_name, sim_length, streamflow, val_period, &
                        area_basin,valid
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
  valid = .false.

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
	if(streamflow(i2) .lt. -1.0_dp) then
	  streamflow(i2) = -999.0_dp
	else
	  valid(i2) = .true.
	endif

	if(i2 .eq. 1) then
	  print *,'streamflow calib: ',yr,mn,dy,streamflow(i2),i2,area_basin
	endif

      else
      !don't store record
	read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,dum_real
      endif
    enddo
    print *,'obs',val_length,i2
    print *,'obs end',yr,mn,dy,streamflow(i2)

  !validation period
  else
  !now if val_period .ne. 0 read rest of file
    cnt = 0
    i2  = 0
    ios = 0
    do while(ios .ge. 0)
      if(cnt .ge. obs_val_offset) then
	i2 = i2 + 1
	read (UNIT=50,FMT=read_format,IOSTAT=ios) gauge,yr,mn,dy,streamflow(i2)
	if(streamflow(i2) .lt. -1.0_dp) then
	  streamflow(i2) = -999.0_dp
	else
	  valid(i2) = .true.
	endif

	if(i2 .eq. 1) then
	  print *,'streamflow val: ',yr,mn,dy,streamflow(i2),i2
	endif
      else
      !don't keep record if not in correct time window
	read (UNIT=50,FMT=read_format,IOSTAT=ios) dum_int,dum_int,dum_int,dum_int,dum_real
      endif
      cnt = cnt + 1
    enddo
    print *,'val obs end: ',yr,mn,dy,val_length,i2,streamflow(i2-1)
  endif !end val_period if check


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
