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


  pt_1 = model_out
  pt_2 = '.model_state'
  pt_1 = trim(pt_1)//trim(pt_2)


  open(UNIT=88,file=pt_1,form='formatted')
  
  do i = 1,sim_length
    read(88,30) dum_int,dum_int,dum_int,dum_int, &
                     cal_uztwc,cal_uzfwc,cal_lztwc,cal_lzfsc, &
                     cal_lzfpc,cal_adimc,dum_flt

  enddo
  
  close(UNIT=88)

  30 FORMAT(I4.4, 3(1x,I2.2),7(F12.4))

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
  forcing_val_offset = forcing_offset + sim_length-1

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

subroutine read_cida_areal_forcing()
  use nrtype
  use constants,  only: cfs_cms, sec_day
  use snow17_sac

  implicit none

!input variables

!local variables
  integer(I4B)				:: i,jday_s,wy_jday,dum_int,i2,end_pt
  integer(I4B)				:: num_valid,cnt,ios,force_length,val_length_cor
  integer(I4B)				:: in_year
  integer(I4B)				:: in_month
  integer(I4B)				:: in_day
  integer(I4B)				:: in_hour

  character(len=64), parameter		:: read_format = "(I4.4, 3(1x,I2.2),1x,7(F10.2))"
  character(len = 1024)			:: dum_str
  real(DP),dimension(36500)		:: swe
  real(DP)				:: dum_real
  real(DP)				:: sum_obs
  real(DP)				:: in_dayl
  real(DP)				:: in_precip
  real(DP)				:: in_swdown
  real(DP)				:: in_swe
  real(DP)				:: in_tmax
  real(DP)				:: in_tmin
  real(DP)				:: in_vpd
  
  logical				:: read_flag

!code

  read_flag = .False.

!read met file once to get length
  open (UNIT=50,file=forcing_name,form='formatted',status='old')
  cnt = 1
  ios = 0
  read (UNIT=50,FMT='(F7.2)') lat
  read (UNIT=50,FMT='(F7.2)') elev
  read (UNIT=50,FMT='(F11.0)') area_basin
  read (UNIT=50,FMT='(94A)') dum_str

  do while(ios .ge. 0)

    read (UNIT=50,FMT=*,IOSTAT=ios) in_year, in_month, in_day,in_hour, &
         in_dayl,in_precip,in_swdown,in_swe,in_tmax,in_tmin,in_vpd

    if(start_yr .eq. in_year .and. start_month .eq. in_month .and. start_day .eq. in_day) then
      read_flag = .True.
    end if

    if(read_flag) then


      year(cnt) = in_year
      month(cnt) = in_month
      day(cnt) = in_day
      hour(cnt) = in_hour

      dayl(cnt) = in_dayl
      precip(cnt) = in_precip
      swdown(cnt) = in_swdown
      swe(cnt) = in_swe
      tmax(cnt) = in_tmax
      tmin(cnt) = in_tmin
      tair(cnt) = (in_tmax+in_tmin)/2.0_dp
      vpd(cnt) = in_vpd

!print *,cnt,read_flag,precip(cnt),year(cnt),month(cnt),day(cnt),tmax(cnt),tmin(cnt)

  
      cnt = cnt + 1
    end if
    if(in_year .eq. end_yr .and. in_month .eq. end_month .and. in_day .eq. end_day) then
      read_flag = .False.
    end if
  enddo
  force_length = cnt-1
  sim_length = cnt

  close(unit=50)


print *,'cida start',year(1),month(1),day(1),precip(1),tmax(1)
print *,'cida end',cnt,year(cnt-1),month(cnt-1),day(cnt-1),precip(cnt-1)

  return

end subroutine read_cida_areal_forcing

!cccccccccccccccccccccccccccccccccccccccccc

subroutine read_streamflow(stream_pos)
  use nrtype
  use constants,  only: cfs_cms, sec_day
  use snow17_sac, only: stream_name, sim_length, streamflow, val_period, &
                        area_basin,valid,stream_yr,stream_day,stream_month, &
                        end_yr,end_day,end_month,mean_obs,obs_month,obs_year
  implicit none

!input variables
  integer(I4B), intent(in)  :: stream_pos

!local variables
  integer(I4B) :: i,i2,end_pt,dum_int,jday_s,wy_jday
  integer(I4B) :: yr,mn,dy,gauge,cnt,ios,error
  integer(I4B) :: num_valid,leap
  real(dp)     :: dum_real,st_1, sum_obs

  integer(I4B) :: diff_yr

  real(DP)	:: in_streamflow
  character     :: dum_char

  logical	:: read_flag


!this subroutine assumes streamflow is daily data of the following format:
  character(len=64), parameter :: read_format = "(I8.8,1x,I4.4, 2(1x,I2.2),1x,1(F8.2),1x,1A)"


!code
  sum_obs = 0.0
  num_valid = 0

  valid = .false.

  read_flag = .false.

! open streamflow file
  open (UNIT=50,file=stream_name,form='formatted',status='old')

  cnt = stream_pos
  ios = 0
  do while(ios .ge. 0)
    read (UNIT=50,FMT=read_format,IOSTAT=ios) gauge,yr,mn,dy,in_streamflow,dum_char
 
!    if(yr .eq. start_yr .and. mn .eq. start_month .and. dy .eq. start_day) then
!      read_flag = .true.
!    end if

    if(yr .eq. stream_yr .and. mn .eq. stream_month .and. dy .eq. stream_day) then
      read_flag = .true.
    end if

    if(read_flag) then
      if(streamflow(cnt) .ge. 0) then
        !pull observation year and month out
        obs_month(cnt)  = mn
        obs_year(cnt)   = yr

        !observed streamflow
        streamflow(cnt) = in_streamflow

        !convert streamflow (cfs) to cms
        streamflow(cnt) = streamflow(cnt)*cfs_cms  !now in cubic meters per second

        !need to convert to mm/day
        streamflow(cnt) = streamflow(cnt)*sec_day !now in cubic meters per day m^3 day^-1

        !1 cubic meter per day is 1000 mm per square m -> mm*m^2/day
        streamflow(cnt) = streamflow(cnt)*1000./area_basin  !now in mm/day
	sum_obs = sum_obs + streamflow(cnt)
	num_valid = num_valid + 1
        valid(cnt) = .true.
      else
        streamflow(cnt) = -999.0_dp
      endif

      cnt = cnt + 1
    end if
    if(yr .eq. end_yr .and. mn .eq. end_month .and. dy .eq. end_day) then
      read_flag = .false.
    end if
  end do
  mean_obs = sum_obs/count(valid(1:cnt))

  print *,streamflow(1)
  print *,streamflow(cnt-1)

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
