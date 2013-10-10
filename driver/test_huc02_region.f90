program test_upper_colo
  use nrtype
  use snow17_sac
  use constants, only: sec_hour, sec_day
  use gauge_calib, only: read_namelist, calc_pet_pt, sfc_pressure, &
                         sce_param_setup, read_cida_areal_forcing, &
                         read_streamflow, calc_rmse, calc_mse, calc_nse, &
                         calc_kge,get_start_points, get_model_state,     &
		         spin_up_first_year
  implicit none


!local variables
  integer(I4B) :: i,ntau,k,m,j,cnt,start_offset

  logical :: spin_up_flag

  integer(I4B) :: error,isce,num_basin

  integer(I4B) :: end_pt	!length of simulation

  !integer :: opt

  real(dp) :: obj_val	!return value from objective function
  real(sp) :: dtuh	!for unit hydrograph

!  character(len=1024) :: opt_name
!  character(len=2)    :: huc_02
  character(len=512) :: pt1
  character(len=512) :: pt2

  !variables for SCE
!  real, dimension(:), allocatable   :: a      !parameter set
!  real                              :: af     !objective function value
!  real, dimension(:), allocatable   :: bl     !lower bounds on parameter set
!  real, dimension(:), allocatable   :: bu     !upper bounds on parameter set

  real(sp), dimension(30)   :: a      !parameter set
  real(sp)                  :: af     !objective function value
  real(sp), dimension(30)   :: bl     !lower bounds on parameter set
  real(sp), dimension(30)   :: bu     !upper bounds on parameter set


!sac-sma state variables
  real(dp)              :: uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc

!single precision sac-sma state variables
  real(sp)		:: uztwc_sp
  real(sp)		:: uzfwc_sp
  real(sp)		:: lztwc_sp
  real(sp)		:: lzfsc_sp
  real(sp)		:: lzfpc_sp
  real(sp)		:: adimc_sp

  real(sp)		:: pet_sp
  real(sp)		:: tair_sp
  real(sp)		:: precip_sp

!state variables from end of calibration period
  real(dp)              :: cal_uztwc, cal_uzfwc, cal_lztwc
  real(dp)              :: cal_lzfsc, cal_lzfpc, cal_adimc


!sac-sma output variables
!  real, dimension(:), allocatable    :: qs,qg,eta,tci,route_tci

!snow-17 output variables
!  real, dimension(:), allocatable    :: snowh, sneqv, snow, raim_snow17      !output variables

!sac-sma output variables
  real(sp), dimension(36500)    :: qs,qg,eta,tci,route_tci

!double precision
  real(dp), dimension(36500)    :: tci_dp,route_tci_dp,streamflow_dp


!snow-17 output variables
  real(sp), dimension(36500)    :: snowh, sneqv, snow, raim_snow17 	!output variables

!snow-17 carry over variables
  real(sp) :: tprev				!carry over variable
  real(sp), dimension(19)  :: cs			!carry over variable

!snow-17 surface pressure
  real(dp) :: pa

!unit hydrograph
  real(sp),dimension(1000)       :: unit_hydro

!single iteration parameter info
  integer(I4B)                          :: bid,loc,sid
  integer(I4B),dimension(1000)            :: basin,seed
  real(dp),   dimension(1000,21)         :: params

!file read location information
  integer(I4B)			:: obs_offset
  integer(I4B)			:: obs_val_offset
  integer(I4B)			:: forcing_offset
  integer(I4B)			:: forcing_val_offset
  integer(I4B)			:: val_length

  real(dp)			:: spinup_crit   !criteria for spin-up convergence

!
!   code starts below
!

!set spin-up criteria
!may want to not have this hardcoded in the future (AJN 9/9/2013)
  spinup_crit = 0.1_dp

!read namelists
  call read_namelist



!allocate variables  !dynamic allocation removed for now (AJN, 6/30/13)
!  if(val_period .eq. 0) then
!    ALLOCATE (year(sim_len),STAT=error)
!    ALLOCATE (month(sim_len),STAT=error)
!    ALLOCATE (day(sim_len),STAT=error)
!    ALLOCATE (hour(sim_len),STAT=error)
!    ALLOCATE (jday(sim_len),STAT=error)

!    ALLOCATE (pet(sim_len),STAT=error)
!    ALLOCATE (precip(sim_len),STAT=error)
!    ALLOCATE (swdown(sim_len),STAT=error)
!    ALLOCATE (vpd(sim_len),STAT=error)
!    ALLOCATE (tmax(sim_len),STAT=error)
!    ALLOCATE (tmin(sim_len),STAT=error)
!    ALLOCATE (dayl(sim_len),STAT=error)
!    ALLOCATE (raim(sim_len),STAT=error)
!    ALLOCATE (raim_snow17(sim_len),STAT=error)
!    ALLOCATE (tair(sim_len),STAT=error)

!    ALLOCATE (streamflow(sim_len),STAT=error)
!    ALLOCATE (sneqv_verif(sim_len),STAT=error)
!    ALLOCATE (qs(sim_len),STAT=error)
!    ALLOCATE (qg(sim_len),STAT=error)
!    ALLOCATE (tci(sim_len),STAT=error)
!    ALLOCATE (route_tci(sim_len),STAT=error)
!    ALLOCATE (eta(sim_len),STAT=error)

!    ALLOCATE (snowh(sim_len),STAT=error)
!    ALLOCATE (sneqv(sim_len),STAT=error)
!    ALLOCATE (snow(sim_len),STAT=error)
!  endif


!allocate space for parameter array  !model options and dynamic allocation removed (AJN, 6/30/13)
!  if(model .eq. 1) then
    !!!if only running and optimizing snow-17
!    ALLOCATE (a(11),STAT=error)
!    ALLOCATE (bl(11),STAT=error)
!    ALLOCATE (bu(11),STAT=error)
!  elseif(model .eq. 2) then
    !!!if only running and optimizing sacsma
!    ALLOCATE (a(16),STAT=error)
!    ALLOCATE (bl(16),STAT=error)
!    ALLOCATE (bu(16),STAT=error)
!  elseif(model .eq. 3) then
    !!!if running snow-17/sacsma combo without optimizing adc
!    ALLOCATE (a(30),STAT=error)
!    ALLOCATE (bl(30),STAT=error)
!    ALLOCATE (bu(30),STAT=error)
!  endif




!allocate if validation period
!removed for now (AJN, 6/30/13)
!  if(val_period .ne. 0) then
!    ALLOCATE (year(val_length),STAT=error)
!    ALLOCATE (month(val_length),STAT=error)
!    ALLOCATE (day(val_length),STAT=error)
!    ALLOCATE (hour(val_length),STAT=error)
!    ALLOCATE (jday(val_length),STAT=error)

!    ALLOCATE (pet(val_length),STAT=error)
!    ALLOCATE (precip(val_length),STAT=error)
!    ALLOCATE (swdown(val_length),STAT=error)
!    ALLOCATE (vpd(val_length),STAT=error)
!    ALLOCATE (tmax(val_length),STAT=error)
!    ALLOCATE (tmin(val_length),STAT=error)
!    ALLOCATE (dayl(val_length),STAT=error)
!    ALLOCATE (raim(val_length),STAT=error)
!    ALLOCATE (raim_snow17(val_length),STAT=error)
!    ALLOCATE (tair(val_length),STAT=error)

!    ALLOCATE (sneqv_verif(val_length),STAT=error)
!    ALLOCATE (qs(val_length),STAT=error)
!    ALLOCATE (qg(val_length),STAT=error)
!    ALLOCATE (tci(val_length),STAT=error)
!    ALLOCATE (route_tci(val_length),STAT=error)
!    ALLOCATE (eta(val_length),STAT=error)

!    ALLOCATE (snowh(val_length),STAT=error)
!    ALLOCATE (sneqv(val_length),STAT=error)
!    ALLOCATE (snow(val_length),STAT=error)

!  end if


!first, find the proper time ranges for the gauge being worked with
  call get_start_points(obs_offset,obs_val_offset,forcing_offset,forcing_val_offset,val_length)

!read in verification streamflow data
!this determines the starting point for the calibration based on observed record...
  call read_streamflow(obs_offset,obs_val_offset,val_length)

!get forcing data
  call read_cida_areal_forcing(forcing_offset,forcing_val_offset,val_length)


!setup parameter, upper & lower bound arrays
  call sce_param_setup(a,bl,bu)


! open up ASCII output file for sce
  isce = 50 
  OPEN(unit=isce,FILE=TRIM(sce_fname),FORM='formatted')


!  opt = 1   !opt=1 runs sce code
!call sce
  if(opt .eq. 1) then

    call sceua(a,af,bl,bu,nopt,maxn,kstop,pcento,iseed,&
               ngs,npg,nps,nspl,mings,iniflg,iprint,isce)
  else

    if(val_period .eq. 0)then
      end_pt = sim_length
    else
      end_pt = val_length
    endif

    !setup a few parameters quick

    !which basin do we want?
    !read(forcing_name(36:44),'(i9)') bid
    bid = gage_id    

    !which seed are we on?
    !read(model_out(38:39),'(i2)') sid
    sid = iseed

    print *,trim(opt_name)

!read in region_%02d_opt file
    open(unit=99,file=opt_name,form='formatted')
    read(99,*) num_basin
    do i = 1,num_basin
      read(99,*) basin(i), seed(i)
      read(99,FMT='(21(F8.3))') params(i,:)
!print *,basin(i),seed(i)
    enddo


!find a match to the basin and seed we are currently processing
    do i = 1,num_basin
      if(bid .eq. basin(i) .and. sid .eq. seed(i)) then
	loc = i
!print *,loc,num_basin,basin(i)
      endif
!print *,basin(i),sid,num_basin,bid
    enddo


!place optimized parameters in to parameter arrays
    do i = 2,21
      a(i-1) = params(loc,i)
!      print *,params(loc,i)
    enddo
!print *,sid,bid,a
!place non optimized parameters (from namelist)
    a(21) = nmf(1)
    a(22) = tipm(1)
    a(23) = mbase(1)
    a(24) = plwhc(1)
    a(25) = daygm(1)
    a(26) = adimp(1)
    a(27) = pctim(1)
    a(28) = riva
    a(29) = side
    a(30) = rserv

!    print *,elev,a(1),a(2),a(3),a(4)
    !set surface pressure for snow17
    call sfc_pressure(elev,pa)

    call calc_pet_pt(a)

    if(val_period .ne. 0) then
	call get_model_state(cal_uztwc, cal_uzfwc, cal_lztwc, &
                             cal_lzfsc, cal_lzfpc, cal_adimc)

	uztwc = cal_uztwc
	uzfwc = cal_uzfwc
	lztwc = cal_lztwc
	lzfsc = cal_lzfsc
	lzfpc = cal_lzfpc
	adimc = cal_adimc

	print *,cal_uztwc, cal_uzfwc, cal_lztwc, &
                             cal_lzfsc, cal_lzfpc, cal_adimc
    else
      call spin_up_first_year(a, spinup_crit, uztwc, uzfwc, lztwc, &
                              lzfsc, lzfpc, adimc)
    endif

	print *,uztwc,uzfwc,lztwc, &
                lzfsc,lzfpc,adimc

    !setup a file for model state output quickly...
    pt1=trim(model_out)
    if(val_period .eq. 0) then
      pt2='.model_state'
    else
      pt2='.model_state_val'
    endif
    pt1 = trim(pt1)//trim(pt2)
!    print *,trim(pt1)

    open(unit=46,FILE=trim(pt1),FORM='formatted')


!set output, carry over variables to zero
    tprev = 0.0
    cs    = 0.0

    qs = 0.0
    qg = 0.0
    tci = 0.0
    eta = 0.0

!set single precision sac state variables
    uztwc_sp = real(uztwc,kind(sp))
    uzfwc_sp = real(uzfwc,kind(sp))
    lztwc_sp = real(lztwc,kind(sp))
    lzfsc_sp = real(lzfsc,kind(sp))
    lzfpc_sp = real(lzfpc,kind(sp))
    adimc_sp = real(adimc,kind(sp))

print *,'al;sdf',end_pt,count(valid)


    do i = 1,end_pt,1
    !set single precision inputs
      tair_sp   = real(tair(i),kind(sp))
      precip_sp = real(precip(i),kind(sp))
      pet_sp    = real(pet(i),kind(sp))

	CALL EXSNOW19(int(dt),int(dt/sec_hour),day(i),month(i),year(i),&
	  !SNOW17 INPUT AND OUTPUT VARIABLES
			    precip_sp,tair_sp,raim_snow17(i),sneqv(i),snow(i),snowh(i),&
	  !SNOW17 PARAMETERS
  !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
!			    alat,a(1),a(2),a(3),a(4),a(5),a(7),a(8),a(9),&
!			    a(6),a(10),a(11),elev,pa,adc(1),&
			    real(lat,kind(sp)),a(1),a(2),a(3),a(4),a(5),a(21),a(22),a(23),&
			    a(6),a(24),a(25),real(elev,kind(sp)),real(pa,kind(sp)),adc,&
	  !SNOW17 CARRYOVER VARIABLES
			    cs,tprev) 

 ! print *,'here 4'
	call exsac(1,real(dt),raim_snow17(i),tair_sp,pet_sp,&
	  !SAC PARAMETERS
!UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC, &
!REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE, &
!SIDE,RSERV, &
!a(12),a(13),a(18),a(23),a(17),a(25),a(21),a(22),a(14),a(16),a(15),a(20),a(19),a(24),a(26),a(27)
!			a(12),a(13),a(18),a(23),a(17),a(25),a(21),&
!			a(22),a(14),a(16),a(15),a(20),a(19),a(24),&
!			a(26),a(27),&
			a(7),a(8),a(12),a(27),a(26),a(28),a(15), &
			a(16),a(9),a(11),a(10),a(14),a(13),a(17),&
			a(29),a(30), &
	  !SAC State variables
			  uztwc_sp,uzfwc_sp,lztwc_sp,lzfsc_sp,lzfpc_sp,adimc_sp,&
	  !SAC OUTPUTS
			  qs(i),qg(i),tci(i),eta(i))
!print *,pet(i),a(20),tair(i),precip(i)

      write(unit=46,30) year(i),month(i),day(i),hour(i),uztwc_sp,uzfwc_sp,lztwc_sp,lzfsc_sp,lzfpc_sp,adimc_sp,eta(i)

    enddo  !end simulation loop
    close(unit=46)

    dtuh = real(dt/sec_day)

    if (a(18) .le. 0.0 .and. a(19) .le. 0.0) THEN
      k = 0
      m = 1
    else
      k = 1
      m = 1000
    end if
    ntau = 0

  !call unit hydrograph routine
    if(a(18) .gt. 0.0) then
      call DUAMEL(tci,1,unit_hydro,a(18),a(19),dtuh,end_pt-1,m,route_tci,k,ntau)
				!shape,scale
    endif


  !need to pass kind(dp) to objective function subroutines
  route_tci_dp = real(route_tci, kind(dp))
  streamflow_dp = real(streamflow, kind(dp))
  tci_dp = real(tci, kind(dp))

  if(a(18) .gt. 0.0) then
    if(trim(metric) .eq. "rmse" .or. trim(metric) .eq. "RMSE") then
      call calc_rmse(route_tci_dp,streamflow_dp,end_pt,valid,obj_val)
    elseif(trim(metric) .eq. "mse" .or. trim(metric) .eq. "MSE") then
      call calc_mse(route_tci_dp,streamflow_dp,end_pt,valid,obj_val)
    elseif(trim(metric) .eq. "nse" .or. trim(metric) .eq. "NSE") then
      call calc_nse(route_tci_dp,streamflow_dp,end_pt,valid,obj_val)
    elseif(trim(metric) .eq. "kge" .or. trim(metric) .eq. "KGE") then
      call calc_kge(route_tci_dp,streamflow_dp,end_pt,valid,obj_val)
    endif
  else
    if(trim(metric) .eq. "rmse" .or. trim(metric) .eq. "RMSE") then
      call calc_rmse(tci_dp,streamflow_dp,end_pt,valid,obj_val)
    elseif(trim(metric) .eq. "mse" .or. trim(metric) .eq. "MSE") then
      call calc_mse(tci_dp,streamflow_dp,end_pt,valid,obj_val)
    elseif(trim(metric) .eq. "nse" .or. trim(metric) .eq. "NSE") then
      call calc_nse(tci_dp,streamflow_dp,end_pt,valid,obj_val)
    elseif(trim(metric) .eq. "kge" .or. trim(metric) .eq. "KGE") then
      call calc_kge(tci_dp,streamflow_dp,end_pt,valid,obj_val)
    endif
  endif

    print *,'Objective function value: ',obj_val
    print *,'mean obs:',mean_obs


    !format statement for output
    30 FORMAT(I4.4, 3(1x,I2.2),7(F12.4))

    !output file name setup
    if(val_period .eq. 0) then   !calibration
      open(unit=45,FILE=trim(model_out),FORM='formatted')
    else	!validation
      pt1 = trim(model_out)
      pt2 = '_val'
      pt1 = trim(pt1)//trim(pt2)
      open(unit=45,FILE=trim(pt1),FORM='formatted')
    endif

    !output to a file
    do i = 1,end_pt
      if(a(18) .gt. 0) then
!	write(unit=45,30) year(i),month(i),day(i),hour(i),sneqv(i)*1000.,precip(i),raim_snow17(i),pet(i),tair(i),(route_tci(i)+qg(i)),streamflow(i)
	write(unit=45,30) year(i),month(i),day(i),hour(i),sneqv(i)*1000.,precip(i),raim_snow17(i),pet(i),tair(i),route_tci(i),streamflow(i)
      else
!	write(unit=45,30) year(i),month(i),day(i),hour(i),sneqv(i)*1000.,precip(i),raim_snow17(i),pet(i),tair(i),(tci(i)+qg(i)),streamflow(i)
	write(unit=45,30) year(i),month(i),day(i),hour(i),sneqv(i)*1000.,precip(i),raim_snow17(i),pet(i),tair(i),tci(i),streamflow(i)
      endif
    enddo
    close(unit=45)

  end if



  ! close ASCII output file
  CLOSE(isce)

end program
