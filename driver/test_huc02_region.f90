program test_upper_colo
  use snow17_sac

  implicit none



!local variables
  integer :: i,ntau,k,m,j,cnt,start_offset
  logical :: spin_up_flag
  integer :: error,isce,num_basin,ll
  !integer :: opt
  real :: rmse,dtuh

!  character(len=1024) :: opt_name
!  character(len=2)    :: huc_02
  character(len=512) :: pt1
  character(len=512) :: pt2

  !variables for SCE
!  real, dimension(:), allocatable   :: a      !parameter set
!  real                              :: af     !objective function value
!  real, dimension(:), allocatable   :: bl     !lower bounds on parameter set
!  real, dimension(:), allocatable   :: bu     !upper bounds on parameter set

  real, dimension(30)   :: a      !parameter set
  real                  :: af     !objective function value
  real, dimension(30)   :: bl     !lower bounds on parameter set
  real, dimension(30)   :: bu     !upper bounds on parameter set


!sac-sma state variables
  real              :: uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc

!previous state for spinup check
  real              :: uztwc_prev,uzfwc_prev,lztwc_prev
  real              :: lzfsc_prev,lzfpc_prev,adimc_prev

!diff variables
  real              :: uztwc_diff,uzfwc_diff,lztwc_diff
  real              :: lzfsc_diff,lzfpc_diff,adimc_diff

!state variables from end of calibration period
  real              :: cal_uztwc, cal_uzfwc, cal_lztwc
  real              :: cal_lzfsc, cal_lzfpc, cal_adimc


!sac-sma output variables
!  real, dimension(:), allocatable    :: qs,qg,eta,tci,route_tci

!snow-17 output variables
!  real, dimension(:), allocatable    :: snowh, sneqv, snow, raim_snow17      !output variables

!sac-sma output variables
  real, dimension(36500)    :: qs,qg,eta,tci,route_tci

!snow-17 output variables
  real, dimension(36500)    :: snowh, sneqv, snow, raim_snow17 	!output variables

!snow-17 carry over variables
  real :: tprev				!carry over variable
  real, dimension(19)  :: cs			!carry over variable

!snow-17 surface pressure
  real :: pa

!unit hydrograph
  real,dimension(1000)       :: unit_hydro

!single iteration parameter info
  integer                          :: bid,loc,sid
  integer,dimension(1000)            :: basin,seed
  real,   dimension(1000,21)         :: params


!
!   code starts below
!


!read namelists
  call read_namelist

!set spin_up_flag,cnt
  cnt = 0

  if(val_period .eq. 0) then
    spin_up_flag = .true.
  else
    spin_up_flag = .false.
  endif


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


!read in verification streamflow data
!this determines the starting point for the calibration based on observed record...
  call read_streamflow(stream_name,sim_len,start_offset)

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


!get forcing data
  call read_cida_areal_forcing(forcing_name,start_offset,sim_len)

  alat = lat

!calculate potential evapotranspiration via priestley-taylor method since daymet has no wind data
  call julianday()

!moved pet to sce portion (6/11/2013)
!  call calc_pet_pt(pet)

!setup parameter, upper & lower bound arrays

  call sce_param_setup(a,bl,bu)


! open up ASCII output file for sce
!  isce = 50 
!  OPEN(unit=isce,FILE=TRIM(sce_fname),FORM='formatted')

!  opt = 1   !opt=1 runs sce code
!call sce
  if(opt .eq. 1) then

    call sceua(a,af,bl,bu,nopt,maxn,kstop,pcento,iseed,&
               ngs,npg,nps,nspl,mings,iniflg,iprint,isce)
  else

    if(val_period .eq. 0)then
      ll = sim_len
    else
      ll = val_length
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
!    a(20) = nmf(1)
!    a(21) = tipm(1)
!    a(22) = mbase(1)
!    a(23) = plwhc(1)
!    a(24) = daygm(1)
!    a(25) = adimp(1)
!    a(26) = pctim(1)
!    a(27) = riva
!    a(28) = side
!    a(29) = rserv

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
!    print *,'here'


!    print *,elev,a(1),a(2),a(3),a(4)
    !set surface pressure for snow17
    pa   = 33.86 * (29.9 - (0.335 * (elev/100.0)) + (0.00022*((elev/100.)**2.4)))

    call calc_pet_pt(a)

  !set initial state
    uztwc = init_smois(1)
    uzfwc = init_smois(2)
    lztwc = init_smois(3)
    lzfsc = init_smois(4)
    lzfpc = init_smois(5)
    adimc = init_smois(6)

    tprev = 0.
    cs    = 0.

    qs = 0.0
    qg = 0.0
    tci = 0.0
    eta = 0.0
    
!    print *,'here'
    !need to do model spin up

    do while (spin_up_flag)
      !run model combo over first year (use first full water year,see namelist)
      !first set previous state variables to initial state variables
      uztwc_prev = uztwc
      uzfwc_prev = uzfwc
      lztwc_prev = lztwc
      lzfsc_prev = lzfsc
      lzfpc_prev = lzfpc
      adimc_prev = adimc
      !run first complete water year
      do i = 1,365

	CALL EXSNOW19(int(dt),int(dt/3600.),day(i),month(i),year(i),&
	  !SNOW17 INPUT AND OUTPUT VARIABLES
			    precip(i),tair(i),raim_snow17(i),sneqv(i),snow(i),snowh(i),&
	  !SNOW17 PARAMETERS
  !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
!			    alat,a(1),a(2),a(3),a(4),a(5),a(7),a(8),a(9),&
!			    a(6),a(10),a(11),elev,pa,adc(1),&
			    alat,a(1),a(2),a(3),a(4),a(5),a(21),a(22),a(23),&
			    a(6),a(24),a(25),elev,pa,adc,&
	  !SNOW17 CARRYOVER VARIABLES
			    cs(1),tprev) 

 ! print *,'here 4'
	call exsac(1,real(dt),raim_snow17(i),tair(i),pet(i),&
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
			  uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,&
	  !SAC OUTPUTS
			  qs(i),qg(i),tci(i),eta(i))

      enddo !end model loop

      !check for convergence
      !use diff of all state variables.
      uztwc_diff = abs(uztwc-uztwc_prev)
      uzfwc_diff = abs(uzfwc-uzfwc_prev)
      lztwc_diff = abs(lztwc-lztwc_prev)
      lzfsc_diff = abs(lzfsc-lzfsc_prev)
      lzfpc_diff = abs(lzfpc-lzfpc_prev)
      adimc_diff = abs(adimc-adimc_prev)


      cnt = cnt + 1
      !print *,cnt,uztwc_diff,uzfwc_diff,lztwc_diff,lzfsc_diff,lzfpc_diff,adimc_diff
      !print *,cnt,lztwc,lztwc_prev,lztwc_diff
      !print *,cnt,lzfpc,lzfpc_prev,lzfpc_diff

      if(uztwc_diff .lt. 0.1 .and. uzfwc_diff .lt. 0.1 .and. lztwc_diff .lt. 0.1 .and.&
         lzfsc_diff .lt. 0.1 .and. lzfpc_diff .lt. 0.1 .and. adimc_diff .lt. 0.1) then

	spin_up_flag = .false.

      endif

    enddo  !end while loop for spin up


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
    endif

    !steup a file for model state output quickly...
    pt1=trim(model_out)
    if(val_period .eq. 0) then
      pt2='.model_state'
    else
      pt2='.model_state_val'
    endif
    pt1 = trim(pt1)//trim(pt2)
!    print *,trim(pt1)

    open(unit=46,FILE=trim(pt1),FORM='formatted')

    do i = 1,ll

	CALL EXSNOW19(int(dt),int(dt/3600.),day(i),month(i),year(i),&
	  !SNOW17 INPUT AND OUTPUT VARIABLES
			    precip(i),tair(i),raim_snow17(i),sneqv(i),snow(i),snowh(i),&
	  !SNOW17 PARAMETERS
  !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
!			    alat,a(1),a(2),a(3),a(4),a(5),a(7),a(8),a(9),&
!			    a(6),a(10),a(11),elev,pa,adc(1),&
			    alat,a(1),a(2),a(3),a(4),a(5),a(21),a(22),a(23),&
			    a(6),a(24),a(25),elev,pa,adc,&
	  !SNOW17 CARRYOVER VARIABLES
			    cs(1),tprev) 

 ! print *,'here 4'
	call exsac(1,real(dt),raim_snow17(i),tair(i),pet(i),&
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
			  uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,&
	  !SAC OUTPUTS
			  qs(i),qg(i),tci(i),eta(i))
!print *,pet(i),a(20),tair(i),precip(i)

      write(unit=46,30) year(i),month(i),day(i),hour(i),uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,eta(i)

    enddo  !end simulation loop
    close(unit=46)

    dtuh = real(dt/86400.)

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
      call DUAMEL(tci,1,unit_hydro,a(18),a(19),dtuh,ll-1,m,route_tci,k,ntau)
				!shape,scale

!      call calc_rmse(route_tci+qg,streamflow,mean_obs,rmse)
      call calc_rmse(route_tci,streamflow,mean_obs,rmse)
      if(trim(metric) .eq. "rmse" .or. trim(metric) .eq. "RMSE") then
	print *,'rmse or mse: ',rmse
	print *,'mean obs:',mean_obs
      else
	print *,'nse: ',rmse*-1
	print *,'mean obs:',mean_obs
      end if
    else
!      call calc_rmse(tci+qg,streamflow,mean_obs,rmse)
      call calc_rmse(tci,streamflow,mean_obs,rmse)
      print *,'score: ',rmse
      print *,'mean obs:',mean_obs
    endif
    !format statement for output
    30 FORMAT(I4.4, 3(1x,I2.2),7(F12.4))
    if(val_period .eq. 0) then
      open(unit=45,FILE=trim(model_out),FORM='formatted')
    else
      pt1 = trim(model_out)
      pt2 = '_val'
      pt1 = trim(pt1)//trim(pt2)
      open(unit=45,FILE=trim(pt1),FORM='formatted')
    endif
    do i = 1,sim_len
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
  !CLOSE(isce)

end program

