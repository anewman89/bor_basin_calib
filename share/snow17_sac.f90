module snow17_sac
  implicit none



!initialization variables
  integer :: dt
  integer :: sim_len  
  character(len = 1024) :: forcing_name,stream_name,metric,model_out,opt_name
  real, dimension(6)    :: init_smois
  real,dimension(3)     :: pet_coef
  integer :: opt
  integer :: model
  integer :: start_calib,val_period,val_length,gage_id


!variables for snow17,pet,streamflow calculations
  real :: alat,lat
  real :: elev
  real :: area_basin


!SCE input parameters
  integer :: nopt             ! number of parameters to be optimized (NUMPAR in module multiparam)
  integer :: maxn             ! maximum number of trials before optimization is terminated
  integer :: kstop            ! number of shuffling loops the value must change by PCENTO (MAX=9)
  real    :: pcento       ! the percentage
  integer :: ngs              ! number of complexes in the initial population
  integer :: npg         ! number of points in each complex
  integer :: nps         ! number of points in a sub-complex
  integer :: nspl        ! number of evolution steps allowed for each complex before shuffling
  integer :: mings          ! minimum number of complexes required
  integer :: iniflg              ! 1 = include initial point in the population
  integer :: iprint               ! 0 = supress printing
  integer :: iseed         !starting seed for random number generator
  character(len = 1024) :: sce_fname

!SAC_model params
  real,dimension(3) :: uztwm,uzfwm,uzk,pctim,adimp,zperc,rexp
  real,dimension(3) :: lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree
  real,dimension(3) :: unit_shape,unit_scale
  real              :: riva,side,rserv


!Snow17_model params
  real,dimension(3)    :: scf,mfmax,mfmin,uadj,si,pxtemp
  real,dimension(3)    :: nmf,tipm,mbase,plwhc,daygm
  real, dimension(11)  :: adc


!date variables
!  integer, dimension(:),allocatable :: year,month,day,hour,jday
  integer, dimension(36500) :: year,month,day,hour,jday

!verification variables
!  real, dimension(:),   allocatable :: streamflow
!  real, dimension(:),   allocatable :: sneqv_verif
  real, dimension(36500) :: streamflow
  real, dimension(36500) :: sneqv_verif
  real                              :: mean_obs

!Forcing variables
  real, dimension(36500)    :: tmin,tmax,vpd,dayl,swdown,precip
!derived forcing variables
  real, dimension(36500) :: pet,tair

!sac-sma alone
!  real, dimension(:),   allocatable :: raim
  real, dimension(36500) :: raim

!namelists

!namelist for snow_17
!right now read in the following parameters with upper and lower bounds
!SCF, MFMAX,MFMIN,UADJ,SI, Areal depletion curve info, and PXTMP
!also read in: nmf,tipm,mbase,plwhc,daygm
  namelist / SNOW_17 / scf,mfmax,mfmin,uadj,si,adc,nmf,tipm,&
                       pxtemp,mbase,plwhc,daygm


!namelist for SAC-SMA
!read in the following parameters with upper and lower bounds
!UZTWM,UZFWM,UZK,PCTIM,ADIMP,ZPERC,REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE
  namelist / SAC_SMA / uztwm,uzfwm,uzk,pctim,adimp,zperc,rexp,&
                       lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,riva,side,rserv,&
                       unit_shape,unit_scale

!namelist for SCE-UA algorithm 
!read in the following parameters
  namelist / SCE / nopt, maxn, kstop, pcento, ngs, npg, nps, &
                   nspl, mings, iniflg, iprint,iseed,sce_fname

!init control namelist for running models, input/output files, etc...
  namelist / INIT_CONTROL / dt,sim_len,init_smois,forcing_name,opt,model,&
                            stream_name,start_calib,metric,model_out,    &
                            val_period,pet_coef,gage_id,opt_name

  save

  contains


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cc                                                               ccc
!cc                      SUBROUTINES!!!!!!!!!                     ccc
!cc 								  ccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!ccccccccccccccccccccccc
subroutine read_namelist
  
  implicit none

!local variables
  integer :: ierr

  open(UNIT=30, file="namelist.model",form="FORMATTED")

  read(UNIT=30, NML=INIT_CONTROL, iostat=ierr)
  if (ierr /= 0) then
    write(*,'(/," ***** ERROR: Problem reading namelist INIT_CONTROL",/)')
    rewind(UNIT=30)
    read(UNIT=30, NML=INIT_CONTROL)
    stop " ***** ERROR: Problem reading namelist INIT_CONTROL"
  endif

  read(UNIT=30, NML=SNOW_17, iostat=ierr)
  if (ierr /= 0) then
    write(*,'(/," ***** ERROR: Problem reading namelist SNOW_17",/)')
    rewind(UNIT=30)
    read(UNIT=30, NML=SNOW_17)
    stop " ***** ERROR: Problem reading namelist SNOW_17"
  endif

  read(UNIT=30, NML=SAC_SMA, iostat=ierr)
  if (ierr /= 0) then
    write(*,'(/," ***** ERROR: Problem reading namelist SAC_SMA",/)')
    rewind(UNIT=30)
    read(UNIT=30, NML=SAC_SMA)
    stop " ***** ERROR: Problem reading namelist SAC_SMA"
  endif

  read(UNIT=30, NML=SCE, iostat=ierr)
  if (ierr /= 0) then
    write(*,'(/," ***** ERROR: Problem reading namelist SCE",/)')
    rewind(UNIT=30)
    read(UNIT=30, NML=SCE)
    stop " ***** ERROR: Problem reading namelist SCE"
  endif

  close(UNIT=30)

  return
end subroutine

!cccccccccccccccccccccccccc
subroutine snow17_sacsma_rmse(a,rmse)
  implicit none
!input variables
  real, dimension(30), intent(inout) 	:: a

!output variables
  real, intent(out)            		:: rmse


!local variables
  integer :: i,h,k,m,ntau,end_pt,cnt,ll

  logical :: spin_up_flag

  real    :: dtuh

  real :: rmse_tmp

!sac-sma state variables
  real              :: uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc

!previous state for spinup check
  real              :: uztwc_prev,uzfwc_prev,lztwc_prev
  real              :: lzfsc_prev,lzfpc_prev,adimc_prev


!diff variables
  real              :: uztwc_diff,uzfwc_diff,lztwc_diff
  real              :: lzfsc_diff,lzfpc_diff,adimc_diff

!sac-sma output variables
  real, dimension(36500)    :: qs,qg,eta,tci,route_tci

!snow-17 surface pressure
  real :: pa

!snow-17 output variables
  real, dimension(36500)    :: snowh, sneqv, snow, raim_snow17 	!output variables

!snow-17 carry over variables
  real :: tprev				!carry over variable
  real, dimension(19)  :: cs		!carry over variable

!unit hydrograph
  real,dimension(1000)       :: unit_hydro

!!!!!!!!!!!!
!
!    code
!
!!!!!!!!!!!!

!set spin up flag
  spin_up_flag = .true.

  cnt = 0

!end point for calibration period
  end_pt = start_calib + sim_len

!setup initial state sac-sma
  uztwc = init_smois(1)
  uzfwc = init_smois(2)
  lztwc = init_smois(3)
  lzfsc = init_smois(4)
  lzfpc = init_smois(5)
  adimc = init_smois(6)

!reset snow-17 carryover variables
  tprev = 0.0
  cs = 0.0
!set sac output variables to zero
  qs = 0.0
  qg = 0.0
  tci = 0.0
  eta = 0.0
  unit_hydro = 0.0
  route_tci = 0.0

!set snow17 surface pressure
  pa   = 33.86 * (29.9 - (0.335 * (elev/100.0)) + (0.00022*((elev/100.)**2.4)))



  !print *,elev,lat

!need to set non-optimized parameters here again due to way sce passes things after first iteration...
!  a(20) = nmf(1)
!  a(21) = tipm(1)
!  a(22) = mbase(1)
!  a(23) = plwhc(1)
!  a(24) = daygm(1)
!  a(25) = adimp(1)
!  a(26) = pctim(1)
!  a(27) = riva
!  a(28) = side
!  a(29) = rserv

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
  

!combine snow-17/sac-sma calls into one loop

  if(val_period .eq. 0)then
    ll = sim_len
  else
    ll = val_length
  endif

  !first calc pet (6/11/13 change)
  call calc_pet_pt(a)

!print *,pet(200),a(20)
  do while (spin_up_flag)
    !run model combo over first year (use first full water year (day 274 to 638 if starting on 1 jan)
    !first set previous state variables to initial state variables
    uztwc_prev = uztwc
    uzfwc_prev = uzfwc
    lztwc_prev = lztwc
    lzfsc_prev = lzfsc
    lzfpc_prev = lzfpc
    adimc_prev = adimc
    !run first water year
!      do i = start_calib,start_calib+365
    do i = 1,365
!print *,i,pet(i),precip(i),tair(i)
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
    !the units of all six state variables are mm

    !state variables are:
    !uztwc:	upper-zone tension water storage content
    !uzfwc:	upper-zone free water storage content
    !lztwc:	lower-zone tension water storage content
    !lzfpc:	lower-zone free primary water storage content
    !lzfsc:	lower-zone free secondary water storage content
    !adimc:	additional impervious area content
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
!      print *,cnt,uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc

!      if(uztwc_diff .lt. 0.01 .and. uzfwc_diff .lt. 0.01 .and. lztwc_diff .lt. 0.01 .and.&
!         lzfsc_diff .lt. 0.01 .and. lzfpc_diff .lt. 0.01 .and. adimc_diff .lt. 0.01) then
    if(uztwc_diff .le. 0.1 .and. uzfwc_diff .le. 0.1 .and. lztwc_diff .le. 0.1 .and.&
	lzfsc_diff .le. 0.1 .and. lzfpc_diff .le. 0.1 .and. adimc_diff .le. 0.1) then
      spin_up_flag = .false.

    endif

    if(cnt .gt. 50) then
      spin_up_flag = .false.
!	print *,'failed'
    endif
  enddo  !end while loop for spin up

  !now run model for calibration
!    do i = start_calib,end_pt
    do i = 1,ll

      CALL EXSNOW19(int(dt),int(dt/3600.),day(i),month(i),year(i),&
	!SNOW17 INPUT AND OUTPUT VARIABLES
			  precip(i),tair(i),raim_snow17(i),sneqv(i),snow(i),snowh(i),&
	!SNOW17 PARAMETERS
!ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
!			    alat,a(1),a(2),a(3),a(4),a(5),a(7),a(8),a(9),&
!			    a(6),a(10),a(11),elev,pa,adc(1),&
!			    alat,a(1),a(2),a(3),a(4),a(5),a(20),a(21),a(22),&
!			    a(6),a(23),a(24),elev,pa,adc(1),&
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
!			a(7),a(8),a(12),a(26),a(25),a(27),a(15), &
!			a(16),a(9),a(11),a(10),a(14),a(13),a(17),&
!			a(28),a(29), &
		      a(7),a(8),a(12),a(27),a(26),a(28),a(15), &
		      a(16),a(9),a(11),a(10),a(14),a(13),a(17),&
		      a(29),a(30), &
	!SAC State variables
			uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,&
	!SAC OUTPUTS
			qs(i),qg(i),tci(i),eta(i))


  enddo

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
  endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      calculate rmse for daily streamflow
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!print *,'Model: ',qs(1),qg(1),tci(1),eta(1)

!check model flag
  if(model .eq. 1) then
    call calc_rmse(raim_snow17,raim,mean_obs,rmse_tmp)
  elseif(model .eq. 2 .or. model .eq. 3) then
    if(a(18) .gt. 0.0) then
!      call calc_rmse(route_tci+qg,streamflow,mean_obs,rmse_tmp)
      call calc_rmse(route_tci,streamflow,mean_obs,rmse_tmp)
    else
!      call calc_rmse(tci+qg,streamflow,mean_obs,rmse_tmp)      
      call calc_rmse(tci,streamflow,mean_obs,rmse_tmp)

    endif
  endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      test calc rmse for snow-17 raim output
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    call calc_rmse(raim_snow17,raim,rmse)
!    call calc_rmse(sneqv*1000,sneqv_verif,rmse)

  rmse = rmse_tmp
!print *,rmse
  return
end subroutine

!cccccccccccccccccccccccccc

subroutine calc_rmse(model,streamflow,mean_obs,rmse)

  implicit none

!input variables
  real, dimension(:),     intent(in)  :: model
  real, dimension(:),     intent(in)  :: streamflow
  real,                   intent(in)  :: mean_obs


!output variables
  real,			  intent(out) :: rmse


!local variables
  integer :: i,end_pt,ll
  double precision :: sum_sqr,sum_obs
  
!!! code


  sum_sqr = 0.0
  end_pt = sim_len

  if(val_period .eq. 0)then
    ll = sim_len
  else
    ll = val_length
  endif


  if(trim(metric) .eq. "rmse" .or. trim(metric) .eq. "RMSE") then
!    do i = start_calib,end_pt
     do i = 1,ll
      sum_sqr = sum_sqr + (model(i)-streamflow(i))**2
    enddo
    rmse = sqrt(sum_sqr/(real(ll)))
  elseif(trim(metric) .eq. "mse" .or. trim(metric) .eq. "MSE") then
!    do i = start_calib,end_pt
     do i = 1,ll
      sum_sqr = sum_sqr + (model(i)-streamflow(i))**2
    enddo
    rmse = sum_sqr/(real(ll))
  elseif(trim(metric) .eq. "nse" .or. trim(metric) .eq. "NSE") then
!    do i = start_calib,end_pt
     do i = 1,ll
      sum_sqr = sum_sqr + (model(i)-streamflow(i))**2
      sum_obs = sum_obs + (streamflow(i)-mean_obs)**2
    enddo
      rmse = -1*(1.0 - sum_sqr/sum_obs)
  endif

  return
end subroutine

!cccccccccccccccccccccccccc

subroutine get_model_state(cal_uztwc, cal_uzfwc, cal_lztwc, &
                           cal_lzfsc, cal_lzfpc, cal_adimc)

  implicit none

!input variables

!output variables
  real,intent(out) :: cal_uztwc, cal_uzfwc, cal_lztwc  !sac model state variables
  real,intent(out) :: cal_lzfsc, cal_lzfpc, cal_adimc

!local variables
  integer :: i,dum_int

  real    :: dum_flt

  character(len = 1024)  :: pt_1,pt_2  !file name character arrays


!code

  30 FORMAT(I4.4, 3(1x,I2.2),7(F12.4))

  pt_1 = model_out
  pt_2 = '.model_state'
  pt_1 = trim(pt_1)//trim(pt_2)


  open(UNIT=88,file=pt_1,form='formatted')
  
  do i = 1,sim_len
    read(UNIT=88,30) dum_int,dum_int,dum_int,dum_int, &
                     cal_uztwc,cal_uzfwc,cal_lztwc,cal_lzfsc, &
                     cal_lzfpc,cal_adimc,dum_flt

  enddo
  
  close(UNIT=88)


  return
end subroutine


!cccccccccccccccccccccccccc

subroutine read_cida_areal_forcing(forcing_name,start_offset,sim_len)

  implicit none

!input variables
  character(len = 1024),	intent(in)  :: forcing_name

  integer,			intent(in)  :: sim_len,start_offset
!output variables
!  real, dimension(:),         intent(inout) :: streamflow
!  integer, dimension(:),	intent(out) :: year,month,day,hour

!  real, dimension(:),   	intent(out) :: tmax,tmin,vpd,precip,swdown,dayl
!  real,                         intent(out) :: lat


!local variables
  integer :: i,jday_s,wy_jday,dum_int,i2,end_pt

  character(len=64), parameter :: read_format = "(I4.4, 3(1x,I2.2),1x,7(F10.2))"
  character(len = 1024)         :: dum_str
  real,dimension(36500)         :: swe
  real                          :: dum_real,sum_obs

!code
  sum_obs = 0.0
!read met file
  open (UNIT=50,file=trim(forcing_name),form='formatted',status='old')

  read (UNIT=50,FMT='(F7.2)') lat
  read (UNIT=50,FMT='(F7.2)') elev
  read (UNIT=50,FMT='(F10.0)') area_basin
  read (UNIT=50,FMT='(63A)') dum_str
  !read the rest of the input file
  !start offset is the first day of the first complete water year
  !this is the point at which we want to keep the forcing data
  !keep through the end of the sim_len
  !need to do this because daymet forcing starts 01 Jan 1980
  !observed streamflow varies in its start date by gauge

  if(val_period .eq. 0) then
    print *,start_offset,sim_len,start_offset+sim_len
    i2 = 0
    do i = 1,(start_offset + sim_len)
      if(i .ge. start_offset) then
 	i2 = i2 + 1
	read (UNIT=50,FMT=read_format) year(i2),month(i2),day(i2),hour(i2),&
				      dayl(i2),precip(i2),swdown(i2),swe(i2),tmax(i2),tmin(i2),vpd(i2)

      !need to compute tair too
	tair(i2) = (tmax(i2)+tmin(i2))/2.
      else
	read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,&
				    dum_real,dum_real,dum_real,dum_real,dum_real,dum_real,dum_real
      endif
    enddo
    !need to convert streamflow to mm/day
    do i = 1,sim_len
      !convert streamflow (cfs) to cms
      streamflow(i) = streamflow(i)*0.0283168  !now in cubic meters per second

      !need to convert to mm/day
      streamflow(i) = streamflow(i)*86400. !now in cubic meters per day m^3 day^-1
                                           !m^3/day

      !1 cubic meter per day is 1000 mm per square m -> mm*m^2/day
      streamflow(i) = streamflow(i)*1000./area_basin  !now in mm/day
      sum_obs = sum_obs + streamflow(i)
    enddo
    mean_obs = sum_obs/real(sim_len)
  !print *,'mean: ',mean_obs
  else
  !validation grab
  !now starting point is: start_offset + 15*365
  !end point is start_offset + 15*365 + val_length
    print *,start_offset,val_length,start_offset+sim_len+val_length
    i2 = 0
    do i = 1,(start_offset + sim_len + val_length)
      if(i .ge. start_offset+sim_len-1) then
	i2 = i2 + 1

	read (UNIT=50,FMT=read_format) year(i2),month(i2),day(i2),hour(i2),&
				      dayl(i2),precip(i2),swdown(i2),swe(i2),tmax(i2),tmin(i2),vpd(i2)
  !print *,i
      !need to compute tair too
	tair(i2) = (tmax(i2)+tmin(i2))/2.
      else
	read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,&
				    dum_real,dum_real,dum_real,dum_real,dum_real,dum_real,dum_real
      endif
    enddo
!print *,'cida',year(1),month(1),day(1),precip(1),streamflow(1)
  !need to convert streamflow to mm/day
    do i = 1,val_length
      !convert streamflow (cfs) to cms
      streamflow(i) = streamflow(i)*0.0283168  !now in cubic meters per second

      !need to convert to mm/day too
      streamflow(i) = streamflow(i)*86400. !now in cubic meters per day m^3 day^-1

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

end subroutine

!cccccccccccccccccccccccccccccccccccccccccc

subroutine read_mopex_forcing(forcing_name,sim_len)
  implicit none

!input variables
  character(len = 1024),	intent(in)  :: forcing_name
  integer,			intent(in)  :: sim_len

!output variables
!  integer, dimension(:),  intent(out) :: year,month,day,hour
!  real, dimension(:),     intent(out) :: raim
!  real, dimension(:),     intent(out) :: precip
!  real, dimension(:),     intent(out) :: pet
!  real, dimension(:),     intent(out) :: tair
!  real, dimension(:),     intent(out) :: streamflow
!  real, dimension(:),     intent(out) :: sneqv_verif
!  real,                   intent(out) :: mean_obs

!local variables
  integer              :: i,dum_int,end_pt,i2
  character(len=1000)  :: dum_string
  real                 :: est_et,est_q,surf_q,grnd_q,soil_m
  real                 :: sum_obs,dum_real

  character(len=64), parameter :: read_format = "(I4.4, 3(3x,I2.2),11(F10.2))"

!code
  sum_obs = 0.0

  end_pt = start_calib + sim_len

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
  do i = 1,start_calib+sim_len

    if(i .ge. start_calib .and. i .lt. end_pt) then
      i2 = i-start_calib+1
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
  mean_obs = sum_obs/real(sim_len)
!  print *,'read: ', year(1),month(1),day(1),hour(1),raim(1),streamflow(1)
  return
end subroutine

!ccccccccccccccccccccccccccccccccc


subroutine read_streamflow(stream_name,sim_len,start_offset)
  implicit none

!input variables
  character(len = 1024),	intent(in)  :: stream_name
  integer,			intent(in)  :: sim_len


!output variables
  integer,            intent(out) :: start_offset

!local variables
  integer :: i,i2,end_pt,dum_int,jday_s,wy_jday
  integer :: yr,mn,dy,gauge,cnt,ios,error
  real    :: dum_real,st_1

!this subroutine assumes streamflow is daily data of the following format:
  character(len=64), parameter :: read_format = "(I8.8,1x,I4.4, 2(1x,I2.2),1x,1(F8.2))"


!code

  cnt = 0

!read streamflow file
  open (UNIT=50,file=stream_name,form='formatted',status='old')

!read first line to get starting point
  read (UNIT=50,FMT=read_format) gauge,yr,mn,dy,st_1
  rewind(unit=50)

!ok, now need to find start of first complete wy...
!from first date, calculate julian day.

  call julianday_scalar(yr,mn,dy,jday_s)
  call julianday_scalar(yr,10,1,wy_jday)

!then can find how many days left until start of next wy
!wy starts on 01 October of every year,

  if(start_calib .eq. 0) then
    if(wy_jday .ge. jday_s) then
      start_calib = (wy_jday-jday_s)+1
    else
      start_calib = (365-jday_s) + wy_jday+1
    endif
  endif

  
  !calculate ending point based on starting point and simulation length,val period
!  end_pt = start_calib + sim_len


  !set offset to pass to cida daymet read... based off of years from 1980 +wy_jday
  !what is the total number of days from jan 1 1980 to 
  !start calibration date in streamflow record?
  !it is:  jday_s + (year-1980)*365 + start_calib
!  start_offset = (year-1980)*365 + wy_jday
  if(start_calib .eq. 0) then
    if(wy_jday .ge. jday_s) then
      start_offset = (yr-1980)*365 + jday_s + start_calib - 1
    else
      start_offset = (yr-1980)*365 + jday_s + start_calib - 1
    endif
  else
    start_offset = start_calib
  endif

print *,start_calib,start_offset,wy_jday,jday_s,yr
!  do i = 1,sim_len
  if(val_period .eq. 0) then
    streamflow(1) = st_1
    i2 = 0
    do i = 1,start_calib+sim_len

      if(i .ge. start_calib) then
!	i2 = i-start_calib+1
        i2 = i2 + 1

	read (UNIT=50,FMT=read_format) gauge,yr,mn,dy,streamflow(i2)

	if(i2 .eq. 1) then
	  print *,'streamflow calib: ',yr,mn,dy,streamflow(i2),i2,area_basin
	endif

      else
	read (UNIT=50,FMT=read_format) dum_int,dum_int,dum_int,dum_int,dum_real
      endif
    enddo
  else
  !now if val_period .ne. 0 read rest of file, count how many days there are,
  !and pass that streamflow info along as validation streamflow data...
  !along with appropriate start and stop points...
    i2 = 0
    do while(ios .ge. 0)
      cnt = cnt + 1
      if(cnt .ge. start_calib + 15*365-1) then
	i2 = i2+1
	read (UNIT=50,FMT=read_format,IOSTAT=ios) dum_int,dum_int,dum_int,dum_int,dum_real
      else
	read (UNIT=50,FMT=read_format,IOSTAT=ios) dum_int,dum_int,dum_int,dum_int,dum_real
      endif
    enddo
    rewind(unit=50)
    val_length = i2
!    print *,val_length
    !allocate proper space for validation streamflow data
    !allocate (streamflow(val_length),STAT=error)

    !read it in
    i2  = 0
    ios = 0
    cnt = 0
    do while(ios .ge. 0)
      cnt = cnt + 1
      if(cnt .ge. start_calib + 15*365-1) then
	i2 = i2 + 1
	read (UNIT=50,FMT=read_format,IOSTAT=ios) gauge,yr,mn,dy,streamflow(i2)
	if(i2 .le. 1) then
	  print *,'streamflow val: ',yr,mn,dy,streamflow(i2),i2,area_basin
	endif
      else
	read (UNIT=50,FMT=read_format,IOSTAT=ios) dum_int,dum_int,dum_int,dum_int,dum_real
      endif
    enddo
  endif !end val_period if check

!print *,'val',val_length,cnt
  close(UNIT=50)

  return
end subroutine


!ccccccccccccccccccccccccccccccccc

subroutine calc_pet_pt(a)
  implicit none

  !input variable
  real, dimension(30), intent(in) 	:: a
  !output variable
!  real,dimension(:),intent(out)  :: pet

!local variables
  integer          :: i,ll

  real             :: c_p = 1.013e-3
  real             :: l_v = 2.501
  real             :: e   = 0.622
  double precision :: sbc = 4.903e-9 !in MJ K-4 m-2 day-1
  real             :: pi  = 3.141592
  real             :: gsc = 0.0820
  real             :: apt              !p-t coefficient for aird regions...

  real               :: l
  real               :: g
  real               :: s
  real               :: tavg
  real               :: r_net
  real               :: r_nl
  real               :: r_ns
  real               :: f_c
  real               :: f_h
  real               :: pressure
  real               :: r_s
  real               :: r_a
  real               :: r_so
  real               :: d_r
  real               :: dec
  real               :: lat_rad
  real               :: sha
  real               :: e_a
  real               :: e_s


!!set a from namelist parameter
  apt = a(20)

!!calculate pressure from elevation using standard atmosphere (taken from Snow-17)
  pressure = 33.86 * (29.9 - (0.335 * (elev/100.0)) + (0.00022*((elev/100.)**2.4)))
  pressure = pressure/10. !in kPa



!now lets get going on p-t pet
  if(val_period .eq. 0)then
    ll = sim_len
  else
    ll = val_length
  endif

  do i = 1,ll
!    tavg = (tmax(i)+tmin(i))/2
    tavg = tair(i)
    l = 2.501 - 0.002361*tavg
    s = 0.04145*exp(0.06088*tavg)
    g = (c_p*pressure)/(e*l)

    e_s = 0.6108*exp((17.27*tavg)/(tavg+237.3))  !in kPa
!    e_a = -vpd(i)/1000. + e_s  !in kPa
    e_a = vpd(i)/1000.

    !radiation terms
    d_r = 1 + 0.033*cos(((2*pi)/365.) * jday(i))
    dec = 0.409*sin( (((2*pi)/365.) * jday(i)) - 1.39)
    lat_rad = (pi/180.)*lat
    sha = acos(-tan(lat_rad)*tan(dec))

    r_a = ((24.*60.)/pi)*gsc*d_r*((sha*sin(lat_rad)*sin(dec)) + (cos(lat_rad)*cos(dec)*sin(sha))) !in MJ m-2 day-1
    r_so = (0.75 + 2e-5*elev)*r_a !in MJ m-2 day-1
    r_s  = (swdown(i)*dayl(i)/86400.)*0.0864 !in MJ m-2 day-1
    r_ns = (1-0.20)*r_s  !assume a constant albedo of ~0.25    !in MJ m-2 day-1

    r_nl = sbc*((((tmax(i)+273.16)**4)+((tmin(i)+273.16)**4))/2)*(0.34-0.14*sqrt(e_a))*(1.35*(r_s/r_so)-0.35) !in MJ m-2 day-1

    r_net = r_ns - r_nl


    pet(i) = (1./l)*((s*r_net)/(s+g))*apt

  enddo

  return
end subroutine

!ccccccccccccccccccccccccccccccccc

subroutine calc_pet_pm(sim_len,year,jday,wspd,tair,rh,pressure,swdown,lat,elev)
  implicit none

!input variables
  integer,			  intent(in)  :: sim_len
  real,                           intent(in)  :: lat,elev

  integer, dimension(:),    intent(in)  :: jday
  integer, dimension(:),    intent(in)  :: year

  real, dimension(:), 	  intent(in)  :: wspd
  real, dimension(:),	  intent(in)  :: tair
  real, dimension(:),	  intent(in)  :: rh
  real, dimension(:), 	  intent(in)  :: pressure
  real, dimension(:),  	  intent(in)  :: swdown

!output variables
!  real, dimension(sim_len),	  intent(out)  :: pet

!local variables
  integer :: i,j

  real :: c_p = 1.013e-3
  real :: l_v = 2.45
  real :: e   = 0.622
  real :: pi  = 3.141592
  real :: gsc = 0.0820
  real :: cd_day = 0.25
  real :: cd_night = 1.7
  real :: cn_day = 66.
  real :: cn_night = 66.

  double precision :: sbc = 4.903e-9/24. !in MJ K-4 m-2 h-1

  real :: tmean,r_s_mean,u_2,slope_sat,pres,psy_const,e_s,e_a
  real :: d_r,dec,lat_rad,sha,r_a,r_so,r_ns,r_nl,r_n
  real,dimension(36500) :: r_s  
  real :: j_day
  real :: tmp_r_s


!calculate r_s first
  tmp_r_s = 0.0
  do i = 1,sim_len
    if(i .lt. 24) then
     do j = 1,48
        if(year(j) .eq. year(i) .and. jday(j) .eq. jday(i)) then
          tmp_r_s = tmp_r_s + swdown(i)
        endif
     enddo
    elseif(i .gt. sim_len-24) then
     do j = sim_len-48,sim_len
        if(year(j) .eq. year(i) .and. jday(j) .eq. jday(i)) then
          tmp_r_s = tmp_r_s + swdown(i)
        endif
     enddo
    else
     do j = i-25,i+25
        if(year(j) .eq. year(i) .and. jday(j) .eq. jday(i)) then
          tmp_r_s = tmp_r_s + swdown(i)
        endif
     enddo
    end if
    r_s(i) = tmp_r_s
    tmp_r_s = 0.0
  enddo

!get cracking
  do i = 1,sim_len
    
!temperature,wind,vapor pressure terms
    tmean = tair(i)-273.15 !convert to celcius
    r_s_mean = r_s(i) * 0.0864/24.0 !convert to MJ m-2 hr-1
    u_2 = wspd(i) * (4.87/log(67.8*3. - 5.42))
    slope_sat = (4098.0 * (0.6108*exp((17.27*tmean)/(tmean+237.3))))/((tmean+273.3)**2)
    pres = pressure(i)
    psy_const = (c_p*pres)/(e*l_v)
    e_s = 0.6108*exp((17.27*tmean)/(tmean+237.3))
    e_a = e_s * (rh(i)/100.0)

!radiation terms
    d_r = 1 + 0.033*cos(((2*pi)/365.) * jday(i))
    dec = 0.409*sin( (((2*pi)/365.) * jday(i)) - 1.39)
    lat_rad = (pi/180.)*lat
    sha = acos(-tan(lat_rad)*tan(dec))

    r_a = (60./pi)*gsc*d_r*((sha*sin(lat_rad)*sin(dec)) + (cos(lat_rad)*cos(dec)*sin(sha))) !in MJ m-2 h-1
    r_so = (0.75 + 2e-5*elev)*r_a !in MJ m-2 h-1
    r_ns = (1-0.23)*r_s_mean !in MJ m-2 h-1
    r_nl = sbc*((tmean+273.16)**4)*(0.34-0.14*sqrt(e_a))*(1.35*(r_s_mean/r_so)-0.35) !in MJ m-2 h-1
    r_n = r_ns - r_nl  !r_n in MJ m-2 hr-1

!pet finally (mm/hr-1)
    if(swdown(i) .gt. 0) then
      pet(i) = ( 0.408*slope_sat*r_n+psy_const*(cn_day/(tmean+273.15))*u_2*(e_s-e_a) )/(slope_sat + psy_const*(1+cd_day*u_2))
    else
      pet(i) = ( 0.408*slope_sat*r_n+psy_const*(cn_night/(tmean+273.15))*u_2*(e_s-e_a) )/(slope_sat + psy_const*(1+cd_night*u_2))
    endif  
  enddo

  return
end subroutine

!ccccccccccccccccccccccccccccccccc

subroutine julianday()
  implicit none
!
! Taken from Glen Liston's SnowModel code, updated to F90
!
!input variables
!  integer,                   intent(in) :: sim_len
!  integer,dimension(:),intent(in) :: iyear
!  integer,dimension(:),intent(in) :: imonth
!  integer,dimension(:),intent(in) :: iday

!output variables
!  integer,dimension(:),intent(out) :: J_day

!local variables
  integer      :: i


! Calculate the day of year (1...365,366) corresponding to the date
!   iyear-imonth-iday. 
  jday = day &
          + min(1,max(0,month-1))*31 &
          + min(1,max(0,month-2))*(28+(1-min(1,mod(year,4)))) &
          + min(1,max(0,month-3))*31 &
          + min(1,max(0,month-4))*30 &
          + min(1,max(0,month-5))*31 &
          + min(1,max(0,month-6))*30 &
          + min(1,max(0,month-7))*31 &
          + min(1,max(0,month-8))*31 &
          + min(1,max(0,month-9))*30 &
          + min(1,max(0,month-10))*31 &
          + min(1,max(0,month-11))*30 &
          + min(1,max(0,month-12))*31

  return
end subroutine

!ccccccccccccccccccccccccccccccccc

subroutine julianday_scalar(iyear,imonth,iday,jday_scalar)
  implicit none

!
!  Based on above subroutine from Glen Liston's SnowModel code
!

!input variables
  integer :: iyear
  integer :: imonth
  integer :: iday

!output variables
  integer,intent(out) :: jday_scalar



! Calculate the day of year (1...365,366) corresponding to the date
!daymet is always on 365 day calendar...
!but the last day of the year is chopped off.

  jday_scalar = iday &
          + min(1,max(0,imonth-1))*31 &
          + min(1,max(0,imonth-2))*(28+(1-min(1,mod(iyear,4)))) &
!	  + min(1,max(0,imonth-2))*28 &
          + min(1,max(0,imonth-3))*31 &
          + min(1,max(0,imonth-4))*30 &
          + min(1,max(0,imonth-5))*31 &
          + min(1,max(0,imonth-6))*30 &
          + min(1,max(0,imonth-7))*31 &
          + min(1,max(0,imonth-8))*31 &
          + min(1,max(0,imonth-9))*30 &
          + min(1,max(0,imonth-10))*31 &
          + min(1,max(0,imonth-11))*30 &
          + min(1,max(0,imonth-12))*31

  if(jday_scalar .eq. 366) then
    jday_scalar = 365
  endif

  return
end subroutine

!ccccccccccccccccccccccccccccccccc

subroutine sce_param_setup(a,bl,bu)
  implicit none
!output variables
  real,dimension(30),intent(out) :: a,bl,bu


!snow17
!don't optimize the adc, just use regionally appropriate ones in the namelist
!SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE, &
!PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC, &
  a(1) = scf(1)
  bl(1) = scf(2)
  bu(1) = scf(3)

  a(2) = mfmax(1)
  bl(2) = mfmax(2)
  bu(2) = mfmax(3)

  a(3) = mfmin(1)
  bl(3) = mfmin(2)
  bu(3) = mfmin(3)

  a(4) = uadj(1)
  bl(4) = uadj(2)
  bu(4) = uadj(3)

  a(5) = si(1)
  bl(5) = si(2)
  bu(5) = si(3)

  a(6) = pxtemp(1)
  bl(6) = pxtemp(2)
  bu(6) = pxtemp(3)

!sac-sma
!uztwm,uzfwm,uzk,pctim,adimp,zperc,rexp,
!lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,riva,side,rserv

!UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC, &
!REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE, &
!SIDE,RSERV, &
  a(7) = uztwm(1)
  bl(7) = uztwm(2)
  bu(7) = uztwm(3)

  a(8) = uzfwm(1)
  bl(8) = uzfwm(2)
  bu(8) = uzfwm(3)

  a(9) = lztwm(1)
  bl(9) = lztwm(2)
  bu(9) = lztwm(3)

  a(10) = lzfpm(1)
  bl(10) = lzfpm(2)
  bu(10) = lzfpm(3)

  a(11) = lzfsm(1)
  bl(11) = lzfsm(2)
  bu(11) = lzfsm(3)

  a(12) = uzk(1)
  bl(12) = uzk(2)
  bu(12) = uzk(3)

  a(13) = lzpk(1)
  bl(13) = lzpk(2)
  bu(13) = lzpk(3)

  a(14) = lzsk(1)
  bl(14) = lzsk(2)
  bu(14) = lzsk(3)

  a(15) = zperc(1)
  bl(15) = zperc(2)
  bu(15) = zperc(3)

  a(16) = rexp(1)
  bl(16) = rexp(2)
  bu(16) = rexp(3)

  a(17) = pfree(1)
  bl(17) = pfree(2)
  bu(17) = pfree(3)

!unit hydrograph parameters
  a(18) = unit_shape(1)
  bl(18) = unit_shape(2)
  bu(18) = unit_shape(3)

  a(19) = unit_scale(1)
  bl(19) = unit_scale(2)
  bu(19) = unit_scale(3)

!pet a parameter
  a(20) = pet_coef(1)
  bl(20) = pet_coef(2)
  bu(20) = pet_coef(3)

!non-optimized snow-17 parameters
!  a(20) = nmf(1)
!  bl(20) = nmf(2)
!  bu(20) = nmf(3)

!  a(21) = tipm(1)
!  bl(21) = tipm(2)
!  bu(21) = tipm(3)

!  a(22) = mbase(1)
!  bl(22) = mbase(2)
!  bu(22) = mbase(3)

!  a(23) = plwhc(1)
!  bl(23) = plwhc(2)
!  bu(23) = plwhc(3)

!  a(24) = daygm(1)
!  bl(24) = daygm(2)
!  bu(24) = daygm(3)

  a(21) = nmf(1)
  bl(21) = nmf(2)
  bu(21) = nmf(3)

  a(22) = tipm(1)
  bl(22) = tipm(2)
  bu(22) = tipm(3)

  a(23) = mbase(1)
  bl(23) = mbase(2)
  bu(23) = mbase(3)

  a(24) = plwhc(1)
  bl(24) = plwhc(2)
  bu(24) = plwhc(3)

  a(25) = daygm(1)
  bl(25) = daygm(2)
  bu(25) = daygm(3)

!non-optimized sac parameters
!  a(25) = adimp(1)
!  bl(25) = adimp(2)
!  bu(25) = adimp(3)

!  a(26) = pctim(1)
!  bl(26) = pctim(2)
!  bu(26) = pctim(3)

!  a(27) = riva
!  bl(27) = riva
!  bu(27) = riva

!  a(28) = side
!  bl(28) = side
!  bu(28) = side

!  a(29) = rserv
!  bl(29) = rserv
!  bu(29) = rserv

  a(26) = adimp(1)
  bl(26) = adimp(2)
  bu(26) = adimp(3)

  a(27) = pctim(1)
  bl(27) = pctim(2)
  bu(27) = pctim(3)

  a(28) = riva
  bl(28) = riva
  bu(28) = riva

  a(29) = side
  bl(29) = side
  bu(29) = side

  a(30) = rserv
  bl(30) = rserv
  bu(30) = rserv
  return
end subroutine

!ccccccccccccccccccccccccccccccccc


!these sce setup subroutines are no longer used
!held for future reference...  if ever needed.

subroutine sce_param_setup_snow17_opt_all(a,bl,bu)
  implicit none
!output variables
  real,dimension(22),intent(out) :: a,bl,bu


!snow17
  a(1) = scf(1)
  bl(1) = scf(2)
  bu(1) = scf(3)

  a(2) = mfmax(1)
  bl(2) = mfmax(2)
  bu(2) = mfmax(3)

  a(3) = mfmin(1)
  bl(3) = mfmin(2)
  bu(3) = mfmin(3)

  a(4) = uadj(1)
  bl(4) = uadj(2)
  bu(4) = uadj(3)

  a(5) = si(1)
  bl(5) = si(2)
  bu(5) = si(3)

  a(6) = pxtemp(1)
  bl(6) = pxtemp(2)
  bu(6) = pxtemp(3)

  a(7) = nmf(1)
  bl(7) = nmf(2)
  bu(7) = nmf(3)

  a(8) = tipm(1)
  bl(8) = tipm(2)
  bu(8) = tipm(3)

  a(9) = mbase(1)
  bl(9) = mbase(2)
  bu(9) = mbase(3)

  a(10) = plwhc(1)
  bl(10) = plwhc(2)
  bu(10) = plwhc(3)

  a(11) = daygm(1)
  bl(11) = daygm(2)
  bu(11) = daygm(3)

!snow-17 adc
  a(12) = adc(1)
  bl(12) = 0.
  bu(12) = 1.

  a(13) = adc(2)
  bl(13) = 0.
  bu(13) = 1.

  a(14) = adc(3)
  bl(14) = 0.
  bu(14) = 1.

  a(15) = adc(4)
  bl(15) = 0.
  bu(15) = 1.

  a(16) = adc(5)
  bl(16) = 0.
  bu(16) = 1.

  a(17) = adc(6)
  bl(17) = 0.
  bu(17) = 1.

  a(18) = adc(7)
  bl(18) = 0.
  bu(18) = 1.

  a(19) = adc(8)
  bl(19) = 0.
  bu(19) = 1.

  a(20) = adc(9)
  bl(20) = 0.
  bu(20) = 1.

  a(21) = adc(10)
  bl(21) = 0.
  bu(21) = 1.

  a(22) = adc(11)
  bl(22) = 0.
  bu(22) = 1.

  return
end subroutine


subroutine sce_param_setup_sacsma(a,bl,bu)
  implicit none
!output variables
  real,dimension(16),intent(out) :: a,bl,bu

!sac-sma
!uztwm,uzfwm,uzk,pctim,adimp,zperc,rexp,
!lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,riva,side,rserv
  a(1) = uztwm(1)
  bl(1) = uztwm(2)
  bu(1) = uztwm(3)

  a(2) = uzfwm(1)
  bl(2) = uzfwm(2)
  bu(2) = uzfwm(3)

  a(3) = lztwm(1)
  bl(3) = lztwm(2)
  bu(3) = lztwm(3)

  a(4) = lzfpm(1)
  bl(4) = lzfpm(2)
  bu(4) = lzfpm(3)

  a(5) = lzfsm(1)
  bl(5) = lzfsm(2)
  bu(5) = lzfsm(3)

  a(6) = adimp(1)
  bl(6) = adimp(2)
  bu(6) = adimp(3)

  a(7) = uzk(1)
  bl(7) = uzk(2)
  bu(7) = uzk(3)

  a(8) = lzpk(1)
  bl(8) = lzpk(2)
  bu(8) = lzpk(3)

  a(9) = lzsk(1)
  bl(9) = lzsk(2)
  bu(9) = lzsk(3)

  a(10) = zperc(1)
  bl(10) = zperc(2)
  bu(10) = zperc(3)

  a(11) = rexp(1)
  bl(11) = rexp(2)
  bu(11) = rexp(3)

  a(12) = pctim(1)
  bl(12) = pctim(2)
  bu(12) = pctim(3)

  a(13) = pfree(1)
  bl(13) = pfree(2)
  bu(13) = pfree(3)

!non-optimized sac parameters
  a(14) = riva
  bl(14) = riva
  bu(14) = riva

  a(15) = side
  bl(15) = side
  bu(15) = side

  a(16) = rserv
  bl(16) = rserv
  bu(16) = rserv

  return
end subroutine


subroutine sce_param_setup_snow17(a,bl,bu)
  implicit none
!output variables
  real,dimension(11),intent(out) :: a,bl,bu


!snow17
  a(1) = scf(1)
  bl(1) = scf(2)
  bu(1) = scf(3)

  a(2) = mfmax(1)
  bl(2) = mfmax(2)
  bu(2) = mfmax(3)

  a(3) = mfmin(1)
  bl(3) = mfmin(2)
  bu(3) = mfmin(3)

  a(4) = uadj(1)
  bl(4) = uadj(2)
  bu(4) = uadj(3)

  a(5) = si(1)
  bl(5) = si(2)
  bu(5) = si(3)

  a(6) = pxtemp(1)
  bl(6) = pxtemp(2)
  bu(6) = pxtemp(3)

  a(7) = nmf(1)
  bl(7) = nmf(2)
  bu(7) = nmf(3)

  a(8) = tipm(1)
  bl(8) = tipm(2)
  bu(8) = tipm(3)

  a(9) = mbase(1)
  bl(9) = mbase(2)
  bu(9) = mbase(3)

  a(10) = plwhc(1)
  bl(10) = plwhc(2)
  bu(10) = plwhc(3)

  a(11) = daygm(1)
  bl(11) = daygm(2)
  bu(11) = daygm(3)

!snow-17 adc
!  a(12) = adc(1)
!  bl(12) = 0.
!  bu(12) = 1.

!  a(13) = adc(2)
!  bl(13) = 0.
!  bu(13) = 1.

!  a(14) = adc(3)
!  bl(14) = 0.
!  bu(14) = 1.

!  a(15) = adc(4)
!  bl(15) = 0.
!  bu(15) = 1.

!  a(16) = adc(5)
!  bl(16) = 0.
!  bu(16) = 1.

!  a(17) = adc(6)
!  bl(17) = 0.
!  bu(17) = 1.

!  a(18) = adc(7)
!  bl(18) = 0.
!  bu(18) = 1.

!  a(19) = adc(8)
!  bl(19) = 0.
!  bu(19) = 1.

!  a(20) = adc(9)
!  bl(20) = 0.
!  bu(20) = 1.

!  a(21) = adc(10)
!  bl(21) = 0.
!  bu(21) = 1.

!  a(22) = adc(11)
!  bl(22) = 0.
!  bu(22) = 1.

  return
end subroutine

end module