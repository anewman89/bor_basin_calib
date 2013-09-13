module snow17_sac
  use nrtype

  implicit none

!initialization variables
!these are in the namelist bloc &INIT_CONTROL
  integer(I4B)			:: dt			!model time step (seconds)
  integer(I4B) 			:: sim_length  		!simulation length, number of time steps
  character(len = 1024) 	:: forcing_name		!name of forcing data file 
  character(len = 1024) 	:: stream_name		!name of observed streamflow data
  character(len = 1024) 	:: metric		!name of metric for objective function
  character(len = 1024) 	:: model_out		!base name for output files
  character(len = 1024) 	:: opt_name		!optimal parameter file name
  real(sp), dimension(6)     	:: init_smois		!initial soil moisture states for sac
  real(sp), dimension(3)     	:: pet_coef		!coefficient for p-t pet calculation
  integer(I4B) 			:: opt			!flag for running sce or not
  integer(I4B) 			:: val_period		!flag for validation or calibration period
  integer(I4B) 			:: gage_id		!usgs gage id 
  integer(I4B)			:: start_month		!starting month for calibration period
  integer(I4B)			:: start_day		!starting day for calibration period


!these are not in the namelist block
!  integer(I4B) 			:: val_length		!length of validation period
								!remove from this module.  now passed through functions

!  integer(I4B) 			:: val_length		!remove this variable (AJN 9/9/2013)
!  integer(I4B) 			:: model		!unused (check this for sure, AJN 9/9/2013)


!variables for snow17,pet,streamflow calculations
!read in from the forcing file
  real(dp) :: lat
  real(dp) :: elev
  real(dp) :: area_basin

!  real(dp) :: alat        !remove this variable (AJN 9/9/2013)

!SCE input parameters
!these variables are in the namelist block &SCE
  integer :: nopt             ! number of parameters to be optimized (NUMPAR in module multiparam)
  integer :: maxn             ! maximum number of trials before optimization is terminated
  integer :: kstop            ! number of shuffling loops the value must change by PCENTO (MAX=9)
  real(sp):: pcento       ! the percentage
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
!in the namelist block &SAC_SMA
  real(sp),dimension(3) :: uztwm,uzfwm,uzk,pctim,adimp,zperc,rexp
  real(sp),dimension(3) :: lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree
  real(sp),dimension(3) :: unit_shape,unit_scale
  real(sp)              :: riva,side,rserv


!Snow17_model params
!in the namelist block &SNOW_17
  real(sp),dimension(3)    :: scf,mfmax,mfmin,uadj,si,pxtemp
  real(sp),dimension(3)    :: nmf,tipm,mbase,plwhc,daygm
  real(sp), dimension(11)  :: adc


!date variables
!  integer, dimension(:),allocatable :: year,month,day,hour,jday
  integer(I4B), dimension(36500) :: year,month,day,hour,jday

!observed streamflow
!  real, dimension(:),   allocatable :: streamflow
  real(dp), dimension(36500)	:: streamflow
  real(dp)			:: mean_obs

!variables from mopex forcing data
!  real, dimension(:),   allocatable :: sneqv_verif
  real(dp), dimension(36500) :: sneqv_verif
!  real, dimension(:),   allocatable :: raim
  real(dp), dimension(36500) :: raim

!Atmospheric forcing variables
  real(dp), dimension(36500)    :: tmin,tmax,vpd,dayl,swdown,precip
!derived forcing variables
  real(dp), dimension(36500) :: pet,tair


  logical, dimension(36500)  :: valid


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
!  namelist / INIT_CONTROL / dt,sim_len,init_smois,forcing_name,opt,model,&
!                            stream_name,start_calib,metric,model_out,    &
!                            val_period,pet_coef,gage_id,opt_name

  namelist / INIT_CONTROL / dt,sim_length,init_smois,forcing_name,opt, &
                            stream_name,metric,model_out,start_day,start_month, &
                            val_period,pet_coef,gage_id,opt_name
  save
end module
