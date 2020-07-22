module gauge_calib

  interface 

    subroutine julian_day
      use nrtype
      use snow17_sac, only: jday,year,month,day
    end subroutine julian_day


    subroutine julianday_scalar(iyear,imonth,iday,jday_scalar)
      use nrtype

      integer(I4B), intent(in) :: iyear
      integer(I4B), intent(in) :: imonth
      integer(I4B), intent(in) :: iday

      integer(I4B),intent(out) :: jday_scalar
    end subroutine julianday_scalar


    subroutine read_namelist(namelist_name)
      use nrtype
      !input variable
      character(len=2000),intent(in)    :: namelist_name

    end subroutine read_namelist

    subroutine snow17_sacsma_eval(a,obj_val)
      use nrtype
      use constants, only: sec_day, sec_hour

      real(sp), dimension(30), intent(inout):: a

      real(dp), intent(out)		:: obj_val
    end subroutine snow17_sacsma_eval


    subroutine spin_up_first_year(a, spinup_crit, uztwc, uzfwc, &
                                  lztwc, lzfsc, lzfpc, adimc)
      use nrtype

      real(dp), intent(in)		:: spinup_crit

      real(sp), dimension(30), intent(inout):: a
 
      real(dp), intent(out)		:: uztwc,uzfwc,lztwc
      real(dp), intent(out)		:: lzfsc,lzfpc,adimc
    end subroutine spin_up_first_year


    subroutine sfc_pressure(elev, pres)
      use nrtype
      use constants, only: sfc_pres_a,sfc_pres_b,sfc_pres_c,&
                       sfc_pres_d,sfc_pres_e

      real(dp), intent(in)		:: elev

      real(dp), intent(out)		:: pres
    end subroutine sfc_pressure


    subroutine calc_pet_pt(a)
      use constants
      use nrtype
      use snow17_sac, only: pet, tair, year, month, day, jday, &
                        vpd, swdown, dayl, tmax, tmin, elev, lat

      real(sp), intent(in), dimension(30) :: a
    end subroutine calc_pet_pt


    subroutine sce_param_setup(a,bl,bu)
      use nrtype
      use snow17_sac, only: scf,mfmax,mfmin,uadj,si,pxtemp, &
                        uztwm,uzfwm,lztwm,lzfpm,lzfsm,lzpk,lzsk, uzk, &
                        zperc,rexp,pctim, pfree,unit_shape,unit_scale,pet_coef, &
                        nmf,tipm,mbase,plwhc,daygm,adimp,riva,side,rserv

      real(sp), intent(out), dimension(30) :: a
      real(sp), intent(out), dimension(30) :: bl
      real(sp), intent(out), dimension(30) :: bu
    end subroutine sce_param_setup


    subroutine get_start_points(obs_offset,obs_val_offset,forcing_offset,forcing_val_offset,val_length)
      use nrtype
      use snow17_sac, only: stream_name, start_month, start_day, sim_length

      integer(I4B), intent(out)	:: obs_offset			!offset from start of obs file (in days) to correct starting point for calibration
      integer(I4B), intent(out)	:: forcing_offset		!offset from start of forcing data file (in days) to starting point for calibration
      integer(I4B), intent(out)	:: obs_val_offset		!offset from start of obs file (in days) to correct starting point for validation
      integer(I4B), intent(out)	:: forcing_val_offset		!offset from start of forcing data file (in days) to starting point for validation
      integer(I4B), intent(out)	:: val_length			!length of validation period in observed file based on calibration period ending point
    end subroutine get_start_points


    subroutine read_streamflow(stream_pos)
      use nrtype
      use constants,  only: cfs_cms, sec_day
      use snow17_sac, only: stream_name, sim_length, streamflow, val_period, &
			    area_basin,valid,stream_yr,stream_day,stream_month, &
			    end_yr,end_day,end_month

      integer(I4B),intent(in)	:: stream_pos
    end subroutine read_streamflow


    subroutine read_cida_areal_forcing()
      use nrtype
      use constants,  only: cfs_cms, sec_day
      use snow17_sac, only: forcing_name, sim_length, lat, elev, area_basin, &
			    year, month, day, hour, dayl, precip, swdown, &
			    tmax, tmin, tair, vpd, streamflow, mean_obs, &
			    val_period,start_yr,start_day,start_month, &
			    end_yr,end_day,end_month


    end subroutine read_cida_areal_forcing


    subroutine get_model_state(cal_uztwc, cal_uzfwc, cal_lztwc, &
                           cal_lzfsc, cal_lzfpc, cal_adimc)
      use nrtype
      use snow17_sac, only: model_out, sim_length

      real(dp),intent(out)	:: cal_uztwc, cal_uzfwc, cal_lztwc  !sac model state variables
      real(dp),intent(out)	:: cal_lzfsc, cal_lzfpc, cal_adimc
    end subroutine get_model_state


    subroutine calc_rmse(model, obs, length, valid,rmse)
      use nrtype

      real(dp), dimension(36500),     intent(in)  :: model
      real(dp), dimension(36500),     intent(in)  :: obs
      integer(I4B),	intent(in)		:: length
      logical, dimension(36500), intent(in)	:: valid

      real(dp),		  intent(out) :: rmse
    end subroutine calc_rmse


    subroutine calc_mse(model,obs,length, valid,mse)
      use nrtype

      real(dp), dimension(36500),     intent(in)  :: model
      real(dp), dimension(36500),     intent(in)  :: obs
      integer(I4B),	intent(in)		:: length
      logical, dimension(36500), intent(in)	:: valid

      real(dp),		  intent(out) :: mse
    end subroutine calc_mse


    subroutine calc_nse(model,obs,length, valid,nse)
      use nrtype

      real(dp), dimension(36500),     intent(in)  :: model
      real(dp), dimension(36500),     intent(in)  :: obs
      integer(I4B),	intent(in)		:: length
      logical, dimension(36500), intent(in)	:: valid

      real(dp),		  intent(out) :: nse
    end subroutine calc_nse


    subroutine calc_kge(model,obs,length, valid,kge)
      use nrtype

      real(dp), dimension(36500),     intent(in)  :: model
      real(dp), dimension(36500),     intent(in)  :: obs
      integer(I4B),	intent(in)		:: length
      logical, dimension(36500), intent(in)	:: valid

      real(dp),		  intent(out) :: kge
    end subroutine calc_kge

    subroutine calc_kge_volume(model,obs,length,valid,objfunc_weight,kge_volume)
      use nrtype
  
      !input variables
      real(dp), dimension(36500),     intent(in)    :: model
      real(dp), dimension(36500),     intent(in)    :: obs
      real(dp),                       intent(in)    :: objfunc_weight

      integer(I4B), intent(in)                      :: length
      logical, dimension(36500), intent(in)         :: valid

      !output variables
      real(dp),     intent(out) :: kge_volume
  
    end subroutine calc_kge_volume

    subroutine calc_volume_mae(model,obs,length,valid,volume_mae)
      use nrtype
      use snow17_sac, only: obs_month,obs_year

      !input variables
      real(dp), dimension(36500),     intent(in)    :: model
      real(dp), dimension(36500),     intent(in)    :: obs

      integer(I4B), intent(in)                      :: length
      logical, dimension(36500), intent(in)         :: valid

      !output variables
      real(dp),     intent(out)    :: volume_mae
    end subroutine calc_volume_mae

    subroutine pearson(model, obs, length,valid,corr)
      use nrtype

      real(dp), dimension(36500),intent(in)	:: model
      real(dp), dimension(36500),intent(in)	:: obs
      integer(I4B),	intent(in)			:: length
      logical, dimension(36500), intent(in)	:: valid

      real(dp),intent(out)		:: corr
    end subroutine pearson

  end interface

end module gauge_calib
