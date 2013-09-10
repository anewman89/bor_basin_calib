subroutine calc_rmse(model,obs,length,rmse)
  use nrtype

  implicit none

!input variables
  real(dp), dimension(36500),     intent(in)  :: model
  real(dp), dimension(36500),     intent(in)  :: obs
  
  integer(I4B),	intent(in)			:: length

!output variables
  real(dp),		  intent(out) :: rmse

!local variables
  integer(I4B) :: i
  integer(I4B) :: end_pt
  real(dp) :: sum_sqr


!!! code

!  end_pt = size(model)

  end_pt = length

  sum_sqr = 0.0_dp

!print *,'rmse size arrays:',end_pt

  do i = 1,end_pt
    sum_sqr = sum_sqr + (model(i)-obs(i))**2
  enddo
  rmse = sqrt(sum_sqr/(real(end_pt)))

  return
end subroutine calc_rmse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine calc_mse(model,obs,length,mse)
  use nrtype

  implicit none

!input variables
  real(dp), dimension(36500),     intent(in)  :: model
  real(dp), dimension(36500),     intent(in)  :: obs

  integer(I4B),	intent(in)			:: length

!output variables
  real(dp),		  intent(out) :: mse

!local variables
  integer(I4B) :: i
  integer(I4B) :: end_pt
  real(dp) :: sum_sqr


!!! code

  sum_sqr = 0.0_dp

!  end_pt = size(model)
  end_pt = length

  do i = 1,end_pt
    sum_sqr = sum_sqr + (model(i)-obs(i))**2
  enddo
  mse = sum_sqr/(real(end_pt))

  return
end subroutine calc_mse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine calc_nse(model,obs,length,nse)
  use nrtype
  use snow17_sac, only: mean_obs

  implicit none

!input variables
  real(dp), dimension(36500),     intent(in)  :: model
  real(dp), dimension(36500),     intent(in)  :: obs

  integer(I4B),	intent(in)			:: length

!output variables
  real(dp),		  intent(out) :: nse

!local variables
  integer(I4B) :: i
  integer(I4B) :: end_pt

  real(dp) :: sum_sqr
  real(dp) :: sum_obs


!!! code

  sum_sqr = 0.0_dp

!  end_pt = size(model)
  end_pt = length

  do i = 1,end_pt
    sum_sqr = sum_sqr + (model(i)-obs(i))**2
    sum_obs = sum_obs + (obs(i)-mean_obs)**2
  enddo
  nse = -1*(1.0 - sum_sqr/sum_obs)  !inverted sign for optimization

  return
end subroutine calc_nse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine calc_kge(model,obs,length,kge)
  use nrtype
  use gauge_calib, only: pearson
  implicit none

! This subroutine calculates the negative of the Kling-Gupta Efficiency
! in order to solve a minimization problem
! from Pablo Mendoza
! modified by Andy Newman


!input variables
  real(dp), dimension(36500),     intent(in)  :: model
  real(dp), dimension(36500),     intent(in)  :: obs

  integer(I4B),	intent(in)			:: length

!output variables
  real(dp),     intent(out) :: kge

!local variables
  integer(I4B) :: itime
  real(dp) :: cc,alpha,betha,mu_s,mu_o,sigma_s,sigma_o
  integer(I4B) :: end_pt

!code
!  end_pt = size(model)
  end_pt = length

  mu_s = 0.0_dp
  mu_o = 0.0_dp
  sigma_s = 0.0_dp
  sigma_o = 0.0_dp
  !We first compute the mean
  mu_s = sum(model)/real(end_pt,kind(dp))
  mu_o = sum(obs)/real(end_pt,kind(dp))
  betha = mu_s/mu_o
  !Now we compute the standard deviation
  do itime = 1,end_pt
    sigma_s = sigma_s + (model(itime)-mu_s)**2
    sigma_o = sigma_o + (obs(itime)-mu_o)**2
  enddo
  sigma_s = sqrt(mu_s/real(end_pt,kind(dp)))
  sigma_o = sqrt(mu_s/real(end_pt,kind(dp)))
  alpha = sigma_s/sigma_o

  !Compute linear correlation coefficient
  call pearson(model,obs,length,cc)

  !inverted sign for minimization
  kge = -( 1.0 - sqrt((cc-1.0)**2 + (alpha-1.0)**2 + (betha-1.0)**2) )

  return
end subroutine calc_kge

subroutine pearson(model,obs,length,corr)
  use nrtype

  implicit none

  !input variables
  real(dp), dimension(36500),intent(in)	:: model
  real(dp), dimension(36500),intent(in)	:: obs

  integer(I4B),	intent(in)			:: length

  !output variables
  real(dp), intent(out)			:: corr

  !local variables
  real(dp)	:: model_mean
  real(dp)	:: obs_mean
  real(dp)	:: model_var
  real(dp)	:: obs_var
  real(dp)	:: cov

  integer(dp)	:: end_pt


  !code

!  end_pt = size(model)
  end_pt = length

!compute means
  model_mean = sum(model)/real(end_pt,kind(dp))
  obs_mean   = sum(obs)/real(end_pt,kind(dp))

!compute variance,covariance
  model_var = sum((model - model_mean)**2)
  obs_var   = sum((obs   - obs_mean)**2)
  cov       = sum((model - model_mean)*(obs - obs_mean))


!compute correlation
  corr = cov/(sqrt(model_var)*sqrt(obs_var))

  return
end subroutine pearson