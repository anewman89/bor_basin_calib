subroutine calc_rmse(model,obs,length,valid,rmse)
  use nrtype

  implicit none

!input variables
  real(dp), dimension(36500),     intent(in)  :: model
  real(dp), dimension(36500),     intent(in)  :: obs
  
  integer(I4B),	intent(in)			:: length
  logical, dimension(36500),intent(in)		:: valid

!output variables
  real(dp),		  intent(out) :: rmse

!local variables
  integer(I4B) :: i
  integer(I4B) :: num_valid
  integer(I4B) :: end_pt
  real(dp) :: sum_sqr


!!! code

!  end_pt = size(model)

  end_pt = length

  num_valid = count(valid(1:end_pt))

!print *,'num missing',end_pt-num_valid
 
  sum_sqr = 0.0_dp

!print *,'rmse size arrays:',end_pt

!  do i = 1,end_pt
!    sum_sqr = sum_sqr + (model(i)-obs(i))**2
!  enddo

!  rmse = sqrt(sum_sqr/(real(end_pt)))

  sum_sqr = sum((model-obs)**2,MASK=valid)
  rmse = sqrt(sum_sqr/real(num_valid))

  return
end subroutine calc_rmse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine calc_mse(model,obs,length,valid,mse)
  use nrtype

  implicit none

!input variables
  real(dp), dimension(36500),     intent(in)  :: model
  real(dp), dimension(36500),     intent(in)  :: obs

  integer(I4B),	intent(in)			:: length
  logical, dimension(36500),intent(in)		:: valid


!output variables
  real(dp),		  intent(out) :: mse

!local variables
  integer(I4B) :: i
  integer(I4B) :: end_pt
  integer(I4B) :: num_valid
  real(dp) :: sum_sqr


!!! code

  sum_sqr = 0.0_dp

  end_pt = length

  num_valid = count(valid(1:end_pt))

!  end_pt = size(model)
!  end_pt = length

!  do i = 1,end_pt
!    sum_sqr = sum_sqr + (model(i)-obs(i))**2
!  enddo
!  mse = sum_sqr/(real(end_pt))


  sum_sqr = sum((model-obs)**2,MASK=valid)
  mse = sum_sqr/real(num_valid)

  return
end subroutine calc_mse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine calc_nse(model,obs,length,valid,nse)
  use nrtype
  use snow17_sac, only: mean_obs

  implicit none

!input variables
  real(dp), dimension(36500),     intent(in)  :: model
  real(dp), dimension(36500),     intent(in)  :: obs

  integer(I4B),	intent(in)			:: length
  logical, dimension(36500), intent(in)	:: valid

!output variables
  real(dp),		  intent(out) :: nse

!local variables
  integer(I4B) :: i
  integer(I4B) :: end_pt
  integer(I4B) :: num_valid

  real(dp) :: sum_sqr
  real(dp) :: sum_obs


!!! code

  sum_sqr = 0.0_dp
  sum_obs = 0.0_dp

!  end_pt = size(model)
  end_pt = length

  num_valid = count(valid(1:end_pt))

!print *,'num missing',end_pt,num_valid

!  do i = 1,end_pt
!    sum_sqr = sum_sqr + (model(i)-obs(i))**2
!    sum_obs = sum_obs + (obs(i)-mean_obs)**2
!  enddo
!  nse = -1.0_dp*(1.0 - sum_sqr/sum_obs)  !inverted sign for optimization

  sum_sqr = sum((model-obs)**2,valid)
  sum_obs = sum((obs-mean_obs)**2,valid)

  nse = -1.0_dp*(1.0_dp - sum_sqr/sum_obs)

  return
end subroutine calc_nse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine calc_kge(model,obs,length,valid,kge)
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
  logical, dimension(36500), intent(in)		:: valid

!output variables
  real(dp),     intent(out) :: kge

!local variables
  integer(I4B) :: itime
  real(dp) :: cc,alpha,betha,mu_s,mu_o,sigma_s,sigma_o
  integer(I4B) :: end_pt
  integer(I4B)	:: num_valid

!code
!  end_pt = size(model)
  end_pt = length
  num_valid = count(valid)

  mu_s = 0.0_dp
  mu_o = 0.0_dp
  sigma_s = 0.0_dp
  sigma_o = 0.0_dp
  !We first compute the mean
  mu_s = sum(model,valid)/real(num_valid,kind(dp))
  mu_o = sum(obs,valid)/real(num_valid,kind(dp))
  betha = mu_s/mu_o
  !Now we compute the standard deviation
!  do itime = 1,end_pt
!    sigma_s = sigma_s + (model(itime)-mu_s)**2
!    sigma_o = sigma_o + (obs(itime)-mu_o)**2
!  enddo

  sigma_s = sum((model-mu_s)**2,valid)
  sigma_o = sum((obs-mu_o)**2,valid)

  sigma_s = sqrt(mu_s/real(num_valid,kind(dp)))
  sigma_o = sqrt(mu_s/real(num_valid,kind(dp)))
  alpha = sigma_s/sigma_o

  !Compute linear correlation coefficient
  call pearson(model,obs,length,valid,cc)

  !inverted sign for minimization
  kge = -( 1.0 - sqrt((cc-1.0)**2 + (alpha-1.0)**2 + (betha-1.0)**2) )

  return
end subroutine calc_kge

subroutine pearson(model,obs,length,valid,corr)
  use nrtype

  implicit none

  !input variables
  real(dp), dimension(36500),intent(in)	:: model
  real(dp), dimension(36500),intent(in)	:: obs

  integer(I4B),	intent(in)		:: length
  logical, dimension(36500), intent(in)	:: valid

  !output variables
  real(dp), intent(out)			:: corr

  !local variables
  real(dp)	:: model_mean
  real(dp)	:: obs_mean
  real(dp)	:: model_var
  real(dp)	:: obs_var
  real(dp)	:: cov

  integer(dp)	:: end_pt
  integer(i4b)	:: num_valid


  !code

!  end_pt = size(model)
  end_pt = length
  num_valid = count(valid)

!compute means
  model_mean = sum(model,valid)/real(num_valid,kind(dp))
  obs_mean   = sum(obs,valid)/real(num_valid,kind(dp))

!compute variance,covariance
  model_var = sum((model - model_mean)**2,valid)
  obs_var   = sum((obs   - obs_mean)**2,valid)
  cov       = sum((model - model_mean)*(obs - obs_mean),valid)


!compute correlation
  corr = cov/(sqrt(model_var)*sqrt(obs_var))

  return
end subroutine pearson