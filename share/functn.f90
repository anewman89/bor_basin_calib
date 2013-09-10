FUNCTION FUNCTN(NOPT,A)
  USE snow17_sac
  IMPLICIT NONE
!input variables
  integer,                intent(in) :: nopt
  real, dimension(30), intent(inout) :: a

!local variables 
  real :: rmse
  real :: functn


  call snow17_sacsma_rmse(a,rmse)

! save objective function value
  FUNCTN = rmse 
! ---------------------------------------------------------------------------------------
END FUNCTION FUNCTN