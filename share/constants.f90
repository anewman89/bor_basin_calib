MODULE constants
  use nrtype

  implicit none

  real(DP), parameter	:: sec_day=86400.0_dp			!seconds in a day
  real(DP), parameter	:: cfs_cms=0.0283168_dp  		!cubic feet per second to cubic meters per second
  real(DP), parameter	:: c_p=1.013e-3_dp			!specific heat of air at constant pressure (MJ kg-1 oC-1)
  real(DP), parameter	:: l_v=2.501				!latent heat of vaporization (MJ kg-1)
  real(DP), parameter	:: e=0.622				!ratio of weight of water to dry air
  real(DP), parameter	:: sbc=4.903e-9_dp			!stefan-boltzmann constant (MJ K-4 m-2 day-1)
  real(DP), parameter	:: gsc=0.0820				!solar constant (MJ m-2 min-1)


END MODULE constants