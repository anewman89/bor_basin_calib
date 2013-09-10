
!ccccccccccccccccccccccccccccccccc

subroutine calc_pet_pt(a)
  use constants
  use nrtype
  use snow17_sac, only: pet, tair, year, month, day, jday, &
                        vpd, swdown, dayl, tmax, tmin, elev, lat, &
			sim_length

  implicit none

  interface 

    subroutine julian_day
      use nrtype
      use snow17_sac, only: jday,year,month,day
    end subroutine julian_day

  end interface

  !input variable
  real(sp), dimension(30), intent(in) 	:: a

!local variables
  integer(I4B)          :: i
  integer(I4B)		:: end_pt
  
  real(DP)               :: albedo	!albedo for pet calculation
  real(DP)             	 :: apt		!p-t coefficient for aird regions...

  real(DP)               :: l		!latent heat of vaporization (MJ kg-1)
  real(DP)               :: g		!psychrometric constant
  real(DP)               :: s		!slope of the saturation vapour pressure-temperature relationship
  real(DP)               :: tavg	!daily average temperature ( deg C )
  real(DP)               :: r_net	!daily net radiation (MJ m-2 day-1)
  real(DP)               :: r_nl	!daily net longwave (MJ m-2 day-1)
  real(DP)               :: r_ns	!daily net shortwave (MJ m-2 day-1)
  real(DP)               :: pressure	!surface pressure (kPa)
  real(DP)               :: r_s		!daily estimated solar from forcing dataset (MJ m-2 day-1)
  real(DP)               :: r_a		!daily extraterrestrial radiation (MJ m-2 day-1)
  real(DP)               :: r_so	!daily clear sky solar shortwave (MJ m-2 day-1)
  real(DP)               :: d_r		!inverse relative earth-sun distance
  real(DP)               :: dec		!solar declination in radians
  real(DP)               :: lat_rad	!latitude in radians
  real(DP)               :: sha		!sunset hour angle
  real(DP)               :: e_a		!vapor pressure (kPa)
  real(DP)               :: e_s		!saturation vapor pressure (kPa)


!!set pet coefficient from namelist parameter
  apt = real(a(20),kind(dp))

!!set albedo
  albedo = 0.20_dp

!!calculate pressure from elevation using standard atmosphere (taken from Snow-17)
  call sfc_pressure(elev,pressure) !pressure in hPa
  pressure = pressure/10.0_dp !in kPa

!get day of year array setup
  call julianday

!how many days to do?
!  end_pt = size(pet)
  end_pt = sim_length

!now lets get going on p-t pet
  do i = 1,end_pt
!    tavg = (tmax(i)+tmin(i))/2
    tavg = tair(i)
    l = l_v - tadj*tavg   !(MJ kg-1)
!    l = 2.501 - 0.002361*tavg
    s = slope_svpc_a*exp(slope_svpc_b*tavg)
!    s = 0.04145*exp(0.06088*tavg)
    g = (c_p*pressure)/(e*l)

!    e_s = 0.6108*exp((17.27*tavg)/(tavg+237.3))  !in kPa
    e_s = e_sa*exp((e_sb*tavg)/(tavg+e_sc)) !in kPa
!    e_a = -vpd(i)/1000. + e_s  !in kPa
    e_a = vpd(i)/1000.0_dp

    !radiation terms
!    d_r = 1 + 0.033*cos(((2*pi)/365.) * jday(i))
    d_r = 1 + sun_e_inv_d*cos(((2*pi_d)/365.0_dp) * jday(i))
!    dec = 0.409*sin( (((2*pi)/365.) * jday(i)) - 1.39)
    dec = sol_dec_a*sin( (((2*pi_d)/365.0_dp) * jday(i)) - sol_dec_b)
    lat_rad = (pi_d/180.0_dp)*lat
    sha = acos(-tan(lat_rad)*tan(dec))

    r_a = ((24.*60.)/pi_d)*gsc*d_r*((sha*sin(lat_rad)*sin(dec)) + (cos(lat_rad)*cos(dec)*sin(sha))) !in MJ m-2 day-1
!    r_so = (0.75 + 2e-5*elev)*r_a !in MJ m-2 day-1
    r_so = (clear_sky_a + clear_sky_b*elev)*r_a !in MJ m-2 day-1
!    r_s  = (swdown(i)*dayl(i)/86400.)*0.0864 !in MJ m-2 day-1
    r_s = (swdown(i)*dayl(i)/86400.)*w_to_mj !in MJ m-2 day-1
    r_ns = (1.0_dp-albedo)*r_s  !assume a constant albedo of ~0.20    !in MJ m-2 day-1

!    r_nl = sbc*((((tmax(i)+273.16)**4)+((tmin(i)+273.16)**4))/2)*(0.34-0.14*sqrt(e_a))*(1.35*(r_s/r_so)-0.35) !in MJ m-2 day-1
    r_nl = sbc*((((tmax(i)+273.16)**4)+((tmin(i)+273.16)**4))/2)*(net_lw_a-net_lw_b*sqrt(e_a))*(net_lw_c*(r_s/r_so)-net_lw_d) !in MJ m-2 day-1

    r_net = r_ns - r_nl


    pet(i) = (1./l)*((s*r_net)/(s+g))*apt

  enddo

  return
end subroutine

!ccccccccccccccccccccccccccccccccc

!this subroutine is non-functioning currently AJN (9/9/2013)
subroutine calc_pet_pm(sim_len,year,jday,wspd,tair,rh,pressure,swdown,lat,elev)
  use nrtype
  use snow17_sac
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
!  real :: pi  = 3.141592
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