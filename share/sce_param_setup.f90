
subroutine sce_param_setup(a,bl,bu)
  use nrtype
  use snow17_sac, only: scf,mfmax,mfmin,uadj,si,pxtemp, &
                        uztwm,uzfwm,lztwm,lzfpm,lzfsm,lzpk,lzsk, uzk, &
                        zperc,rexp,pctim, pfree,unit_shape,unit_scale,pet_coef, &
                        nmf,tipm,mbase,plwhc,daygm,adimp,riva,side,rserv

  implicit none

!output variables
  real(sp),dimension(30),intent(out) :: a,bl,bu


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