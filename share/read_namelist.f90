subroutine read_namelist
  use nrtype
  use snow17_sac

  implicit none

!local variables
  integer(I4B) :: ierr

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