#========================================================================
# Makefile to compile the Snow-17/SAC,SCE and driver code for gauge calibrations
#========================================================================
#  Original Makefile from Martyn's FUSE Model
#  Modified:
#  Andy Newman, Sept 2013
#
#========================================================================
# PART 0: Define directory paths
#========================================================================

# Define core directory below which everything resides
F_MASTER_DIR = /home/anewman/bor_basin_calib

# Location of the compiled modules
MOD_PATH = $(F_MASTER_DIR)/driver

# Define the directory for the executables
EXE_PATH = $(F_MASTER_DIR)/bin
#
#========================================================================
# PART 1: Assemble all of the various sub-routines
#========================================================================

# Define directories
DRIVER_DIR = $(F_MASTER_DIR)/driver
sac_dir    = $(F_MASTER_DIR)/sac
sce_dir    = $(F_MASTER_DIR)/sce
snow19_dir = $(F_MASTER_DIR)/snow19
share_dir  = $(F_MASTER_DIR)/share


# share utilities
calib_UTIL= \
		nrtype.f90 \
		constants.f90 \
		snow17_sac.f90 \
		gauge_calib.f90 \
		read_namelist.f90 \
		functn.f90 \
		ascii_io.f90 \
		day_of_year.f90 \
		sfc_pressure.f90 \
		pet_routines.f90 \
		spin_up_first_year.f90 \
		objective_functions.f90 \
		snow17_sacsma_eval.f90 \
		sce_param_setup.f90
UTIL = $(patsubst %, $(share_dir)/%, $(calib_UTIL))


# sce procedure (fortran 77)
calib_sceproc_77= \
		sce_new.f
sceproc_77 = $(patsubst %, $(sce_dir)/%, $(calib_sceproc_77))


# snow-19 code (fortran 90, different options)
calib_snow19= \
		zero19.f \
		rout19.f \
		aesc19.f \
		melt19.f \
		SNEW.f \
		SNOWT.f \
		SNOWPACK.f \
		adjc19.f \
		aeco19.f \
		updt19.f \
		SNDEPTH.f \
		PACK19.f \
		exsnow19.f
snow19 = $(patsubst %, $(snow19_dir)/%, $(calib_snow19))

# sac code (fortran 77)
calib_sac_77= \
		sac1.f \
		ex_sac1.f \
		duamel.f 
sac_77 = $(patsubst %, $(sac_dir)/%, $(calib_sac_77))


# ... and stitch it all together...
calib_pgf90 = $(UTIL)
calib_pgf90_2 = $(snow19)
calib_pgf77 = $(sceproc_77) $(sac_77)

#========================================================================
# PART 2: Define the libraries, driver programs, and executables
#========================================================================

# Define the Fortran Compilers
FC  = pgf90
FC77 = pgf77
#FC  = ifort

# Define the libraries and path to include files
ifeq "$(FC)" "pgf90"
 LOCL_PATH = /usr/local

 LIB = -L$(LOCL_PATH)/lib 
 INC = -I ./
endif

ifeq "$(FC77)" "pgf77"
 LOCL_PATH = /usr/local

 LIB77 = -L$(LOCL_PATH)/lib 
 INC77 = -I ./
endif

# Define the driver routine
calib_DRIVER= \
	test_huc02_region.f90
DRIVER = $(patsubst %, $(DRIVER_DIR)/%, $(calib_DRIVER))

# Define the executable
DRIVER__EX = gage_calib_test.exe


#========================================================================
# PART 3: Compile the puppy
#========================================================================

# Define flags
ifeq "$(FC)" "gfortran"
 FLAGS = -static -Wall -g -ffree-line-length-none
endif
ifeq "$(FC)" "ifort"
 FLAGS = -static -debug -warn all -check all -FR -O0 -auto -WB -traceback -g -fltconsistency -fpe0
endif

ifeq "$(FC)" "pgf90"
 FLAGS_PROF = -Bstatic -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv -Mprof=lines,time
# FLAGS = -Mfreeform -O3 -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv
 FLAGS = -O3 -r8
 FLAGS2 = -O3 -r8
endif
ifeq "$(FC77)" "pgf77"
 FLAGS77 = -O3 -r8
endif

#FLAGS = -O3 -W -v

#.SUFFIXES: .f .o .f90

# Compile
all: compile_calib link clean install

check:
	echo test
	echo $(FC)

# compile calibration code
compile_calib:
	$(FC77) $(FLAGS77) -c $(calib_pgf77) $(INC77)
	$(FC) $(FLAGS2) -c $(calib_pgf90_2) $(INC)
	$(FC) $(FLAGS) -c $(calib_pgf90) $(DRIVER) \
	$(INC)

# link routines
link:
#	$(FC) -fPIC -Bstatic_pgi -rpath,/usr/local/netcdf4-pgi/lib *.o -I./ $(LIBNETCDF) -o $(DRIVER__EX)
	$(FC) -fPIC -I./ $(LIB) -o $(DRIVER__EX) *.o 

# Remove object files
clean:
						rm -f *.o
						rm -f *.mod
#
# Copy the executable to the bin directory
install:
		mv $(DRIVER__EX) $(EXE_PATH)

