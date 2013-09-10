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
F_MASTER = /home/anewman/gauge_calib

# Location of the compiled modules
MOD_PATH = $(F_MASTER)/driver

# Define the directory for the executables
EXE_PATH = $(F_MASTER)/bin
#
#========================================================================
# PART 1: Assemble all of the various sub-routines
#========================================================================

# Define directories
DRIVER_DIR = $(F_MASTER_DIR)/driver
sac_dir    = $(F_MASTER_DIR)/sac
sce_dir    = $(F_MASTER_DIR)/sce
snow19_dir = ${F_MASTER_DIR)/snow19
share_dir  = $(F_MASTER_DIR)/share


# share utilities
calib_UTIL= \
		nrtype.f90 \
                snow17_sac.f90 \
		functn.f90 
UTIL = $(patsubst %, $(share_DIR)/%, $(all_NRUTIL))


# sce procedure (fortran 77 compiler)
calib_sceproc_77= \
		sce_new.f90
sceproc_77 = $(patsubst %, $(sce_dir)/%, $(calib_sceproc_77))


# snow-19 code
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
snow19 = $(patsubst %, $(snow19_dir)/%, $(SCRF_DATAMS))

# sac code (fortran 77)
calib_sac_77= \
		sac1.f \
		ex_sac1.f \
		duamel.f 
sac_77 = $(patsubst %, $(sac_dir)/%, $(calib_sac_77))


# ... and stitch it all together...
calib_pgf90 = $(NRUTIL) $(NRPROC) $(DATAMS) $(qpe) $(GEN)
calib_pgf77 = 

#========================================================================
# PART 2: Define the libraries, driver programs, and executables
#========================================================================