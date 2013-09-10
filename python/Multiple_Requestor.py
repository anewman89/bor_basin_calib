#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      ksampson
#
# Created:     17/05/2013
# Copyright:   (c) ksampson 2013
# Licence:     <your licence>
#-------------------------------------------------------------------------------

#import subprocess
import time
tic = time.time()
import os
import csv
import subsetFeaturesNParams_limited
import arcpy
import zipfile
import glob

# Set up input and output directories
datadir = 'E:/Projects/Clark/MoWS/test_datadir'

# Generate dictionary of POIs
poidict = {}
with open(r'E:\Projects\Clark\MoWS\hru_pois_to_kevin.csv', 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
        if row[0] in poidict.keys():
            poidict[row[0]].append(row[1])
        else:
            poidict[row[0]] = [row[1]]

# Create list of regions
#Regions = poidict.keys()
#Regions = ['11', '13', '12', '15', '14', '17', '16', '18', '1', '3', '2', '5', '4', '7', '6', '9', '8', '10L', '10U']    # All Regions
#Regions = ['11', '15', '16', '18', '1', '3', '2', '6', '8', '10', '17', '10U']    # Problem Regions
Regions = ['10L', '10U']    # Single Test Region

for Reg in Regions:
    # Initiate shapefile list
    shplist = []

    # Convert for 10L and 10U strings
    if Reg == '10L' or Reg == '10U':
        Regi = '10'
    else:
        Regi = Reg

    # Convert to 2-digit region number
    if int(Regi) < 10:
        Region = '0' + Regi
    elif int(Regi) == 10:
        Region = Reg
    else:
        Region = Regi
    print "Region being processed: %s" %Region

    # Generate list of input gages to process
    POIs = poidict[Region]
    print "Number of POIDs to process: %s" %len(POIs)

    for gage in POIs:
        # Fix gage number to 9 digits except certain regions
        if Region in ['16', '18']:
            gage1 = gage
        else:
            gage1 = '0' + gage

        # Setup parameters and execute
        print 'Request: python subsetFeaturesNParams_limisted.py %s %s %s' %(datadir, Region, gage1)

        try:
            subsetFeaturesNParams_limited.main(datadir, Region, gage1)

            # Clean up unecessary GDB and ZIP
            #arcpy.Delete_management(datadir + "/NHDPlus" + Region + "_" + gage1 + "_Final.gdb")
            os.remove(datadir + "/R" + Region + "_" + gage1 + "_nhru_shp.zip")
            os.remove(datadir + "/Region" + Region + "_" + gage1 + ".wpar")
            os.remove(datadir + "/Region" + Region + "_" + gage1 + "wdefaults.wpar")
            os.remove(datadir + "/Region" + Region + "_" + gage1 + "wdefaults.wpar.zip")

            shpfile = datadir + "/R" + Region + "_" + gage1 + "_nhru.shp"
            shplist.append(shpfile)

            #Add field "GAGEID" (LONG) and populate gage number
            arcpy.AddField_management(shpfile, "GAGEID", "TEXT", 8)
            arcpy.CalculateField_management(shpfile, "GAGEID", '"%s"' %gage1, "PYTHON")

        except:
            print "...............script failed, but we are moving on..............."
            pass

    # Merge all shapefiles together into one shapefile
    mergeshapefile = r'E:/Projects/Clark/MoWS/output' + "/Region_" + Region + "_nhru.shp"
    arcpy.Merge_management(shplist, mergeshapefile)

    # Simplify lake polygons.
    outshapefile = r'E:/Projects/Clark/MoWS/output' + "/Region_" + Region + "_nhru_.shp"
    arcpy.SimplifyPolygon_cartography(mergeshapefile, outshapefile, "POINT_REMOVE", 100, "#", "NO_CHECK", "NO_KEEP")
    arcpy.Delete_management(mergeshapefile)

    # Zip up all shapefiles into a zip file
    outzipfile = r'E:/Projects/Clark/MoWS/output' + "/Region_" + Region + "_nhru.zip"
    if not arcpy.Exists(outzipfile):
        os.chdir(r'E:/Projects/Clark/MoWS/output')
        shapefileBits=glob.glob("Region_" + Region + "_nhru*.*")
        try:
            shapefileBits.remove("Region_" + Region + "_nhru.zip")
        except:
            dummy=0
        z=zipfile.ZipFile("Region_" + Region + "_nhru.zip", 'w')
        for bit in shapefileBits:
            print 'subsetFeaturesNParams: '+bit
            z.write(bit)
        z.close()

    print "Shapefile %s zipped up." %outshapefile

    # Clean up
    arcpy.Delete_management(outshapefile)
    for x in shplist:
        arcpy.Delete_management(x)

toc = time.time()
print "Time Elapsed: %ss" %int(toc-tic)

