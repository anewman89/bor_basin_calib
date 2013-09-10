#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      ksampson
#
# Created:     31/05/2013
# Copyright:   (c) ksampson 2013
# Licence:     <your licence>
#-------------------------------------------------------------------------------

#import subprocess
import time
tic = time.time()
import os
import csv
import arcpy
import sys

inputfile = r'E:\Projects\Clark\MoWS\hru_pois_to_kevin.csv'

def findgage(datadir, Region, gage):

    #gage='06754000' # example of specification of an outlet. This could be convereted to a command-line argument

    datadir=datadir.replace('\\','/')

    gdbFinal=datadir+'/NHDPlus'+Region+'_Final.gdb'
    gdbFinalFD=gdbFinal+'/PRMS/'

    arcpy.env.workspace = gdbFinalFD
    arcpy.env.overwriteOutput = True

    cleanupList=list()

    arcpy.MakeFeatureLayer_management('POIs', 'POIFL')
    arcpy.SelectLayerByAttribute_management("POIFL", "NEW_SELECTION", '"COMID"='+gage+' or "Type_GagesII"=\''+gage+'\' or "Type_RFC"=\''+gage+'\' or "Type_SPRW"=\''+gage+'\'')
    num=int(arcpy.GetCount_management('POIFL')[0])
    if num <=0:
        print '  Gage %s in Region %s is not in the list of POIs. Cannot subset using this gage.' %(gage, Region)
    elif num>1:
        print '  Gage %s in Region %s occurs more than once in the set of POIs, which is an error. Cannot subset using this gage.' %(gage, Region)

# Set up input and output directories
datadir = 'E:/Projects/Clark/MoWS/test_datadir'

# Generate dictionary of POIs
poidict = {}
with open(inputfile, 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
        if row[0] in poidict.keys():
            poidict[row[0]].append(row[1])
        else:
            poidict[row[0]] = [row[1]]

# Create list of regions
#Regions = poidict.keys()
#Regions = ['11', '15', '17', '16', '18', '1', '3', '2', '6', '8']    # Problem Regions
Regions = ['10U', '10L', '17']

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
        #print 'Finding POI %s in Region %s' %(gage1, Region)
        findgage(datadir, Region, gage1)

toc = time.time()
print "Time Elapsed: %ss" %int(toc-tic)
