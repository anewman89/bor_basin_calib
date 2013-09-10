# -*- coding: iso-8859-1 -*-
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

def main():

    # Import necessary modules
    import time, os, shutil, sys
    import pyGDP

    outdir = sys.argv[1]
    shapefilezip = sys.argv[2]

    # Choose the input OpenDAP resource
    datasetURI = 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet'

    # initialize a pyGDP wps object
    pyGDP = pyGDP.pyGDPwebProcessing()

    # Selection to upload shapefile
    uploadname = 'upload:' + os.path.basename(shapefilezip)[:-4]
    shapefiles = pyGDP.getShapefiles()

    # Logic check in case the shapefile has already been uploaded.
    if uploadname in shapefiles:
        print "The file already exists in the geoserver."
        shapefile = shapefiles[shapefiles.index(uploadname)]
    else:
        shapefile = pyGDP.uploadShapeFile(shapefilezip)
        print "Shapefile uploaded successfully."

    # Get attributes for the chosen shapefile
    attributes = pyGDP.getAttributes(shapefile)
    #attribute = 'PROD_UNIT'
#    attribute = 'hru_id'
#    attribute = 'GAGEID'
    attribute = 'Gage_Band'
#    attribute = 'None'

    # Get the values for the chosen attribute
    values = pyGDP.getValues(shapefile,attribute)
#    values = None

    # Get the data types out of that resource
    dataTypes  = pyGDP.getDataType(datasetURI)

    # Get time-range for the variable selected
    timeRange = pyGDP.getTimeRange(datasetURI,dataTypes[4])

    # Other variables
    dataType = dataTypes
    timeStart = timeRange[0]
    timeEnd   = timeRange[1]
#    value = values[0]
#    value = values[:]
#    value = values[0:16]
#    value=values[17:33]
    value = None
    gmlIDs = None
    verbose = False
    coverage = 'true'
    delim = 'COMMA'
    stat = ['MEAN', 'MINIMUM', 'MAXIMUM', 'VARIANCE', 'STD_DEV', 'SUM', 'COUNT']
    #stat = 'MEAN'
    grpby = 'STATISTIC'

    # Submit Feature Weighted Grid Statistics request.
    outputFile_handle = pyGDP.submitFeatureWeightedGridStatistics(shapefile, datasetURI, dataType, timeStart, timeEnd, attribute, value, gmlIDs, verbose, coverage, delim, stat, grpby)
    print outputFile_handle

    # Move and rename output file
    curpath = os.getcwd()
    outputfile = os.path.join(curpath, outputFile_handle)
    shutil.move(outputfile, outdir)
    os.rename(os.path.join(outdir, outputFile_handle), os.path.join(outdir, os.path.basename(shapefilezip)[:-8] + '.csv'))

if __name__ == '__main__':
    main()


