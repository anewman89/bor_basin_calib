#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      ksampson
#
# Created:     02/05/2013
# Copyright:   (c) ksampson 2013
# Licence:     <your licence>
#-------------------------------------------------------------------------------

# Import pyGDP module
#import os
import pyGDP
import time
tic = time.time()

# Initialize dictionary object for storing input:output
inout = {}

# initialize a pyGDP wps object
pyGDP = pyGDP.pyGDPwebProcessing()

# Methods to use pre-defined shapefiles on GDP
##shapefiles = pyGDP.getShapefiles()
shapefile  = 'sample:CONUS_States'

# Get attributes for the chosen shapefile
##attributes = pyGDP.getAttributes(shapefile)
attribute = 'STATE'

# Get the values for the chosen attribute
##values = pyGDP.getValues(shapefile,attribute)
value = 'Colorado'

# Choose the input OpenDAP resource
datasetURI = 'dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet'            #Daymet
#datasetURI = 'http://cida.usgs.gov/thredds/dodsC/prism'                                #PRISM
#datasetURI = 'dods://cida.usgs.gov/thredds/dodsC/maurer/maurer_brekke_w_meta.ncml'      #BCSD

# Get the data types out of that resource
dataTypes  = pyGDP.getDataType(datasetURI)
#dataType  = 'tmax'                                                                     #Daymet
#dataType  = 'sresa1b_ncar-ccsm3-0_1_Prcp'                                               #BCSD
#dataTypes  = ['sresa1b_ncar-ccsm3-0_1_Prcp', 'sresb1_ncar-ccsm3-0_2_Prcp']

# Get time-range for the variable selected
timeRange = pyGDP.getTimeRange(datasetURI,dataType)
timeStart = timeRange[0]                                                        #   '1980-01-01T00:00:00.000Z'
timeEnd   = timeRange[1]                                                        #   '2011-12-31T00:00:00.000Z'

# Choose Statistics
##stats = ["MEAN", "MINIMUM", "MAXIMUM", "VARIANCE", "STD_DEV", "WEIGHT_SUM", "COUNT"]

# After choosing a start and end time that are within the range of the dataset, all parameters for a Feature Weighted Grid Statistics request have been defined.
outputFile_handle = pyGDP.submitFeatureWeightedGridStatistics(shapefile, datasetURI, dataTypes, timeStart, timeEnd, attribute, value)

# Which will print out the file handle for the downloaded file (which will be in the python working directory) after the processed is finished.
# For this example, the output file was named 7695435223154905129OUTPUT.db38e168-3321-439f-bac4-e46777390dc3
# The file can be opened in python, excel, a text editor, or any other program which supports delimited (in this case, comma delimited) text.
print outputFile_handle

# Add record to dictionary
inout[shapefile] = outputFile_handle

toc = time.time()
print 'Time elapsed: %s seconds.' %(toc-tic)
