# ------------------------------------------------------------------------------
# This script extracts elevation bands (100m intervals) from the 'Contours' reference
# mosaic dataset based on NHD Plus v.2 data.  The contours are simplified and
# exported to shapefiels for each region.
# ------------------------------------------------------------------------------

import arcpy
import os
import glob
import zipfile
import time

tic = time.time()

# Spatial Analyst Extension
arcpy.CheckOutExtension('spatial')
from arcpy.sa import *

# Unzip function
def unzip(zipFilePath, destDir):
    zfile = zipfile.ZipFile(zipFilePath)
    for name in zfile.namelist():
        (dirName, fileName) = os.path.split(name)
        if fileName == '':
            # directory
            newDir = destDir + '/' + dirName
            if not os.path.exists(newDir):
                os.mkdir(newDir)
        else:
            # file
            fd = open(destDir + '/' + name, 'wb')
            fd.write(zfile.read(name))
            fd.close()
    zfile.close()

# Zip function
def zipfiles(basename, destDir, outzipfile):
    if not arcpy.Exists(outzipfile):
        os.chdir(destDir)
        shapefileBits=glob.glob("%s.*" %basename)
        try:
            shapefileBits.remove(basename)
        except:
            dummy=0
        z=zipfile.ZipFile(outzipfile, 'w')
        for bit in shapefileBits:
            z.write(bit)
        z.close()

datadir = r'E:\Projects\Clark\MoWS\output\To_Andy'
destdir = r'E:\Projects\Clark\MoWS\output'
inelev = r'\\gisData.ucar.edu\data\elevation\NHDPlusV21\NHDPlusv21.gdb\Contours'

# Set environments(extent and mask)
arcpy.env.overwriteOutput = True
arcpy.env.workspace = os.path.dirname(datadir)

# Create Mosaic Layer
arcpy.MakeMosaicLayer_management(inelev, "Contours")

# Initiate zip file list
ziplist = glob.glob(datadir + r'\*nhru.zip')

# Unzip all files
for zipfilename in ziplist:
    unzip(zipfilename, destdir)
    print "Unzipping: %s" %os.path.basename(zipfilename)

    # Initialize parameters
    infc = destdir + os.path.sep + os.path.basename(zipfilename).replace('.zip', '.shp')          #r'E:\Projects\Clark\MoWS\output\Region_01_nhru_simplify_100.shp'
    outfinal = os.path.dirname(infc) + os.path.sep + os.path.basename(infc).replace('_nhru', '_contours')

    # Set output coordinate system
    sr = arcpy.Describe(infc).spatialReference
    arcpy.env.outputCoordinateSystem = sr

    # Dissolve and simplify input feature class by GAGEID attribute
    infc2 = infc.replace('_nhru', '')
    arcpy.Dissolve_management(infc, infc2, "GAGEID", "", "MULTI_PART")
    infc3 = infc.replace('_nhru', '_')
    arcpy.SimplifyPolygon_cartography(infc2, infc3, "POINT_REMOVE", 100, "#", "NO_CHECK", "NO_KEEP")

    # Create Feature Layer
    arcpy.MakeFeatureLayer_management(infc3, "Basins")

    # Use SearchCursor to get geometry of each polygon
    i = 0
    with arcpy.da.SearchCursor(infc3, ["OID@", "SHAPE@", "GAGEID"]) as cur:
        for row in cur:
            # Extract raster by polygon  mask
            raster = arcpy.sa.ExtractByMask("Contours", row[1])

            # Convert to Polygon
            outPolygon = "OutPolygons.shp"
            arcpy.RasterToPolygon_conversion(raster, outPolygon, "SIMPLIFY")            # SIMPLIFY

            # Add GAGEID info into the vector layer and then insert into new shapefile
            arcpy.AddField_management(outPolygon, "Gage_Band", "TEXT")
            codeblock = """Gage='%s'""" %(row[2])
            arcpy.CalculateField_management(outPolygon, "Gage_Band", 'Gage + "_" + str(int(!GRIDCODE!))', "PYTHON_9.3", codeblock)

            if i == 0:
                arcpy.CopyFeatures_management(outPolygon, outfinal)
            elif i > 0:
                outfinal2 = outfinal.replace('.shp', '2.shp')
                arcpy.CopyFeatures_management(outfinal, outfinal2)
                arcpy.Merge_management([outPolygon, outfinal2], outfinal)

            # Clean up
            arcpy.Delete_management(outPolygon)
            del raster

            i += 1
            print '    Done with feature %s' %i

        dissolvefield = "Gage_Band"
        arcpy.CopyFeatures_management(outfinal, outfinal2)
        #outfinal3 = outfinal.replace('.shp', '3.shp')
        arcpy.Dissolve_management(outfinal2, outfinal, dissolvefield, "", "MULTI_PART")
        #arcpy.SimplifyPolygon_cartography(outfinal3, outfinal, "POINT_REMOVE", 100, "#", "NO_CHECK", "NO_KEEP")
        arcpy.Delete_management(outfinal2)
        #arcpy.Delete_management(outfinal3)
        arcpy.Delete_management(infc)
        arcpy.Delete_management(infc2)
        arcpy.Delete_management(infc3)

    zipfiles(os.path.basename(outfinal)[:-4], os.path.dirname(infc3), outfinal.replace('.shp', '.zip'))
    print "Done creating file: %s" %(outfinal.replace('.shp', '.zip'))

    arcpy.Delete_management(outfinal)
    toc = time.time()
    print 'Time elapsed for this file: %s minutes' %str((toc-tic)/60)
    tic = time.time()