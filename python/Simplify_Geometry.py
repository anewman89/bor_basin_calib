#-------------------------------------------------------------------------------
# This script will pull shapefiles out of the zip archives they reside in and then
# simplify the geometry (100m) and re-zip the output.
#-------------------------------------------------------------------------------

#import subprocess
import os
import arcpy
import zipfile
import glob

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

# Set up input and output directories
datadir = r'E:\Projects\Clark\MoWS\output\To_Andy'

# Initiate zip file list
ziplist = glob.glob(datadir + r'\*_contours.zip')

# Unzip all files
for zipfilename in ziplist:
    unzip(zipfilename, datadir)
    print "Unzipping: %s" %os.path.basename(zipfilename)

# List feature classes
arcpy.env.workspace = datadir
shplist = arcpy.ListFeatureClasses()

# Iterate over shapefiles
for shp in shplist:
    sr = arcpy.Describe(shp).spatialReference
    arcpy.env.outputCoordinateSystem = sr

    # Create output name from input filename
    newname = shp.replace('.shp', '_simplify_100')
    outshapefile = datadir + r'/' + newname + '.shp'
    arcpy.SimplifyPolygon_cartography(shp, outshapefile, "POINT_REMOVE", 100, "#", "NO_CHECK", "NO_KEEP")
    #arcpy.SimplifyPolygon_cartography(shp, outshapefile, "POINT_REMOVE", '100 METERS', "#", "NO_CHECK", "NO_KEEP")

    # Zip up output
    print 'Creating Archive: %s' %str(newname + '.shp')
    zipfiles(newname, datadir, outshapefile.replace('.shp', '.zip'))
    arcpy.Delete_management(shp)
    arcpy.Delete_management(outshapefile)