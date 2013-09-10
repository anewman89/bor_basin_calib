def freshRun(arcpy, features, fieldList):

    print 'subsetFeaturesNParams: freshRun: Removing previously derived parameter values from '+features+' prior to parameterizing.'
    for fields in fieldList:
        for field in fields:
            if arcpy.ListFields(features, field):
                print 'subsetFeaturesNParams: freshRun: freshRun: ...Removing '+field+'.'
                arcpy.DeleteField_management(features, field)
            else:
                print 'subsetFeaturesNParams: freshRun: ...'+field+' is not present.'

def getInflowingSegments(POI_ID, segArr):

    segArr[segArr[...,0]==POI_ID,2]=1
    POI_IDs=segArr[segArr[...,1]==POI_ID]
    if POI_IDs.any():
        for id in POI_IDs:
            newID=id[0]
            getInflowingSegments(newID, segArr)
    return segArr

def makeSubsetFGDB(stuff, gage, comid):
    arcpy=stuff['arcpy']
    datadir=stuff['datadir']
    Region=stuff['Region']

    gdbOutWork=stuff['gdbOutWork']
    gdbOutFinal=stuff['gdbOutFinal']
    gdbOutFinalFD=stuff['gdbOutFinalFD']
    gage=stuff['gage']

    arcpy.MakeFeatureLayer_management('nhru', 'nhruFL')
    arcpy.MakeFeatureLayer_management('nsegment', 'nsegmentFL')
    arcpy.MakeFeatureLayer_management('POIs', 'POIFL')

    if not arcpy.ListFields('nhruFL', 'hru_id_prod_unit'):
        arcpy.AddField_management('nhruFL', 'hru_id_prod_unit', 'LONG')
        arcpy.CalculateField_management('nhruFL', 'hru_id_prod_unit', '[hru_id]')

    import numpy
    segList=list()
    for row in arcpy.SearchCursor('nsegmentFL'):
        segList.append([row.getValue('POI_ID'),row.getValue('TO_POI_ID')])
    segArr=numpy.array(segList)
    segArr=numpy.column_stack([segArr,numpy.zeros((len(segArr),1))])
    segArr=getInflowingSegments(comid, segArr)

    segmentClause='POI_ID IN ('
    for seg in segArr[segArr[...,2]==1]:
        segid=seg[0]
        segmentClause+=str(segid)+', '
    segmentClause=segmentClause[0:len(segmentClause)-2]+')'

    import os
    spatial_ref = os.path.join(arcpy.GetInstallInfo()["InstallDir"], "Coordinate Systems/USGS_Favorites/USA Contiguous Albers Equal Area Conic USGS.prj")
    if arcpy.Exists(gdbOutWork):
        arcpy.Delete_management(gdbOutWork)
    print 'subsetFeaturesNParams: Creating (empty) geodatabase, '+gdbOutWork+', with subset of \'work\' products.'
    arcpy.CreateFileGDB_management (datadir, os.path.basename(gdbOutWork),"CURRENT")
    print 'subsetFeaturesNParams: ...Adding \'Output\' feature dataset to the new Final geodatabase.'
    arcpy.CreateFeatureDataset_management (gdbOutWork,'Output',spatial_ref)

    if arcpy.Exists(gdbOutFinal):
        arcpy.Delete_management(gdbOutFinal)
    print 'subsetFeaturesNParams: Creating (empty) geodatabase, '+gdbOutFinal+', with subset of \'Final\' products.'
    arcpy.CreateFileGDB_management (datadir, os.path.basename(gdbOutFinal),"CURRENT")
    print 'subsetFeaturesNParams: ...Adding \'PRMS\' feature dataset to the new Final geodatabase.'
    arcpy.CreateFeatureDataset_management (gdbOutFinal,'PRMS',spatial_ref)
    del os

    print 'subsetFeaturesNParams: Copying the segments above this gage to '+gdbOutFinal+'.'
    arcpy.SelectLayerByAttribute_management('nsegmentFL', 'NEW_SELECTION',segmentClause)
    arcpy.CopyFeatures_management('nsegmentFL', gdbOutFinalFD+'/nsegment')

    print 'subsetFeaturesNParams: Copying the POIs above this gage to '+gdbOutFinal+'.'
    poiClause=segmentClause.replace('POI_ID', 'COMID')
    arcpy.SelectLayerByAttribute_management('POIFL', 'NEW_SELECTION',poiClause)
    arcpy.CopyFeatures_management('POIFL', gdbOutFinalFD+'/POIs')

    print 'subsetFeaturesNParams: Copying the HRUs above this gage to '+gdbOutFinal+'.'
    arcpy.SelectLayerByAttribute_management('nhruFL', 'NEW_SELECTION', segmentClause)
    arcpy.CopyFeatures_management('nhruFL', gdbOutFinalFD+'/nhru')

    arcpy.MakeFeatureLayer_management(gdbOutFinalFD+'/nhru', 'nhru2FL')

    print 'subsetFeaturesNParams: Updating the hru_id_local field to run from 1-n within your subset. Be sure to re-run param_routing-lumen.'
    rows=arcpy.UpdateCursor('nhru2FL')
    counter=1
    for row in rows:
        row.setValue('hru_id_local', counter)
        rows.updateRow(row)
        counter+=1
    del rows, row

    arcpy.env.workspace=gdbOutFinalFD
    arcpy.Dissolve_management('nhru', 'one')
    # Roland believes we'll have to add this back in, but it is not needed for now.
##    import Final_folder_creation
##    Final_folder_creation.singleAOI(arcpy, arcpy.env.workspace)

    featuresShp=datadir+'/R'+Region+'_'+gage+'_nhru.shp'
    print 'subsetFeaturesNParams: Creating zipped archive of shapefile of hrus, '+featuresShp+'.zip.'
    if not arcpy.Exists(featuresShp):
        arcpy.FeatureClassToShapefile_conversion('nhru', datadir)
        arcpy.Rename_management(datadir+'/nhru.shp', featuresShp)
    if not arcpy.Exists('R'+Region+'_nhru_shp.zip'):
        import zipfile, os, glob
        os.chdir(datadir)
        shapefileBits=glob.glob('R'+Region+'_'+gage+'_nhru.*')
        try:
            shapefileBits.remove('R'+Region+'_'+gage+'_nhru_shp.zip')
        except:
            dummy=0
        z=zipfile.ZipFile('R'+Region+'_'+gage+'_nhru_shp.zip', 'w')
        for bit in shapefileBits:
            print 'subsetFeaturesNParams: '+bit
            z.write(bit)
        z.close()

def main(datadir, Region, gage):

    import sys, os, arcpy

    #gage='06754000' # example of specification of an outlet. This could be convereted to a command-line argument

    datadir=datadir.replace('\\','/')

    gdbFinal=datadir+'/NHDPlus'+Region+'_Final.gdb'
    gdbFinalFD=gdbFinal+'/PRMS/'
    gdbOutWork=datadir+'/NHDPlus'+Region+'_'+gage+'_Final.gdb'
    gdbOutWorkFD=gdbOutWork+'/Output/'
    gdbOutFinal=datadir+'/NHDPlus'+Region+'_'+gage+'_Final.gdb'
    gdbOutFinalFD=gdbOutFinal+'/PRMS/'

    arcpy.env.workspace = gdbFinalFD
    arcpy.env.overwriteOutput = True

    stuff={}
    stuff['arcpy']=arcpy
    stuff['datadir']= datadir
    stuff['Region']= Region

    stuff['gdbFinal']=gdbFinal
    stuff['gdbFinalFD']=gdbFinalFD
    stuff['gdbOutWork']=gdbOutWork
    stuff['gdbOutWorkFD']=gdbOutWorkFD
    stuff['gdbOutFinal']=gdbOutFinal
    stuff['gdbOutFinalFD']=gdbOutFinalFD
    stuff['gage']=gage

    cleanupList=list()

    arcpy.MakeFeatureLayer_management('POIs', 'POIFL')
    arcpy.SelectLayerByAttribute_management("POIFL", "NEW_SELECTION", '"COMID"='+gage+' or "Type_GagesII"=\''+gage+'\' or "Type_RFC"=\''+gage+'\' or "Type_SPRW"=\''+gage+'\'')
    num=int(arcpy.GetCount_management('POIFL')[0])
    if num <=0:
        print 'subsetFeaturesNParams: This gage, '+gage+', is not in the list of POIs. Cannot subset using this gage. Program terminating.'
        exit()
    elif num>1:
        print 'subsetFeaturesNParams: This gage, '+gage+', occurs more than once in the set of POIs, which is an error. Cannot subset using this gage. Program terminating. The input to the MoWS_NHDPlus_Python processing should be modified and the process re-run.'
        exit()

    csr=arcpy.SearchCursor('POIFL')
    row=csr.next()
    comid=row.getValue('COMID')

    makeSubsetFGDB(stuff, gage, comid)

    for loop in [['nsegment', ['seg_id', 'tosegment']], ['nhru', ['hru_segment']], ['POIs', ['poi_gage_segment']]]:
        feature=loop[0]
        params=loop[1]
        for param in params:
            print param
            arcpy.AddField_management(gdbOutFinalFD+feature, param+'_old', 'SHORT')
            ##arcpy.CalculateField_management(gdbOutFinalFD+feature, param+'_old', '['+param+']')
            arcpy.CalculateField_management(gdbOutFinalFD+feature, param+'_old', '!%s!' %param, 'PYTHON')
            ##arcpy.CalculateField_management(gdbOutFinalFD+feature, param, '-9999')
            arcpy.CalculateField_management(gdbOutFinalFD+feature, param, '-9999', 'PYTHON')

    arcpy.MakeTableView_management(gdbOutFinalFD+'/nhru', 'nhruTV')
    arcpy.MakeTableView_management(gdbOutFinalFD+'/nsegment', 'nsegmentTV')
    arcpy.MakeTableView_management(gdbOutFinalFD+'/POIs', 'POIsTV')

    id=1
    csr=arcpy.UpdateCursor('nsegmentTV')
    for seg in csr:
        seg.setValue('seg_id', id)
        csr.updateRow(seg)
        id+=1
    del csr, seg, id

    arcpy.AddJoin_management( 'nhruTV', 'hru_segment_old', 'nsegmentTV', 'seg_id_old')
    ##arcpy.CalculateField_management('nhruTV', 'hru_segment', '[nsegment.seg_id]')
    arcpy.CalculateField_management('nhruTV', 'hru_segment', '!nsegment.seg_id!', 'PYTHON')
    arcpy.RemoveJoin_management('nhruTV', 'nsegment')
    arcpy.AddJoin_management( 'POIsTV', 'poi_gage_segment_old', 'nsegmentTV', 'seg_id_old')
    ##arcpy.CalculateField_management('POIsTV', 'poi_gage_segment', '[nsegment.seg_id]')
    arcpy.CalculateField_management('POIsTV', 'poi_gage_segment', '!nsegment.seg_id!', 'PYTHON')
    arcpy.RemoveJoin_management('POIsTV', 'nsegment')

    arcpy.Copy_management(gdbOutFinalFD+'/nsegment', gdbOutFinalFD+'/nsegment2')
    arcpy.MakeTableView_management(gdbOutFinalFD+'/nsegment2', 'nsegmentTV2')
    arcpy.AddJoin_management( 'nsegmentTV', 'tosegment_old', 'nsegmentTV2', 'seg_id_old')
    ##arcpy.CalculateField_management('nsegmentTV', 'tosegment', '[nsegment2.seg_id]')
    arcpy.CalculateField_management('nsegmentTV', 'tosegment', '!nsegment2.seg_id!', 'PYTHON')
    arcpy.RemoveJoin_management('nsegmentTV', 'nsegment2')
    arcpy.Delete_management(gdbOutFinalFD+'/nsegment2')

    import param_prms_limited
    param_prms_limited.main(datadir, Region+'_'+gage)

if __name__ == '__main__':
    import sys
    main(sys.argv[1], sys.argv[2], sys.argv[3])
