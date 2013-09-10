def writeWParFile(stuff, fieldListNhru, fieldListNsegment, fieldListNpoigages, fieldListOne, fieldListNssr, fieldListNgw):
    arcpy=stuff['arcpy']
    datadir=stuff['datadir']
    Region=stuff['Region']



    arcpy.MakeTableView_management('one', 'oneTV')
    arcpy.MakeTableView_management('nhru', 'nhruTV')
    nhru=str(arcpy.GetCount_management('nhruTV')[0])
    arcpy.MakeTableView_management('nsegment', 'nsegmentTV')
    nsegment=str(arcpy.GetCount_management('nsegmentTV')[0])
##    arcpy.MakeTableView_management('POIs', 'POIsTV', 'Type_GagesII > 0 or Type_User > 0') # Note: this will include "non-reference" gages
    arcpy.MakeTableView_management('POIs', 'POIsTV')
    arcpy.SelectLayerByAttribute_management('POIsTV', 'NEW_SELECTION', '"poi_gage_id" <> \'0\'')
    npoigages=str(arcpy.GetCount_management('POIsTV')[0])

    arcpy.MakeTableView_management('one', 'oneTV')

    import numpy
    print 'param_prms: writeWParFile: Extracting parameters to numpy array for quicker export. Working on nhru.'
    nhruParam={}
    for row in arcpy.SearchCursor('nhruTV'):
        for fields in fieldListNhru:
            for param in fields:
                if not param in nhruParam:
                    nhruParam[param]=list()
                nhruParam[param].append(row.getValue(param))
    del row

    print 'param_prms: writeWParFile: Extracting parameters to numpy array for quicker export. Working on ngw.'
    ngwParam={}
    for row in arcpy.SearchCursor('nhruTV'):
        for fields in fieldListNgw:
            for param in fields:
                if not param in ngwParam:
                    ngwParam[param]=list()
                ngwParam[param].append(row.getValue(param))
    del row

    print 'param_prms: writeWParFile: Extracting parameters to numpy array for quicker export. Working on nssr.'
    nssrParam={}
    for row in arcpy.SearchCursor('nhruTV'):
        for fields in fieldListNssr:
            for param in fields:
                if not param in nssrParam:
                    nssrParam[param]=list()
                nssrParam[param].append(row.getValue(param))
    del row


    print 'param_prms: writeWParFile: Extracting parameters to numpy array for quicker export. Working on nsegment.'
    nsegmentParam={}
    for row in arcpy.SearchCursor('nsegmentTV'):
        for fields in fieldListNsegment:
            for param in fields:
                if not param in nsegmentParam:
                    nsegmentParam[param]=list()
                nsegmentParam[param].append(row.getValue(param))
    del row
    if int(arcpy.GetCount_management('POIsTV')[0])> 0:
        print 'param_prms: writeWParFile: Extracting parameters to numpy array for quicker export. Working on POIs.'
        npoigagesParam={}
        for row in arcpy.SearchCursor('POIsTV'):
            for fields in fieldListNpoigages:
                for param in fields:
                    if not param in npoigagesParam:
                        npoigagesParam[param]=list()
                    npoigagesParam[param].append(row.getValue(param))
        del row
    else:
        print 'There no POIs in the modeling domain.'
    print 'param_prms: writeWParFile: Extracting parameters to numpy array for quicker export. Working on one (parameters for the whole basin).'
    oneParam={}
    for row in arcpy.SearchCursor('oneTV'):
        for fields in fieldListOne:
            for param in fields:
                print param
                if not param in oneParam:
                    oneParam[param]=list()
                oneParam[param].append(row.getValue(param))
    del row

    import numpy
    for param in nhruParam:
        arr=numpy.array(nhruParam[param])
        nnulls=len(arr[arr[...]==-99999])
        print 'param_prms: writeWParFile: There are '+str(nnulls)+' "-99999" values in '+param+'. These are in HRUs: '+str()

    import os
    cwd=os.getcwd()
    os.chdir(datadir)
    filename='Region'+Region+'.wpar'
    file=open(filename, 'w')
    file.write('NHDPlus-generated parameters for nhru\n')
    file.write('Version: 1.7\n')
    file.write('** Dimensions **\n')
    file.write('####\n')
    file.write('one\n')
    file.write('1\n')
    file.write('####\n')
    file.write('nhru\n')
    file.write(nhru+'\n')
    file.write('####\n')
    file.write('nsegment\n')
    file.write(nsegment+'\n')
    if npoigages>0:
        file.write('####\n')
        file.write('npoigages\n')
        file.write(npoigages+'\n')
    file.write('** Parameters **\n')

    for fields in fieldListOne:
        for param in fields:
            file.write('####\n')
            file.write(param+' 5\n')
            file.write('1\n')
            file.write('one\n')
            file.write('1\n')
            if param=='elev_units':
                file.write('1\n')
            else:
                file.write('2\n')
            print 'param_prms: writeWParFile: '+param
            for row in oneParam[param]:
                file.writelines(str(row)+'\n')
            del row

    for fields in fieldListNhru:
        for param in fields:
            file.write('####\n')
            file.write(param+' 5\n')
            file.write('1\n')
            file.write('nhru\n')
            file.write(nhru+'\n')
            if param=='hru_deplcrv' or param=='soil_type' or param=='cov_type' or param=='hru_segment':
                file.write('1\n')
            else:
                file.write('2\n')
            print 'param_prms: writeWParFile: '+param
            for row in nhruParam[param]:
                file.writelines(str(row)+'\n')
            del row

    for fields in fieldListNssr:
        for param in fields:
            file.write('####\n')
            file.write(param+' 5\n')
            file.write('1\n')
            file.write('nssr\n')
            file.write(nhru+'\n')
            if param=='elev_units':
                file.write('1\n')
            else:
                file.write('2\n')
            print 'param_prms: writeWParFile: '+param
            for row in nssrParam[param]:
                file.writelines(str(row)+'\n')
            del row

    for fields in fieldListNgw:
        for param in fields:
            file.write('####\n')
            file.write(param+' 5\n')
            file.write('1\n')
            file.write('ngw\n')
            file.write(nhru+'\n')
            if param=='elev_units':
                file.write('1\n')
            else:
                file.write('2\n')
            print 'param_prms: writeWParFile: '+param
            for row in ngwParam[param]:
                file.writelines(str(row)+'\n')
            del row

    for fields in fieldListNsegment:
        for param in fields:
            file.write('####\n')
            file.write(param+' 5\n')
            file.write('1\n')
            file.write('nsegment\n')
            file.write(nsegment+'\n')
            if param=='tosegment':
                file.write('1\n')
            else:
                file.write('2\n')
            print 'param_prms: writeWParFile: '+param
            for row in nsegmentParam[param]:
                file.writelines(str(row)+'\n')
            del row


    if int(arcpy.GetCount_management('POIsTV')[0])>0:
        for fields in fieldListNpoigages:
            for param in fields:
                file.write('####\n')
                file.write(param+' 5\n')
                file.write('1\n')
                file.write('npoigages\n')
                file.write(npoigages+'\n')
                if param=='poi_gage_id':
                    file.write('4\n')
                else:
                    file.write('1\n')
                print 'param_prms: writeWParFile: '+param
                for row in npoigagesParam[param]:
                    file.writelines(str(row)+'\n')
                del row

    else:
        print 'param_prms: writeWParFile: There are no POI gages in this basin (although there are '+str(arcpy.GetCount_management('POIs')[0])+' POIs).'

    file.close()

##    os.remove(filename)
    print 'param_prms: writeWParFile: Completed creating preliminary parameter file, '+datadir+'/'+filename+'. Default values for non-GIS parameters will be added next.'
    os.chdir(cwd)
    del os

    import param_prmsAddDefaults
    param_prmsAddDefaults.main(datadir, Region)

def routingLumen(stuff, features):
    arcpy=stuff['arcpy']
    gdbFinal=stuff['gdbFinal']
    gdbFinalFD=stuff['gdbFinalFD']
    Region=stuff['Region']
    datadir=stuff['datadir']
    gdbWk=gdbFinal.replace('_Final', '_wk')

    arcpy.MakeFeatureLayer_management('nhru','nhruFL')
    arcpy.MakeFeatureLayer_management('nsegment', 'nsegmentFL')
    if arcpy.Exists('POIs'):
        arcpy.MakeFeatureLayer_management('POIs','POIsFL')

    if arcpy.ListFields('nsegmentFL', 'to_seg'):
        arcpy.DeleteField_management('nsegmentFL', 'to_seg') # just cleans up artifacts from previous version of the code

##    if arcpy.ListFields('nsegmentFL', 'tosegment'):
##        print 'param_prms: routingLumen: Routing parameters have already been calculated.'

    if not arcpy.ListFields('nsegmentFL', 'seg_id'):
        print '...Assigning seg_id values to the new nsegment features.'
        arcpy.AddField_management ('nsegmentFL', 'seg_id', 'SHORT')
        counter=1
        rows=arcpy.UpdateCursor('nsegmentFL')
        for row in rows:
            row.setValue('seg_id', counter)
            rows.updateRow(row)
            counter+=1

    arcpy.MakeTableView_management('nsegment', 'nsegmentTV')
    arcpy.Sort_management('nsegmentTV', gdbFinalFD+'/nsegment_sort', 'seg_id')
    arcpy.Delete_management('nsegment')
    arcpy.Rename_management(gdbFinalFD+'/nsegment_sort', 'nsegment')
    arcpy.MakeFeatureLayer_management('nsegment', 'nsegmentFL')

    arcpy.AddField_management('nsegmentFL','tosegment','LONG')
    arcpy.CalculateField_management('nsegmentFL','tosegment','0', 'PYTHON')

    arcpy.MakeFeatureLayer_management(gdbWk+'\\Output\\nsegment_flowlines', 'nsegment_flowlinesFL')

    print 'param_prms: routingLumen: Beginning calculation of tosegment parameter.'
    if arcpy.ListFields('nsegment_flowlinesFL', 'seg_id'):
        arcpy.DeleteField_management('nsegment_flowlinesFL', 'seg_id')
    if arcpy.ListFields('nsegment_flowlinesFL', 'tosegment'):
        arcpy.DeleteField_management('nsegment_flowlinesFL', 'tosegment')

    print 'param_prms: routingLumen: Ensuring that \"flowline\" version of segment map has segment identifiers assigned.'
    arcpy.AddField_management('nsegment_flowlinesFL', 'seg_id', 'LONG')
    arcpy.AddField_management('nsegment_flowlinesFL', 'tosegment', 'LONG')
    arcpy.CalculateField_management('nsegment_flowlinesFL', 'seg_id', '0', 'PYTHON')
    arcpy.CalculateField_management('nsegment_flowlinesFL', 'tosegment', '0', 'PYTHON')

    print 'param_prms: routingLumen: ...finding flowines at end of each segment.'

    print 'param_prms: routingLumen: ...assigning segment identifiers for all flowlines terminating segments with non-zero POI IDs. (zer POI_IDs frequently seen at a network terminus).'
    print 'param_prms: routingLumen: ...first creating midpoints of these flowlines.'
    arcpy.FeatureVerticesToPoints_management('nsegment_flowlinesFL', 'outletMidPoints', 'MID')
    print 'param_prms: routingLumen: ...now overlaying with segment map to capture segment identifier.'
    arcpy.SpatialJoin_analysis('outletMidPoints', 'nsegment', 'outletMidPoints2', 'JOIN_ONE_TO_ONE')

    print 'param_prms: routingLumen: ...now pushing these segment identifiers back into the flowline map of segments.'
    arcpy.MakeFeatureLayer_management('outletMidPoints2', 'outletMidPoints2FL')
    arcpy.AddJoin_management('nsegment_flowlinesFL', 'COMID', 'outletMidPoints2FL', 'COMID', 'KEEP_COMMON')
    arcpy.CalculateField_management('nsegment_flowlinesFL', 'nsegment_flowlines.seg_id', '!outletMidPoints2.seg_id!', 'PYTHON')
    arcpy.RemoveJoin_management('nsegment_flowlinesFL', 'outletMidPoints2')

    print 'param_prms: routingLumen: Now calculating the tosegment parameter in flowline version of segment map.'
    print 'param_prms: routingLumen: ...now determining identity of downstream flowline (still working up to the downstream segment identification).'
    print 'param_prms: routingLumen: ...selecting flowines at bottoms of segments.'

    arcpy.Statistics_analysis('nsegment_flowlinesFL', gdbFinal+'\\junk_seq', [['HYDROSEQ', 'MIN']], 'seg_id')
    arcpy.AddJoin_management('nsegment_flowlinesFL', 'seg_id', gdbFinal+'\\junk_seq', 'seg_id')
    arcpy.SelectLayerByAttribute_management('nsegment_flowlinesFL', 'NEW_SELECTION', '"nsegment_flowlines.HYDROSEQ" = "junk_seq.MIN_HYDROSEQ"')
    arcpy.CopyFeatures_management('nsegment_flowlinesFL', 'outletFlowlines')
    arcpy.MakeFeatureLayer_management('outletFlowlines', 'outletFlowlinesFL')
    arcpy.RemoveJoin_management('nsegment_flowlinesFL', 'junk_seq')
    arcpy.SelectLayerByAttribute_management('nsegment_flowlinesFL', 'CLEAR_SELECTION')

    print 'param_prms: routingLumen: ...using TOCOMID2 to set tosegment in flowline version of segment map.'
    arcpy.AddJoin_management('outletFlowlinesFL', 'nsegment_flowlines_TOCOMID2', 'nsegment_flowlinesFL', 'COMID', 'KEEP_COMMON')
    arcpy.CalculateField_management('outletFlowlinesFL', 'outletFlowlines.nsegment_flowlines_tosegment', '!nsegment_flowlines.seg_id!', 'PYTHON')
    arcpy.RemoveJoin_management('outletFlowlinesFL', 'nsegment_flowlines')
    arcpy.AddJoin_management('nsegment_flowlinesFL', 'COMID', 'outletFlowlinesFL', 'nsegment_flowlines_COMID', 'KEEP_COMMON')
    arcpy.CalculateField_management('nsegment_flowlinesFL', 'nsegment_flowlines.tosegment', '!outletFlowlines.nsegment_flowlines_tosegment!', 'PYTHON')
    arcpy.RemoveJoin_management('nsegment_flowlinesFL', 'outletFlowlines')

    for junk in ['outletMidPoints', 'outletMidPoints2', gdbFinal+'\\junk_seq', 'outletFlowlines', 'nsegment_flowlines_copy']:
        if arcpy.Exists(junk):
            arcpy.Delete_management(junk)

    print 'param_prms: routingLumen: Setting tosegment values in segment map based on tosegment values calculated for the flowline version of the segment map.'
    if not arcpy.ListFields('nsegmentFL', 'tosegment'):
        arcpy.AddField_management('nsegmentFL', 'tosegment', 'LONG')
    arcpy.CalculateField_management('nsegmentFL', 'tosegment', '0', 'PYTHON')
    arcpy.SelectLayerByAttribute_management('nsegment_flowlinesFL', 'NEW_SELECTION', '"tosegment" > 0')
    arcpy.CopyFeatures_management('nsegment_flowlinesFL', 'junk')
    arcpy.AddJoin_management('nsegmentFL','seg_id','junk','seg_id', 'KEEP_COMMON')
    arcpy.CalculateField_management('nsegmentFL','nsegment.tosegment','!junk.tosegment!', 'PYTHON')
    arcpy.RemoveJoin_management('nsegmentFL', 'junk')

    # new checks mostly related to still having coastlines in there as streams--this should go eventually.
    arcpy.SelectLayerByAttribute_management('nsegmentFL', 'NEW_SELECTION', 'tosegment IS NULL')
    if int(arcpy.GetCount_management('nsegmentFL')[0]) > 0:
        arcpy.CalculateField_management('nsegmentFL', 'tosegment', '0', 'PYTHON')
    arcpy.SelectLayerByAttribute_management('nsegmentFL', 'NEW_SELECTION', 'seg_id = tosegment')
    if int(arcpy.GetCount_management('nsegmentFL')[0]) > 0:
        print 'There are '+str(int(arcpy.GetCount_management('nsegmentFL')[0]))+' segments that route to themselves. These are presumed to be artifacts (usually a coastline). These segments will be reattributed to route to "0".'
        arcpy.CalculateField_management('nsegmentFL', 'tosegment', '0', 'PYTHON')
    arcpy.SelectLayerByAttribute_management('nsegmentFL', 'CLEAR_SELECTION')

    if arcpy.Exists('POIs'):
        if arcpy.ListFields('POIsFL', 'poi_gage_id'):
            arcpy.DeleteField_management('POIsFL', 'poi_gage_id')
        arcpy.AddField_management('POIsFL','poi_gage_id','STRING', '', '', '', '', 'NULLABLE')
        cnt=0
        rows=arcpy.UpdateCursor('POIsFL')
        for row in rows:
            poi_gage_id=row.getValue('Type_GagesII')
            if poi_gage_id=='0':
                poi_gage_id=''
            if not poi_gage_id:
                poi_gage_id=row.getValue('Type_User')
            if not poi_gage_id:
                poi_gage_id='0'
            else:
                cnt+=1
            row.setValue('poi_gage_id', poi_gage_id)
            rows.updateRow(row)
        del row, rows

        if not arcpy.ListFields('POIsFL', 'poi_gage_segment'):
            arcpy.AddField_management ('POIsFL', 'poi_gage_segment', 'LONG')
        arcpy.CalculateField_management ('POIsFL','poi_gage_segment','0', 'PYTHON')

        arcpy.AddJoin_management ('POIsFL','COMID','nsegmentFL','POI_ID','KEEP_COMMON')
        arcpy.CalculateField_management ('POIsFL', 'POIs.poi_gage_segment', '!nsegment.seg_id!', 'PYTHON')
        arcpy.RemoveJoin_management('POIsFL', 'nsegment')

    if not arcpy.ListFields('nhruFL', 'hru_segment'):
        arcpy.AddField_management('nhruFL', 'hru_segment', 'LONG')
    arcpy.CalculateField_management ('nhruFL', 'hru_segment', '0', 'PYTHON')

    arcpy.MakeFeatureLayer_management ('nhruFL', 'nhruFL')
    arcpy.AddJoin_management ('nhruFL','POI_ID','nsegmentFL','POI_ID','KEEP_COMMON')
    arcpy.CalculateField_management ('nhruFL', 'nhru.hru_segment', '!nsegment.seg_id!', 'PYTHON')
    arcpy.RemoveJoin_management ('nhruFL', 'nsegment')

    # checking for non-contributing areas. This might need to be turned off in the future.
    arcpy.SelectLayerByAttribute_management ('nhruFL',"NEW_SELECTION","hru_segment IS NULL")
    if int(arcpy.GetCount_management('nhruFL')[0]) > 0:
        print 'param_prms: routingLumen: There was a NULL value for hru_segment. Please fix this. Terminating program.'
    else:
        print 'param_prms: routingLumen: hru_seg values are all legitimate.'

    if arcpy.Exists('POIs'):
        # Check for and remove POIs with dupclicate poi_gage_id values
        # this really should not be in here. Should be handled in the POI creation routines.
        arcpy.Select_analysis('POIsFL', 'POIsNonGages', 'poi_gage_id IS NULL')
        arcpy.Select_analysis('POIsFL', 'POIsGages', 'poi_gage_id IS NOT NULL')
        arcpy.MakeFeatureLayer_management('POIsGages','POIsgagesFL')
        arcpy.DeleteIdentical_management('POIsGagesFL', 'poi_gage_id', '','')

        # Sort the POIs by poi_gage_id so the parameter file is in this order

        if arcpy.Exists('POIsort'):
            arcpy.Delete_management('POIsSort')
        arcpy.Sort_management('POIs', 'POIsSort', 'poi_gage_id')
        arcpy.Delete_management('POIs')
        arcpy.Rename_management('POIsSort', 'POIs')

    if not arcpy.ListFields('nsegmentFL', 'K_coef'):
        arcpy.AddField_management ('nsegmentFL', 'K_coef', 'DOUBLE')
    arcpy.CalculateField_management ('nsegmentFL','K_coef','-99999', 'PYTHON')
    arcpy.CalculateField_management ('nsegmentFL','K_coef','!SUM_TRAV_TIME!', 'PYTHON')


def freshRunLumen(stuff):

##    print 'param_prms: freshRunLumen: this is turned off by default. Any pre-existing routing parameters will be reused.'
##    return

    cleanList=[['seg_id', 'tosegment']]
    freshRun(stuff['arcpy'], stuff['gdbFinalFD']+'/nsegment', cleanList)
    cleanList=[['poi_gage_segment']]
    freshRun(stuff['arcpy'], stuff['gdbFinalFD']+'/POIs', cleanList)
    cleanList=[['hru_segment']]
    freshRun(stuff['arcpy'], stuff['gdbFinalFD']+'/nhru', cleanList)

def elev_units(stuff, features):
    arcpy=stuff['arcpy']

    if arcpy.ListFields(features, 'elev_units'):
        print 'param_prms: elev_units: Elevation units already been determined.'
        return

    arcpy.AddField_management(features, 'elev_units', 'INTEGER')
    arcpy.CalculateField_management(features, 'elev_units', '1', 'PYTHON')

def basin_lat(stuff, features):
    arcpy=stuff['arcpy']
    gdbFinal=stuff['gdbFinal']
    spatialRef=stuff['spatialRef']

    if arcpy.ListFields(features, 'basin_lat'):
        print 'param_prms: basin_lat: basin_long and basin_lat have already been calculated.'
        return

    fieldListOneLocal=['basin_long', 'basin_lat']

    for field in fieldListOneLocal:
        arcpy.AddField_management(features, field, 'DOUBLE')
        arcpy.CalculateField_management(features, field, '-99999', 'PYTHON')

    arcpy.MakeFeatureLayer_management(features, 'featuresFL')

    print 'param_prms: basin_lat: Deriving Lat/Long coordinates for basin.'
    print 'param_prms: basin_lat: ...Deriving basin centroid.'
##    arcpy.FeatureToPoint_management('featuresFL', 'featurespts', 'INSIDE')
    if arcpy.ListFields('featuresFL', 'point_x'):
        arcpy.DeleteField_management('featuresFL', 'point_x')

    if not arcpy.Exists('featurespts'):
        arcpy.FeatureToPoint_management('featuresFL', 'featurespts', 'INSIDE')

    arcpy.Project_management('featurespts', gdbFinal+'/one_pts_geog', spatialRef) # this is weird. Project does not want to add output to feature dataset, only to top-level of gdb
    arcpy.AddXY_management(gdbFinal+'/one_pts_geog')
    arcpy.AddJoin_management('featuresFL', 'OBJECTID', gdbFinal+'/one_pts_geog', 'ORIG_FID', 'KEEP_COMMON')
    arcpy.CalculateField_management ("featuresFL", "basin_long", "!one_pts_geog.point_x!", 'PYTHON')
    arcpy.CalculateField_management ("featuresFL", "basin_lat", "!one_pts_geog.point_y!", 'PYTHON')
    arcpy.RemoveJoin_management('featuresFL', 'one_pts_geog')

    print 'param_prms: basin_lat: ...Cleaning up.'
    arcpy.Delete_management('featurespts')
    arcpy.Delete_management(gdbFinal+'/one_pts_geog')

def basin_area(stuff, features):
    arcpy=stuff['arcpy']

    if arcpy.ListFields(features, 'basin_area'):
        print 'param_prms: basin_area: basin_area has already been calculated.'
        return

    arcpy.AddField_management(features, 'basin_area', 'DOUBLE')
    arcpy.CalculateField_management(features, 'basin_area', '-99999', 'PYTHON')
    arcpy.CalculateField_management(features, 'basin_area', '!shape.area@acres!', 'PYTHON')

def freshRun(arcpy, features, fieldList):

    if arcpy.Exists(features):
        print 'param_prms: freshRun: Removing previously derived parameter values from '+features+' prior to parameterizing.'
        for fields in fieldList:
            for field in fields:
                if arcpy.ListFields(features, field):
                    print 'param_prms: freshRun: ...Removing '+field+'.'
                    arcpy.DeleteField_management(features, field)
                else:
                    print 'param_prms: freshRun: ...'+field+' is not present.'

def main(datadir, Region):

##    ok = raw_input('Do you want to erase all previous derivatives and have a \"fresh run\"? (blank entry/default is understood as \"No\")')
    ok='No'

    import gc, re, arcpy
    gc.enable()

    timberline=str(3500) #str(11500 / 3.280840)

    gdbFinal = datadir+ "/NHDPlus"+Region+"_Final.gdb"
    db_wk = datadir+ "/NHDPlus"+Region+"_wk.gdb"
    gdbFinalFD = gdbFinal+"/PRMS"
    arcpy.env.workspace = gdbFinalFD
    arcpy.env.overwriteOutput = True

##    import mows_util
##    prod_units = mows_util.ensureProdUnitList(arcpy, db_wk)
    if not arcpy.Exists(gdbFinal+'/prod_units'):
        arcpy.MakeTableView_management(gdbFinal+'/PRMS/nhru', 'nhruTV')
        arcpy.Statistics_analysis('nhruTV', gdbFinal+'/prod_units', [['PROD_UNIT', 'MIN']], 'PROD_UNIT')

    prod_units = list()
    for row in arcpy.SearchCursor(gdbFinal+'/prod_units'):
        prod_units.append(row.getValue('PROD_UNIT'))
##

    if not arcpy.ListFields('nhru', 'hru_id'):
        arcpy.AddField_management('nhru', 'hru_id', 'LONG')
        arcpy.CalculateField_management('nhru', 'hru_id', '!hru_id_local!', 'PYTHON')

    if not arcpy.ListFields('nhru','hru_id_prod_unit'):
        arcpy.AddField_management('nhru', 'hru_id_prod_unit', 'Integer')
        arcpy.CalculateField_management('nhru', 'hru_id_prod_unit', 0, 'PYTHON')
        arcpy.MakeFeatureLayer_management('nhru', 'featuresFL')
        for prod_unit in prod_units:
            arcpy.SelectLayerByAttribute_management('featuresFL', 'NEW_SELECTION', 'PROD_UNIT=\''+prod_unit+'\'')
            print 'hru_creation: Adding production unit-based HRU identification numbers.'
            rows=arcpy.UpdateCursor('featuresFL')
            counter=1
            for row in rows:
                row.setValue('hru_id_prod_unit', counter)
                rows.updateRow(row)
                counter+=1

    # "units" is kind of dumb, but end up wanting a shorter suffix so I don't run into grid name length limits
    units=list()
    suffix=re.compile('[0-9]+')
    arcpy.MakeFeatureLayer_management('nhru', 'featuresFL')
    for prod_unit in prod_units:
        units.append(re.sub(suffix, '', prod_unit))
##        if arcpy.Exists('nhru_'+prod_unit):
##            arcpy.Delete_management('nhru_'+prod_unit)
        if not arcpy.Exists('nhru_'+prod_unit):
            arcpy.SelectLayerByAttribute_management('featuresFL', 'NEW_SELECTION', 'PROD_UNIT=\''+prod_unit+'\'')
            arcpy.CopyFeatures_management('featuresFL', gdbFinalFD+'/nhru_'+prod_unit)
        elif not arcpy.ListFields('nhru_'+prod_unit,'hru_id_prod_unit'):
            arcpy.Delete_management('nhru_'+prod_unit)
            arcpy.SelectLayerByAttribute_management('featuresFL', 'NEW_SELECTION', 'PROD_UNIT=\''+prod_unit+'\'')
            arcpy.CopyFeatures_management('featuresFL', gdbFinalFD+'/nhru_'+prod_unit)

    token=re.compile('_[0-9a-z]*')
    RegionNoSubset=re.sub(token, '', Region)
    databin=datadir+'/databin'+RegionNoSubset

    arcpy.env.cellSize=databin+'\\extra\wk_topo\\elev_'+units[0]
    arcpy.env.snapRaster=databin+'\\extra\wk_topo\\elev_'+units[0]

    import os, sys
    spatialRef = arcpy.SpatialReference(102039)     #USA Contiguous Albers Equal Area Conic USGS.prj

    del os

    stuff={}
    stuff['arcpy']=arcpy
    stuff['datadir']= datadir
    stuff['Region']= Region
    stuff['prod_units']=prod_units
    stuff['units']=units
    stuff['gdbFinal']=gdbFinal
    stuff['gdbFinalFD']=gdbFinalFD
    stuff['spatialRef']= spatialRef
    stuff['databin']=databin
    stuff['timberline']= timberline

    arcpy.CheckOutExtension("Spatial")

    fieldListNhru=list()
    fieldListNssr=list()
    fieldListNgw=list()
    fieldListNsegment=list()
    fieldListNpoigages=list()
    fieldListOne=list()
    fieldListNhru.append(['hru_x', 'hru_y', 'hru_lat', 'hru_area'])
    fieldListNhru.append(['hru_elev', 'hru_slope'])
    fieldListNhru.append(['hru_aspect']) #, 'asp_cos', 'asp_sin'])
    fieldListNhru.append(['jh_coef_hru'])
    fieldListNhru.append([ 'tmin_adj', 'tmax_adj'])
    fieldListNhru.append(['snarea_thresh'])
    fieldListNhru.append(['hru_deplcrv'])
    fieldListNhru.append(['soil_moist_max', 'soil_rechr_max'])
    fieldListNhru.append(['soil_type'])
    fieldListNhru.append(['cov_type'])
    fieldListNhru.append(['covden_sum'])
    fieldListNhru.append(['covden_win'])
    fieldListNhru.append(['rad_trncf'])
    fieldListNhru.append(['srain_intcp', 'wrain_intcp','snow_intcp'])
    fieldListNhru.append(['hru_percent_imperv'])
    fieldListNhru.append(['dprst_area'])
    fieldListNhru.append(['sro_to_dprst'])
    fieldListNhru.append(['soil2gw_max', 'fastcoef_lin', 'slowcoef_lin', 'dprst_seep_rate_open', 'dprst_seep_rate_clos', 'dprst_flow_coef'])
    fieldListNssr.append(['ssr2gw_rate'])
    fieldListNgw.append(['gwflow_coef'])
    fieldListNhru.append(['hru_segment'])

    fieldListNsegment.append(['tosegment', 'K_coef'])

    fieldListNpoigages.append(['poi_gage_id', 'poi_gage_segment'])

    fieldListOne.append(['basin_area'])
    fieldListOne.append(['elev_units'])
    fieldListOne.append(['basin_lat'])

    if ok in ('y', 'Y', 'yes', 'YES', 'Yes'):
        freshRun(arcpy, 'nhru', fieldListNhru)
        freshRun(arcpy, 'nhru', fieldListNssr)
        freshRun(arcpy, 'nhru', fieldListNgw)
        freshRun(arcpy, 'nsegment', fieldListNsegment)
        freshRun(arcpy, 'POIs', fieldListNpoigages)
        freshRun(arcpy, 'one', fieldListOne)

### clean-up for rerunning the routing
### BE SURE TO ERASE SEG_ID AND TOSEGMENT FROM THE NSEGMENT_FLOWLINES FC IN THE *_WK.GDB!!!!!!!
##    cleanList=[['seg_id', 'poi_gage_segment', 'tosegment']]
##    freshRun(arcpy, 'nsegment', cleanList)
##    cleanList=[['hru_long', 'temp_adj', 'rad_trncfd', 'hru_segment', 'poi_gage_segment', 'tosegment']]
##    freshRun(arcpy, 'nhru', cleanList)
##
##    cleanList=[['hru_long', 'temp_adj', 'rad_trncfd']] #'srain_intcp', 'wrain_intcp', 'snow_intcp']] #
##    freshRun(arcpy, 'nhru', cleanList)
##
##    freshRunLumen(stuff)
##    routingLumen(stuff, 'nhru')

    basin_area(stuff, 'one')
    elev_units(stuff, 'one')
    basin_lat(stuff, 'one')

    writeWParFile(stuff, fieldListNhru, fieldListNsegment, fieldListNpoigages, fieldListOne, fieldListNssr, fieldListNgw)

    del arcpy

if __name__ == '__main__':
    import sys
    main(sys.argv[1], sys.argv[2])