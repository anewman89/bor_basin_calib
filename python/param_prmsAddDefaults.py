def writeDefault(wparFileNew, defaults, dimSizes, hruElev, snareaCurves):

    wparFileNew.write('####\n')
    wparFileNew.write(defaults[0]+' 10\n')
    wparFileNew.write(defaults[1]+'\n')
    wparFileNew.write(defaults[2]+'\n')
    dim=defaults[2]
    if defaults[1]=='2':
        wparFileNew.write(defaults[3]+'\n')
        dim=defaults[2]+'\n'+defaults[3]
    wparFileNew.write(str(dimSizes[dim])+'\n')
    wparFileNew.write(defaults[4]+'\n')
    if defaults[0]=='snarea_curve_hru':
        for i in range(dimSizes['nhru']):
            if hruElev[i] > 3500:
                curveNumber=1
            else:
                curveNumber=2
            for j in range(11):
                wparFileNew.write(str(snareaCurves[curveNumber][j])+'\n')
    if defaults[0]=='snarea_curve':
        for curveNumber in range(2):
            for j in range(11):
                wparFileNew.write(str(snareaCurves[curveNumber][j])+'\n')
    else:
        for i in range(dimSizes[dim]):
            if defaults[0] in ['tmin_cbh_adj', 'tmax_cbh_adj']:
                wparFileNew.write(defaults[5][i]+'\n')
            else:
                wparFileNew.write(defaults[5]+'\n')

def main(datadir, Region):

    import re, urllib

    print 'param_prmsAddDefaults: Reading in default parameter settings.'
    defaultPropertiesFile=urllib.urlopen('ftp://brrftp.cr.usgs.gov/pub/mows/params/PAR4_param_defaults.txt')
    defaults={}
    line=defaultPropertiesFile.readline()
    for line in defaultPropertiesFile:
        fields=line.split('\t')
        name=fields[0]
        print name
        ndims=fields[1]
        dimension1=fields[2]
        dimension2=fields[3]
        type=fields[4]
        value=fields[5]
        monthly=fields[6]
        defaults[name]=[name, ndims, dimension1, dimension2, type, value, monthly]

    del defaultPropertiesFile
    urllib.urlcleanup()

    # KLUDGES
##    defaults['dday_intcp_hru'][2]='nhru\nnmonths'
##    import copy
##    defaults['snarea_curve_hru']=copy.copy(defaults['snarea_curve'])
##    defaults['snarea_curve_hru'][0]='snarea_curve_hru'
##    defaults['snarea_curve_hru'][2]='nhru\nndeplval'
    snareaCurves=[[0.05,0.22,0.40,0.53,0.65,0.75,0.82,0.88,0.93,0.99,1.0] , \
    [0.05,0.25,0.40,0.48,0.54,0.58,0.61,0.64,0.66,0.68,0.70]]

    inFile=datadir+'/Region'+Region+'.wpar'
    print 'param_prmsAddDefaults: Reading in the initial PRMS parameter file and writing to output file, '+inFile+'.wdefaults.'
    wparFile=open(inFile, 'r')
    wparFileNew=open(datadir+'/Region'+Region+'wdefaults.wpar', 'w')

    dimSizes={}
    inHeader=True
    lastLine=''
    hruElev=list();flagHruElev=False;flagHruElevCounter=0
    tmax_adj=list();flagTmax_adj=False;flagTmax_adjCounter=0
    tmin_adj=list();flagTmin_adj=False;flagTmin_adjCounter=0

    tokenNhru=re.compile('nhru[ ]*\n'); tokenNsegment=re.compile('nsegment[ ]*\n'); tokenNpoigages=re.compile('npoigages[ ]*\n')
    for line in wparFile:
        if line=='** Parameters **\n':
            inHeader=False
            dimSizes['nssr']=dimSizes['nhru']
            dimSizes['ngw']=dimSizes['nhru']
            dimSizes['nobs']=dimSizes['npoigages']
            dimSizes['one']=1
            dimSizes['nmonths']=12
            dimSizes['ndepl']=2
            dimSizes['ndeplval']=11 # actually 11 * depl val, but this is expressed oddly in the PRMS file
            dimSizes['ntemp']=0
            dimSizes['nrain']=0
            dimSizes['ndays']=366
            dimSizes['nhru\nnmonths']=dimSizes['nhru']*dimSizes['nmonths']
            dimSizes['ndepl\nndeplval']=dimSizes['ndepl']*dimSizes['ndeplval']
            dimSizes['nhru\nndeplval']=dimSizes['nhru']*dimSizes['ndeplval']
            dimSizes['ndeplval']=22 # actually 11 * depl val, but this is expressed oddly in the PRMS file

            for dim in ['ngw', 'nssr', 'nmonths', 'ndepl', 'ndeplval', 'nobs']:
                wparFileNew.write('####\n')
                wparFileNew.write(dim+'\n')
                wparFileNew.write(str(dimSizes[dim])+'\n')

        if re.findall('hru_elev', line):
            flagHruElev=True
        elif flagHruElev and re.findall('####', line):
            flagHruElev=False
        elif flagHruElev and flagHruElevCounter < 4:
            flagHruElevCounter+=1
        elif flagHruElev:
            hruElev.append(line.replace('\n', ''))

        if re.findall('tmax_adj', line):
            flagTmax_adj=True
        elif flagTmax_adj and re.findall('####', line):
            defaults['tmax_cbh_adj'][5]=tmax_adj
            flagTmax_adj=False
        elif flagTmax_adj and flagTmax_adjCounter < 4:
            flagTmax_adjCounter+=1
        elif flagTmax_adj:
            tmax_adj.append(line.replace('\n', ''))

        if re.findall('tmax_adj', line):
            flagTmin_adj=True
        elif flagTmin_adj and re.findall('####', line):
            defaults['tmin_cbh_adj'][5]=tmin_adj
            flagTmin_adj=False
        elif flagTmin_adj and flagTmin_adjCounter < 4:
            flagTmin_adjCounter+=1
        elif flagTmin_adj:
            tmin_adj.append(line.replace('\n', ''))

        wparFileNew.write(line)

        if (tokenNhru.match(lastLine) or tokenNsegment.match(lastLine) or tokenNpoigages.match(lastLine)) and inHeader:
            dim=lastLine.replace('\n','')
            dimSizes[dim]=int(line.replace('\n',''))
        lastLine=line

    wparFile.close()


    print 'param_prmsAddDefaults: Writing default parameters out.'
    for parName in defaults:
        print 'param_prmsAddDefaults: ...'+parName
        if defaults[parName][6]!='jan-dec':
            writeDefault(wparFileNew, defaults[parName], dimSizes, hruElev, snareaCurves)
        else:
            origName=defaults[parName][0]
            for month in ['jan','feb','mar','apr','may','jun', 'jul','aug','sep','oct','nov','dec']:
                monthlyParam=defaults[parName]
                monthlyParam[0]=origName.strip()+month
                writeDefault(wparFileNew, monthlyParam, dimSizes, hruElev, snareaCurves)

    wparFileNew.close()

    import zipfile, os
    os.chdir(datadir)
    z=zipfile.ZipFile('Region'+Region+'wdefaults.wpar'+'.zip', 'w')
    z.write('Region'+Region+'wdefaults.wpar')
    z.close()
    import os
##    os.remove(inFile)

    print 'param_prmsAddDefaults: Done.'

if __name__ == '__main__':
    import sys
    main(sys.argv[1], sys.argv[2])