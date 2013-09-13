#!/bin/bash

#script to get USGS streamflow gauge data from web via wget
#note that this script will download daily data
#for an individual station over a specified time range
#data comes as tab delimited ascii file (not compressed)
#file contains all available data from station (e.g. streamflow, water temperature, conductivity, etc)
#creator: Andy Newman
#date:    05 Sept 2013



#specify time interval
#both need to be YYYYMMDD
start_date=19800101
end_date=20101231


#for individul values, use: 1 2 3 4
#user needs to enter list of gauge ids after 'for gauge in'
#gauge ids are either 8 or 9 digits with one leading zero always
for gauge in 02051000
do

  wget 'http://waterdata.usgs.gov/nwis/dv?referred_module=sw&search_site_no='${gauge}'&search_site_no_match_type=exact&site_tp_cd=OC&site_tp_cd=OC-CO&site_tp_cd=ES&site_tp_cd=LK&site_tp_cd=ST&site_tp_cd=ST-CA&site_tp_cd=ST-DCH&site_tp_cd=ST-TS&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&range_selection=date_range&begin_date='${start_date}'&end_date='${end_date}'&format=rdb&date_format=YYYY-MM-DD&rdb_compression=value&list_of_search_criteria=search_site_no%2Csite_tp_cd%2Crealtime_parameter_selection' -O /d2/anewman/usgs_streamflow/orig/gauges/${gauge}_usgs_streamflow_${start_date}_${end_date}.txt  

done

