#!/bin/bash

#script to run through and reformat usgs streamflow output files

for file in huc_??_orig_1980
do
  region="${file:4:2}"

  awk -f reformat_streamflow_general_leap.awk < ${file} > huc_${region}_streamflow_1980_leap_v2

done