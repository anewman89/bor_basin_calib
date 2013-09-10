#!/bin/bash

region=01

#switch for calibration/validation time period
#val_f = 0 is for calibration
#val_f = 1 is for validation
val_f=1

#for basin in ./hcdn_forcing/*0.txt
for basin in 001013500 001022500 001030500 001031500 001047000 001052500 001054200 001055000 001057000 001073000 001078000 001118300 001121000 001123000 001134500 001137500 001139000 001139800 001142500 001144000 001162500 001169000 001170100 001181000 001187300 001195100 004296000
do
  for seed in 05 11 27 33 48 59 66 72 80 94
  do
    echo "Working on: " ${basin} " , seed: " ${seed} 

    gauge="${basin:0:9}"
    

    awk '{a="'${basin}'";b="'${gauge}'";r="'${region}'";s="'${seed}'";v="'${val_f}'";
	  if ($1 == "forcing_name") printf("forcing_name  = \"/d2/anewman/daymet_forcing/lump/%02d/%s_lump_cida_forcing.txt\"\n",r,b);
	  else if ($1 == "stream_name") printf("stream_name  = \"/d2/anewman/usgs_streamflow/%02d/365/%s_streamflow_1980.txt\"\n",r,b); 
	  else if ($1 == "model_out")   printf("model_out = \"/d2/anewman/hcdn_output/%02d/%s_%02d_model_output.txt\"\n",r,b,s);
	  else if ($1 == "opt_name")     printf("opt_name = \"/d2/anewman/hcdn_output/%02d/region_%02d_opt.txt\"\n",r,r);
          else if ($1 == "gage_id") printf("gage_id = %09d\n",b)
	  else if ($1 == "opt")   printf("opt = 0\n");
	  else if ($1 == "val_period")   printf("val_period = %d\n",v);
	  else if ($1 == "iseed") printf("iseed = %02d\n",s);
	  else print $0}' namelist.model.huc_${region} > namelist.model.region_${region}

    rm namelist.model
    ln -sf namelist.model.region_${region} namelist.model

    ./test_hcdn_huc02.exe
  done
done