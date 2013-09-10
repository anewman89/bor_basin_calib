#!/bin/bash

region=01

#for basin in ./hcdn_forcing/*0.txt
for basin in 001013500 001022500 001030500 001031500 001047000 001052500 001054200 001055000 001057000 001073000 001078000 001118300 001121000 001123000 001134500 001137500 001139000 001139800 001142500 001144000 001162500 001169000 001170100 001181000 001187300 001195100 004296000
do
  for seed in 05 11 27 33 48 59 66 72 80 94
  do
    echo "Working on: " ${basin} " , seed: " ${seed} 

    gauge="${basin:0:9}"
    

    awk '{a="'${basin}'";b="'${gauge}'";r="'${region}'";s="'${seed}'";
	  if ($1 == "forcing_name") printf("forcing_name  = \"/snowdata/anewman/daymet/cida_areal_average/huc_02_regions/simple/%02d/%s_lump_cida_forcing.txt\"\n",r,b);
	  else if ($1 == "stream_name") printf("stream_name  = \"/d1/anewman/sacsnow17/operational_system/hcdn_streamflow/%02d/%s_streamflow_1980.txt\"\n",r,b); 
	  else if ($1 == "model_out")   printf("model_out = \"/d1/anewman/sacsnow17/operational_system/hcdn_output/huc_02_regions/simple/%02d/%s_model_output.txt\"\n",r,b);
	  else if ($1 == "opt")   printf("opt = 1\n");
	  else if ($1 == "iseed") printf("iseed = %02d\n",s);
	  else print $0}' namelist.model.huc_${region} > namelist.model.region_${region}

    rm namelist.model
    ln -sf namelist.model.region_${region} namelist.model

  #   ./test_hcdn_opt.exe
  #  ./test_hcdn_opt.exe >& ${gauge}_optimal_sceua.txt
    ./test_hcdn_huc02.exe >& ${gauge}_${seed}_optimal_sceua.txt
    mv *optimal_sceua.txt /d1/anewman/sacsnow17/operational_system/hcdn_output/huc_02_regions/simple/${region}/
  done
done