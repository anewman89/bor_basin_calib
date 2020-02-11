#!/bin/bash


for basin in 001013500 009081600
do
  for seed in 41
  do
    echo "Working on: " ${basin} " , seed: " ${seed} 

    gauge="${basin:0:9}"
    

    awk '{a="'${basin}'";b="'${gauge}'";s="'${seed}'";
	  if ($1 == "forcing_name") printf("forcing_name  = \"/snowdata/anewman/daymet/cida_areal_average/huc_02_regions/simple/%02d/%s_lump_cida_forcing.txt\"\n",b);
	  else if ($1 == "stream_name") printf("stream_name  = \"/d1/anewman/sacsnow17/operational_system/hcdn_streamflow/%02d/%s_streamflow_1980.txt\"\n",b);
	  else if ($1 == "model_out")   printf("model_out = \"/d1/anewman/sacsnow17/operational_system/hcdn_output/huc_02_regions/simple/%02d/%s_model_output.txt\"\n",b);
	  else if ($1 == "opt")   printf("opt = 1\n");
	  else if ($1 == "iseed") printf("iseed = %02d\n",s);
	  else print $0}' ./namelists/namelist.model.example > ./run/namelist.model.region_${basin}

    ./nwsrfs_calib.exe >& ${gauge}_${seed}_optimal_sceua.txt #sequential calibrations
#    ./nwsrfs_calib.exe >& ${gauge}_${seed}_optimal_sceua.txt  &    #ampersand sends process to background so you could launch all basins in basin list at same time
    mv *optimal_sceua.txt ./output/
  done
done

#if using ampersand for calibration processes need wait line so script will wait for background processes to finish before exiting
#wait
