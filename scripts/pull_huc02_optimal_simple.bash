#!/bin/bash


#region=16
#for r in 1[01234568]
#for r in 0[123456789]
#for r in 10
#for r in 08 09 10 11 12
#for r in 01 03 04 05 06 07 08 09 11 12 13 14 15 16 18 
for r in 03
do
#  region="${r:2:2}"
  region=${r}
  filename=region_${region}_opt.txt

  if [ -f /d2/anewman/hcdn_output/${region}/${filename} ]; then
    rm /d2/anewman/hcdn_output/${region}/${filename}
    touch /d2/anewman/hcdn_output/${region}/${filename}
  else
    touch /d2/anewman/hcdn_output/${region}/${filename}
  fi

  for basin in /d2/anewman/hcdn_output/${region}/*optimal*.txt
  #for basin in ./hcdn_output/009065500*seed*.txt
  do

    gauge="${basin:27:9}"
    seed="${basin:37:2}"
    echo ${gauge} ${seed} | tee -a /d2/anewman/hcdn_output/${region}/${filename}

    tail -n 1 ${basin} >> /d2/anewman/hcdn_output/${region}/${filename}

  done
done