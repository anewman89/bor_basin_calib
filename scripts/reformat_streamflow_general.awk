BEGIN{FS="\t"}
{
  if(substr($1,31,39) == "Discharge, cubic feet per second (Mean)")
    a = sprintf("%s_%s_%s",substr($1,6,2),substr($1,11,5),substr($1,21,5))
  
  if($1 == "agency_cd")
  {
    for(i = 1; i <= NF; i++)
    {
      if($i == a)
        dis = i
    }
  }
#print a,dis

  if($1 == "USGS")
  {
    year = substr($3,1,4)

    #daymet forcing is on 365 day calendar 
    #they keep leap day and then drop 12/31 of that year...
    #so do the same for streamflow
    if((year%4) == 0)
      flag = 1
    else
      flag = 0

    month = substr($3,6,2)
    day   = substr($3,9,2)
    if(flag == 1 && month == 12 && day == 31)
    {
    }
    else
    {
      printf("%s %s %s %s %8.2f\n",$2,year,month,day,$dis)
    }

  }
}