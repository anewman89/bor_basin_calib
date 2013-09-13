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
    month = substr($3,6,2)
    day   = substr($3,9,2)
#    print length($dis) 
#    print $dis
    if(length($dis) == 0)
      flow = -999.0
    else
      flow = $dis


    printf("%s %s %s %s %8.2f\n",$2,year,month,day,flow)

  }
}