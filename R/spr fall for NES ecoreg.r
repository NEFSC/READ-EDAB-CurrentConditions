# trans dates from sst already extracted by grid file areas
library(raster)

# path for files
#setwd("E:/4_rs_data_temperature/oisst_25deg_daily/FULL_AV.TEMP.1DAY/NESREG")
setwd("e:/4_rs_data_temperature/oisst_25deg_daily/FULL_AV.TEMP.1DAY/NESREG") #### We will need to deal with this
### Sometimes there is not a fall transition if the transition date falls outside of the search window




# where the extracted data is located
# TS_SHP_adv rep MAB GOM GBK NES SCSPoly.RData
# TS_SHP_nes five areasPoly.csv
# TS_SHP_DFO_GBKPoly.csv
# TS_SHP_adv rep MAB GOM GBK NES SCSPoly.csv
regdata=read.csv(file.choose(),header=T)

# where the data output goes, file in in wd
ofile = "nes ecoreg ftrans.csv"
#ofile = "nes five areas ftrans.csv"
#ofile = "dfo gbk ftrans.csv"

# get rid of previous output file
file.remove(ofile)

# set lastyear
lastyear = max(regdata$Y)


table(regdata$Location)

eregs=c("GBK","GOM","MAB","NES","SCS")
# or just get them
eregs = unique(regdata$Location)

for (yy in 1982:lastyear){
  for (ereg in eregs){
    print(c(yy,ereg))

    # long term mean based on 1982 to 2011, ie 30 years
    erdata=regdata$Mean[regdata$Y>1981 & regdata$Y<2012 & regdata$Location==ereg]
    ltmean=mean(erdata, na.rm=T)
#    minld=min(tyr)
#    maxld=max(tyr)

    sprtrans=NA
    falltrans=NA
    maxday=NA
    sumlen=NA

    tyr=regdata$Mean[regdata$Y==yy & regdata$Location==ereg]
    styr=movingFun(tyr, 5, fun=mean, type='around', circular=FALSE, na.rm=T)

    maxsstyr=max(styr)

    for (dayc in 105:195){
      if (styr[dayc] > ltmean){
        sprtrans=dayc
        break
      }
    }

    if(length(styr)>364){

    if(length(styr)>182){
      for (dayc in 275:365){
      if (styr[dayc] < ltmean){
        falltrans=dayc
        break
      }
    }
    for (dayc in 1:365){
      if (styr[dayc] == maxsstyr){
        maxday=dayc
        break
      }
    }
   }

    } # loop to guide aginst incomplete last year

    sumlen=falltrans-sprtrans

    outline=paste(ereg,yy, sprtrans, falltrans, maxday, sumlen)
    write.table(outline,file=ofile,row.name=F,col.names=F,append=TRUE)

  }  # ecoregion
} #year








plot(styr)



ldmean=mean(tyr, na.rm=T)
minld=min(tyr)
maxld=max(tyr)

sprtrans=NA
falltrans=NA
maxday=NA


styr=movingFun(tyr, 5, fun=mean, type='around', circular=FALSE, na.rm=T)
maxslocaldata=max(styr)


  for (dayc in 105:195){
    if (styr[dayc] > ldmean){
      sprtrans=dayc
      break
    }
  }
  for (dayc in 275:365){
    if (styr[dayc] < ldmean){
      falltrans=dayc
      break
    }
  }
  for (dayc in 1:365){
    if (styr[dayc] == maxslocaldata){
      maxday=dayc
      break
    }
  }



