library(dplyr)
library(dataRetrieval)

rm(list=ls())

siteCoor <- readRDS("stageTrack/siteCoords.rds")

mm <- lapply(X=siteCoor$SiteCode, function(x)
    readNWISstat(siteNumbers=x, parameterCd="00065", statReportType="daily"))

## Collaps list to single df
nn <- do.call("rbind",mm)




nn$dateThing <- as.Date(paste0("2018-", nn$month_nu,"-", nn$day_nu))
nn <- nn[,c(2,11,12)]


## Bingo...
n3 <- reshape(nn,
              v.names="mean_va",
              idvar="dateThing",
              direction="wide",
              timevar="site_no")

## Save to rds
saveRDS(n3, file="stageTrack/histDailyFlow.rds")

## Load rds into stage tracker app

## have stage tracker plotly plots include noon points representing average stage for that day.

#############################################################################
## Historical Series
rm(list=ls())
histDaily <- readRDS("stageTrack/histDailyFlow.rds")


st <- st <- as.POSIXct("2018-01-01 11:00", tz="America/Panama")
dtSeq <- c(st + cumsum(c(0,rep(c(7200,3600,21*3600),365),100)))[1:1095]
dateSeq <- data.frame(dates=as.Date(dtSeq))

## merge the flow stats with the repetitive dates
bb <- merge(dateSeq, histDaily, by.x="dates", by.y="dateThing")

## take posix dts on
cc <- cbind(bb, dtSeq)

## NA out 14:00 readings
cc[seq(3,1095, by=3),2:54] <- NA

saveRDS(cc, file="stageTrack/histDailyFlow_Formatted.rds")


## df 'cc' looks ready to go.  In plotly, id the column of data for selected site,
##  identify the row range for current plot, add lines

#############################################################################
#############################################################################




plotSite <- "02146300"
colIndex <- which(grepl(plotSite, names(histDaily)))
datesPresent <- unique(as.Date(ds$dateTime))

dateVec <- .POSIXct(NA[seq(1,length(datesPresent)*4)])
dateVec <-  .POSIXct(character(length(datesPresent)*4), tz="America/Panama")
i11 <- seq(2,length(dateVec),4)
dateVec[i11-1] <- as.POSIXct(paste(datesPresent,"10:00"), tz="America/Panama")
dateVec[i11] <- as.POSIXct(paste(datesPresent,"11:00"), tz="America/Panama")
dateVec[i11+1] <- as.POSIXct(paste(datesPresent,"13:00"), tz="America/Panama")
dateVec[i11+2] <- as.POSIXct(paste(datesPresent,"14:00"), tz="America/Panama")

sVec <- rep(NA, length(dateVec))

sVec[