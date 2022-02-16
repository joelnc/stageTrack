library(dplyr)
library(dataRetrieval)

rm(list=ls())

## Load file with USGS site codes, names, lat longs
siteCoor <- readRDS("stageTrack/siteCoords.rds")

## Drop the briar tribs that were discontiuned in 2020
siteCoor <- siteCoor[-c(43,45),]

## Pull all usgs daily stage data for site codes in siteCoor file
listOfDfs <- lapply(X=siteCoor$SiteCode, function(x)
    readNWISstat(siteNumbers=x, parameterCd="00065", statReportType="daily"))

## Collaps list to single df
stageDf <- do.call("rbind", listOfDfs)

## Format R date
stageDf$dateThing <- as.Date(paste0(substr(Sys.time(), 1,4),"-",
                               stageDf$month_nu,"-", stageDf$day_nu))

## drop columns
stageDf <- stageDf[,c(2,11,12)]


## Bingo... do some reshaping 
siteCol <- reshape(stageDf,
              v.names="mean_va",
              idvar="dateThing",
              direction="wide",
              timevar="site_no")

dateCol <- reshape(stageDf,
              v.names="mean_va",
              idvar="site_no",
              direction="wide",
              timevar="dateThing")

histStuff <- list(siteCol=siteCol, dateCol=dateCol)

## Save to rds
saveRDS(histStuff, file="stageTrack/histDailyFlow.rds")

## Load rds into stage tracker app

## have stage tracker plotly plots include noon points representing average stage for that day.

#############################################################################
#############################################################################
## Historical Series
rm(list=ls())
histDaily <- readRDS("stageTrack/histDailyFlow.rds")

histDaily.bySite <- histDaily[["siteCol"]]
histDaily.byDate <- histDaily[["dateCol"]]

## st <- st <- as.POSIXct("2020-01-01 11:00", tz="America/Panama")
st <- as.POSIXct("2022-01-01 11:00", tz="America/Panama")
dtSeq <- c(st + cumsum(c(0,rep(c(7200,3600,21*3600),366),100)))[1:1095]
dateSeq <- data.frame(dates=as.Date(dtSeq))

## merge the flow stats with the repetitive dates
bb <- merge(dateSeq, histDaily.bySite, by.x="dates", by.y="dateThing")

## tack posix dts on
cc <- cbind(bb, dtSeq)

## NA out 14:00 readings
cc[seq(3,1095, by=3),2:52] <- NA

saveRDS(cc, file="stageTrack/histDailyFlow_Formatted.rds")

dd <- cc[seq(1,1095,3),]
dd$dtSeq <- dd$dtSeq-(60*60*5)
saveRDS(dd, file="stageTrack/histDailyFlow_FormattedPoints.rds")

saveRDS(histDaily.byDate, file="stageTrack/histFlowTable.rds")
