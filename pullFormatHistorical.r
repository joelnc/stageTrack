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



## Reshape to 365 rows by date, columns for each site's value
## n2 <- reshape(nn,
##               v.names="mean_va",
##               idvar="site_no",
##               direction="wide",
##               timevar="dateThing")

## Bingo...
n3 <- reshape(nn,
              v.names="mean_va",
              idvar="dateThing",
              direction="wide",
              timevar="site_no")

## Save to rds

## Load rds into stage tracker app

## have stage tracker plotly plots include noon points representing average stage for that day.
