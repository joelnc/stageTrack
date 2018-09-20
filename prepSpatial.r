library(dplyr)
rm(list=ls())

## Read in fins working table from florence
ds <- read.table("stageTrack/FinsliveStreamDataDump.csv", header=TRUE,
                 stringsAsFactors=FALSE,sep=",", fill=TRUE,
                 colClasses=c("integer", "character", "character",
                              "character", "character", rep("numeric",4),
                              "character", rep("numeric",35)),
                 strip.white=TRUE)

ds <- ds[,c(2:3,6:7)]
colnames(ds)[1] <- "SiteName"
colnames(ds)[2] <- "SiteCode"


ds$usgsLink <- paste0("https://waterdata.usgs.gov/nc/nwis/uv?site_no=", ds$SiteCode)

saveRDS(ds, "stageTrack/siteCoords.rds")
