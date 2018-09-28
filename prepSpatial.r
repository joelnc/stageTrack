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


#############################################################################
#############################################################################
## Flood stage and reference elevation stuff

#############################################################################
#############################################################################
library(dplyr)
library(dataRetrieval)

rm(list=ls())
siteCoor <- readRDS("stageTrack/siteCoords.rds")

mm <- lapply(X=siteCoor$SiteCode, function(x)
    readNWISsite(siteNumbers=x))


## Collaps list to single df
nn <- do.call("rbind",mm)
n2 <- nn[, c("site_no", "dec_lat_va", "dec_long_va", "alt_va")]


#############################################################################
## load gages shapefile with flood stages

bb <- readRDS("leaflet/gisRds/stageGages.rds")

b2 <- bb@data
b2$usgs_stati <- as.character(b2$usgs_stati)
b2$usgs_stati <- paste0("0", b2$usgs_stati)
b2$floodstage <- as.numeric(as.character(b2$floodstage))

bc <- merge(n2, b2, by.x="site_no", by.y="usgs_stati", all.x=TRUE)
bc$floodHeight <- bc$floodstage-bc$alt_va

## Clean up
bc2 <- merge(bc, siteCoor, by.x="site_no", by.y="SiteCode", all=TRUE)

bc2 <- bc2[,c("site_no","dec_lat_va","dec_long_va","alt_va",
              "floodstage","floodHeight","usgsLink","SiteName")]

saveRDS(bc2, "stageTrack/siteCoordsFloodStage.rds")

