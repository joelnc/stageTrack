rm(list=ls())


ds <- read.table("stageTrack/FinsliveStreamDataDump.csv", header=TRUE,
                 stringsAsFactors=FALSE,sep=",", fill=TRUE,
                 colClasses=c("integer", "character", "character",
                              "character", "character", rep("numeric",4),
                              "character", rep("numeric",35)),
                 strip.white=TRUE)

ds <- ds[,c(1:3,6:9)]

saveRDS(ds, "stageTrack/siteCoords.rds")
