library(shiny)
library(dataRetrieval)
library(dplyr)
library(DT)


#############################################################################
# Pars
QParameterCd <- "00065" ## stage, feet
startTime <- Sys.Date()-.05 ## yesterday
## Stage list from Florence excel set
siteCodes <- c("02146670","02146670","0214642825","0214266000","02146211",
               "0214678175","02146315","0214291555","02146348","0214657975",
               "0214655255","02146562","0214627970","02146285","0212467451",
               "0212467595","0212466000","0214266080","0214297160","02124080",
               "0214643860","0214640410","0212393300","0214643820","02142914",
               "02146449","0214668150","0214643770","02146420","0214645080",
               "0212430293","0214645075","0214676115","0214265808","02142654",
               "02146330","0212430653","02146614","02124269","0212427947")

## Pull most recent data
recentQ <- readNWISuv(siteNumbers=siteCodes,
                             parameterCd=QParameterCd,
                             startDate=startTime, tz="America/New_York")


rQ <- recentQ[,c("site_no", "dateTime", "X_00065_00000")]


r2 <- reshape(rQ, idvar="site_no", v.names="X_00065_00000",
              timevar="dateTime", direction="wide")

names(r2[-1]) <- substring(names(r2[-1]), 19, 30)

datatable(data=r2,
          rownames=FALSE,
          options = list(pageLength = 60))


## Subset out last hours worth
rQ <- filter(recentQ, dateTime>Sys.time()-3600)
