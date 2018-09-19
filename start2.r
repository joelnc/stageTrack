library(shiny)
library(dataRetrieval)
library(dplyr)
library(DT)
library(formattable)


#############################################################################
# Pars
QParameterCd <- "00065" ## stage, feet
startTime <- Sys.Date()-1 ## yesterday
## Stage list from Florence excel set

siteNames <- data.frame(SiteName=c("Irwin Creek @ WWTP","Little Sugar @ Archdale Drive",
                                   "McAlpine @ Sardis", "Briar Creek @ Colony Road",
                                   "McAlpine @ WWTP","Long Creek @ Paw Creek",
                                   "Sugar Creek @ NC51 Pineville","Paw Creek @ Wilkinson Blvd",
                                   "Mallard Creek @ Harrisburg","Little Sugar @ Medical Center",
                                   "Little Hope @ Seneca Place","SixMile Creek nr Pineville",
                                   "McMullen Creek @ Sharon View","Little Sugar @ NC51 Pineville",
                                   "Fourmile Creek @ Elm Lane","Briar @ Shamrock Dr",
                                   "McDowell Creek @ Beatties", "Irwin Creek @ Statesville Ave",
                                   "Steele Creek @ Carowinds Blvd", "Taggart Creek @ West Blvd",
                                   "Long Creek @ Rhyne NC","Coffey Creek @ NC49",
                                   "Irvins @ Sam Newell Road","McAlpine Creek @ Idlewild",
                                   "Campbell Creek @ Idlewild", "Stewart Creek @ State St",
                                   "Stewart Creek @ Morehead",
                                   "Goose Creek at 1524 nr Indian Trail","Goose Creek @ SR1525",
                                   "Clear Creek @ SR3181", "Gar Creek @ SR2074",
                                   "Beaverdam Creek Shopton","ClarkeCreek nr Harrisburg",
                                   "Briar Creek bl Edwards B","Little Sugar @ 36 St",
                                   "W Br Rocky River bl Mth", "Edwards Branch @ Sheffield",
                                   "Gum Branch nr Thrift NC","Briar Creek @ Providence",
                                   "McMullen Creek @ Lincrest","Briar Creek @ Independence",
                                   "Little Sugar @ Hillside", "Trib to Briar Creek",
                                   "Reedy Creek bl I-485","Tr Briar Creek @ Colony",
                                   "McAlpine Creek @ SR2964","Torrence Creek @ Bradford",
                                   "McDowell Creek nr Huntersville", "Sugar Creek @ Arrowood",
                                   "McKee Creek @ SR2804","McAlpine Creek @ Colony Rd",
                                   "Back Creek @ SR1173","Reedy Creek @ SR2803"),
                        stringsAsFactors=FALSE)

siteCodes <- c("02146300","02146507","02146600","0214645022","02146750","02142900","02146381",
               "0214295600","0212414900","02146409","02146470","0214685800","02146700",
               "02146530","02146670","0214642825","0214266000","02146211","0214678175",
               "02146315","0214291555","02146348","0214657975","0214655255","02146562",
               "0214627970","02146285","0212467451","0212467595","0212466000","0214266080",
               "0214297160","02124080","0214643860","0214640410","0212393300","0214643820",
               "02142914","02146449","0214668150","0214643770","02146420","0214645080",
               "0212430293","0214645075","0214676115","0214265808","02142654","02146330",
               "0212430653","02146614","02124269","0212427947")


## Pull most recent data
recentQ <- readNWISuv(siteNumbers=siteCodes,
                             parameterCd=QParameterCd,
                             startDate=startTime, tz="America/New_York")
rQ <- recentQ[,c("site_no", "dateTime", "X_00065_00000")]

#############################################################################

## File naming not working...
## insert code here to make graphs to hyperlink to sites
list1 <- split(rQ,rQ$site_no)

quickPlot <- function(df,name0)  {
    png(filename=paste0("stageTrack/", name0,".png"), width=5, height=3, units="in",
        res=300)
    plot(df$dateTime, df$X_00065_00000, type="l")
    dev.off()
}


mapply(df=list1, name0=names(list1), FUN=quickPlot)

#############################################################################
## Keep last 12 hours
rQ <- filter(rQ,
             dateTime>=Sys.time()-(60*60*12))



## Reshape
r2 <- reshape(rQ, idvar="site_no", v.names="X_00065_00000",
              timevar="dateTime", direction="wide")

## Rename
names(r2) <- substring(names(r2), 20, 30)
names(r2)[1] <- "SiteCode"
r3 <- r2[sort(names(r2), decreasing=TRUE)]

## Add on site names
r4 <- cbind(siteNames, r3)

## Change columns
r4$change5 <- r4[,3]-r4[,4]
r4$change5YN <- rep("Na",53)
r4$change5YN[which(r4$change5<0)] <- "Down"
r4$change5YN[which(r4$change5==0)] <- "Same"
r4$change5YN[which(r4$change5>0)] <- "Up"

r4$change15 <- r4[,3]-r4[,6]
r4$change15YN <- rep("Na",53)
r4$change15YN[which(r4$change15<0)] <- "Down"
r4$change15YN[which(r4$change15==0)] <- "Same"
r4$change15YN[which(r4$change15>0)] <- "Up"

r4$change30 <- r4[,3]-r4[,9]
r4$change30YN <- rep("Na",53)
r4$change30YN[which(r4$change30<0)] <- "Down"
r4$change30YN[which(r4$change30==0)] <- "Same"
r4$change30YN[which(r4$change30>0)] <- "Up"


## r4$Site <- paste0("<a href='C:\\Users\\95218.CHARLOTTE\\Documents\\R\\stageTrack\\",
##                   r4$SiteCode,".png'> ", r4$SiteName,"</a>")
r4$Site <- paste0("<a href='#'><img src='C:/Users/95218.CHARLOTTE/Documents/R/stageTrack/",
                  r4$SiteCode,".png' /><span>", r4$SiteName,"</span></a>")

## Reorder
r4 <- select(r4, SiteName, Site, SiteCode,
             change5, change15, change30,
             change5YN, change15YN, change30YN,
             everything())

#############################################################################
## Data Tables
datatable(data=r4,
          rownames=FALSE,
          escape=FALSE,
          options = list(pageLength = 60,
                         dom = 't',
                         escape=FALSE,
                         autoWidth = TRUE,
                         columnDefs = list(
                             list(visible=FALSE, targets = c(0,2,6:8)),
                             list(width = '200px', targets = 1))
                         )
          ) %>%
    formatStyle(
        'change30', 'change30YN',
        backgroundColor = styleEqual(c('Up', 'Down', 'Niether', 'Na'), c('red', 'green','white','white'))
    ) %>%
    formatStyle(
        'change15', 'change15YN',
        backgroundColor = styleEqual(c('Up', 'Down', 'Niether', 'Na'), c('red', 'green','white','white'))
    ) %>%
    formatStyle(
        'change5', 'change5YN',
        backgroundColor = styleEqual(c('Up', 'Down', 'Niether', 'Na'), c('red', 'green','white','white'))
    ) %>%
    formatRound('change30', 2) %>%
    formatRound('change15', 2) %>%
    formatRound('change5', 2)

#############################################################################



                                           list(visible=FALSE, targets = 'change30YN'))


r4 %>% replace(., is.na(.), "")


#r4 <- replace_na(list(current=0, minus5=0))


sign_formatter <- formatter("span",
                            style = x ~ style(color = ifelse(x > 0, "red",
                                                      ifelse(x < 0, "green", "black")),
                                              "font-weight" = ifelse(x > 0, "bold",
                                                      ifelse(x < 0, "bold","italics")
                                                  )
                                              )
                            )


formattable(r4,list(change30 = sign_formatter))

as.datatable(formattable(r4,list(change30 = sign_formatter)),
             rownames=FALSE,
             options = list(pageLength = 60,
                            dom = 't',
                            autoWidth = TRUE,
                            columnDefs = list(list(width = '200px', targets = c(0))))
             )








names(r4)[3] <- "current"
names(r4)[4] <- "minus5"
names(r4)[5] <- "minus10"
names(r4)[6] <- "minus15"


cl <- function(dfC1, dfC2) {

}

colorIs <- data.frame(current=rep(NA, 53), minus5=rep(NA, 53),
                      minus10=rep(NA, 53), minus15=rep(NA,53),
                      stringsAsFactors=FALSE)





entity <- c('entity1', 'entity2', 'entity3')
value1 <- c(21000, 23400, 26800)
value2 <- c(21234, 23445, 26834)
value3 <- c(21123, 234789, 26811)
value4 <- c(27000, 23400, 26811)
entity.data <- data.frame(entity, value1, value2, value3, value4)

# Create a vector of max values
max_val <- apply(entity.data[, -1], 2, max)

## greater than previous fun
gtp0 <- r4$current>r4$minus5
gtp0[which(is.na(gtp0))] <- 0
gtp0[which(gtp0==FALSE)] <- 1
gtp0[which(gtp0==TRUE)] <- 2

gtp0 <- rep(NA,53)
gtp0[which(is.na(r4$current>r4$minus5))] <- 0
gtp0[which(r4$current>r4$minus5)] <- 1
gtp0[which(r4$current<r4$minus5)] <- 2

gtp1 <- rep(NA,53)
gtp1[which(is.na(r4$minus5>r4$minus10))] <- 0
gtp1[which(r4$minus5>r4$minus10)] <- 2
gtp1[which(r4$minus5<r4$minus10)] <- 3
gtp1[which(r4$minus5==r4$minus10)] <- 1

gtp1 <- data.frame(gtp1=gtp1)


datatable(data=r4,
          rownames=FALSE,
          options = list(pageLength = 60,
                         dom = 't',
                         autoWidth = TRUE,
                         columnDefs = list(list(width = '200px', targets = c(0))))
          ) %>%
    formatStyle(
        'minus5', 'minus10',
        backgroundColor = styleEqual(c(2.22,1,2,3), c("red", "blue", "green", "grey"))
    )




## or





## Subset out last hours worth
rQ <- filter(recentQ, dateTime>Sys.time()-3600)
