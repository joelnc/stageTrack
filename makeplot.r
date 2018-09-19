
        ########################################################################
        ## Pars
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

## Made up
floodStages <- c(rep(c(10,9,7),18))[-1]


        ## Pull most recent data
        recentQ <- readNWISuv(siteNumbers=siteCodes,
                              parameterCd=QParameterCd,
                              startDate=startTime, tz="America/New_York")
        rQ <- recentQ[,c("site_no", "dateTime", "X_00065_00000")]

list1 <- split(rQ,rQ$site_no)

df <- list1[[1]]

## png(filename=paste0("www/",name0,".png"), width=5, height=3, units="in",
##     res=150)
graphics.off()
dev.new(width=5, height=3)
par(mai=c(.5,.5,.4,.1), xaxs="i", yaxs="i")
plot(df$dateTime, df$X_00065_00000, type="l",
     ylim=c(0,11))
lines(x=c(min(df$dateTime), max(df$dateTime)),
      y=rep(sample(floodStages,1),2),
      col="red",lty=2)

##dev.off()




        #############################################################################
        ## insert code here to make graphs to hyperlink to sites
        list1 <- split(rQ,rQ$site_no)

        quickPlot <- function(df,name0)  {
            png(filename=paste0("www/",name0,".png"), width=5, height=3, units="in",
                res=150)
            plot(df$dateTime, df$X_00065_00000, type="l")
            dev.off()
        }

        mapply(df=list1, name0=names(list1), FUN=quickPlot)
