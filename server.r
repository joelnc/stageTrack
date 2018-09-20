library(shiny)
library(plotly)

#############################################################################

shinyServer(function(input, output, session) {

    ## Maps
    output$mapy <- renderLeaflet({

        ## icons <- awesomeIcons(
        ##     icon = 'glyphicon-arrow-up',
        ##     iconColor = 'red',
        ##     markerColor = '#FF0000',
        ##     library = 'glyphicon'
        ## )

        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik, group="Streets") %>%
            addProviderTiles(providers$Esri.WorldImagery, group="Aerials") %>%
            addCircleMarkers(data=siteCoor, radius=3,
                             group="Selected Monitoring Location") %>%
            ## addAwesomeMarkers(data=siteCoor, icon=icons) %>%
            fitBounds(lng1=min(siteCoor$longitude),
                      lng2=max(siteCoor$longitude),
                      lat1=min(siteCoor$latitude),
                      lat2=max(siteCoor$latitude)) %>%
            addLayersControl(
                baseGroups = c("Streets", "Aerials"),
                overlayGroups = c("Watershed", "Stream")
            )
    })


    ## add CSS style 'cursor: pointer' to the 0-th column (i.e. row names)
    output$x1 = DT::renderDataTable({
        datatable(data=refrData(),
                  rownames=FALSE,
                  escape=FALSE,
                  selection="none",
                  options = list(pageLength = 60,
                                 dom = 't',
                                 escape=TRUE,
                                 autoWidth = TRUE,
                                 scrollX=TRUE,
                                 columnDefs = list(
                                     list(visible=FALSE, targets = c(0,2,6:8)),
                                     list(width = '250px', targets = c(1)))
                                 )
                  ) %>%
            formatStyle(
                'change30', 'change30YN',
                backgroundColor = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('#ef6548', '#addd8e',NA,NA)),
                fontWeight = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('bold', 'bold',NA,NA))
            ) %>%
            formatStyle(
                'change15', 'change15YN',
                backgroundColor = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('#ef6548', '#addd8e',NA,NA)),
                fontWeight = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('bold', 'bold',NA,NA))
            ) %>%
            formatStyle(
                'change5', 'change5YN',
                backgroundColor = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('#ef6548', '#addd8e',NA,NA)),
                fontWeight = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('bold', 'bold',NA,NA))
            ) %>%
            formatRound('change30', 2) %>%
            formatRound('change15', 2) %>%
            formatRound('change5', 2)

    })


    ## Reacive dataset
    refrData <- eventReactive(input$go, {

        ########################################################################
        ## Pars
        QParameterCd <- "00065" ## stage, feet
        startTime <- Sys.Date()-input$daysBack ## yesterday
        ## Stage list from Florence excel set
        sitesData <<- data.frame(SiteNames=c("Irwin Creek @ WWTP","Little Sugar @ Archdale Drive",
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
                                SiteCodes=c("02146300","02146507","02146600","0214645022","02146750","02142900",
                                            "02146381","0214295600","0212414900","02146409","02146470",
                                            "0214685800","02146700","02146530","02146670","0214642825",
                                            "0214266000","02146211","0214678175","02146315","0214291555",
                                            "02146348","0214657975","0214655255","02146562","0214627970",
                                            "02146285","0212467451","0212467595","0212466000","0214266080",
                                            "0214297160","02124080","0214643860","0214640410","0212393300",
                                            "0214643820", "02142914","02146449","0214668150","0214643770",
                                            "02146420","0214645080","0212430293","0214645075","0214676115",
                                            "0214265808","02142654","02146330","0212430653","02146614","02124269",
                                            "0212427947"),
                                stringsAsFactors=FALSE)

        ## Pull most recent data
        recentQ <- readNWISuv(siteNumbers=sitesData$SiteCodes,
                              parameterCd=QParameterCd,
                              startDate=startTime, tz="America/New_York")
        rQ <- recentQ[,c("site_no", "dateTime", "X_00065_00000")]

        ## #############################################################################
        ## ## insert code here to make graphs to hyperlink to sites
        list1 <<- split(rQ,rQ$site_no)

        ## quickPlot <- function(df,name0)  {
        ##     png(filename=paste0("www/",name0,".png"), width=5, height=3, units="in",
        ##         res=150)
        ##     par(mai=c(.5,.5,.4,.1), xaxs="i", yaxs="i")
        ##     plot(df$dateTime, df$X_00065_00000, type="l")#,
        ##          ## ylim=c(0,2))
        ##     ## lines(x=c(min(df$dateTime), max(df$dateTime)),
        ##     ##       y=rep(sample(c(5,6,7),1),2),
        ##     ##       col="red",lty=2)
        ##     mtext(side=3,
        ##           text=paste(name0, sitesData$SiteNames[which(sitesData$SiteCodes==name0)]))
        ##     ## plot(df$dateTime, df$X_00065_00000, type="l")
        ##     dev.off()
        ## }

        ## mapply(df=list1, name0=names(list1), FUN=quickPlot)

        #############################################################################
        ## Keep last 12 hours
        rQ <- filter(rQ,
                     dateTime>=Sys.time()-(60*60*2))


        ## Reshape
        r2 <- reshape(rQ, idvar="site_no", v.names="X_00065_00000",
                      timevar="dateTime", direction="wide")

        ## Rename
        names(r2) <- substring(names(r2), 20, 30)
        names(r2)[1] <- "SiteCode"
        r3 <- r2[sort(names(r2), decreasing=TRUE)]
        r3 <- r3[order(r3$SiteCode),]


        SiteNames <- sitesData$SiteNames[order(sitesData$SiteCodes)]
        ## Add on site names
        r4 <- cbind(SiteNames, r3)

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


        r4$Site <- r4$SiteNames

        r4 <<- select(r4, Site, SiteNames, SiteCode,
                      change5, change15, change30,
                      change5YN, change15YN, change30YN,
                      everything())

    })



    observeEvent(input$x1_cell_clicked, {

        info <- input$x1_cell_clicked

        if (is.null(info$value)) {
            return()
        } else if (!is.null(info$value) & info$col==1){
            showModal(
                modalDialog(
                    ## htmlOutput("text"),
                    plotlyOutput("graphs"),
                    easyClose = TRUE,
                    footer = NULL,
                    size="l"
                )
            )
        }
    })


    observeEvent(input$x1_cell_clicked, {

        sitesData <<- data.frame(SiteNames=c("Irwin Creek @ WWTP","Little Sugar @ Archdale Drive",
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
                                SiteCodes=c("02146300","02146507","02146600","0214645022","02146750","02142900",
                                            "02146381","0214295600","0212414900","02146409","02146470",
                                            "0214685800","02146700","02146530","02146670","0214642825",
                                            "0214266000","02146211","0214678175","02146315","0214291555",
                                            "02146348","0214657975","0214655255","02146562","0214627970",
                                            "02146285","0212467451","0212467595","0212466000","0214266080",
                                            "0214297160","02124080","0214643860","0214640410","0212393300",
                                            "0214643820", "02142914","02146449","0214668150","0214643770",
                                            "02146420","0214645080","0212430293","0214645075","0214676115",
                                            "0214265808","02142654","02146330","0212430653","02146614","02124269",
                                            "0212427947"),
                                stringsAsFactors=FALSE)

        info <- input$x1_cell_clicked

        ##browser()
        if (is.null(info$value)) {
            return()
        } else if (!is.null(info$value) & info$col==1){

            ds <- list1[[sitesData$SiteCode[which(sitesData$SiteName==info$value)]]]

            output$graphs <- renderPlotly({
                mm <- plot_ly(data=ds,
                              x=~dateTime) %>%
                    add_lines(y=~X_00065_00000,
                              name=sitesData$SiteCode[which(sitesData$SiteName==info$value)]) %>%
                    layout(
                        title=paste(info$value, sitesData$SiteCode[which(sitesData$SiteName==info$value)]),
                        xaxis = list(
                            rangeselector = list(
                                buttons = list(
                                    list(
                                        count = 3,
                                        label = "3 h",
                                        step = "hour",
                                        stepmode = "backward"),
                                    list(
                                        count = 12,
                                        label = "12 h",
                                        step = "hour",
                                        stepmode = "backward"),
                                    list(
                                        step = "all")
                                    )),
                            rangeslider = list(type="date")),
                        yaxis=list(title="Stage (ft)")
                    )
                mm
            })

        }



        ## output$text <- renderUI({
        ##     HTML(paste0("<img src='",
        ##                 sitesData$SiteCode[which(sitesData$SiteName==info$value)],
        ##                 ".png'>"))
        ## })
    })

})


