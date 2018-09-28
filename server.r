library(shiny)
library(plotly)

#############################################################################

shinyServer(function(input, output, session) {


    ######################################################################
    ######################################################################
    ## Data

    ## Pull data for full period of interest
    baseData <- reactive({

        ## Pars
        QParameterCd <- "00065" ## stage, feet
        ## startTime <- Sys.Date()-input$daysBack ## yesterday
        startTime <- as.Date(format(Sys.time(), tz="America/Panama"))-input$dSlide

        ## Pull most recent data
        recentQ <- readNWISuv(siteNumbers=siteCoor$SiteCode,
                              parameterCd=QParameterCd,
                              startDate=startTime, tz="America/New_York")
        rQ <- recentQ[,c("site_no", "dateTime", "X_00065_00000")]

        return(list(rQ=rQ,
                    latLon=attributes(recentQ)$siteInfo[,c("site_no","dec_lat_va","dec_lon_va")]
                    )
               )

    })

    ## If button clicked, add on most recent data.  Otherwise pass through
    useData <- reactive({

        if (input$updData==0) {
            rQ <- as.data.frame(baseData()[["rQ"]])
            list1 <- split(rQ, rQ$site_no)
            outData <- list(rQ=rQ, list1=list1,
                            latLon=as.data.frame(baseData()[["latLon"]])
                            )

        } else {

            ## original data
            rQ_Orig <- as.data.frame(baseData()[["rQ"]])

            ## Pull today
            ## Pars
            QParameterCd <- "00065" ## stage, feet
            ## startTime <- Sys.Date()-input$daysBack ## yesterday
            startTime <- as.Date(format(Sys.time(), tz="America/Panama"))

            ## Pull most recent data
            recentQ <- readNWISuv(siteNumbers=siteCoor$SiteCode,
                                  parameterCd=QParameterCd,
                                  startDate=startTime, tz="America/New_York")
            rQ_New <- recentQ[,c("site_no", "dateTime", "X_00065_00000")]

            ## Rbind old and new, drop dupes
            rQ_Updated <- rbind(rQ_Orig, rQ_New)
            rQ_Updated$key <- paste(rQ_Updated$site_no, rQ_Updated$dateTime)
            rQ_Updated <- rQ_Updated[-which(duplicated(rQ_Updated$key)),c(1:3)]

            list1 <- split(rQ_Updated, rQ_Updated$site_no)
            outData <- list(rQ=rQ_Updated, list1=list1,
                            latLon=as.data.frame(baseData()[["latLon"]])
                            )
        }
        return(outData)
    })

    ## Take
    fData <- reactive({

        ##browser()
        rQ <- as.data.frame(useData()[["rQ"]])
        latLon <- as.data.frame(useData()[["latLon"]])

        ## Keep last 12 hours
        rQ <- filter(rQ,
                     dateTime>=Sys.time()-(60*60*2))

        ## Pull out newest data dateTime for each site
        newestDt <- rQ %>%
            group_by(site_no) %>%
            filter(dateTime == max(dateTime)) %>%
            select(site_no, dateTime) %>%
            mutate(def=difftime(Sys.time(), dateTime, units="min")) %>%
            as.data.frame()

        ## Reshape
        r2 <- reshape(rQ, idvar="site_no", v.names="X_00065_00000",
                      timevar="dateTime", direction="wide")

        ## Rename
        names(r2) <- substring(names(r2), 20, 30)
        names(r2)[1] <- "SiteCode"
        r3 <- r2[sort(names(r2), decreasing=TRUE)]
        r3 <- r3[order(r3$SiteCode),]

        SiteNames <- siteCoor$SiteName[order(siteCoor$SiteCode)]

        ## Add on site names
        r4 <- cbind(SiteNames, r3)

        r5 <- merge(r4, latLon,
                    by.x="SiteCode", by.y="site_no", all=TRUE)

        ## Also merge the "how old is newest data" column
        r5 <- merge(r5, newestDt, by.x="SiteCode", by.y="site_no", all=TRUE)


        ## This produces an array with column indexes correpsondig to first numeric
        ## data per site
        b <- rep(NA,53)
        for (i in 1:53) {
            b[i] <- which(names(r5)==format(r5$dateTime[i], format="%m-%d %H:%M"))
        }

        ## Change columns
        for (i in 1:53) {
                    r5$change5[i] <- r5[i,b[i]]-r5[i,b[i]+1]
                    r5$change5YN <- rep("Na",53)
                    r5$change5YN[which(r5$change5<0)] <- "Down"
                    r5$change5YN[which(r5$change5==0)] <- "Same"
                    r5$change5YN[which(r5$change5>0)] <- "Up"

                    r5$change15[i] <- r5[i,b[i]]-r5[i,b[i]+3]
                    r5$change15YN <- rep("Na",53)
                    r5$change15YN[which(r5$change15<0)] <- "Down"
                    r5$change15YN[which(r5$change15==0)] <- "Same"
                    r5$change15YN[which(r5$change15>0)] <- "Up"

                    r5$change30[i] <- r5[i,b[i]]-r5[i,b[i]+6]
                    r5$change30YN <- rep("Na",53)
                    r5$change30YN[which(r5$change30<0)] <- "Down"
                    r5$change30YN[which(r5$change30==0)] <- "Same"
                    r5$change30YN[which(r5$change30>0)] <- "Up"
        }

        r5$Site <- r5$SiteNames

        ## Try adding on the first data column indexes
        r5$currentI <- b

        r5$Graph <- paste0("'<img src='icon", r5$SiteCode,  ".png' height='20'></img>'")

        r5 <- select(r5, Site, Graph, SiteNames, SiteCode,
                     "5-Min Change"=change5, "15-Min Change"=change15,
                     "30-Min Change"=change30, change5YN, change15YN, change30YN,
                     "Minutes Since"=def, everything(), latitude=dec_lat_va,
                      longitude=dec_lon_va, currentI)

        return(list(r5=r5))
    })

    ######################################################################
    ######################################################################
    ## Maps
    output$mapy <- renderLeaflet({

        ## This needs to be linked up with actual stage or whatever data....
        pal <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#d1e5f0',
                 '#92c5de','#4393c3','#2166ac','#053061')

        ds <- fData()[["r5"]]
        ##browser()
        dd <- rep(NA, 53)
        for (i in 1:53) {
            dd[i] <- ds[i,ds$currentI[i]+9]
        }

        myPal <- colorNumeric(
            palette = "RdYlBu",
            domain = dd, reverse=FALSE)

        myPalR <- colorNumeric(
            palette = "RdYlBu",
            domain = dd, reverse=TRUE)


        leaflet() %>%
            ## addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group="Streets") %>% #.Mapnik
            addProviderTiles(providers$Esri.WorldGrayCanvas, group="Streets") %>% #.Mapnik
            addCircleMarkers(data=ds, radius=3, color=~myPalR(dd),
                             fillOpacity=0.75, stroke=FALSE,
                             popup=paste(ds$SiteNames,
                                         br()
                                         )
                             ) %>%
            addLegend(position = c("topright"), myPal, values=dd, na.label = "NA",
                      bins = 10, opacity = 0.7, labels = NULL,
                      className = "info legend",
                      layerId = NULL, group = NULL,
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                      title="Gage Height (ft)"
                      ) %>%

              fitBounds(lng1=min(ds$longitude),
                      lng2=max(ds$longitude),
                      lat1=min(ds$latitude),
                      lat2=max(ds$latitude)
                      )

    })

    ## Previuos row holder
    prev_row <- reactiveVal()


    observeEvent(input$x1_rows_selected, {
        row_selected = as.data.frame(fData()[["r5"]])[input$x1_rows_selected,]

        ## set new value to reactiveVal
        prev_row(row_selected)

        proxy <- leafletProxy('mapy')

        ## Reset previously selected marker
        if(!is.null(prev_row()))
        {
            proxy %>%
                addCircleMarkers(radius=5, color="red",
                    popup=paste(prev_row()$SiteNames,
                               br(),
                               paste0("<a href='",
                                      siteCoor$usgsLink[which(siteCoor$SiteName==prev_row()$SiteNames)],
                                      "'>USGS</a>"),
                               "(Right click to open in new window)"),
                    layerId = "Selected Site",
                    lng=prev_row()$longitude,
                    lat=prev_row()$latitude)
        }
    })


    ######################################################################
    ######################################################################
    ## Table
    output$x1 = DT::renderDataTable({
        datatable(data=as.data.frame(fData()[["r5"]]),##data=refrData(),
                  rownames=FALSE,
                  escape=FALSE,
                  selection = "single",
                  options = list(pageLength = 60,
                                 stateSave = TRUE,
                                 dom = 't',
                                 escape=TRUE,
                                 autoWidth = TRUE,
                                 scrollX=TRUE,
                                 columnDefs = list(
                                     list(visible=FALSE, targets = c(0,3,7:9,34:36)),
                                     list(width = '225px', targets = c(2)),
                                     list(className='dt-center', targets=c(4:6,10:15))
                                 )
                                 )
                  ) %>%
            formatStyle(
                '30-Min Change', 'change30YN',
                backgroundColor = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('#ef6548', '#addd8e',NA,NA)),
                fontWeight = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('bold', 'bold',NA,NA))
            ) %>%
            formatStyle(
                '15-Min Change', 'change15YN',
                backgroundColor = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('#ef6548', '#addd8e',NA,NA)),
                fontWeight = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('bold', 'bold',NA,NA))
            ) %>%
            formatStyle(
                '5-Min Change', 'change5YN',
                backgroundColor = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('#ef6548', '#addd8e',NA,NA)),
                fontWeight = styleEqual(c('Up', 'Down', 'Niether', 'Na'),
                                             c('bold', 'bold',NA,NA))
            ) %>%
            formatRound('30-Min Change', 2) %>%
            formatRound('15-Min Change', 2) %>%
            formatRound('5-Min Change', 2) %>%
            formatRound('Minutes Since',1)

    })

    ######################################################################
    ######################################################################
    ## Graphing functions, possible that two observeEvents not needed
    observeEvent(input$x1_cell_clicked, {

        info <- input$x1_cell_clicked

        if (is.null(info$value)) {
            return()
        } else if (!is.null(info$value) & info$col==1){
            showModal(
                modalDialog(
                    plotlyOutput("graphs"),
                    easyClose = TRUE,
                    footer = NULL,
                    size="l"
                )
            )
        }
    })


    observeEvent(input$x1_cell_clicked, {

        info <- input$x1_cell_clicked

        if (is.null(info$value)) {
            return()
        } else if (!is.null(info$value) & info$col==1){
            tic("click graph")

            plotSite <- substr(gsub(".*on(.*)","\\1", info$value),
                               1, nchar(gsub(".*on(.*)","\\1", info$value))-25)

            ds <- useData()[["list1"]][[plotSite]]

            ## Subset historical daily flows
            hData <- histDaily %>%
                filter(dates %in% as.Date(ds$dateTime, tz="America/New_York")) %>%
                select(dtSeq, names(histDaily)[which(grepl(plotSite, names(histDaily)))])

            output$graphs <- renderPlotly({
                mm <- plot_ly(data=ds, x=~dateTime) %>%
                    add_lines(y=~X_00065_00000,
                              name=plotSite) %>%
                    add_lines(x=c(min(ds$dateTime), max(ds$dateTime)), y=rep(hData[2,2]*4,2),
                              name="Fake Flood Stage") %>%
                    add_lines(x=hData$dtSeq, y=hData[,2], name="LT Average") %>%

                    layout(
                        title=paste(plotSite, siteCoor$SiteName[which(siteCoor$SiteCode==plotSite)]),
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
                toc()
                mm
            })
        }
    })
})
