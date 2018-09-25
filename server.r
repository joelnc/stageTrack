library(shiny)
library(plotly)

#############################################################################

shinyServer(function(input, output, session) {

    ######################################################################
    ######################################################################
    ## Maps
    output$mapy <- renderLeaflet({

        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik, group="Streets") %>%
            addCircleMarkers(data=siteCoor, radius=3,
                             popup=paste(siteCoor$SiteName,
                                         br(),
                                         paste0("<a href='",
                                                siteCoor$usgsLink[which(siteCoor$SiteName==siteCoor$SiteNames)],
                                               "'>USGS</a>", " (Right click to open in new window)")
                                         )) %>%
                             ## popup=siteCoor$SiteName) %>%
            ## addAwesomeMarkers(data=siteCoor, icon=icons) %>%
            fitBounds(lng1=min(siteCoor$longitude),
                      lng2=max(siteCoor$longitude),
                      lat1=min(siteCoor$latitude),
                      lat2=max(siteCoor$latitude)
                      )
    })

    ## Previuos row holder
    prev_row <- reactiveVal()


    observeEvent(input$x1_rows_selected, {
        row_selected = refrData()[input$x1_rows_selected,]

        ## set new value to reactiveVal
        prev_row(row_selected)

        proxy <- leafletProxy('mapy')

        ## proxy %>%
        ##     addMarkers(popup=paste(prev_row()$SiteNames,
        ##                        br(),
        ##                        paste0("<a href='",
        ##                               siteCoor$usgsLink[which(siteCoor$SiteName==prev_row()$SiteNames)],
        ##                               "'>USGS</a>"),
        ##                        "(Right click to open in new window)"),##as.character(row_selected$SiteNames),
        ##                       ## layerId = as.character(row_selected$id),
        ##                       layerId = "Selected Site",
        ##                       lng=row_selected$longitude,
        ##                       lat=row_selected$latitude)

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
                    ##popup=as.character(prev_row()$SiteNames),
                    ## Not this....layerId = as.character(prev_row()$id),
                    layerId = "Selected Site",
                    lng=prev_row()$longitude,
                    lat=prev_row()$latitude)
        }
    })

    ## observeEvent(input$mapy_marker_click, {
    ##     clickId <- input$mapy_marker_click$lat
    ##     dataTableProxy("x1") %>%
    ##         selectRows(which(refrData()$latitude == clickId))
    ## })



    ## add CSS style 'cursor: pointer' to the 0-th column (i.e. row names)
    output$x1 = DT::renderDataTable({
        datatable(data=refrData(),
                  rownames=FALSE,
                  escape=FALSE,
                  selection = "single",
                  ##selection="none",
                  options = list(pageLength = 60,
                                 stateSave = TRUE,
                                 dom = 't',
                                 escape=TRUE,
                                 autoWidth = TRUE,
                                 scrollX=TRUE,
                                 columnDefs = list(
                                     list(visible=FALSE, targets = c(0,3,7:9)),
                                     list(width = '225px', targets = c(2)))
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
        print(Sys.timezone())
        QParameterCd <- "00065" ## stage, feet
        ## startTime <- Sys.Date()-input$daysBack ## yesterday
        startTime <- as.Date(format(Sys.time(), tz="America/Panama"))-input$daysBack

        tic("pull USGS data")

        ## Pull most recent data
        recentQ <- readNWISuv(siteNumbers=siteCoor$SiteCode,
                              parameterCd=QParameterCd,
                              startDate=startTime, tz="America/New_York")
        toc()

        tic("process usgs data")
        rQ <- recentQ[,c("site_no", "dateTime", "X_00065_00000")]

        ## #############################################################################
        ## ## insert code here to make graphs to hyperlink to sites
        list1 <<- split(rQ,rQ$site_no)


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


        SiteNames <- siteCoor$SiteName[order(siteCoor$SiteCode)]

        ## Add on site names
        r4 <- cbind(SiteNames, r3)

        r5 <- merge(r4, attributes(recentQ)$siteInfo[,c("site_no","dec_lat_va","dec_lon_va")],
                    by.x="SiteCode", by.y="site_no", all=TRUE)


        ## Change columns
        r5$change5 <- r5[,3]-r5[,4]
        r5$change5YN <- rep("Na",53)
        r5$change5YN[which(r5$change5<0)] <- "Down"
        r5$change5YN[which(r5$change5==0)] <- "Same"
        r5$change5YN[which(r5$change5>0)] <- "Up"

        r5$change15 <- r5[,3]-r5[,6]
        r5$change15YN <- rep("Na",53)
        r5$change15YN[which(r5$change15<0)] <- "Down"
        r5$change15YN[which(r5$change15==0)] <- "Same"
        r5$change15YN[which(r5$change15>0)] <- "Up"

        r5$change30 <- r5[,3]-r5[,9]
        r5$change30YN <- rep("Na",53)
        r5$change30YN[which(r5$change30<0)] <- "Down"
        r5$change30YN[which(r5$change30==0)] <- "Same"
        r5$change30YN[which(r5$change30>0)] <- "Up"

        r5$Site <- r5$SiteNames

        r5$Graph <- paste0("'<img src='icon", r5$SiteCode,  ".png' height='20'></img>'")

        toc()

        r5 <<- select(r5, Site, Graph, SiteNames, SiteCode,
                      change5, change15, change30,
                      change5YN, change15YN, change30YN,
                      everything(), latitude=dec_lat_va,
                      longitude=dec_lon_va)
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

        info <- input$x1_cell_clicked

        if (is.null(info$value)) {
            return()
        } else if (!is.null(info$value) & info$col==1){
            tic("click graph")

            ## ds <- list1[[siteCoor$SiteCode[which(siteCoor$SiteName==info$value)]]]
            plotSite <- substr(gsub(".*on(.*)","\\1", info$value),
                               1, nchar(gsub(".*on(.*)","\\1", info$value))-25)


            ## ds <- list1[[siteCoor$SiteCode[which(siteCoor$SiteCode==plotSite)]]]
            ds <- list1[[plotSite]]

            ## Subset historical daily flows
            ## index <- which(grepl(plotSite, names(histDaily)))
            hData <- histDaily %>%
                filter(dates %in% as.Date(ds$dateTime)) %>%
                select(dtSeq, names(histDaily)[which(grepl(plotSite, names(histDaily)))])

            output$graphs <- renderPlotly({
                mm <- plot_ly(data=ds, x=~dateTime) %>%
                    add_lines(y=~X_00065_00000,
                              name=plotSite) %>%
                    ## name=siteCoor$SiteCode[which(siteCoor$SiteName==plotSite)]) %>%
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


