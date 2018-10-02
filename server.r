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
        startTime <- as.Date(format(Sys.time(), tz="America/Panama"))-input$dNum

        ## Pull most recent data
        recentQ <- readNWISuv(siteNumbers=siteCoor$site_no,
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
            QParameterCd <- "00065" ## stage, feet
            startTime <- as.Date(format(Sys.time(), tz="America/Panama"))

            ## Pull most recent data
            recentQ <- readNWISuv(siteNumbers=siteCoor$site_no,
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

    ## Take the data set and calculate metrics of interest
    fData <- reactive({

        ## Pull dfs out of list
        rQ <- as.data.frame(useData()[["rQ"]])
        latLon <- as.data.frame(useData()[["latLon"]])

        ## Keep last 2 hours
        rQ <- filter(rQ,
                     dateTime>=Sys.time()-(60*60*2))

        ## Pull out newest data dateTime for each site
        newestDt <- rQ %>%
            group_by(site_no) %>%
            filter(dateTime == max(dateTime)) %>%
            dplyr::select(site_no, dateTime) %>%
            mutate(def=difftime(Sys.time(), dateTime, units="min")) %>%
            as.data.frame()

        ## Reshape
        r2 <- reshape(rQ, idvar="site_no", v.names="X_00065_00000",
                      timevar="dateTime", direction="wide")

        ## Rename
        names(r2) <- substring(names(r2), 20, 30)
        names(r2)[1] <- "site_no"
        r3 <- r2[sort(names(r2), decreasing=TRUE)]
        r3 <- r3[order(r3$site_no),]

        ## SiteNames <- siteCoor$SiteName[order(siteCoor$site_no)]
        SiteNamesAndFlooding <- siteCoor[order(siteCoor$site_no),
                                         c("SiteName", "floodHeight")]

        ## Add on site names
        r4 <- cbind(SiteNamesAndFlooding, r3)

        ## Merge lat long
        r5 <- merge(r4, latLon,
                    by.x="site_no", by.y="site_no", all=TRUE)

        ## Also merge the "how old is newest data" column
        r5 <- merge(r5, newestDt, by.x="site_no", by.y="site_no", all=TRUE)

        ## This produces an array with column indexes correpsondig to first numeric
        ## data per site
        firstI <- rep(NA,53)
        for (i in 1:53) {
            firstI[i] <- which(names(r5)==format(r5$dateTime[i], format="%m-%d %H:%M"))
        }

        ## Add on current days histortical flow depth, calc frac
        hToday <- dplyr::select(histFlowTab, site_no, todaysLow=names(histFlowTab)[
                                                   which(grepl(as.Date(format(Sys.time(),
                                                                              tz="America/Panama")),
                                                               names(histFlowTab))
                                                         )
                                               ]
                         )

        r5 <- merge(r5, hToday, by="site_no", all=TRUE)

        ## Initialize the current reading dependent columns
        r5$change5YN <- rep("Na",53)
        r5$change15YN <- rep("Na",53)
        r5$change30YN <- rep("Na",53)
        r5$floodDefecit <- rep(NA,53)

        ## Calculate the dependent columns
        for (i in 1:53) {
            r5$change5[i] <- r5[i,firstI[i]]-r5[i,firstI[i]+1]
            r5$change15[i] <- r5[i,firstI[i]]-r5[i,firstI[i]+3]
            r5$change30[i] <- r5[i,firstI[i]]-r5[i,firstI[i]+6]
            r5$floodDefecit[i] <- r5$floodHeight[i]-r5[i,firstI[i]]
            r5$frac[i] <- (r5[i,firstI[i]]-r5$todaysLow[i])/(r5$floodHeight[i]-r5$todaysLow[i])
        }

        ## Classify the current dependent columns
        r5$change5YN[which(r5$change5<0)] <- "Down"
        r5$change5YN[which(r5$change5==0)] <- "Same"
        r5$change5YN[which(r5$change5>0)] <- "Up"
        r5$change15YN[which(r5$change15<0)] <- "Down"
        r5$change15YN[which(r5$change15==0)] <- "Same"
        r5$change15YN[which(r5$change15>0)] <- "Up"
        r5$change30YN[which(r5$change30<0)] <- "Down"
        r5$change30YN[which(r5$change30==0)] <- "Same"
        r5$change30YN[which(r5$change30>0)] <- "Up"

        r5$Site <- r5$SiteName

        ## Try adding on the first data column indexes
        r5$currentI <- firstI

        ## Site specific graph icon b/c can't find better way....
        r5$Graph <- paste0("'<img src='icon", r5$site_no,
                           ".png' height='20'></img>'")

        ## Reconfig output
        r5 <- dplyr::select(r5, Site, Graph, SiteName, SiteCode=site_no,
                     "Flood Fraction"=frac, "5-Min Change"=change5, "15-Min Change"=change15,
                     "30-Min Change"=change30, "Minutes Since"=def, "Below Flood Stage"=floodDefecit,
                     everything(), floodHeight, latitude=dec_lat_va,
                     longitude=dec_lon_va, currentI,change5YN, change15YN, change30YN)

        return(list(r5=r5))
    })

    ######################################################################
    ######################################################################
    ## Maps
    output$mapy <- renderLeaflet({

        ## Color scheme, r/b diverging from color brewer
        pal <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7',
                 '#d1e5f0', '#92c5de','#4393c3','#2166ac','#053061')

        ## Pull results df out of fData list
        ds <- fData()[["r5"]]

        if (input$mapDef=="Stage") {

            ## Extract vector of most recent stage at each site
            stg <- rep(NA, 53)
            for (i in 1:53) {
                stg[i] <- ds[i,ds$currentI[i]+11]
            }

            ## This loopy.  To get high values red, and red at top of legend..
            ## make a palette, then make a reverse palette, then use as in below.
            myPal <- colorNumeric(
                palette = "RdYlBu",
                domain = stg, reverse=FALSE)

            myPalR <- colorNumeric(
                palette = "RdYlBu",
                domain = stg, reverse=TRUE) ## reversed


            leaflet() %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas, group="Streets") %>%
                addCircleMarkers(data=ds, radius=3, color=~myPalR(stg), ## use reverse palette
                                 fillOpacity=0.75, stroke=FALSE,
                                 popup=paste(ds$SiteName,
                                             br()
                                             )
                                 ) %>%
                addLegend(position = c("topright"), opacity = 0.7, myPal, ## use regular palette
                          values=stg, na.label = "NA", bins = 10,
                          labels = NULL, className = "info legend",
                          layerId = NULL, group = NULL,
                          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                          title="Gage Height (ft)"
                          ) %>%
                fitBounds(lng1=min(ds$longitude),
                          lng2=max(ds$longitude),
                          lat1=min(ds$latitude),
                          lat2=max(ds$latitude)
                          )

        } else {
            ## Extract vector of most recent stage at each site

            frac <- rep(NA, 53)
            for (i in 1:53) {
                frac[i] <- ds[i, "Flood Fraction"]
            }

            ## This loopy.  To get high values red, and red at top of legend..
            ## make a palette, then make a reverse palette, then use as in below.
            myPal <- colorNumeric(
                palette = "RdYlBu",
                domain = c(0,1), reverse=FALSE)

            myPalR <- colorNumeric(
                palette = "RdYlBu",
                domain = c(0,1), reverse=TRUE) ## reversed


            makeMap <- leaflet() %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas, group="Streets") %>%
                addCircleMarkers(data=ds, radius=3, color=~myPalR(frac), ## use reverse palette
                                 fillOpacity=0.75, stroke=FALSE,
                                 popup=paste(ds$SiteName,
                                             br()
                                             )
                                 ) %>%
                addLegend(position = c("topright"), opacity = 0.7, myPal, ## use regular palette
                          values=seq(0,1,.10), na.label = "NA", bins = 10,
                          labels = NULL, className = "info legend",
                          layerId = NULL, group = NULL,
                          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                          title="Fraction"
                          ) %>%
                fitBounds(lng1=min(ds$longitude),
                          lng2=max(ds$longitude),
                          lat1=min(ds$latitude),
                          lat2=max(ds$latitude)
                          )
            ## browser()

            if (max(ds[,"Flood Fraction"], na.rm=TRUE)==1) {
                dsExtra <- filter(ds, ds[,"Flood Fraction"]>=1)

                makeMap <- leaflet() %>%
                    addCircleMarkers(map=makeMap,
                                     lat=dsExtra$latitude,
                                     lng=dsExtra$longitude,
                                     radius=6, color="red")
            }

            makeMap
        }
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
                               br(), fillOpacity=0,
                               paste0("Current Reading: ", prev_row()[prev_row()$currentI+11],
                                      br(),
                                      "From: ", prev_row()$dateTime, br(),
                                      "<a href='",
                                      siteCoor$usgsLink[which(siteCoor$SiteName==prev_row()$SiteName)],
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
        datatable(data=as.data.frame(fData()[["r5"]]),
                  extensions = 'FixedHeader',
                  rownames=FALSE,
                  escape=FALSE,
                  selection = "single",
                  options = list(pageLength = 60,
                                 stateSave = TRUE,
                                 dom = 't',
                                 fixedHeader='TRUE',
                                 escape=TRUE,
                                 autoWidth = TRUE,
                                 scrollX=TRUE,
                                 columnDefs = list(
                                     list(visible=FALSE, targets = c(0,3,35:37)),
                                     list(width = '225px', targets = c(2)),
                                     list(className='dt-center', targets=c(1,4:9))
                                 )
                                 )
                  ) %>%
            formatStyle(
                'Flood Fraction',
                backgroundColor = styleInterval(c(0.2, 0.4, 0.6, 0.8, 1),
                                                c('#fef0d9','#fdd49e','#fdbb84','#fc8d59',
                                                  '#e34a33','#b30000'))
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
            formatRound('Minutes Since',1) %>%
            formatRound('floodHeight',1) %>%
            formatRound('Below Flood Stage', 1) %>%
            formatRound('Flood Fraction', 2)

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
                dplyr::select(dtSeq, names(histDaily)[which(grepl(plotSite, names(histDaily)))])

            output$graphs <- renderPlotly({
                mm <- plot_ly(data=ds, x=~dateTime) %>%
                    add_lines(y=~X_00065_00000,
                              name=plotSite) %>%
                    add_trace(x=c(min(ds$dateTime), max(ds$dateTime)),
                              y=rep(siteCoor$floodHeight[which(siteCoor$site_no==plotSite)],2),
                              name="NWS Floodstage", type="scatter", mode="lines",
                              line=list(color="red")) %>%
                    add_trace(x=hData$dtSeq, y=hData[,2], name="LT Daily Avg.",
                              type="scatter", mode="markers",
                              marker=list(symbol="triangle-up-open", color="green", size=7)) %>%
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
