# Packages----------------------------------------------------------------------
library(shiny)
library(shinybusy)
library(leafem)
library(dplyr)
library(magrittr)
library(rgdal)
library(leaflet)
library(plotly)
library(sf)
library(shinyWidgets)
library(DT)
library(shinydashboard)
library(leaflet.extras)
library(tidyr)
library(lubridate)
library(scales)
library(padr)
library(tableHTML)



# Global------------------------------------------------------------------------

# Raw daily data
log.data.raw <- as.data.frame(read.csv(
        "GP_P1-3_logger_daily_2022-03-22.csv",
        stringsAsFactors = FALSE)) %>%
        mutate(missing.rec = recLength - total.rec.values) 

# Raw temp hourly data
temp.raw <- as.data.frame(read.csv(
        "GP_P1-3_logger_hourly_2022-03-22.csv",
        stringsAsFactors = FALSE))

# Logger data summary
log.data.sum <- as.data.frame(read.csv(
        "GP_P1-3_summary_2022-03-22.csv",
        stringsAsFactors = FALSE)) 

# S123 field data
s123.data <- as.data.frame(read.csv(
        "S123_field_data_2022-03-22.csv",
        stringsAsFactors = FALSE)) 

# Selection Lists
log.sc <- paste0(unique(log.data.raw$Site.Code, sep = ""))
log.state <- paste0(unique(log.data.sum$state.name, sep = ""))
log.uid <- paste0(unique(log.data.raw$UID, sep = ""))



# function to remedy shiny dashboard sidebar issues with tabName
convertMenuItem <- function(mi,tabName) {
        mi$children[[1]]$attribs['data-toggle']="tab"
        mi$children[[1]]$attribs['data-value'] = tabName
        mi
}

# UI----------------------------------------------------------------------------

ui <- dashboardPage(
        title = "Stream Intermittency Visualization Dashboard",
        dashboardHeader(
                
                # Set height of dashboardHeader
                tags$li(class = "dropdown",
                        tags$style(".main-header {max-height: 50px}"),
                        tags$style(".main-header .logo {height: 50px}"),
                        tags$style(".main-header .logo {width: 275px; text-align: left;}"),
                        tags$style(".main-header .logo {background-color: #005ea2 !important}"),
                        tags$style(".main-header .navbar{margin-left: 275px;}"),
                        tags$style(".main-header .navbar{background-color: #005ea2 !important}")
                ),
                titleWidth = 300,
                title = span("",
                             style = "color: #dedbd3; font-size: 14px; font-weight: bold")
        ),
        
        ## Side Bar Menu-----
        dashboardSidebar(width = 275,
                         tags$head(
                                 tags$style(HTML("
                                        .dropdown-menu a{
                                            color: black !important;
                                        }
                                  "))),
                         
                         sidebarMenu(
                                 convertMenuItem(
                                         menuItem("Visualization Splash Board",
                                                  startExpanded = TRUE, 
                                                  tabName = "home", 
                                                  icon = icon("dashboard"),
                                                  # Filter sites by state
                                                  pickerInput("state", "Select State", width = '89%',
                                                              choices = log.state, 
                                                              options = list('actions-box' = TRUE),
                                                              multiple = TRUE,
                                                              selected = "Colorado"),
                                                  # Filter sites by total data days
                                                  knobInput(
                                                          inputId = "totaldays",
                                                          label = "Minimum Days of Data",
                                                          value = 50,
                                                          min = 0,
                                                          rotation = 'anticlockwise',
                                                          max = max(log.data.sum$total.data.days, na.rm = TRUE),
                                                          displayPrevious = TRUE,
                                                          skin = "tron",
                                                          lineCap = "round",
                                                          fgColor = "#428BCA",
                                                          inputColor = "#FFFFFF",
                                                          immediate = FALSE
                                                  ),
                                                  uiOutput("site.code")
                                         ),
                                         tabName = "home"),
                                 convertMenuItem(
                                         menuItem("Daily Logger Data",
                                                  tabName = "site_data",
                                                  icon = icon("database"),
                                                  selectInput("data.uid", 
                                                              "Select UID", 
                                                              width = '80%',
                                                              choices = log.uid,
                                                              # options = list('actions-box' = TRUE),
                                                              multiple = FALSE,
                                                              selected = "COCB1_L1"),
                                                  pickerInput("raw_columns", "Select Columns", width = '89%',
                                                              choices = names(log.data.raw), 
                                                              options = list('actions-box' = TRUE),
                                                              multiple = TRUE,
                                                              selected = names(log.data.raw))
                                 ),
                                 tabName = "site_data"),
                                 convertMenuItem(
                                         menuItem("Data Quality Rankings",
                                                  tabName = "site_data2",
                                                  icon = icon("database"),
                                                  pickerInput("sum_columns", "Select Columns", width = '89%',
                                                              choices = names(log.data.sum), 
                                                              options = list('actions-box' = TRUE),
                                                              multiple = TRUE,
                                                              selected = names(log.data.sum))
                                         ),
                                         tabName = "site_data2"),
                                 convertMenuItem(
                                         menuItem("Field Observations Data",
                                                  tabName = "field_data",
                                                  icon = icon("database"),
                                                  pickerInput("baseline_columns", "Select Columns", width = '89%',
                                                              choices = names(s123.data), 
                                                              options = list('actions-box' = TRUE),
                                                              multiple = TRUE,
                                                              selected = names(s123.data))
                                         ),
                                         tabName = "field_data")

                         )
                         
        ), # End Dashboard Sidebar
        
        ## Dashboard Body-------------------------------------------------------
        dashboardBody(
                # Code to add title to the right side of dashboard header
                tags$head(tags$style(HTML(
                        '.myClass { 
                        font-size: 20px;
                        line-height: 50px;
                        text-align: left;
                        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                        padding: 0 15px;
                        overflow: hidden;
                        color: white;
                      }
                    '))),
                                tags$script(HTML('
                      $(document).ready(function() {
                        $("header").find("nav").append(\'<span class="myClass"> Stream Intermittency Visualization Dashboard </span>\');
                      })
                     ')),
                tabItems(
                        tabItem(tabName = "home", 
                                setBackgroundColor(c("#798291", "#f5faf6"),
                                                   gradient = "linear", direction = "left",
                                                   shinydashboard = TRUE),
                                tags$style('.nav-tabs-custom .nav-tabs li.active {
                                                            border-top-color: #d73925;
                                                            background-color: #2d5696;!important
                                                        }"'),
                                
                                ### Fluid Row----------------------------------
                               
                                fluidRow(
                                        box(title = "High Frequency Data",
                                            height = "450px",
                                            solidHeader = TRUE,
                                            status = 'warning',
                                        tabBox(id = "tabs",
                                               width = 12,
                                               height = "387px",
                                               tabPanel("Site L1 Data",
                                                        tags$style(type = "text/css", "#intensity.l1 {height: 10vh; !important;}"),
                                                        conditionalPanel("output.panelStatus == 'Site L1 Data'", 
                                                                         actionButton("precip.button", "Precip Overlay"),
                                                                         actionButton("temp.button", "Temp Overlay"),
                                                                         actionButton("thourly.button", "Hourly Temp Graph"),
                                                                         actionButton("reset.button", "Reset Graph"),
                                                                         
                                                                         plotlyOutput("intensity.l1",
                                                                                      height = "300px"))
                                               ),
                                               tabPanel("Site L2 Data",
                                                        conditionalPanel("output.panelStatus == 'Site L2 Data' & output.L2 == 'L2 Present'",
                                                                         actionButton("precip2.button", "Precip Overlay"),
                                                                         actionButton("temp2.button", "Temp Overlay"),
                                                                         actionButton("thourly2.button", "Hourly Temp Graph"),
                                                                         actionButton("reset2.button", "Reset Graph"),
                                                                         plotlyOutput("intensity.l2",
                                                                                      height = "300px")),
                                                        conditionalPanel(
                                                                condition = "output.panelStatus == 'Site L2 Data' & output.L2 == 'L2 Not Present'",
                                                                tags$div(id = 'box_l2', 
                                                                         class = 'l2_class',
                                                                         
                                                                             span(HTML("<br><br><br><br>L2 Data Not Available for this Site"),
                                                                                  style = "font-size: 1.6em; font-color: Red; text-align: center;")))
                                               ),
                                               tabPanel("Site L1 & L2 Data",
                                                        conditionalPanel("output.panelStatus == 'Site L1 & L2 Data' & output.L2 == 'L2 Present'",
                                                                        
                                                                         plotlyOutput("intensity.l1l2",
                                                                                      height = "332px")),
                                                        conditionalPanel(
                                                                condition = "output.panelStatus == 'Site L1 & L2 Data' & output.L2 == 'L2 Not Present'",
                                                                tags$div(id = 'box_l2', 
                                                                         class = 'l2_class',
                                                                         
                                                                         span(HTML("<br><br><br><br>L2 Data Not Available for this Site"),
                                                                              style = "font-size: 1.6em; font-color: Red; text-align: center;")))
                                               )

                                        )),
                 
                                        column(width = 3,
                                               
                                                conditionalPanel("output.panelStatus == 'Site L1 Data'",
                                                                 box(width = NULL, 
                                                                        title = "Site Summary Metrics",
                                                                        status = "primary", 
                                                                        solidHeader = T,
                                                                        htmlOutput("html.table1"))
                                                ),
                                                conditionalPanel("output.panelStatus == 'Site L1 Data'",
                                                                box(width = NULL,
                                                                        title = "Data Quality Rankings",
                                                                        status = "primary",
                                                                        solidHeader = T,
                                                                        htmlOutput("html.table2"))
                                                ),
                                               conditionalPanel("output.panelStatus == 'Site L2 Data' & output.L2 == 'L2 Present'",
                                                                box(width = NULL,
                                                                    title = "Site Summary Metrics",
                                                                    status = "primary",
                                                                    solidHeader = T,
                                                                    htmlOutput("html.table3"))
                                               ),
                                               conditionalPanel("output.panelStatus == 'Site L2 Data' & output.L2 == 'L2 Present'",
                                                                box(width = NULL,
                                                                    title = "Data Quality Rankings",
                                                                    status = "primary",
                                                                    solidHeader = T,
                                                                    htmlOutput("html.table4"))
                                               )
                                        )
                                ), # End fluid row
                                
                                ### Fluid Row----------------------------------
                                fluidRow(
                                        tags$style(type = "text/css", "#map {height: calc(100vh - 65vh) !important;}"),
                                        tags$style(type = "text/css", "#piechart {height: calc(100vh - 65vh) !important;}"),
                                        tags$style(HTML("
                                                .box.box-solid.box-danger>.box-header {
                                                  color:#fff;
                                                  background:#666666
                                                                    }
                                                
                                                .box.box-solid.box-danger{
                                                background-color: #666666
                                                border-bottom-color:#666666;
                                                border-left-color:#666666;
                                                border-right-color:#666666;
                                                border-top-color:#666666;
                                                }

                                        ")),
                                         tags$style(HTML("div.box-header {font-weight: bold; 
                                             text-align: center !important;
                                             }")),
                                         tags$style(HTML("#box_l2 {text-align: center;
                                             color: Red;")),
                                         conditionalPanel(
                                                 condition = "output.panelStatus == 'Site L2 Data' & output.L2 == 'L2 Present'| output.panelStatus == 'Site L1 Data'",
                                                         box(width = 3,
                                                             id = "box_height",
                                                             status = "danger",
                                                             title = "Record Completeness",
                                                             solidHeader = TRUE,
                                                             plotlyOutput("piechart")
                                                             ),
                                                         box(width = 3,
                                                             id = "box_height",
                                                             status = 'danger',
                                                             title = "Site Location",
                                                             solidHeader = TRUE,
                                                             leafletOutput("map")
                                                             ),
                                                         box(width = 3,
                                                             id = "box_height",
                                                             status = 'primary',
                                                             title = "Field Observations",
                                                             solidHeader = TRUE,
                                                             tabBox(id = "tabs.field",
                                                                     width = 12,
                                                                     tabPanel("Visit 1",
                                                                                htmlOutput("html.table.field1")),
                                                                     tabPanel("Visit 2",
                                                                              htmlOutput("html.table.field2")),
                                                                     tabPanel("Visit 3",
                                                                              htmlOutput("html.table.field3")),
                                                                       
                                                                     tabPanel("Visit 4",
                                                                              htmlOutput("html.table.field4"))
                                                                        )
                                                 
                                                                )
                                         )
                                )

                        ), # End Tab Item
                        
                        ## Raw Data tab--------------------------------------------
                        tabItem(tabName = "site_data",
                                tags$head(tags$style(HTML( '.has-feedback .form-control { padding-right: 0px; width: 150% !important}' ))),
                                setBackgroundColor(c("#798291", "#f5faf6"),
                                                   gradient = "linear", direction = "left",
                                                   shinydashboard = TRUE),
                                box(width = 10,
                                    status = "danger",
                                    title = "Daily Logger Data",
                                    solidHeader = TRUE,
                                    div(style = 'overflow-x: scroll',
                                        DT::dataTableOutput("rawdata")
                                        )
                                )
                        ), # End Tab Item
                        
                        ## Sum Data tab--------------------------------------------
                        tabItem(tabName = "site_data2",
                                tags$head(tags$style(HTML( '.has-feedback .form-control { padding-right: 0px; width: 150% !important}' ))),
                                setBackgroundColor(c("#798291", "#f5faf6"),
                                                   gradient = "linear", direction = "left",
                                                   shinydashboard = TRUE),
                                box(width = 10,
                                    status = "danger",
                                    title = "Data Quality Rankings",
                                    solidHeader = TRUE,
                                    div(style = 'overflow-x: scroll',
                                        DT::dataTableOutput("sumdata")
                                    )
                                )
                        ), # End Tab Item
                        ## Field Data tab----------------------------------------
                        tabItem(tabName = "field_data",
                                tags$head(tags$style(HTML( '.has-feedback .form-control { padding-right: 0px; width: 150% !important}' ))),
                                setBackgroundColor(c("#798291", "#f5faf6"),
                                                   gradient = "linear", direction = "left",
                                                   shinydashboard = TRUE),
                                box(width = 10,
                                    status = "danger",
                                    title = "Field Observations Data",
                                    solidHeader = TRUE,
                                    div(style = 'overflow-x: scroll',
                                        DT::dataTableOutput("fielddata")
                                    )
                                )
                        )

                ) # End Tab Items
        ) # End Dashboard Body
) # End Dashboard Page


# server function---------------------------------------------------------------
server <- function(input, output, session) {

        # load html table code
        source("server_html_table_demoApp.R", local = TRUE)

        # intro modal
        showModal(modalDialog(
                title = "App Instructions",
                HTML("<strong>Welcome to the Stream Intermittency Visualization Dashboard</strong><br>
                        <br>
                A sample dataset for Stream Temperature, Intermittency and Conductivity (STIC) loggers is presented along with PRISM precipitation data for the same period of record. 
                Summary data table and graphics are available for visualization using the splash board tab.
                Raw data is available for download and viewing in the data table tabs.
")
                )
        )

        # Filter data by state and update input
        output$site.code <- renderUI({
                data.days.sort <- log.data.sum %>%
                        filter(total.data.days >= input$totaldays)
                
                state.data <- log.data.raw %>% 
                        filter(state.name == input$state  
                               & UID %in% data.days.sort$UID)
                log.sc <- paste0(unique(state.data$Site.Code, sep = ""))
                selectInput("site.code",
                            "Select Site Code",
                            width = '80%',
                            choices = log.sc,
                            # options = list('actions-box' = TRUE),
                            multiple = FALSE)
        })
        
        # Error event when input$site.code == NULL
        observeEvent(input$site.code, {
                if(input$site.code == ''){
                        show_alert(
                                title = "Error!",
                                text = "No Site Codes Meet the Criteria!",
                                type = "error"
                        )
                }
                
        })
        
        # Reactive expression for the data subsetted to what the user selected
        filteredData <- eventReactive(input$site.code, {
                
                log.data.raw %>%
                        filter(Site.Code == input$site.code) 
        })
        
        tempFiltered <- eventReactive(input$site.code, {
                
                temp.raw %>%
                        filter(Site.Code == input$site.code) %>%
                        arrange(DateFormat)
        })

        # Server inputs for UI
        var <- eventReactive(c(input$tabs, input$site.code, filteredData()), {
                input$tabs
                # print(var)
        })

        output$panelStatus <- renderText({
                var()
        })
        # check to see if site has logger 2 data
        outputOptions(output, "panelStatus", suspendWhenHidden=FALSE)
        var1 <- eventReactive(c(input$tabs, input$site.code, filteredData()), {

                if ("L2" %in% filteredData()$Site.L) {
                        var1 <- "L2 Present"
                        print(var1)
                } else {
                        var1 <- "L2 Not Present"
                        print(var1)
                }
        })

        output$L2 <- renderText({
                var1()
        })


        outputOptions(output, "L2", suspendWhenHidden=FALSE)

        ### HTML tables-----------------------------------------------------------
        
        observeEvent(c(input$site.code, input$tabs), {
                #### site summary metrics----
                if (input$tabs == "Site L1 Data") {
                        
                        html.data <- log.data.sum %>%
                                filter(UID == paste0(input$site.code, "_L1")) %>%
                                distinct(UID, .keep_all = TRUE)
                        
                        if (!is.null(html.data)){
                                output$html.table1 <- renderUI({
                                        isolate({
                                                build_table1(html.data)
                                        })
                                })
                        }
                }
                
                if (input$tabs == "Site L2 Data") {
                        html.data <- log.data.sum %>%
                                filter(UID == paste0(input$site.code, "_L2")) %>%
                                distinct(UID, .keep_all = TRUE)
                        
                        if (!is.null(html.data)){
                                output$html.table3 <- renderUI({
                                        isolate({
                                                build_table1(html.data)
                                        })
                                })
                        } 
                }
                ### site record metrics----
                if (input$tabs == "Site L1 Data") {
                        
                        html.data <- log.data.sum %>%
                                filter(UID == paste0(input$site.code, "_L1")) %>%
                                distinct(UID, .keep_all = TRUE)
                        
                        if (!is.null(html.data)){
                                output$html.table2 <- renderUI({
                                        isolate({
                                                build_table2(html.data)
                                        })
                                })
                                
                        }
                        
                }
                
                if (input$tabs == "Site L2 Data") {
                        
                        html.data <- log.data.sum %>%
                                filter(UID == paste0(input$site.code, "_L2")) %>%
                                distinct(UID, .keep_all = TRUE)
                        
                        if (!is.null(html.data)){
                                output$html.table4 <- renderUI({
                                        isolate({
                                                build_table2(html.data)
                                        })
                                })
                        } 
                }
                ### field metrics----
                if (input$tabs == "Site L1 Data" | input$tabs == "Site L2 Data") {
                        field.metrics <- eventReactive(input$site.code, {
                                
                                s123.data %>%
                                        filter(Site.Code == input$site.code & VisitNo == 1)
                        })


                        if (!is.null(field.metrics())){
                                output$html.table.field1 <- renderUI({
                                        isolate({
                                                build_table3(field.metrics())
                                        })
                                })

                        }

                }

                if (input$tabs == "Site L1 Data" | input$tabs == "Site L2 Data") {
                        field.metrics2 <- eventReactive(input$site.code, {
                                
                                s123.data %>%
                                        filter(Site.Code == input$site.code & VisitNo == 2)
                        })
                        

                        if (!is.null(field.metrics2())){
                                output$html.table.field2 <- renderUI({
                                        isolate({
                                                build_table4(field.metrics2())
                                        })
                                })
                        }
                }

                if (input$tabs == "Site L1 Data" | input$tabs == "Site L2 Data") {
                        field.metrics3 <- eventReactive(input$site.code, {
                                
                                s123.data %>%
                                        filter(Site.Code == input$site.code & VisitNo == 3)
                        })

                        if (!is.null(field.metrics3())){
                                output$html.table.field3 <- renderUI({
                                        isolate({
                                                build_table5(field.metrics3())
                                        })
                                })
                        }
                }
                
                if (input$tabs == "Site L1 Data" | input$tabs == "Site L2 Data") {
                        field.metrics4 <- eventReactive(input$site.code, {
                                
                                s123.data %>%
                                        filter(Site.Code == input$site.code & VisitNo == 4) %>%
                                        distinct(Site.Code, .keep_all = TRUE)
                        })
                        
                        if (!is.null(field.metrics4())){
                                output$html.table.field4 <- renderUI({
                                        isolate({
                                                build_table6(field.metrics4())
                                        })
                                })
                                
                        }
                        
                }
                
        })
        
        # Leaflet map-----------------------------------------------------------
        
        observe({
                map.data <- filteredData() %>%
                        slice()
                general_icon = awesomeIcons(
                        icon = 'chevron-down',
                        markerColor = 'darkblue')
                
                proxy <- leafletProxy("map")
                
                proxy %>%
                        clearGroup(group = "markers") %>%
                        setView(lng = min(map.data$Longitude), 
                                lat = min(map.data$Latitude),
                                zoom = 14) %>%
                        addAwesomeMarkers(data=map.data, 
                                          lng = ~Longitude, 
                                          lat = ~Latitude,
                                          icon = general_icon,
                                          group = "markers") %>%
                        addMouseCoordinates() 
        })

        output$map <- renderLeaflet({
                map.data.begin <- filteredData() %>%
                        slice()
                general_icon = awesomeIcons(
                        icon = 'chevron-down',
                        markerColor = 'darkblue')
                qMap <- leaflet() %>%
                        setView(lng = min(map.data.begin$Longitude), 
                                lat = min(map.data.begin$Latitude),
                                zoom = 14) %>%
                        addAwesomeMarkers(data=map.data.begin, 
                                          lng = ~Longitude, 
                                          lat = ~Latitude,
                                          icon = general_icon,
                                          group = "markers") %>%
                        addProviderTiles(providers$Esri.NatGeoWorldMap,
                                         group = 'NatGeo World (Default)') %>%
                        addProviderTiles(providers$Esri.WorldImagery,
                                         group = 'Imagery') %>%
                        addLayersControl(
                                baseGroups=c("NatGeo World (Default)", "Imagery")) %>%
                        addMouseCoordinates() %>%
                        addFullscreenControl()

                qMap

        })

        
        
        
        # Plots------------------------------------------------------------


        #### Pie Chart total----
        observeEvent(c(input$tabs, input$site.code), {
                req(input$site.code)
                if(input$tabs == "Site L1 Data") {
                        pie.data.filter <- filteredData() %>%
                                filter(Site.L == "L1") %>%
                                ungroup() %>%
                                distinct(UID, missing.rec, total.rec.values) %>%
                                rename(!!paste0("Missing") := missing.rec,
                                       !!paste0("Complete") := total.rec.values)
                        pie.data <- as.data.frame(do.call(rbind, c(pie.data.filter)))
                        names(pie.data) <- pie.data[1, ]
                        pie.data %<>%
                                mutate(category = rownames(pie.data))
                        pie.data <- pie.data[-1,]
                        uid <- names(pie.data)[1]
                        pie.data %<>%
                                mutate(uid := as.numeric(!!sym(uid)))
                        output$piechart <- renderPlotly({
                                plot_ly(pie.data,
                                        labels = ~pie.data$category,
                                        values = ~pie.data$uid,
                                        textposition = 'inside',
                                        texttemplate = "%{label} <br>(%{percent})",
                                        insidetextfont = list(color = '#FFFFFF'),
                                        hoverinfo = 'text',
                                        text = ~paste('Days',
                                                      pie.data$category, ':',
                                                      pie.data$uid,
                                                      '/', sum(pie.data$uid)),
                                        type = 'pie',
                                        showlegend = FALSE,
                                        marker = list(
                                                colors = c('rgb(155,191,133)', 'rgb(179,88,154)'))
                                ) %>%
                                        layout(
                                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

                        }) # End render plotly

                } else if ("L2" %in% filteredData()$Site.L & input$tabs == "Site L2 Data") {
                        pie.data.filter <- filteredData() %>%
                                filter(Site.L == "L2") %>%
                                ungroup() %>%
                                distinct(UID, missing.rec, total.rec.values) %>%
                                rename(!!paste0("Percent ", "Missing") := missing.rec,
                                       !!paste0("Percent ", "Complete") := total.rec.values)
                        pie.data <- as.data.frame(do.call(rbind, c(pie.data.filter)))
                        names(pie.data) <- pie.data[1, ]
                        pie.data %<>%
                                mutate(category = rownames(pie.data))
                        pie.data <- pie.data[-1,]
                        uid <- names(pie.data)[1]
                        pie.data %<>%
                                mutate(uid := as.numeric(!!sym(uid)))
                        output$piechart <- renderPlotly({
                                plot_ly(pie.data,
                                        labels = ~pie.data$category,
                                        values = ~pie.data$uid,
                                        textposition = 'inside',
                                        texttemplate = "%{label} <br>(%{percent})",
                                        insidetextfont = list(color = '#FFFFFF'),
                                        hoverinfo = 'text',
                                        text = ~paste('Days',
                                                      pie.data$category, ':',
                                                      pie.data$uid,
                                                      '/', sum(pie.data$uid)),
                                        type = 'pie',
                                        showlegend = FALSE,
                                        marker = list(
                                                colors = c('rgb(155,191,133)', 'rgb(179,88,154)'))
                                ) %>%
                                        layout(
                                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

                        }) # End render plotly pie chart
                }
        })
        
        # Bar Charts -----------------------------------------------------------
        
        #### Data sorting----
        observeEvent(c(input$tabs, input$site.code), {
                req(input$site.code)
                intensity.data.l1 <- filteredData() %>%
                        filter(Site.L == "L1") %>%
                        mutate(day = as.Date(day),
                               rec.end.date = as.Date(rec.end.date),
                               rec.end.date = case_when(
                                       is.na(rec.end.date) ~ max(day),
                                       TRUE ~ rec.end.date
                                        )
                               ) %>%
                        # fill in missing days
                        complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                              as.Date(rec.end.date[1]),
                                                by = "day")) %>%
                        # create variable to show bars for missing days
                        mutate(intensity.na = case_when(
                                is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                )
                        )
                temp.data.l1 <- tempFiltered() %>%
                        filter(Site.L == "L1") %>%
                        mutate(intensity.na = case_when(
                                is.na(Intensity) ~ max(Intensity, na.rm = TRUE) + 500
                                )
                        )

                
                if("L2" %in% filteredData()$Site.L) {
               
                intensity.data.l2 <- filteredData() %>%
                        filter(Site.L == "L2") %>%
                        mutate(day = as.Date(day),
                               rec.end.date = as.Date(rec.end.date),
                               rec.end.date = case_when(
                                       is.na(rec.end.date) ~ max(day),
                                       TRUE ~ rec.end.date
                               )
                        ) %>%
                        # fill in missing days
                        complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                as.Date(rec.end.date[1]),
                                                by = "day")) %>%
                        # create variable to show bars for missing days
                        mutate(intensity.na = case_when(
                                is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                )
                        )
                }
                
                if("L2" %in% filteredData()$Site.L) {
                temp.data.l2 <- tempFiltered() %>%
                        filter(Site.L == "L2") %>%
                        mutate(intensity.na = case_when(
                                is.na(Intensity) ~ max(Intensity, na.rm = TRUE) + 500
                                )
                        )

                }
                
                
                #### l1 intensity plot----
                output$intensity.l1 <- renderPlotly({

                        p <- plot_ly(intensity.data.l1,
                                     x = ~as.Date(intensity.data.l1$day),
                                     y = round(intensity.data.l1$mean.intensity, 1),
                                     type = "bar",
                                     opacity = 0.7,
                                     hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                     name = "Daily Mean Intensity") %>%
                                layout(
                                        xaxis = list(title = ""),
                                        yaxis = list(title = "Daily Mean Intensity",
                                                     showGrid = TRUE),
                                        legend = list(orientation = 'h'),
                                        plot_bgcolor = "#e5ecf6",
                                        bargap = 0,
                                        dragmode='pan'
                                ) %>%
                        add_bars(y = intensity.data.l1$intensity.na, 
                                 name = "Missing Data", opacity = 0.6,
                                 hovertemplate = "", 
                                 marker = list(color = 'pink')) %>%
                                add_segments(x = min(intensity.data.l1$day),
                                             xend = max(intensity.data.l1$day),
                                             y = min(intensity.data.l1$Cal.Intensity),
                                             yend = min(intensity.data.l1$Cal.Intensity),
                                             name = paste0("Calibration Intensity (", min(intensity.data.l1$Cal.Intensity),")"),
                                             line = list(color = 'orange', width = 3))
                        
                        if(!is.na(max(intensity.data.l1$per1.start, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Field Visit',
                                                     showlegend = FALSE)
                                # ggplotly(p, width = '100%', height = 100)
                                
                        } else {
                                p
                        }
                        
                        
                        if(!is.na(max(intensity.data.l1$per1.end.date, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Field Visit')
                                # ggplotly(p, width = '100%', height = 100)
                                
                        } else {
                                p
                        }
                        
                        if(!is.na(max(intensity.data.l1$per2.end.date, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Period 2 End',
                                                     showlegend = FALSE)
                                p
                                
                        } else {
                                p
                        }
                        
                        if(!is.na(max(intensity.data.l1$per3.end.date, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Period 3 End',
                                                     showlegend = FALSE)
                                p
                                
                        } else {
                                p
                        }

                }) # End render plotly
        })
                
                
                # l1 temp plot--------------
                observeEvent(c(input$tabs, input$site.code, input$temp.button),{
                        
                        req(input$site.code)
                        intensity.data.l1 <- filteredData() %>%
                                filter(Site.L == "L1") %>%
                                mutate(day = as.Date(day),
                                       rec.end.date = as.Date(rec.end.date),
                                       rec.end.date = case_when(
                                               is.na(rec.end.date) ~ max(day),
                                               TRUE ~ rec.end.date
                                       )
                                ) %>%
                                # fill in missing days
                                complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                        as.Date(rec.end.date[1]),
                                                        by = "day")) %>%
                                # create variable to show bars for missing days
                                mutate(intensity.na = case_when(
                                        is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                )
                                )
                        temp.data.l1 <- tempFiltered() %>%
                                filter(Site.L == "L1") %>%
                                mutate(intensity.na = case_when(
                                        is.na(Intensity) ~ max(Intensity, na.rm = TRUE) + 500
                                )
                                )

                output$intensity.l1 <- renderPlotly({
                
                        
                        y2 <- list(
                                tickfont = list(color = "#c20c06"),
                                titlefont = list(color = "#c20c06"),
                                overlaying = "y",
                                side = "right",
                                automargin = TRUE,
                                title = "Daily Mean Temp. (F)")
                        
                        p <- plot_ly(intensity.data.l1,
                                     x = ~as.Date(intensity.data.l1$day),
                                     y = round(intensity.data.l1$mean.intensity, 1),
                                     type = "bar",
                                     opacity = 0.7,
                                     hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                     name = "Daily Mean Intensity") %>%
                                layout(
                                        xaxis = list(title = ""),
                                        yaxis = list(title = "Daily Mean Intensity",
                                                     showGrid = TRUE),
                                        yaxis2 = y2,
                                        legend = list(orientation = 'h'),
                                        plot_bgcolor = "#e5ecf6",
                                        bargap = 0,
                                        dragmode='pan'
                                ) %>%
                                add_trace(
                                        x = ~as_datetime(intensity.data.l1$day),
                                        y = intensity.data.l1$mean.temp,
                                        name = "Daily Temp. (F)",
                                        yaxis = "y2",
                                        type = "scatter",
                                        mode = "lines",
                                        line = list(color = "#c20c06")
                                ) %>%
                                add_bars(y = intensity.data.l1$intensity.na, 
                                         name = "Missing Data", opacity = 0.6,
                                         hovertemplate = "", 
                                         marker = list(color = 'pink')) %>%
                                add_segments(x = min(intensity.data.l1$day),
                                             xend = max(intensity.data.l1$day),
                                             y = intensity.data.l1$Cal.Intensity,
                                             yend = intensity.data.l1$Cal.Intensity,
                                             name = paste0("Calibration Intensity (", intensity.data.l1$Cal.Intensity,")"),
                                             line = list(color = 'orange', width = 3))
                        
                        if(!is.na(max(intensity.data.l1$per1.start, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Field Visit',
                                                     showlegend = FALSE)
                                # ggplotly(p, width = '100%', height = 100)
                                
                        } else {
                                p
                        }
                        
                        if(!is.na(max(intensity.data.l1$per1.end.date, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Field Visit')
                                
                        } else {
                                p
                        }
                        
                        if(!is.na(max(intensity.data.l1$per2.end.date, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Period 2 End',
                                                     showlegend = FALSE)
                                p
                                
                        } else {
                                p
                        }
                        
                        if(!is.na(max(intensity.data.l1$per3.end.date, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Period 3 End',
                                                     showlegend = FALSE)
                                p
                                
                        } else {
                                p
                        }
                        
                        
                        }) # End render plotly
                
                }) # End observe event
                
                
                # l1 precip plot--------------
                observeEvent(c(input$tabs, input$site.code, input$precip.button),{
                        
                        req(input$site.code)
                        intensity.data.l1 <- filteredData() %>%
                                filter(Site.L == "L1") %>%
                                mutate(day = as.Date(day),
                                       rec.end.date = as.Date(rec.end.date),
                                       rec.end.date = case_when(
                                               is.na(rec.end.date) ~ max(day),
                                               TRUE ~ rec.end.date
                                       )
                                ) %>%
                                # fill in missing days
                                complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                        as.Date(rec.end.date[1]),
                                                        by = "day")) %>%
                                # create variable to show bars for missing days
                                mutate(intensity.na = case_when(
                                        is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                        )
                                )
                        
                        
                        output$intensity.l1 <- renderPlotly({
                                
                                y2 <- list(
                                        tickfont = list(color = "#543118"),
                                        titlefont = list(color = "#543118"),
                                        overlaying = "y",
                                        side = "right",
                                        # anchor = "free",
                                        # position = 0.9,
                                        automargin = TRUE,
                                        title = "Daily Precipitation (in)")
                                
                                p <- plot_ly(intensity.data.l1,
                                             x = ~as.Date(intensity.data.l1$day),
                                             y = round(intensity.data.l1$mean.intensity, 1),
                                             type = "bar",
                                             opacity = 0.7,
                                             hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                             name = "Daily Mean Intensity") %>%
                                        layout(
                                                xaxis = list(title = ""),
                                                yaxis = list(title = "Daily Mean Intensity",
                                                             showGrid = TRUE),
                                                yaxis2 = y2,
                                                # yaxis3 = y3,
                                                legend = list(orientation = 'h'),
                                                plot_bgcolor = "#e5ecf6",
                                                bargap = 0,
                                                dragmode='pan'
                                        ) %>%
                                        add_trace(
                                                x = ~as.Date(intensity.data.l1$day),
                                                y = intensity.data.l1$precip.inches,
                                                name = "Daily Precipitation",
                                                yaxis = "y2",
                                                type = "scatter",
                                                mode = "lines",
                                                line = list(color = "#543118")
                                        ) %>%
                                        add_bars(y = intensity.data.l1$intensity.na, 
                                                 name = "Missing Data", opacity = 0.6,
                                                 hovertemplate = "", 
                                                 marker = list(color = 'pink')) %>%
                                        add_segments(x = min(intensity.data.l1$day),
                                                     xend = max(intensity.data.l1$day),
                                                     y = intensity.data.l1$Cal.Intensity,
                                                     yend = intensity.data.l1$Cal.Intensity,
                                                     name = paste0("Calibration Intensity (", intensity.data.l1$Cal.Intensity,")"),
                                                     line = list(color = 'orange', width = 3))
                                
                                if(!is.na(max(intensity.data.l1$per1.start, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit',
                                                             showlegend = FALSE)
                                        # ggplotly(p, width = '100%', height = 100)
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l1$per1.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit')
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l1$per2.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 2 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l1$per3.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 3 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                
                        }) # End render plotly
                        
                }) # End observe event

                # l1 hourly temp----
                
                observeEvent(c(input$tabs, input$site.code, input$thourly.button),{
                        
                        req(input$site.code)
                        intensity.data.l1 <- filteredData() %>%
                                filter(Site.L == "L1") %>%
                                mutate(day = as.Date(day),
                                       rec.end.date = as.Date(rec.end.date),
                                       rec.end.date = case_when(
                                               is.na(rec.end.date) ~ max(day),
                                               TRUE ~ rec.end.date
                                       )
                                ) %>%
                                # fill in missing days
                                complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                        as.Date(rec.end.date[1]),
                                                        by = "day")) %>%
                                # create variable to show bars for missing days
                                mutate(intensity.na = case_when(
                                        is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                        )
                                )
                        temp.data.l1 <- tempFiltered() %>%
                                filter(Site.L == "L1") %>%
                                mutate(intensity.na = case_when(
                                        is.na(Intensity) ~ max(Intensity, na.rm = TRUE) + 500
                                        )
                                )
                        
                        output$intensity.l1 <- renderPlotly({
                                
                                y2 <- list(
                                        tickfont = list(color = "#c20c06"),
                                        titlefont = list(color = "#c20c06"),
                                        overlaying = "y",
                                        side = "right",
                                        automargin = TRUE,
                                        rangemode = "tozero",
                                        title = "Temp. (F)")
                                
                                p <- plot_ly(temp.data.l1,
                                             x = ~as_datetime(temp.data.l1$DateFormat),
                                             y = temp.data.l1$Intensity,
                                             type = "bar",
                                             opacity = 0.7,
                                             hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                             name = "Hourly Intensity") %>%
                                        layout(
                                                xaxis = list(title = ""),
                                                yaxis = list(title = "Intensity",
                                                             showGrid = TRUE),
                                                yaxis2 = y2,
                                                # yaxis3 = y3,
                                                legend = list(orientation = 'h'),
                                                plot_bgcolor = "#e5ecf6",
                                                bargap = 0,
                                                dragmode='pan'
                                        ) %>%
                                        add_trace(
                                                x = ~as_datetime(temp.data.l1$DateFormat),
                                                y = temp.data.l1$Temp.F,
                                                name = "Hourly Temp. (F)",
                                                yaxis = "y2",
                                                type = "scatter",
                                                mode = "lines",
                                                line = list(color = "#c20c06")
                                        ) %>%
                                        add_segments(x = min(temp.data.l1$DateFormat),
                                                     xend = max(temp.data.l1$DateFormat),
                                                     y = intensity.data.l1$Cal.Intensity,
                                                     yend = intensity.data.l1$Cal.Intensity,
                                                     name = paste0("Calibration Intensity (", intensity.data.l1$Cal.Intensity,")"),
                                                     line = list(color = 'orange', width = 3))
                                
                                if(!is.na(max(intensity.data.l1$per1.start, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit',
                                                             showlegend = FALSE)
                                        # ggplotly(p, width = '100%', height = 100)
                                        
                                } else {
                                        p
                                }

                                if(!is.na(max(intensity.data.l1$per1.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit')
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l1$per2.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 2 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l1$per3.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 3 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                
                        }) # End render plotly
                }) # End observe event
        
                # reset plot-----
                observeEvent(c(input$tabs, input$site.code, input$reset.button),{
                        
                        output$intensity.l1 <- renderPlotly({
                                
                                req(input$site.code)
                                intensity.data.l1 <- filteredData() %>%
                                        filter(Site.L == "L1") %>%
                                        mutate(day = as.Date(day),
                                               rec.end.date = as.Date(rec.end.date),
                                               rec.end.date = case_when(
                                                       is.na(rec.end.date) ~ max(day),
                                                       TRUE ~ rec.end.date
                                               )
                                        ) %>%
                                        # fill in missing days
                                        complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                                as.Date(rec.end.date[1]),
                                                                by = "day")) %>%
                                        # create variable to show bars for missing days
                                        mutate(intensity.na = case_when(
                                                is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                        )
                                        )
                                
                                p <- plot_ly(intensity.data.l1,
                                             x = ~as.Date(intensity.data.l1$day),
                                             y = round(intensity.data.l1$mean.intensity, 1),
                                             type = "bar",
                                             opacity = 0.7,
                                             hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                             name = "Daily Mean Intensity") %>%
                                        layout(
                                                xaxis = list(title = ""),
                                                yaxis = list(title = "Daily Mean Intensity",
                                                             showGrid = TRUE),
                                                legend = list(orientation = 'h'),
                                                plot_bgcolor = "#e5ecf6",
                                                bargap = 0,
                                                dragmode='pan'
                                        ) %>%
                                        add_bars(y = intensity.data.l1$intensity.na, 
                                                 name = "Missing Data", opacity = 0.6,
                                                 hovertemplate = "", 
                                                 marker = list(color = 'pink')) %>%
                                        add_segments(x = min(intensity.data.l1$day),
                                                     xend = max(intensity.data.l1$day),
                                                     y = intensity.data.l1$Cal.Intensity,
                                                     yend = intensity.data.l1$Cal.Intensity,
                                                     name = paste0("Calibration Intensity (", intensity.data.l1$Cal.Intensity,")"),
                                                     line = list(color = 'orange', width = 3))
                                
                                if(!is.na(max(intensity.data.l1$per1.start, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per1.start, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit',
                                                             showlegend = FALSE)
                                        # ggplotly(p, width = '100%', height = 100)
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l1$per1.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per1.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit')
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l1$per2.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per2.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 2 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l1$per3.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l1$per3.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l1$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 3 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                
                        }) # End render plotly
                }) # End observe event
        
                
                
                # l2 intensity plot----
                output$intensity.l2 <- renderPlotly({

                        p <- plot_ly(intensity.data.l2,
                                     x = ~as.Date(intensity.data.l2$day),
                                     y = round(intensity.data.l2$mean.intensity, 1),
                                     type = "bar",
                                     opacity = 0.7,
                                     hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                     name = "Daily Mean Intensity") %>%
                                layout(
                                        xaxis = list(title = ""),
                                        yaxis = list(title = "Daily Mean Intensity",
                                                     showGrid = TRUE),
                                        # yaxis2 = y2,
                                        # yaxis3 = y3,
                                        legend = list(orientation = 'h'),
                                        plot_bgcolor = "#e5ecf6",
                                        bargap = 0,
                                        dragmode='pan'
                                ) %>%
                        add_bars(y = intensity.data.l2$intensity.na, 
                                 name = "Missing Data", opacity = 0.6,
                                 hovertemplate = "", 
                                 marker = list(color = 'pink')) %>%
                                add_segments(x = min(intensity.data.l2$day),
                                             xend = max(intensity.data.l2$day),
                                             y = intensity.data.l2$Cal.Intensity,
                                             yend = intensity.data.l2$Cal.Intensity,
                                             name = paste0("Calibration Intensity (", intensity.data.l2$Cal.Intensity,")"),
                                             line = list(color = 'orange', width = 3))
                        
                        if(!is.na(max(intensity.data.l2$per1.start, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Field Visit',
                                                     showlegend = FALSE)
                                # ggplotly(p, width = '100%', height = 100)
                                
                        } else {
                                p
                        }
                        
                        if(!is.na(max(intensity.data.l2$per1.end.date, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Field Visit')
                        } else {
                                p
                        }
                        
                        if(!is.na(max(intensity.data.l2$per2.end.date, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Period 2 End',
                                                     showlegend = FALSE)
                                p
                                
                        } else {
                                p
                        }
                        
                        if(!is.na(max(intensity.data.l2$per3.end.date, na.rm = TRUE))){
                                p <- p %>%
                                        add_segments(x = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                     xend = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                     y = 0,
                                                     yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                     line = list(color = 'black', width = 5),
                                                     name = 'Period 3 End',
                                                     showlegend = FALSE)
                                p
                                
                        } else {
                                p
                        }
                        
                        
                }) # End render plotly
                
                
                # l2 temp plot-----
                observeEvent(c(input$tabs, input$site.code, input$temp2.button),{
                        
                        
                        
                        if("L2" %in% filteredData()$Site.L) {
                                
                                intensity.data.l2 <- filteredData() %>%
                                        filter(Site.L == "L2") %>%
                                        mutate(day = as.Date(day),
                                               rec.end.date = as.Date(rec.end.date),
                                               rec.end.date = case_when(
                                                       is.na(rec.end.date) ~ max(day),
                                                       TRUE ~ rec.end.date
                                               )
                                        ) %>%
                                        # fill in missing days
                                        complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                                as.Date(rec.end.date[1]),
                                                                by = "day")) %>%
                                        # create variable to show bars for missing days
                                        mutate(intensity.na = case_when(
                                                is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                                )
                                        )
                        }

                        output$intensity.l2 <- renderPlotly({

                                y2 <- list(
                                        tickfont = list(color = "#c20c06"),
                                        titlefont = list(color = "#c20c06"),
                                        overlaying = "y",
                                        side = "right",
                                        # anchor="free",
                                        automargin = TRUE,
                                        # position=.95,
                                        title = "Daily Mean Temp. (F)")
                                
                                p <- plot_ly(intensity.data.l2,
                                             x = ~as.Date(intensity.data.l2$day),
                                             y = round(intensity.data.l2$mean.intensity, 1),
                                             type = "bar",
                                             opacity = 0.7,
                                             hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                             name = "Daily Mean Intensity") %>%
                                        layout(
                                                xaxis = list(title = ""),
                                                yaxis = list(title = "Daily Mean Intensity",
                                                             showGrid = TRUE),
                                                yaxis2 = y2,
                                                # yaxis3 = y3,
                                                legend = list(orientation = 'h'),
                                                plot_bgcolor = "#e5ecf6",
                                                bargap = 0,
                                                dragmode='pan'
                                        ) %>%
                                        add_trace(
                                                x = ~as.Date(intensity.data.l2$day),
                                                y = intensity.data.l2$mean.temp,
                                                name = "Daily Temp. (F)",
                                                yaxis = "y2",
                                                type = "scatter",
                                                mode = "lines",
                                                line = list(color = "#c20c06")
                                        ) %>%
                                        add_bars(y = intensity.data.l2$intensity.na, 
                                                 name = "Missing Data", opacity = 0.6,
                                                 hovertemplate = "", 
                                                 marker = list(color = 'pink')) %>%
                                        add_segments(x = min(intensity.data.l2$day),
                                                     xend = max(intensity.data.l2$day),
                                                     y = intensity.data.l2$Cal.Intensity,
                                                     yend = intensity.data.l2$Cal.Intensity,
                                                     name = paste0("Calibration Intensity (", intensity.data.l2$Cal.Intensity,")"),
                                                     line = list(color = 'orange', width = 3))
                                
                                if(!is.na(max(intensity.data.l2$per1.start, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit',
                                                             showlegend = FALSE)
                                        # ggplotly(p, width = '100%', height = 100)
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per1.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit')
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per2.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 2 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per3.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 3 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                
                        }) # End render plotly
                        
                }) # End observe event
                
                
                # l2 precip plot----
                observeEvent(c(input$tabs, input$site.code, input$precip2.button),{
                        
                        if("L2" %in% filteredData()$Site.L) {
                                
                                intensity.data.l2 <- filteredData() %>%
                                        filter(Site.L == "L2") %>%
                                        mutate(day = as.Date(day),
                                               rec.end.date = as.Date(rec.end.date),
                                               rec.end.date = case_when(
                                                       is.na(rec.end.date) ~ max(day),
                                                       TRUE ~ rec.end.date
                                               )
                                        ) %>%
                                        # fill in missing days
                                        complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                                as.Date(rec.end.date[1]),
                                                                by = "day")) %>%
                                        # create variable to show bars for missing days
                                        mutate(intensity.na = case_when(
                                                is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                                )
                                        )
                        }

                        output$intensity.l2 <- renderPlotly({
                                
                                y2 <- list(
                                        tickfont = list(color = "#543118"),
                                        titlefont = list(color = "#543118"),
                                        overlaying = "y",
                                        side = "right",
                                        automargin = TRUE,
                                        title = "Daily Precipitation (in)")

                                p <- plot_ly(intensity.data.l2,
                                             x = ~as.Date(intensity.data.l2$day),
                                             y = round(intensity.data.l2$mean.intensity, 1),
                                             type = "bar",
                                             opacity = 0.7,
                                             hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                             name = "Daily Mean Intensity") %>%
                                        layout(
                                                xaxis = list(title = ""),
                                                yaxis = list(title = "Daily Mean Intensity",
                                                             showGrid = TRUE),
                                                yaxis2 = y2,
                                                # yaxis3 = y3,
                                                legend = list(orientation = 'h'),
                                                plot_bgcolor = "#e5ecf6",
                                                bargap = 0,
                                                dragmode='pan'
                                        ) %>%
                                        add_trace(
                                                x = ~as.Date(intensity.data.l2$day),
                                                y = intensity.data.l2$precip.inches,
                                                name = "Daily Precipitation",
                                                yaxis = "y2",
                                                type = "scatter",
                                                mode = "lines",
                                                line = list(color = "#543118")
                                        ) %>%
                                        add_bars(y = intensity.data.l2$intensity.na, 
                                                 name = "Missing Data", opacity = 0.6,
                                                 hovertemplate = "", 
                                                 marker = list(color = 'pink')) %>%
                                        add_segments(x = min(intensity.data.l2$day),
                                                     xend = max(intensity.data.l2$day),
                                                     y = intensity.data.l2$Cal.Intensity,
                                                     yend = intensity.data.l2$Cal.Intensity,
                                                     name = paste0("Calibration Intensity (", intensity.data.l2$Cal.Intensity,")"),
                                                     line = list(color = 'orange', width = 3))
                                
                                if(!is.na(max(intensity.data.l2$per1.start, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit',
                                                             showlegend = FALSE)
                                        # ggplotly(p, width = '100%', height = 100)
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per1.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit')
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per2.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 2 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per3.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 3 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                
                        }) # End render plotly
                        
                }) # End observe event
                

                # l2 hourly temp----
                observeEvent(c(input$tabs, input$site.code, input$thourly2.button),{
                        
                        if("L2" %in% filteredData()$Site.L) {
                                
                                intensity.data.l2 <- filteredData() %>%
                                        filter(Site.L == "L2") %>%
                                        mutate(day = as.Date(day),
                                               rec.end.date = as.Date(rec.end.date),
                                               rec.end.date = case_when(
                                                       is.na(rec.end.date) ~ max(day),
                                                       TRUE ~ rec.end.date
                                               )
                                        ) %>%
                                        # fill in missing days
                                        complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                                as.Date(rec.end.date[1]),
                                                                by = "day")) %>%
                                        # create variable to show bars for missing days
                                        mutate(intensity.na = case_when(
                                                is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                        )
                                        )
                        }
                        
                        if("L2" %in% filteredData()$Site.L) {
                                temp.data.l2 <- tempFiltered() %>%
                                        filter(Site.L == "L2") %>%
                                        mutate(intensity.na = case_when(
                                                is.na(Intensity) ~ max(Intensity, na.rm = TRUE) + 500
                                                )
                                        )
                        }
                        
                        output$intensity.l2 <- renderPlotly({
                                
                                y2 <- list(
                                        tickfont = list(color = "#c20c06"),
                                        titlefont = list(color = "#c20c06"),
                                        overlaying = "y",
                                        side = "right",
                                        automargin = TRUE,
                                        title = "Temp. (F)")
                                
                                p <- plot_ly(temp.data.l2,
                                             x = ~as_datetime(temp.data.l2$DateFormat),
                                             y = temp.data.l2$Intensity,
                                             type = "bar",
                                             opacity = 0.7,
                                             hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                             name = "Hourly Intensity") %>%
                                        layout(
                                                xaxis = list(title = ""),
                                                yaxis = list(title = "Intensity",
                                                             showGrid = TRUE),
                                                yaxis2 = y2,
                                                legend = list(orientation = 'h'),
                                                plot_bgcolor = "#e5ecf6",
                                                bargap = 0,
                                                dragmode='pan'
                                        ) %>%
                                        add_trace(
                                                x = ~as_datetime(temp.data.l2$DateFormat),
                                                y = temp.data.l2$Temp.F,
                                                name = "Hourly Temp. (F)",
                                                yaxis = "y2",
                                                type = "scatter",
                                                mode = "lines",
                                                line = list(color = "#c20c06")
                                        ) %>%

                                add_segments(x = min(temp.data.l2$DateFormat),
                                             xend = max(temp.data.l2$DateFormat),
                                             y = intensity.data.l2$Cal.Intensity,
                                             yend = intensity.data.l2$Cal.Intensity,
                                             name = paste0("Calibration Intensity (", intensity.data.l2$Cal.Intensity,")"),
                                             line = list(color = 'orange', width = 3))
                                
                                if(!is.na(max(intensity.data.l2$per1.start, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit',
                                                             showlegend = FALSE)
                                        # ggplotly(p, width = '100%', height = 100)
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per1.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit')
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per2.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 2 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per3.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 3 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                
                        }) # End render plotly
                        
                }) # End observe event
        
        
                # reset plot--------------
                observeEvent(c(input$tabs, input$site.code, input$reset2.button),{
                        
                        if("L2" %in% filteredData()$Site.L) {
                                
                                intensity.data.l2 <- filteredData() %>%
                                        filter(Site.L == "L2") %>%
                                        mutate(day = as.Date(day),
                                               rec.end.date = as.Date(rec.end.date),
                                               rec.end.date = case_when(
                                                       is.na(rec.end.date) ~ max(day),
                                                       TRUE ~ rec.end.date
                                               )
                                        ) %>%
                                        # fill in missing days
                                        complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                                as.Date(rec.end.date[1]),
                                                                by = "day")) %>%
                                        # create variable to show bars for missing days
                                        mutate(intensity.na = case_when(
                                                is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                                )
                                        )
                        }

                        output$intensity.l2 <- renderPlotly({

                                p <- plot_ly(intensity.data.l2,
                                             x = ~as.Date(intensity.data.l2$day),
                                             y = round(intensity.data.l2$mean.intensity, 1),
                                             type = "bar",
                                             opacity = 0.7,
                                             hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                             name = "Daily Mean Intensity") %>%
                                        layout(
                                                xaxis = list(title = ""),
                                                yaxis = list(title = "Daily Mean Intensity",
                                                             showGrid = TRUE),
                                                legend = list(orientation = 'h'),
                                                plot_bgcolor = "#e5ecf6",
                                                bargap = 0,
                                                dragmode='pan'
                                        ) %>%
                                add_bars(y = intensity.data.l2$intensity.na, 
                                         name = "Missing Data", opacity = 0.6,
                                         hovertemplate = "", 
                                         marker = list(color = 'pink')) %>%
                                        add_segments(x = min(intensity.data.l2$day),
                                                     xend = max(intensity.data.l2$day),
                                                     y = intensity.data.l2$Cal.Intensity,
                                                     yend = intensity.data.l2$Cal.Intensity,
                                                     name = paste0("Calibration Intensity (", intensity.data.l2$Cal.Intensity,")"),
                                                     line = list(color = 'orange', width = 3))
                                
                                if(!is.na(max(intensity.data.l2$per1.start, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per1.start, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit',
                                                             showlegend = FALSE)
                                        # ggplotly(p, width = '100%', height = 100)
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per1.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per1.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Field Visit')
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per2.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per2.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 2 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                if(!is.na(max(intensity.data.l2$per3.end.date, na.rm = TRUE))){
                                        p <- p %>%
                                                add_segments(x = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                             xend = as.Date(min(intensity.data.l2$per3.end.date, na.rm = TRUE)),
                                                             y = 0,
                                                             yend = max(intensity.data.l2$mean.intensity, na.rm = TRUE) + 2000,
                                                             line = list(color = 'black', width = 5),
                                                             name = 'Period 3 End',
                                                             showlegend = FALSE)
                                        p
                                        
                                } else {
                                        p
                                }
                                
                                
                        }) # End render plotly
                        
                }) # End observe event
        
                
                # l1l2 intensity plot----
                output$intensity.l1l2 <- renderPlotly({
                        
                        intensity.data.l1 <- filteredData() %>%
                                filter(Site.L == "L1") %>%
                                mutate(day = as.Date(day),
                                       rec.end.date = as.Date(rec.end.date),
                                       rec.end.date = case_when(
                                               is.na(rec.end.date) ~ max(day),
                                               TRUE ~ rec.end.date
                                       )
                                ) %>%
                                # fill in missing days
                                complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                        as.Date(rec.end.date[1]),
                                                        by = "day")) %>%
                                # create variable to show bars for missing days
                                mutate(intensity.na = case_when(
                                        is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                        )
                                )

                        if("L2" %in% filteredData()$Site.L) {
                                
                                intensity.data.l2 <- filteredData() %>%
                                        filter(Site.L == "L2") %>%
                                        mutate(day = as.Date(day),
                                               rec.end.date = as.Date(rec.end.date),
                                               rec.end.date = case_when(
                                                       is.na(rec.end.date) ~ max(day),
                                                       TRUE ~ rec.end.date
                                               )
                                        ) %>%
                                        # fill in missing days
                                        complete(day = seq.Date(as.Date(rec.start.date[1]), 
                                                                as.Date(rec.end.date[1]),
                                                                by = "day")) %>%
                                        # create variable to show bars for missing days
                                        mutate(intensity.na = case_when(
                                                is.na(mean.intensity) ~ max(mean.intensity, na.rm = TRUE) + 500
                                                )
                                        )
                        }
                        
                        plot_ly(intensity.data.l2,
                                type = "bar",
                                hovertemplate = paste('(%{x}, %{y})', '<extra></extra>'),
                                name = "Daily Mean Intensity") %>%
                                layout(xaxis = list(title = ""),
                                       yaxis = list(title = "Daily Mean Intensity"),
                                       plot_bgcolor = "#e5ecf6",
                                       bargap = 0,
                                       dragmode='pan') %>%
                                add_segments(x = min(intensity.data.l2$day),
                                             xend = max(intensity.data.l2$day),
                                             y = intensity.data.l2$Cal.Intensity,
                                             yend = intensity.data.l2$Cal.Intensity,
                                             name = paste0("L2 Calibration Intensity (", intensity.data.l2$Cal.Intensity,")")) %>%
                                add_bars(x = ~as.Date(intensity.data.l1$day),
                                         y = round(intensity.data.l1$mean.intensity, 1), 
                                         name = "Site L1 Data", opacity = 1,
                                         color = I("light blue")) %>%
                                add_bars(x = ~as.Date(intensity.data.l2$day),
                                         y = round(intensity.data.l2$mean.intensity, 1), 
                                         name = "Site L2 Data", opacity = 0.4,
                                         marker = list(color = 'green'))
        
                }) # End render plotly


        
        # Data Tables------
        
        filteredRaw <- eventReactive(c(input$data.uid, input$raw_columns), {
                
                if(!is.null(input$raw_columns)){
                        log.data.raw %>%
                                filter(UID == input$data.uid) %>%
                                select(input$raw_columns)
                } else if(is.null(input$raw_columns)){
                        log.data.raw %>%
                                filter(UID == input$data.uid)
                }
        })

        filteredSumColumns <- eventReactive(input$sum_columns, {
                
                if(!is.null(input$sum_columns)){
                        log.data.sum %>%
                        select(input$sum_columns)
                } else if(is.null(input$sum_columns)){
                log.data.sum
                }
        })
        
        filteredBaseline <- eventReactive(input$baseline_columns, {
                
                if(!is.null(input$baseline_columns)){
                        s123.data %>%
                                select(input$baseline_columns)
                } else if(is.null(input$baseline_columns)){
                        s123.data
                }
        })

        
        observeEvent(c(input$data.uid, input$raw_columns), {
                output$rawdata <- renderDT(server = FALSE, {
                        DT::datatable(filteredRaw(),
                                      extensions = c('Buttons'),
                                      options = list(
                                              autoWidth = FALSE,
                                              scrollx = TRUE,
                                              lengthMenu = c(5, 25, 50),
                                              pageLength = 8,
                                              dom = 'Bfrtip', 
                                              buttons = list(
                                                      list(extend = "csv", text = "Download Current Page", filename = "page",
                                                           exportOptions = list(
                                                                   modifier = list(page = "current")
                                                           )
                                                      ),
                                                      list(extend = "csv", text = "Download Full Results", filename = "GP_site_data",
                                                           exportOptions = list(
                                                                   modifier = list(page = "all")
                                                           ))),
                                              columnDefs = list(list(className = 'dt-center',
                                                                     targets = "_all"))
                                      )
                        )
                })
        })
                
        observeEvent(c(input$sum_columns), {
                output$sumdata <- renderDT(server = FALSE, {
             
                        DT::datatable(filteredSumColumns(),
                                  extensions = c('Buttons'),
                                  # escape = FALSE,
                                  filter = "top",
                                  options = list(
                                          autoWidth = TRUE,
                                          fixedColumns = TRUE,
                                          scrollx = TRUE,
                                          lengthMenu = c(5, 25, 50),
                                          pageLength = 8,
                                          dom = 'Bfrtip', 
                                          buttons =
                                                  list('copy', 'print', list(
                                                          extend = 'collection',
                                                          buttons = list(
                                                                  list(extend = 'csv', filename = "GP_P1-3_sum_data"),
                                                                  list(extend = 'excel', filename = "GP_P1-3_sum_data"),
                                                                  list(extend = 'pdf', filename = "GP_P1-3_sum_data")),
                                                          text = 'Download')),
                                          columnDefs = list(list(className = 'dt-center',
                                                                 targets = "_all"),
                                                            list(width = '100px', 
                                                                 targets = "_all"))
                                  )
                        )
                })
        })
                
        observeEvent(input$baseline_columns, {
                output$fielddata <- renderDT(server = FALSE, {
                        DT::datatable(filteredBaseline(),
                                      extensions = c('Buttons'),
                                      # escape = FALSE,
                                      filter = "top",
                                      options = list(
                                              autoWidth = TRUE,
                                              fixedColumns = TRUE,
                                              scrollx = TRUE,
                                              lengthMenu = c(5, 25, 50),
                                              pageLength = 4,
                                              dom = 'Bfrtip', 
                                              buttons =
                                                      list('copy', 'print', list(
                                                              extend = 'collection',
                                                              buttons = list(
                                                                      list(extend = 'csv', filename = "GP_P1-3_s123_data"),
                                                                      list(extend = 'excel', filename = "GP_P1-3_s123_data"),
                                                                      list(extend = 'pdf', filename = "GP_P1-3_s123_data")),
                                                              text = 'Download')),
                                              columnDefs = list(list(className = 'dt-center',
                                                                     targets = "_all"),
                                                                list(width = '100px', 
                                                                     targets = "_all"))
                                      )
                        )
                })
        })

        
        
}

# shinyApp call----
shinyApp(ui = ui, server = server)




