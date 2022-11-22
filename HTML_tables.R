library(htmltools)
library(stringr)
library(lubridate)

build_table1 <- function(data){
        # Site summary
        html.table <- if (!is.null(data)){
                tags$table(style = "border: 4px solid black; padding: 0px; width: 100%; height 100%;",
                           tags$tr(
                                   tags$th("Site Code", style = 'padding-left: 3px'),
                                   tags$td(data$UID)
                           ),          
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Record Start Date", style = 'padding-left: 3px'),
                                   tags$td(as.Date(data$rec.start.date))
                           ),
                           tags$tr(
                                   tags$th("Record End Date", style = 'padding-left: 3px'),
                                   tags$td(as.Date(data$rec.end.date))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Expected Period of Record Days", style = 'padding-left: 3px'),
                                   tags$td(data$recLength)
                           ),
                           tags$tr(
                                   tags$th("Total Data Days", style = 'padding-left: 3px'),
                                   tags$td(data$total.data.days)
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Consecutive Data Days", style = 'padding-left: 3px'),
                                   tags$td(data$total.consect.days)
                           ),
                           tags$tr(
                                   tags$th("Max Consecutive Flow Days", style = 'padding-left: 3px'),
                                   tags$td(data$myear)
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Max Consecutive Dry Days", style = 'padding-left: 3px'),
                                   tags$td(data$dyear)
                           ),
                           tags$tr(
                                   tags$th("Total Flow Days (% Flow Days)", style = 'padding-left: 3px'),
                                   tags$td(paste0(data$total.flow.days, " (", 
                                                  round(((data$total.flow.days/data$total.data.days) * 100), 1),
                                                  ")"))
                           ),
                           
                )
        }
        
        return(html.table)
}

build_table2 <- function(data){
        # Data rankings
        html.table <- if (!is.null(data)){
                tags$table(style = "border: 4px solid black; padding: 0px; width: 100%; height 100%",
                           tags$tr(
                                   tags$th("Completeness Ranking", style = 'padding-left: 3px'),
                                   tags$td(str_to_title(data$completeness.of.record.confidence))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Length Ranking", style = 'padding-left: 3px'),
                                   tags$td(str_to_title(data$completeness.of.record.confidence))
                           ),
                           tags$tr(
                                   tags$th("Recency Ranking", style = 'padding-left: 3px'),
                                   tags$td(str_to_title(data$recency.confidence))
                           ),
                           
                )
        }
        
        return(html.table)
}

# Field observation Period 1---------
build_table3 <- function(data){
        
        html.table <- if (!is.null(data)){
                tags$table(style = "border: 4px solid black; padding: 0px; width: 100%; height 100%",
                           tags$tr(
                                   tags$th("Period Start Date", style = 'padding-left: 3px'),
                                   tags$td(as.Date(data$per1.start))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Water Depth (cm)", style = 'padding-left: 3px'),
                                   tags$td(data$water_depth_cm)
                           ),
                           tags$tr(
                                   tags$th("Hydrologic Condition", style = 'padding-left: 3px'),
                                   tags$td(str_to_title(data$hydro_conditions))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Reach Surface Flow %", style = 'padding-left: 3px'),
                                   tags$td(data$hi_reachlengthsurface)
                           ),
                           tags$tr(
                                   tags$th("Reach Subsurface Flow %", style = 'padding-left: 3px'),
                                   tags$td(data$hi_reachlengthsub)
                           ),
                           
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Channel Score (0-6)", style = 'padding-left: 3px'),
                                   tags$td(data$hi_channelscore)
                           ),
                           
                )
        }
        
        return(html.table)
}

# Field observation Period 2---------

build_table4 <- function(data){
        
        html.table <- if (!is.null(data)){
                tags$table(style = "border: 4px solid black; padding: 0px; width: 100%; height 100%",
                           tags$tr(
                                   tags$th("Period Start Date", style = 'padding-left: 3px'),
                                   tags$td(as.Date(data$per1.end))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Water Depth (cm)", style = 'padding-left: 3px'),
                                   tags$td(data$water_depth_cm)
                           ),
                           tags$tr(
                                   tags$th("Hydrologic Condition", style = 'padding-left: 3px'),
                                   tags$td(str_to_title(data$hydro_conditions))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Reach Surface Flow %", style = 'padding-left: 3px'),
                                   tags$td(data$hi_reachlengthsurface)
                           ),
                           tags$tr(
                                   tags$th("Reach Subsurface Flow %", style = 'padding-left: 3px'),
                                   tags$td(data$hi_reachlengthsub)
                           ),
                           
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Channel Score (0-6)", style = 'padding-left: 3px'),
                                   tags$td(data$hi_channelscore)
                           ),
                           
                )
        }
        
        return(html.table)
}

# Field observation Period 3---------

build_table5 <- function(data){
        
        html.table <- if (!is.null(data)){
                tags$table(style = "border: 4px solid black; padding: 0px; width: 100%; height 100%",
                           tags$tr(
                                   tags$th("Period Start Date", style = 'padding-left: 3px'),
                                   tags$td(as.Date(data$per2.end))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Water Depth (cm)", style = 'padding-left: 3px'),
                                   tags$td(data$water_depth_cm)
                           ),
                           tags$tr(
                                   tags$th("Hydrologic Condition", style = 'padding-left: 3px'),
                                   tags$td(str_to_title(data$hydro_conditions))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Reach Surface Flow %", style = 'padding-left: 3px'),
                                   tags$td(data$hi_reachlengthsurface)
                           ),
                           tags$tr(
                                   tags$th("Reach Subsurface Flow %", style = 'padding-left: 3px'),
                                   tags$td(data$hi_reachlengthsub)
                           ),
                           
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Channel Score (0-6)", style = 'padding-left: 3px'),
                                   tags$td(data$hi_channelscore)
                           ),
                           
                )
        }
        
        return(html.table)
}

# Field observation Period 4---------

build_table6 <- function(data){
        
        html.table <- if (!is.null(data)){
                tags$table(style = "border: 4px solid black; padding: 0px; width: 100%; height 100%",
                           tags$tr(
                                   tags$th("Period Start Date", style = 'padding-left: 3px'),
                                   tags$td(as.Date(data$per3.end))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Water Depth (cm)", style = 'padding-left: 3px'),
                                   tags$td(data$water_depth_cm)
                           ),
                           tags$tr(
                                   tags$th("Hydrologic Condition", style = 'padding-left: 3px'),
                                   tags$td(str_to_title(data$hydro_conditions))
                           ),
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Reach Surface Flow %", style = 'padding-left: 3px'),
                                   tags$td(data$hi_reachlengthsurface)
                           ),
                           tags$tr(
                                   tags$th("Reach Subsurface Flow %", style = 'padding-left: 3px'),
                                   tags$td(data$hi_reachlengthsub)
                           ),
                           
                           tags$tr(style = "background: #d2d4d2",
                                   tags$th("Channel Score (0-6)", style = 'padding-left: 3px'),
                                   tags$td(data$hi_channelscore)
                           )
                           
                )
        }
        
        return(html.table)
}













