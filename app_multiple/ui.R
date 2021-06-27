# AA,2019.09.19.
# Warehouse Stock Monitoring


library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinycssloaders)
library(shinybusy)
library(RMariaDB)
library(DT)
library(openxlsx)
library(rpivotTable)
library(dplyr)
library(ggplot2)
library(forcats)
library(shinyjs)
library(lubridate)


pw <- {
    "password"
}
u <- {
    "user"
}
db <- {
    "db_name"
}
db2 <- {
    "db2_name"
}
h <- {
    "host_ip"
}
p <- {
    0000
}
tz = "Europe/Prague"



shinyUI(dashboardPage(
    #Header
    dashboardHeader(
        title = textOutput('title'),
        
        tags$li(actionLink(
            "openModal", label = "", icon = icon("info")
        ),
            class = "dropdown"),
        titleWidth = "0%"
    ),
    
    #Sidebar
    dashboardSidebar(
        width = 300,
        collapsed = TRUE,
        sidebarMenu(
            menuItem(
                "Movement of devices",
                tabName = "pivot",
                icon = icon("th"),
                menuSubItem("Type of device", tabName = "pivot_ty"),
                menuSubItem("Devices", tabName = "pivot_all")
            ),
            menuItem(
                "Current stock monitoring",
                tabName = "monitoring",
                icon = icon("th")
            )
        ),
        
        div(class = "cimke", "Server info:"),
        # Server info
        div(class = "szerver_info",
            tags$ul(htmlOutput("text")))
    ),
    
    
    #Body
    dashboardBody(
        useShinyjs(),
        useShinyalert(),
        
        includeCSS("styles.css"),
        
        actionButton("friss", title = "Update", icon("redo"),
            style = "color: #eee; background-color: #224f77; border-color: transparent; position:absolute; left: 50px; top: 9px; z-index: 9999;"),
        
        
        tabItems(
            tabItem(tabName = "pivot_ty",
                div(
                    class = "col-sm-12",
                    div(
                        class = "cim_tipus",
                        p(
                            class = "cim_tipus_text",
                            "How many devices were moved between warhouse stock and costumers? (Group by type)",
                            downloadButton(
                                "dl2",
                                "",
                                class = "btn-xs",
                                title = "Letöltés",
                                style = "color: #eee; border-color: transparent; background-color: #224f77;"
                            )
                        )
                    ),
                    
                    tabsetPanel(
                        type = "tabs",
                        tabPanel(
                            "Plots",
                            icon = icon("bar-chart-o"),
                            
                            fluidRow(
                                box(
                                    width = 6,
                                    height = 820,
                                    withSpinner(
                                        plotOutput("plot_type1", height = 800),
                                        type = 7,
                                        color = getOption("spinner.color", default = "#224f77")
                                    )
                                ),
                                box(
                                    width = 6,
                                    height = 820,
                                    withSpinner(
                                        plotOutput("plot_type2", height = 800),
                                        type = 7,
                                        color = getOption("spinner.color", default = "#224f77")
                                    )
                                )
                            ),
                            fluidRow(
                                box(
                                    width = 6,
                                    height = 820,
                                    withSpinner(
                                        plotOutput("plot_type3", height = 800),
                                        type = 7,
                                        color = getOption("spinner.color", default = "#224f77")
                                    )
                                ),
                                box(
                                    width = 6,
                                    height = 820,
                                    withSpinner(
                                        plotOutput("plot_type4", height = 800),
                                        type = 7,
                                        color = getOption("spinner.color", default = "#224f77")
                                    )
                                )
                            ),
                            fluidRow(
                                box(
                                    width = 6,
                                    height = 820,
                                    withSpinner(
                                        plotOutput("plot_type5", height = 800),
                                        type = 7,
                                        color = getOption("spinner.color", default = "#224f77")
                                    )
                                ),
                                box(
                                    width = 6,
                                    height = 820,
                                    withSpinner(
                                        plotOutput("plot_type6", height = 800),
                                        type = 7,
                                        color = getOption("spinner.color", default = "#224f77")
                                    )
                                )
                            ),
                            fluidRow(
                                box(
                                    width = 6,
                                    height = 820,
                                    withSpinner(
                                        plotOutput("plot_type7", height = 800),
                                        type = 7,
                                        color = getOption("spinner.color", default = "#224f77")
                                    )
                                ),
                                box(
                                    width = 6,
                                    height = 820,
                                    withSpinner(
                                        plotOutput("plot_type8", height = 800),
                                        type = 7,
                                        color = getOption("spinner.color", default = "#224f77")
                                    )
                                )
                            )
                        ),
                        
                        tabPanel(
                            "Pivot",
                            icon = icon("th"),
                            height = "5600px",
                            tags$head(
                                tags$style(
                                    type = 'text/css',
                                    '#pivot_device_type{height: 900px !important; overflow-x: scroll; overflow-y: scroll; background-color: #c4c4c4;}'
                                )
                            ),
                            rpivotTableOutput("pivot_device_type")
                        )
                    )
                )),
            
            
            # Under development...
            tabItem(
                tabName = "pivot_all",
                
                box(
                    title = p(
                        "How many devices were moved between warhouse stock and costumers? (Group by device number)",
                        downloadButton(
                            "dl1",
                            "",
                            class = "btn-xs",
                            title = "Letöltés",
                            style = "color: #eee; border-color: transparent; background-color: #224f77;"
                        )
                    ),
                    status = "primary",
                    width = 12,
                    height = "5600px",
                    tags$head(
                        tags$style(
                            type = 'text/css',
                            '#cikk_pivot{overflow-x: scroll; overflow-y: scroll; background-color: #c4c4c4;}'
                        )
                    ),
                    rpivotTableOutput("cikk_pivot", height = "5500px")
                )
            )
            
        ),
        
        
        
        tabItem(tabName = "monitoring",
            # Under development...
            
            fluidRow())
        
        
        
    )
))