
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shinydashboard)
library(dygraphs)
library(htmlwidgets)
header <- dashboardHeader(
  title = "HBase VS HDFS"
)

body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "speed_test",
      fluidRow(
        box(dygraphOutput("hbase"),width = 600),
        box(dygraphOutput("hdfs"), width = 600))
    ),
    tabItem(tabName = "rhbase",
      fluidRow(
        box(verbatimTextOutput("hbase_original"),title = "rhbase original"),
        box(verbatimTextOutput("hbase_tidyr"), title = "rhbase tidyr version"))
    ),
    tabItem(tabName = "compare",
            fluidRow(
              box(ggvisOutput("compare_HBase_HDFS"
                             ))))
  )
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Speed Test", tabName = "speed_test", icon = icon("dashboard")),
    menuItem("rHBase Formatting", tabName = "rhbase", icon = icon("th")),
    menuItem("Compare Results", tabName = "compare", icon = icon("newspaper-o"))
  ),
  selectInput("airports","Airports:", c("LAX","JFK")),
  selectizeInput("variables","Variables:",c("gear","rpm","speed"),multiple = T, select = c("rpm","speed"))
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

