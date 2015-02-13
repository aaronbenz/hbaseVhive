
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
    tabItem(tabName = "HBase vs HDFS",
      fluidRow(
        box(dygraphOutput("hbase"),width = 600),
        box(dygraphOutput("hdfs"), width = 600))
    ),
    tabItem(tabName = "rHBase Formatting",
      fluidRow(
        box(verbatimTextOutput("hbase_original"),title = "rhbase original"),
        box(verbatimTextOutput("hbase_tidyr"), title = "rhbase tidyr version"))
    )
  )
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Speed Test", tabName = "speed_test", icon = icon("dashboard")),
    menuItem("rHBase", tabName = "rhbase", icon = icon("th"))
  ),
  selectInput("airports","Airports:", c("LAX","JFK")),
  selectizeInput("variables","Variables:",c("gear","rpm","speed"),multiple = T, select = c("rpm","speed"))
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

