
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shinydashboard)
library(dygraphs)
library(htmlwidgets)
header <- dashboardHeader(
  title = "HBase VS Hive"
)

body <- dashboardBody(
  fluidRow(
    box(dygraphOutput("hbase"),width = 800)
  )
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Speed Test", tabName = "speed_test", icon = icon("dashboard")),
    menuItem("rHBase", tabName = "rhbase", icon = icon("th"))
  ),
  selectInput("airports","Airports:", c("LAX","SFO","JFK")),
  selectizeInput("variables","Variables:",c("gear","rpm","speed"),multiple = T, select = "gear")
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

