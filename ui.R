
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shinydashboard)
library(dygraphs)
library(htmlwidgets)
library(ggvis)
header <- dashboardHeader(
  title = "HBase VS HDFS"
)

body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "speed_test",
      fluidRow(
        box(dygraphOutput("hbase"),width = 12),
        box(dygraphOutput("hdfs"), width = 12)),
        box(width = 12,includeCSS("simple_dygraphs.html"))
    ),
    tabItem(tabName = "rhbase",
      fluidRow(
        box(verbatimTextOutput("hbase_original"),title = "rhbase original"),
        box(verbatimTextOutput("hbase_tidyr"), title = "rhbase tidyr version"))
    ),
    tabItem(tabName = "compare",
            fluidRow(box(width = 12,
              ggvisOutput("compare_HBase_HDFS"
                             )))),
    tabItem(tabName = "point_reduce",
            fluidRow(
              box(width = 12,
              sliderInput(inputId = "tol",
                          "Tolerance:",
                          min = 0.001,
                          max = .5,
                          value = .01,
                          step = .001),
              sliderInput(inputId = "size","Size",
                          value=100000,
                          min = 100,
                          max = 10000000,
                          step = 100000),
              textOutput(outputId = "reduced_points"),
              # Show a plot of the generated distribution
              
              plotOutput("ggplot_reduced")
#               ,
#              
              ,
#             includeHTML("vreduce.html")
              includeCSS("vreduce.html")
            ))
    )
  )
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Speed Test", tabName = "speed_test", icon = icon("dashboard")),
    menuItem("rHBase Formatting", tabName = "rhbase", icon = icon("th")),
    menuItem("Compare Results", tabName = "compare", icon = icon("newspaper-o")),
    menuItem("Reduce Points", tabName = "point_reduce", icon = icon("line-chart "))
  ),
  selectInput("airports","Airports:", c("LAX","JFK")),
  selectizeInput("variables","Variables:",c("gear","rpm","speed"),multiple = T, select = c("rpm","speed"))
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

