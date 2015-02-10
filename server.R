
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shinydashboard)
library(shiny)
library(rhbase)
library(data.table)
library(xts)
library(timeseriesr)
server <- function(input, output) { 
  hb.init()
  print("test1")
  #values <- reactiveValues(hbase_data)
  hbase_data <- reactive({
    airport <- input$airports
    if(is.null(input$airports) | is.null(input$variables)) return(NULL)
    data <- hb.pull("Test","test",start = airport,end = paste0(airport,"z"),columns = input$variables) %>%
       tidyr::separate(col = "rowkey", into = c("airport","day","vin"))
    merge_em <- function(values){
      if(length(values)<=1) return(values)
      out <- values[[1]]
      for(i in 2:length(values)){
        out <- merge(out,values[[i]],all=T,by = "date_time")
      }
      out
    }
    data <- data[,list("rbinded" = list(merge_em(values))),by=c("vin","day","airport")] #data.table functionality
print("test2")
    #     data$rbinded <- data$rbinded %>% lapply(dtreplace)  %>% lapply(setDT)#fills in missing NAs
    if(length(input$variables)==1) data$rbinded[[1]] <-  data$rbinded[[1]]
  data  <- (data$rbinded[[1]])
    #     data <- data[!duplicated(data$date_time)]
print("test3")
print(data)
    data$date_time <- as.POSIXct.numeric(data$date_time,origin = "1970-01-01")
#     data$timeDate  <- as.POSIXlt(data$timeDate, origin = "1970-01-01")
    setnames(data,"date_time","timeDate")
    xts(data[,2:length(data),with=F],data[[1]])
  })
  output$hbase <- renderDygraph({
    if(is.null(hbase_data)) return(NULL)
      print(str(hbase_data()))
      dygraph(hbase_data(), main = "HBase") %>% 
        dySeries(name = input$variables[1],axis = 'y') %>%
        dySeries(name = input$variables[length(input$variables)],axis = 'y2') %>%
        dyRangeSelector()
  })
  
}
