
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
library(ggvis)
library(ggplot2)
Sys.setenv("HADOOP_CMD"="/home/aaron/myProgs/hadoop-2.6.0/bin/hadoop")
library(rhdfs)
load("hbaseVhdfs.Rda")

server <- function(input, output) { 
  hb.init()
  hdfs.init()
  #values <- reactiveValues(hbase_data)
  output$hbase_timer  <- renderText({NULL})
  hbase_data <- reactive({
    airport <- input$airports
    if(is.null(input$airports) | is.null(input$variables)) return(NULL)
    data <- hb.pull("Test","test",start = airport,end = paste0(airport,"z"),columns = input$variables) %>%
       tidyr::separate(col = "rowkey", into = c("airport","day","vin"))
    form_data(data)
  })
  
  hdfs_data <- reactive({
    data <- read_hdfs(id_keys = input$airports, variables = input$variables)
    form_data(data)
  })
  
  output$hbase <- renderDygraph({
    start <- Sys.time()
    if(is.null(hbase_data())) return(NULL)
      dygraph(hbase_data(), main = paste("HBase Total Time:",round((Sys.time() - start)*100,2),"(ms)")) %>% 
        dySeries(name = input$variables[1],axis = 'y') %>%
        dySeries(name = input$variables[length(input$variables)],axis = 'y2') %>%
        dyRangeSelector() %>%
        dyOptions(stepPlot = TRUE)
  })
  output$hdfs <- renderDygraph({
    start <- Sys.time()
    if(is.null(hdfs_data())) return(NULL)
    dygraph(hdfs_data(), main = paste("HDFS Total Time:",round((Sys.time() - start)*100,2),"(ms)")) %>% 
      dySeries(name = input$variables[1],axis = 'y') %>%
      dySeries(name = input$variables[length(input$variables)],axis = 'y2') %>%
      dyRangeSelector() %>%
      dyOptions(stepPlot = TRUE)
  })
  read_hdfs <- function(id_keys,variables, parent_dir = "/user/aaron/trucks"){
    df_list <- list()
    files_all <- hdfs.ls(paste0("/user/aaron/trucks/",input$airports), recurse = TRUE)$file
    files <- sapply(variables, grep, x = files_all) %>%
      as.vector %>%
      files_all[.]
    
    for(f in files){
      hdfs_file <- hdfs.file(f, "r")
      x <- hdfs_file %>% hdfs.read(start = 0) %>% unserialize
      hdfs.close(hdfs_file)
      rk <- f %>% 
        gsub(paste0(parent_dir,"/"),"",.) %>% 
        gsub("/data","",.) %>%
        stringr::str_split(pattern ="/") %>%
        unlist
      df_list[[f]] <- data.table(airport = rk[1], day = rk[2], vin = rk[3], variable = rk[4],values = list(x))
    }
    return(data.table::rbindlist(df_list))
  }
  
  form_data  <- function(data){
      merge_em <- function(values){
        if(length(values)<=1) return(values[[1]])
        out <- values[[1]]
        for(i in 2:length(values)){
          out <- merge(out,values[[i]],all=T,by = "date_time")
        }
        out
      }
      data <- data[,list("rbinded" = list(merge_em(values))),by=c("vin","day","airport")] #data.table functionality
      #     data$rbinded <- data$rbinded %>% lapply(dtreplace)  %>% lapply(setDT)#fills in missing NAs
      if(length(input$variables)==1) data$rbinded[[1]] <-  data$rbinded[[1]]
      data  <- (data$rbinded[[1]])
      #     data <- data[!duplicated(data$date_time)]
      data$date_time <- as.POSIXct.numeric(data$date_time,origin = "1970-01-01")
      #     data$timeDate  <- as.POSIXlt(data$timeDate, origin = "1970-01-01")
      setnames(data,"date_time","timeDate")
      xts(data[,2:length(data),with=F],data[[1]])
  }
  
  #-------------------------------------------------------------------------
  colspecs <- reactive(paste("test", input$variables, sep = "::"))
  hbase_scan_output <- reactive(hb.scan(tablename = "Test", startrow = input$airports,end = paste0(input$airport,"z"),colspec =colspecs())$get())
  
  output$hbase_original <- renderPrint({
    x <- hbase_scan_output()
    lapply(x, function(i){
      i[[3]] <- lapply(i[[3]],function(x){
        lapply(x, function(x) data.table(list(setDT(x))))})
      i
    })[1:2]
  })
  
  new_scan_output <- reactive(hb.pull(tablename = "Test",column_family = "test", start = input$airports, end = paste0(input$airport,"z"),columns = input$variables))
  output$hbase_tidyr <- renderPrint({
    a <- new_scan_output()
    setnames(a, "column_family", "c.family")
    a
  })
  
  #----------------------------------------------------
  bind_shiny(plot_id = "compare_HBase_HDFS",
    hbaseVhdfs %>% ggvis(~expr, ~time/1e6) %>% 
      layer_boxplots() %>%
      add_axis("y",title="time (ms)") %>%
      add_axis("x",title = ""))
  
#-----------------------------------------------------------------
#timeseries r reduce
  dt <- reactive({
    dt <- data.table("index" = 1:input$size, "sin" = sin(seq(0,20,10/input$size*2))[-1])
    dt[vreduce(dt$sin,input$tol)]
  })
  output$reduced_points <- renderText(paste(nrow(dt())))
  output$ggplot_reduced <- renderPlot({
    g <- ggplot(dt(), aes(index, sin)) + geom_step()+
      labs(title = paste("Number of Points: ", input$size, "\n Tolerance %: ", input$tol))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    print(g)
  })

}
