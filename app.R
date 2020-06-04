library(shiny)
library(shinydashboard)
library(quantmod)
library(dygraphs)
library(ggplot2)
library(RPostgreSQL)
library(DBI)
library(xts)
library(plotly)
library(shinythemes)
library(dplyr)
library(DT)
library(kableExtra)

#connection whith DB
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "dbhitsa2019",
                 host = "dev.vk.edu.ee", port = 5432,
                 user = "ruuvi_sel", password = "ruuvisel") 



# User interface 
ui <- dashboardPage(skin = "purple",
    
header <- dashboardHeader(title = "dbHITSA2019",titleWidth = 300),
                
sidebar <- dashboardSidebar(width = 300,
                sidebarMenu( id = "tabs",
                            #first menu item
                            menuItem('All Sensors and Current Data',
                                     menuSubItem('Grove VOC and eCO2 Gas Sensor',tabName = 'groove'),
                                     menuSubItem('High Accuracy Temperature', tabName = "high"),
                                     menuSubItem('Light Sensor', tabName = "light"),
                                     menuSubItem('Loudness Sensor', tabName = "loudness"),
                                     menuSubItem("PIR Motion Sensor", tabName = "pir"),
                                     menuSubItem("Sunlight Sensor", tabName = "sunlight"),
                                     menuSubItem('Temperature and Humidity Sensor Pro', tabName = "pro"),
                                     menuSubItem('Current Data',tabName = 'currentdata'),
                                     dateRangeInput("dates",
                                                    "Choose date range",
                                                    start = Sys.Date()-16,
                                                    end = Sys.Date(),
                                                    min = Sys.Date()-16,
                                                    max = Sys.Date(),
                                                    #language = 'et',
                                                    weekstart = 1,
                                                    separator = " - "),br()),
                          
                            #second menu item
                            menuItem('Sensor Data Analysis', tabName = "select", icon = icon("chart-line")),
                            
                            #third menu item
                            menuItem('Two Sensors Behavior', tabName = 'analysis',icon = icon("random")),
                            
                            #fourth menu item
                            menuItem("Data Table", tabName = "data", icon = icon('database'),badgeLabel = "All", badgeColor = "green")                    )
),
                
body <- dashboardBody(
                        tabItems(
                                  # First tab content
                                  tabItem("groove", h2("Grove VOC and eCO2 Gas Sensor"), br(),
                                          fluidRow( box(width = 4, plotlyOutput("plotly2")),box(width = 8, plotlyOutput("plotly22a"))),
                                          br(),
                                          fluidRow(box(width = 4, plotlyOutput("plotly5")),box(width = 8, plotlyOutput("plotly22b"))),
                                          ),
                                  tabItem("pir", h2("PIR Motion Sensor"), br(),
                                          fluidRow(box(width = 4,plotlyOutput("plotly3")),box(width = 8, plotlyOutput("plotly23")))),
                                  tabItem("sunlight", h2("Sunlight Sensor"), br(),
                                          fluidRow( box(width = 4,plotlyOutput("plotly4")), box(width = 8,  plotlyOutput("plotly27a"))),
                                          fluidRow( box(width = 4,plotlyOutput("plotly8")), box(width = 8,  plotlyOutput("plotly27b"))),
                                          fluidRow( box(width = 4,plotlyOutput("plotly9")), box(width = 8, plotlyOutput("plotly27c")))),
                                  tabItem("pro", h2("Temperature and Humidity Sensor Pro"), br(),
                                          fluidRow( box(width = 4,plotlyOutput("plotly6")), box(width = 8,plotlyOutput("plotly26a"))),
                                          fluidRow( box(width = 4,plotlyOutput("plotly7")), box(width = 8,plotlyOutput("plotly26b")))),
                                  tabItem("high", h2("High Accuracy Temperature"), br(),
                                          fluidRow( box(width = 4,plotlyOutput("plotly1")), box(width = 8,plotlyOutput("plotly21")))),
                                  tabItem("light", h2("Light Sensor"), br(),
                                          fluidRow( box(width = 4,plotlyOutput("plotly10")), box(width = 8,plotlyOutput("plotly24")))),
                                  tabItem("loudness", h2("Loudness Sensor"), br(),
                                          fluidRow( box(width = 4, plotlyOutput("plotly11")), box(width = 8,plotlyOutput("plotly25")))),
                                  tabItem("currentdata", h2("Current data from DB"), br(),
                                            downloadButton("downloadData11","Download"),br(),br(),
                                            box(width = 12, DTOutput("tabelCurrent"))),
                                  
                                  #Second tab content
                                  tabItem(tabName = "select",
                                          selectInput("var1",
                                                      label = "Choose a sensor",
                                                      choices = list("Carbon dioxide equivalent CO2eq, ppm",
                                                                     "Humidity, %",
                                                                     "Light Sensor, lux",
                                                                     "Loudness Sensor, db",
                                                                     "PIR Motion Sensor",
                                                                     "Sunlight Sensor, lm",
                                                                     "Temperature, C",
                                                                     "Total Volatile Organic Compounds, ppb",
                                                                     "Ultraviolet index, UV")),
                                          dateRangeInput("dates1",
                                                         "Choose date range",
                                                         start = Sys.Date()-16,
                                                         end = Sys.Date(),
                                                         min = Sys.Date()-16,
                                                         max = Sys.Date(),
                                                         #language = 'et',
                                                         weekstart = 1,
                                                         separator = " - "),
                                          fluidRow( box(width = 12, plotlyOutput("plotly31"))),
                                          fluidRow( box(width = 12, br(), downloadButton("downloadDataSelect","Download"), br(), DTOutput("tabelK"))),
                                          fluidRow( box(htmlOutput("summary")))),
                                 
                                   #Third tab content
                                  tabItem(tabName = "analysis",
                                          fluidRow( box(width = 6,
                                                selectInput("var2",
                                                            label = "Choose a sensor",
                                                            choices = list("Carbon dioxide equivalent CO2eq, ppm",
                                                                           "Humidity, %",
                                                                           "Light Sensor, lux",
                                                                           "Loudness Sensor, db",
                                                                           "PIR Motion Sensor",
                                                                           "Sunlight Sensor, lm",
                                                                           "Temperature, C",
                                                                           "Total Volatile Organic Compounds, ppb",
                                                                           "Ultraviolet index, UV"))),
                                            box(width = 6,
                                                selectInput("var3",
                                                            label = "Choose a sensor",
                                                            choices = list("Carbon dioxide equivalent CO2eq, ppm",
                                                                           "Humidity, %",
                                                                           "Light Sensor, lux",
                                                                           "Loudness Sensor, db",
                                                                           "PIR Motion Sensor",
                                                                           "Sunlight Sensor, lm",
                                                                           "Temperature, C",
                                                                           "Total Volatile Organic Compounds, ppb",
                                                                           "Ultraviolet index, UV")))),
                                          fluidRow( box(width = 6, DTOutput("tabelK1"),),
                                                    box(width = 6, DTOutput("tabelK2")),
                                                    box(title = "Plot", plotlyOutput("plotly41")))),
                                 
                                   #Fourth tab content
                                  tabItem(tabName = "data",
                                          h2("All data from DB"), br(),
                                                      downloadButton("downloadDataAll","Download"), br(),br(),
                                                      box(width = 12, DTOutput("tabelAll")))
                                  
)))

# Server logic
server <- function(input, output,session) {
    
    NewTableAll <- reactive({invalidateLater(300*1000)
      df <-  dbGetQuery(con, "SELECT sensor, room,date_time, valuetype,data,dimension
                        FROM vw_sensorsdata WHERE room LIKE '208'
                        ORDER BY date_time DESC")
      df$date_time <- format(df$date_time,usetz=T)
      df
        })
    
    NewTableCurrent <- reactive({invalidateLater(300*1000)
      df <- dbGetQuery(con, "SELECT sensor, room,date_time, valuetype,data,dimension 
                       FROM vw_sensorsdata WHERE  room like '208' 
                       ORDER BY date_time desc limit 22")
      df$date_time <- format(df$date_time,usetz=T)
      df
    })
    
    NewTableCurrentDate <- reactive({invalidateLater(300*1000)
        
        df <-  dbGetQuery(con, "SELECT * from vw_sensorsdata 
                          WHERE room='208' ORDER BY date_time DESC")
        #df$date_time <- format(df$date_time,usetz=T) 
        df$date <- as.Date(df$date_time)
        df <- df[df$date >=input$dates[1]&df$date <=input$dates[2],]
        df
      })
    
    NewList <- reactive({
      invalidateLater(300*1000)
      dbGetQuery(con, "SELECT sensor FROM vw_sensorsdata where  room like '208'")
    })
    
    NewList1 <- reactive({
      invalidateLater(300*1000)
      dbGetQuery(con,"SELECT valuetype from vw_sensorsdata where room like '208'")
    })
    
    output$vx <- renderUI({
      df <- NewList()
      selectInput("variablex", "Select",choices = unique(df))
    })
    
    output$vy <-renderUI({
      df <- NewList1()
      selectInput("variabley","Select", choices = unique(df))
    })
    
    output$tabelAll <- renderDT(NewTableAll(),
                                options = list(searching = FALSE,ordering=F, pageLength = 15,scrollX = TRUE),
                                class = " display cell-border",
                                colnames= c("ID" =1,'Sensor Name'= 2,'Class'=3,"Date/Time"=4,"Type"=5,"Data"=6, "Dimension"=7)) 
    
    output$tabelCurrent <- renderDT(NewTableCurrent(),
                                options = list(searching = FALSE,ordering=F, pageLength = 11,scrollX = TRUE),
                                class = "cell-border display ",
                                colnames= c("ID" =1,'Sensor Name'= 2,'Class'=3,"Date/Time"=4,"Type"=5, "Data"=6, "Dimension"=7))
    
    output$tabel2 <- renderDT({
      df <- NewTableCurrent()
      df$date <- as.Date(df$date_time)
      df <- df[df$date >=input$dates[1]&df$date <=input$dates[2],]
      #df <- df[,-ncol(df)]
      datatable(df, options = list(searching = FALSE,
                                   ordering=F, pageLength = 10,scrollX = TRUE))})
    
    output$downloadDataAll <- downloadHandler(
      filename = function(){
        paste("dbHitsa2019-",Sys.Date(),".csv", sep = "")
      },
      content = function(file){
        write.csv(NewTableAll(),file)
      }    )
    
    output$downloadData11 <- downloadHandler(
      filename = function(){
        paste("dbHitsa2019-Current-",Sys.Date(),".csv", sep = "")
      },
      content = function(file){
        write.csv(NewTableCurrent(),file)
      }    )
    
    output$downloadDataSelect <- downloadHandler(
      filename = function(){
        paste("dbHitsa2019-", input$var1, Sys.Date(),".csv", sep = "")
      },
      content = function(file){
        write.csv(data_tableK(),file)
      }    )
    
    inputPlot1 <- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor=="High Accuracy Temperature" & df_postgres1$valuetype=="T","data"]
      
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = andmed[1],
        title = list(text = "Temperature, C", font = list(size = 24)),
        delta = list(reference = andmed[2], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 36), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,10),color = "royalblue"),
            list(range = c(10, 18), color = "cyan"),
            list(range = c(18, 27), color = "green"),
            list(range = c(27,36), color = "red"))
        ))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly1 <- renderPlotly({
      print(inputPlot1())
    })
    
    inputPlot2 <- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor=="Grove VOC and eCO2 Gas Sensor" & df_postgres1$valuetype=="Total Volatile Organic Compounds","data"]
      
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = andmed[1],
        title = list(text = "Total Volatile Organic Compounds, ppb", font = list(size = 24)),
        delta = list(reference = andmed[2], increasing = list(color = "RebeccaPurple")), 
        gauge = list(
          axis = list(range = list(NULL, 1), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,0.5),color = "red"),
            list(range = c(0.5,1), color = "green"))))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly2 <- renderPlotly({
      print(inputPlot2())
    })
    
    inputPlot3 <- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor=="PIR Motion Sensor" & df_postgres1$valuetype=="PIR on","data"]
      
      g <- plot_ly( 
        type = "indicator",
        mode = "gauge+number+delta",
        value = andmed[1],
        #title = list(text = "ON/OFF", font = list(size = 24)),
        delta = list(reference = andmed[2], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 1), tickwidth = 1, tickcolor = "darkblue", tickmode = "linear",tick0 = "0", dtick = "1"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,0.5),color = "red"),
            list(range = c(0.5,1), color = "green"))))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly3 <- renderPlotly({
      print(inputPlot3())
    })
    
    inputPlot4 <- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor=="Sunlight Sensor" & df_postgres1$valuetype=="Illuminance (IR)" ,"data"]
      
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        title = list(text = "Lumen, lm", font = list(size = 24)),
        value = andmed[1],
        delta = list(reference = andmed[3], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 400), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,100),color = "royalblue"),
            list(range = c(100, 200), color = "cyan"),
            list(range = c(200, 300), color = "yellow"),
            list(range = c(300,400), color = "white"))
        ))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly4 <- renderPlotly({
      print(inputPlot4())
    })
    
    inputPlot5 <- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor=="Grove VOC and eCO2 Gas Sensor" & df_postgres1$valuetype=="Carbon dioxide equivalent CO2eq","data"]
      
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = andmed[1][1],
        title = list(text = "Carbon dioxide equivalent CO2eq, ppm", font = list(size = 24)),
        delta = list(reference = andmed[1][2], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 1), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,0.5),color = "red"),
            list(range = c(0.5,1), color = "green"))))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly5 <- renderPlotly({
      print(inputPlot5())
    })
    
    inputPlot6 <- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor=="Temperature and Humidity Sensor Pro" & df_postgres1$valuetype=="Humidity","data"]
      
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        title = list(text = "Humidity,% "),
        value = andmed[1],
        delta = list(reference = andmed[2], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 100), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,30),color = "cyan"),
            list(range = c(30, 45), color = "yellow"),
            list(range = c(45, 80), color = "green"),
            list(range = c(80,100), color = "red"))
        ))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly6 <- renderPlotly({
      print(inputPlot6())
    })
    
    inputPlot7 <- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor=="Temperature and Humidity Sensor Pro" & df_postgres1$valuetype=="T","data"]
      
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        title = list(text = "Temperature,C"),
        value = andmed[1],
        delta = list(reference = andmed[2], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 36), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,10),color = "royalblue"),
            list(range = c(10, 18), color = "cyan"),
            list(range = c(18, 27), color = "green"),
            list(range = c(27,36), color = "red"))
        ))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly7 <- renderPlotly({
      print(inputPlot7())
    })
    
    inputPlot8 <- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor=="Sunlight Sensor" & df_postgres1$valuetype=="Lumen","data"]
      
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        title = list(text = "Lumen, lm"),
        value = andmed[2],
        delta = list(reference = andmed[4], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 400), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,100),color = "royalblue"),
            list(range = c(100, 200), color = "cyan"),
            list(range = c(200, 300), color = "yellow"),
            list(range = c(300,400), color = "white"))
        ))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly8 <- renderPlotly({
      print(inputPlot8())
    })
    
    inputPlot9<- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor=="Sunlight Sensor" & df_postgres1$valuetype=="Ultraviolet index","data"]
      
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        title = list(text = "Ultraviolet index, UV"),
        value = andmed,
        delta = list(reference = andmed[2], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 1), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,0.5),color = "red"),
            list(range = c(0.5,1), color = "green"))))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly9 <- renderPlotly({
      print(inputPlot9())
    })
    
    inputPlot10<- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$sensor =="Light Sensor"&
                               df_postgres1$valuetype=="Illuminance (Visible)","data"]
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        title = list(text = "Illuminance, lux"),
        value = andmed[1],
        delta = list(reference = andmed[2], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 3000), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,750),color = "cyan"),
            list(range = c(750,1500), color = "white"),
            list(range = c(1500,2250), color = "lightyellow"),
            list(range = c(2250,3000), color = "yellow"))
        ))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      g
    })
    
    output$plotly10 <- renderPlotly({
      print(inputPlot10())
    })
    
    inputPlot11<- reactive({
      df_postgres1 <- NewTableCurrent()
      andmed <- df_postgres1[df_postgres1$valuetype=="Detsibell","data"]
      
      g <- plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        title = list(text = "Detsibell"),
        value = andmed[1],
        delta = list(reference = andmed[2], increasing = list(color = "RebeccaPurple")),
        gauge = list(
          axis = list(range = list(NULL, 100), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0,30),color = "cyan"),
            list(range = c(30, 45), color = "yellow"),
            list(range = c(45, 80), color = "green"),
            list(range = c(80,100), color = "red"))
        ))
      g <- g %>%
        layout(
          margin = list(l=20,r=30),
          paper_bgcolor = "lavender",
          font = list(color = "darkblue", family = "Arial"))
      
      g
    })
    
    output$plotly11 <- renderPlotly({
      print(inputPlot11())
    })
    
    inputPlot21 <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      
      andmed <- df_postgres1[df_postgres1$sensor=="High Accuracy Temperature"&df_postgres1$valuetype=="T",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data))+geom_line(color="deepskyblue3", size=1)+labs(title=paste("High Accuracy Temperature ")) + xlab("Dates")+ ylab("C")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly21 <- renderPlotly({
      print(inputPlot21())
      
    }) 
    
    inputPlot22a <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="Grove VOC and eCO2 Gas Sensor" & df_postgres1$valuetype=="Total Volatile Organic Compounds",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("Total Volatile Organic Compounds")) +xlab("Dates")+ylab("ppb")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly22a <- renderPlotly({
      print(inputPlot22a())
      
    }) 
    
    inputPlot22b <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="Grove VOC and eCO2 Gas Sensor" & df_postgres1$valuetype=="Carbon dioxide equivalent CO2eq",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("Carbon dioxide equivalent CO2eq")) +xlab("Dates")+ylab("ppm")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly22b <- renderPlotly({
      print(inputPlot22b())
      
    }) 
    
    inputPlot23 <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="PIR Motion Sensor" & df_postgres1$valuetype=="PIR on",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("PIR Motion Sensor")) +xlab("Dates")+ylab("on/off")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly23 <- renderPlotly({
      print(inputPlot23())
      
    }) 
    
    inputPlot24 <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="Light Sensor" &
                               df_postgres1$valuetype=="Illuminance (Visible)",c("date_time","data")]
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("Light Sensor")) +xlab("Dates")+ylab("lm")+
                            theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly24 <- renderPlotly({
      print(inputPlot24())
      
    }) 
    
    inputPlot25 <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="Loudness Sensor" & df_postgres1$valuetype=="Detsibell",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("Loudness Sensor")) +xlab("Dates")+ylab("db")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly25 <- renderPlotly({
      print(inputPlot25())
      
    }) 
    
    inputPlot26a <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="Temperature and Humidity Sensor Pro" & df_postgres1$valuetype=="Humidity",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("Humidity")) +xlab("Dates")+ylab("%")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly26a <- renderPlotly({
      print(inputPlot26a())
      
    }) 
    
    inputPlot26b <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="Temperature and Humidity Sensor Pro" & df_postgres1$valuetype=="T",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("Temperature")) +xlab("Dates")+ylab("C")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly26b <- renderPlotly({
      print(inputPlot26b())
      
    }) 
    
    inputPlot27a <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="Sunlight Sensor" & df_postgres1$valuetype=="Illuminance (IR)",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("Sunlight Sensor 1")) +xlab("Dates")+ylab("Lm")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly27a <- renderPlotly({
      print(inputPlot27a())
      
    }) 
    
    inputPlot27b <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="Sunlight Sensor" & df_postgres1$valuetype=="Lumen",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("Sunlight Sensor 2")) +xlab("Dates")+ylab("Lm")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly27b <- renderPlotly({
      print(inputPlot27b())
      
    }) 
    
    inputPlot27c <- reactive({
      
      df_postgres1 <- NewTableCurrentDate()
      andmed <- df_postgres1[df_postgres1$sensor=="Sunlight Sensor" & df_postgres1$valuetype=="Ultraviolet index",c("date_time","data")]
      
      p <- ggplot(andmed,aes(x=date_time, y=data)) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("Ultraviolet index")) +xlab("Dates")+ylab("LUX")+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly27c <- renderPlotly({
      print(inputPlot27c())
      
    }) 
    
    inputdata <- reactive({
      df <- NewTableAll()
      df$date <- as.Date(df$date_time)
      df <- df[df$date >=input$dates1[1]&df$date <=input$dates1[2],]
      andmed <- switch(input$var1,
                       #get(input$variabley),
                       "Total Volatile Organic Compounds, ppb" = df[df$sensor=="Grove VOC and eCO2 Gas Sensor" & df$valuetype=="Total Volatile Organic Compounds",c("date","data")],
                       "Temperature, C" = df[df$sensor=="Temperature and Humidity Sensor Pro" & df$valuetype=="T",c("date","data")],
                       "Humidity, %" = df[df$sensor=="Temperature and Humidity Sensor Pro" & df$valuetype=="Humidity",c("date","data")],
                       "Loudness Sensor, db"= df[df$sensor=="Loudness Sensor" & df$valuetype=="Detsibell", c("date","data")],
                       "Carbon dioxide equivalent CO2eq, ppm"= df[df$sensor=="Grove VOC and eCO2 Gas Sensor" & df$valuetype=="Carbon dioxide equivalent CO2eq",c("date","data")],
                       "Light Sensor, lux" = df[df$sensor=="Light Sensor" & df$valuetype=="Illuminance (Visible)",c("date","data")],
                       "PIR Motion Sensor"= df[df$sensor=="PIR Motion Sensor" & df$valuetype=="PIR on",c("date","data")],
                       "Sunlight Sensor, lm"= df[df$sensor=="Sunlight Sensor" & df$valuetype=="Lumen",c("date","data")],
                       "Ultraviolet index, UV"= df[df$sensor=="Sunlight Sensor" & df$valuetype=="Ultraviolet index",c("date","data")])
    })
    
    output$summary <- renderText(kable_styling(kable(summary(inputdata()))))
    
    data_tableK <- reactive({
      df <- NewTableAll()
      df$date <- as.Date(df$date_time)
      #df$time <- format(df$date_time, "%X")
      df <- df[df$date >=input$dates1[1]&df$date <=input$dates1[2],]
    
      #df <- df[,-ncol(df)]
      andmed <- switch(input$var1,
        #get(input$variabley),
                       "Total Volatile Organic Compounds, ppb" = df[df$sensor=="Grove VOC and eCO2 Gas Sensor" & df$valuetype=="Total Volatile Organic Compounds",c("date","data")],
                       "Temperature, C" = df[df$sensor=="Temperature and Humidity Sensor Pro" & df$valuetype=="T",c("date","data")],
                       "Humidity, %" = df[df$sensor=="Temperature and Humidity Sensor Pro" & df$valuetype=="Humidity",c("date","data")],
                       "Loudness Sensor, db"= df[df$sensor=="Loudness Sensor" & df$valuetype=="Detsibell", c("date","data")],
                       "Carbon dioxide equivalent CO2eq, ppm"= df[df$sensor=="Grove VOC and eCO2 Gas Sensor" & df$valuetype=="Carbon dioxide equivalent CO2eq",c("date","data")],
                       "Light Sensor, lux" = df[df$sensor=="Light Sensor" & df$valuetype=="Illuminance (Visible)",c("date","data")],
                       "PIR Motion Sensor"= df[df$sensor=="PIR Motion Sensor" & df$valuetype=="PIR on",c("date","data")],
                       "Sunlight Sensor, lm"= df[df$sensor=="Sunlight Sensor" & df$valuetype=="Lumen",c("date","data")],
                       "Ultraviolet index, UV"= df[df$sensor=="Sunlight Sensor" & df$valuetype=="Ultraviolet index",c("date","data")],
                        
                       )
      tabel <- aggregate(andmed$data, by=list(andmed$date), function(x) c(mean = round(mean(x), 2)))
      names(tabel) <- c("Dates", input$var1)
      tabel
    })
    
    output$tabelK <- renderDT(datatable(data_tableK(), options = list(searching = FALSE,ordering=F, pageLength = 16,scrollX = TRUE)))
    
    inputPlot31 <- reactive({
      df <- as.data.frame(data_tableK())
      p <- ggplot(df,aes(x=df[,1], y=df[,2],text = paste(df[,1], input$var, df[,2]))) +
        geom_bar(stat="identity",fill="deepskyblue3") +
        labs(title=paste("Mean ",input$var1)) +xlab("Dates")+ylab(input$var1)+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      
      p <- ggplotly(p, tooltip = "text")
      p <- config(p,modeBarButtons = list(list("toImage")))
      p
    })
    
    output$plotly31 <- renderPlotly({
      print(inputPlot31())
      
    })
    
    data_tableK1 <- reactive({
      df <- NewTableAll()
      df$date <- as.Date(df$date_time)
      #df <- df[df$date >=input$dates1[1]&df$date <=input$dates1[2],]
      
      #df <- df[,-ncol(df)]
      andmed <- switch(input$var2,
                       #get(input$variabley),
                       "Total Volatile Organic Compounds, ppb" = df[df$sensor=="Grove VOC and eCO2 Gas Sensor" & df$valuetype=="Total Volatile Organic Compounds",c("date","data")],
                       "Temperature, C" = df[df$sensor=="Temperature and Humidity Sensor Pro" & df$valuetype=="T",c("date","data")],
                       "Humidity, %" = df[df$sensor=="Temperature and Humidity Sensor Pro" & df$valuetype=="Humidity",c("date","data")],
                       "Loudness Sensor, db"= df[df$sensor=="Loudness Sensor" & df$valuetype=="Detsibell", c("date","data")],
                       "Carbon dioxide equivalent CO2eq, ppm"= df[df$sensor=="Grove VOC and eCO2 Gas Sensor" & df$valuetype=="Carbon dioxide equivalent CO2eq",c("date","data")],
                       "Light Sensor, lux" = df[df$sensor=="Light Sensor" & df$valuetype=="Illuminance (Visible)",c("date","data")],
                       "PIR Motion Sensor"= df[df$sensor=="PIR Motion Sensor" & df$valuetype=="PIR on",c("date","data")],
                       "Sunlight Sensor, lm"= df[df$sensor=="Sunlight Sensor" & df$valuetype=="Illuminance (IR)",c("date","data")],
                       "Ultraviolet index, UV"= df[df$sensor=="Sunlight Sensor" & df$valuetype=="Ultraviolet index",c("date","data")],
                       
      )
      tabel <- aggregate(andmed$data, by=list(andmed$date), function(x) c(mean = round(mean(x), 2)))
      names(tabel) <- c("Dates", input$var2)
      tabel
    })
    
    output$tabelK1 <- renderDT(datatable(data_tableK1(), options = list(searching = FALSE,ordering=F, pageLength = 16,scrollX = TRUE)))
    
    data_tableK2 <- reactive({
      df <- NewTableAll()
      df$date <- as.Date(df$date_time)
      #df <- df[df$date >=input$dates1[1]&df$date <=input$dates1[2],]
      
      #df <- df[,-ncol(df)]
      andmed <- switch(input$var3,
                       #get(input$variabley),
                       "Total Volatile Organic Compounds, ppb" = df[df$sensor=="Grove VOC and eCO2 Gas Sensor" & df$valuetype=="Total Volatile Organic Compounds",c("date","data")],
                       "Temperature, C" = df[df$sensor=="Temperature and Humidity Sensor Pro" & df$valuetype=="T",c("date","data")],
                       "Humidity, %" = df[df$sensor=="Temperature and Humidity Sensor Pro" & df$valuetype=="Humidity",c("date","data")],
                       "Loudness Sensor, db"= df[df$sensor=="Loudness Sensor" & df$valuetype=="Detsibell", c("date","data")],
                       "Carbon dioxide equivalent CO2eq, ppm"= df[df$sensor=="Grove VOC and eCO2 Gas Sensor" & df$valuetype=="Carbon dioxide equivalent CO2eq",c("date","data")],
                       "Light Sensor, LUX" = df[df$sensor=="Light Sensor" & df$valuetype=="Illuminance",c("date","data")],
                       "PIR Motion Sensor"= df[df$sensor=="PIR Motion Sensor" & df$valuetype=="PIR on",c("date","data")],
                       "Sunlight Sensor, lm"= df[df$sensor=="Sunlight Sensor" & df$valuetype=="Lumen",c("date","data")],
                       "Ultraviolet index, UV"= df[df$sensor=="Sunlight Sensor" & df$valuetype=="Ultraviolet index",c("date","data")],
                       
      )
      tabel <- aggregate(andmed$data, by=list(andmed$date), function(x) c(mean = round(mean(x), 2)))
      names(tabel) <- c("Dates", input$var3)
      tabel
    })
    
    output$tabelK2 <- renderDT(datatable(data_tableK2(), options = list(searching = FALSE,ordering=F, pageLength = 16,scrollX = TRUE)))
    
    inputPlot41 <- reactive({
      
      df <- as.data.frame(data_tableK1())
      df1<- as.data.frame(data_tableK2())
      df2=data.frame(df,df1)
      p <- ggplot(df2,aes(x=df2[,2], y=df2[,4])) +
        geom_line(color="deepskyblue3", size=1) +
        labs(title=paste("High Accuracy Temperature ")) +xlab(input$var2)+ylab(input$var3)+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
      p <- ggplotly(p)
      p <- config(p,modeBarButtons = list(list("toImage")))
      p 
    })
    
    output$plotly41 <- renderPlotly({
      print(inputPlot41())
      
    })
   
    onStop(function() {
        dbDisconnect(con)
    }) 
}

# Run the app
shinyApp(ui, server)
