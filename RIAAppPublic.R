

###App to display BRC (Not in Public version) and economic data sources plus the Tweet Hunter which produces a word cloud from mined tweets###

#You will also need the following files:
# AppdataPublic.RDS
#dygraph.css


#Required packaged
library(httr)
library(shiny)
library(twitteR)
library(wordcloud)
library(tm)
library(base64enc)
library(RCurl)
library(ROAuth)
library(RCurl)
library(shiny)
library(shinythemes)
require(formattable)
require(rowr)
require(ggplot2)
require(dplyr)
require(DT)
require(magrittr)
require(shinyjs)
require(dygraphs)
require(forecast)
require(Quandl)
require(shinydashboard)
library(extrafont)
library(wordcloud2)


#########################################
#colours
BRCnavy=rgb(38,34,98,maxColorValue = 255)
BRCfuschia=rgb(146,39,143,maxColorValue = 255)
BRCgreen=rgb(175,202,11,maxColorValue = 255)
BRCblue=rgb(0,187,206,maxColorValue = 255)
BRCcolours=c(BRCnavy,BRCfuschia,BRCblue,BRCgreen)

#twitter keys

MyKey="My Key"
MySecret="My Secret"
MyToken="My Token"
MyAccess="My Access"
my_oauth<-setup_twitter_oauth(MyKey,MySecret,MyToken,MyAccess)


#functions
scaleFUN <- function(x) sprintf("%.1f", x)

#read in data
dataset=readRDS("AppdataPublic.RDS")

#round data to 1dp
dataset=dataset %>% mutate_each(funs(round(.,1)), -date)

#allocate date as date
date=as.Date(dataset$date, format = "%m %y")
dataset$date=date


### App begins

shinyApp(
  
  ## ui.R ##
ui <- 
  
  dashboardPage(skin="blue",
         dashboardHeader(title="BRC Retail Insight Data Explorer (Beta)",titleWidth=350),
                                 
  dashboardSidebar(disable=TRUE),
  
  dashboardBody(
    
    #CSS Styling for Value boxes
    tags$style(".small-box.bg-maroon{ background-color: #262262 !important; color: #FFFFFF !important;font-size:14px }"),
    tags$style(".small-box.bg-aqua{ background-color: #8CC747 !important; color: #FFFFFF !important; }"),
    tags$style(".small-box.bg-orange{ background-color: #F37021 !important; color: #FFFFFF !important; }"),
    tags$style(".small-box.bg-purple{ background-color: #630460  !important; color: #FFFFFF !important; }"),
    
    #set bespoke colour and font  for header
    tags$head(tags$style(HTML('.skin-blue .main-header .logo {
                                              background-color: #92278F;font-family: "Lato", sans-serif;
                                              font-weight: bold;
                                              font-size: 16px;
                                              }
                                              
    
                                              .skin-blue .main-header .logo:hover {
                                              background-color: #92278F;
                                              }
                                              
                                              .skin-blue .main-header .navbar {
                                              background-color: #92278F;
                                              }        '))),
    #set height of value boxes
    tags$head(tags$style(HTML(".small-box {height: 130px;
                              font-size:14px }"))),
   
    #Introduce different tabs to the dashboard
     navbarPage(title="",
    #Details for retail industry data tab
      tabPanel("Retail Industry Data",
      fluidPage(
      fluidRow(
      
    #Dynamic valueBoxes output
      valueBoxOutput("Sales",width=3),
      valueBoxOutput("Online",width=3),
      valueBoxOutput("Footfall",width=3),
      valueBoxOutput("Prices",width=3)
      
    ),

    #dynamic chart output
    fluidRow(
      box(width=8,
       dygraphOutput("Chart")),
      
      
    #data selection panel
      
      #style data selection panel
      box(width=4,
          tags$style(type="text/css", "selectize-input{font-size:20;line-height:20;}.selectize-dropdown { font-size: 14px; line-height: 14px; }"),
          
          
      # select first data series to plot   
        selectInput("selectInput", "Select column:",
                  choices = colnames(dataset[,-which(names(dataset)%in%c("date"))])),
      #select second data series to plot  
      selectInput("selectInput2", "Select column:",
                  choices = colnames(dataset[,-which(names(dataset)%in%c("date"))])),
      #option to plot on secondary axis  
      checkboxInput("SecondaryAxis1","Plot on secondary axis", FALSE),
      
      #select third dataseries to plot
      selectInput("selectInput3", "Select column:",
                  choices = colnames(dataset[,-which(names(dataset)%in%c("date"))])),
      #option to plot on secondary axis 
     checkboxInput("SecondaryAxis2","Plot on secondary axis",FALSE),
      
     #select timeseries
      sliderInput("range_one", "Date Range:",min =as.Date(min(dataset$date)), max =as.Date(max(dataset$date)), timeFormat ="%b-%Y" ,value = c(as.Date(min(dataset$date)),as.Date(max(dataset$date)))),
      
     #insert download button
     #downloadButton("downloadData", "Download Data"),
     
     #submit button - necessary given that tweet hunter has this and it won't run otherwise
     submitButton(text = "Run")
     
      
      )))),
  
   #details of tweet hunter tab
   tabPanel("Tweet Hunter",
            fluidPage(
              
              #select term to search
              sidebarPanel(textInput("term", "Enter a term", "Retail"),
              #select number of tweets to search
                           sliderInput("cant", "Select a number of tweets",min=5,max=300, value = 5),
              #submit to run
                           submitButton(text = "Run")
                           
                           #download data
                           #downloadButton("download", "Download File")),
              
              ),
    #results
              mainPanel(
                h4("Last 5 tweets"),
    #display last 5 tweets
                tableOutput("table"),
    #output wordcloud of most frequent words
                wordcloud2Output("wordcl",height="400px")
            )
            
            
            
            
            )
    )
   
 

    ))),


###  server

server <- function(input, output) {
   
  #value boxes 
  output$Sales<-renderValueBox({
    #Pick-up date of most recent non-NA data point for CPI
    
    latestdate<-max(dataset$date[which(!is.na(dataset[,"Consumer Prices (CPI) (% yoy change):ONS"]))])
    
    #display latest sales growth datapoint with date and series name
    valueBox(paste(dataset[which(dataset$date==latestdate),"Consumer Prices (CPI) (% yoy change):ONS"],"%"),paste("Consumer Prices",format(latestdate,"%b %Y"),sep=" - "),color="purple",icon=icon("shopping-cart"))
    
  })
  
  output$Footfall<-renderValueBox({
    
    latestdate<-max(dataset$date[which(!is.na(dataset[,"Total Pay growth 3 months (% yoy change):ONS"]))])
    
    valueBox(paste(dataset[which(dataset$date==latestdate),"Total Pay growth 3 months (% yoy change):ONS"],"%"),paste("Total Pay Growth 3 months on same 3 months last year",format(latestdate,"%b %Y"),sep=" - "),color="orange",icon=icon("paw"))
    
  })
  
  output$Prices<-renderValueBox({
    
    latestdate<-max(dataset$date[which(!is.na(dataset[,"Unemployment Rate:ONS"]))])
    
    valueBox(paste(dataset[which(dataset$date==latestdate),"Unemployment Rate:ONS"],"%"),paste("Unemployment Rate",format(latestdate,"%b %Y"),sep=" - "),color="aqua",icon=icon("money"))
    
  })
  
  output$Online<-renderValueBox({
    
    latestdate<-max(dataset$date[which(!is.na(dataset[,"Retail sales value ex-fuel NSA (% yoy change):ONS"]))])
    
    valueBox(paste(dataset[which(dataset$date==latestdate),"Retail sales value ex-fuel NSA (% yoy change):ONS"],"%"),paste("Retail sales value growth (year on year)",format(latestdate,"%b %Y"),sep=" - "),color="maroon",icon=icon("laptop"))
    
  })
  
  
  
  #Interactive graph
  
  #create dataset of selected data series
    datasetInput<-reactive({dataset[(dataset$date>=min(input$range_one) & dataset$date<=max(input$range_one)),c("date",input$selectInput,input$selectInput2,input$selectInput3)]
      
    })
  
    #identify series that will be plotted on the main y axis
    columnsy1<-reactive({if(input$SecondaryAxis1==TRUE & input$SecondaryAxis2==TRUE){
        a=2}
          else{if(input$SecondaryAxis1==TRUE & input$SecondaryAxis2==FALSE){
                        a=c(2,4)}
          else{if(input$SecondaryAxis1==FALSE & input$SecondaryAxis2==TRUE){
            a=c(2,3)}
            else{
              a=2:4
          }}}
          
          
    })
    
    #identify series that will be plotted on the secondary y axis
    columnsy2<-reactive({
      x=columnsy1()
      a= subset(2:4,!(2:4 %in% x))
      
    })
    
    
    #convert dataset to timeseries data
    df<-reactive({
      df=datasetInput()
      df2=xts(df,order.by=df$date)
    })

    #define axis minimum for main y-axis based on lowest data value
    minimumy1<-reactive({
      a=datasetInput()
      b=columnsy1()
      min(a[,b],na.rm=T)})
    
    #define axis maximum for main y-axis based on lowest data value
    maximumy1<-reactive({
      a=datasetInput()
      b=columnsy1()
      max(a[,b],na.rm=T)})
    
    #define axis minimum for secondary y-axis based on lowest data value
    minimumy2<-reactive({
      a=datasetInput()
      b=columnsy2()
      
      if(length(b)==0){
        x=minimumy1()
      }else{
      min(a[,b],na.rm=T)}
    })
    
    #define axis maximum for secondary y-axis based on lowest data value
    maximumy2<-reactive({
      a=datasetInput()
      b=columnsy2()
      if(length(b)==0){
        x=maximumy1()
      }else{
        max(a[,b],na.rm=T)}
    })
    
   #identify if a seconary yaxis is needed
    yaxis2=reactive({
      a=columnsy2()
      if(length(a)==0){"y"}else{"y2"}
    })
    #identify axis series 2 to be plotted on
    series2<-reactive({
      a=columnsy2()
      if(3 %in% a){"y2"}else{"y"}
      
    })
    #identify axis series 3 to be plotted on
    series3<-reactive({
      a=columnsy2()
      if(4 %in% a){"y2"}else{"y"}
      
    })
    

    
 
  #dunamic chart
    output$Chart <- renderDygraph({
    #data to be used 
    df2=df()
    #dynamic title, with extra detail trimmed off
      title=paste(gsub(pattern="\\:.*",replacement="",input$selectInput),gsub(pattern="\\:.*",replacement="",input$selectInput2),gsub(pattern="\\:.*",replacement="",input$selectInput3),sep=" & ")
      
    #draw dygraph
       dygraph<-dygraph(df2[,2:4], main=title, height="70%") %>%
      #define line colours
          dyOptions(colors = c(BRCfuschia,BRCblue,BRCnavy))%>%
         
        #allocate series to axes
        dySeries(names(df2)[2],axis="y")%>%
        dySeries(names(df2)[3],axis=series2())%>%
        dySeries(names(df2)[4],axis=series3())%>%
        
         #CSS Styling for the dychart
         dyCSS("dygraph.css")%>%
         
         #set axis limits         
        dyAxis(name="y",valueRange=c(minimumy1()-0.4,maximumy1()+0.4))%>%
        dyAxis(name=yaxis2(),valueRange=c(minimumy2()-0.4,maximumy2()+0.4))%>%
        
         #add legend which appear when mouse rolls over
         dyLegend(labelsSeparateLines = T,show="follow")
        
      
    })
    
    # Retail 2020, Upload Dashboard Images
    
    output$prodimage <- renderImage({
    
      return(list(src = "prod.png",
           contentType = 'image/png',
           width = 200,
           height = 200,
           alt = "This is alternate text"))
    }, deleteFile = FALSE)
    
    output$engimage <- renderImage({
      
      return(list(src = "eng.png",
                  contentType = 'image/png',
                  width = 200,
                  height = 200,
                  alt = "This is alternate text"))
    }, deleteFile = FALSE)
    
    output$empimage <- renderImage({
      
      return(list(src = "emp.png",
                  contentType = 'image/png',
                  width = 200,
                  height = 200,
                  alt = "This is alternate text"))
    }, deleteFile = FALSE)
    
    output$payimage <- renderImage({
      
      return(list(src = "pay.png",
                  contentType = 'image/png',
                  width = 200,
                  height = 200,
                  alt = "This is alternate text"))
    }, deleteFile = FALSE)
    
    #define employment chart
    output$EmpChart <- renderDygraph({
      df=xts(Retail2020,order.by=Retail2020$date)
      min=min(Retail2020[,2])
      max=max(Retail2020[,2])
      dygraph<-dygraph(df[,2]) %>%
        dyOptions(colors = c(BRCfuschia),title="Jobs in retail")%>%
        dyAxis(name="y",valueRange=c(min,max))%>%
        dyLegend(labelsSeparateLines = T,show="follow")
      
      
    })
    #define productivity chart
    output$ProdChart <- renderDygraph({
      df=xts(Retail2020,order.by=Retail2020$date)
      min=min(Retail2020[,3:4])
      max=max(Retail2020[,3:4])
      
      dygraph<-dygraph(df[,3:4]) %>%
        dyOptions(colors = c(BRCfuschia,BRCblue))%>%
        dyAxis(name="y",valueRange=c(min,max))%>%
        dyLegend(labelsSeparateLines = T,show="follow")
      
      
    })
    
    #define pay chart
    output$PayChart<-renderDygraph({
      df=xts(Pay,order.by=Pay$date)
      min1=min(df[,2])
      max1=max(df[,2])
      min1=min(df[,4])
      max1=max(df[,4])
      
      
      dygraph<-dygraph(df[,c(2:4)]) %>%
        dyOptions(colors = c(BRCfuschia,BRCblue))%>%
        dySeries(names(df)[4],axis="y2")%>%
        dyAxis(name="y",valueRange=c(0,15))%>%
        dyAxis(name="y2",valueRange=c(0,60))%>%
        dyLegend(labelsSeparateLines = T,show="follow")
      
    })
    
    #download data
    output$downloadData <- downloadHandler(
      filename = function() { paste('data', '.csv', sep='') 
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
    
    ##Tweet hunter
    
    #download tweets from twitter
    rawData <- reactive({
      term=input$term
      cant=input$cant
      
      
      tweets <- searchTwitter(term, n=cant, lang="en")
      twListToDF(tweets)
    })
    
    #output tweets
    output$table <- renderTable({
      head(rawData()[1])
    })
    
    #draw wordcloud
    output$wordcl <- renderWordcloud2({

      tw.text <- enc2native(rawData()$text)
      tw.text <- tolower(tw.text)
      tw.text <- removeWords(tw.text,c(stopwords(input$lang),"rt","RT","RT ",input$term))
      tw.text <- removePunctuation(tw.text, TRUE)
      tw.text <- gsub("@\\w", "", tw.text)
      tw.text <- gsub("http\\w+", "", tw.text)
      tw.text <- gsub("https\\w+", "", tw.text)
      tw.text <- unlist(strsplit(tw.text," "))
      
      word <- sort(table(tw.text),TRUE)
      
      # wordcloud data set of top 50 words
      wordc <- head(word,n=50)
      
      #draw wordcloud
      wordcloud2(wordc,size=18)
    })
    
    
    
    
  })


shinyApp(ui, server)
