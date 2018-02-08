#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
## Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


packages<-c("shinydashboard", "shinyjs", "shinythemes", "tidyr","tidyverse","ggplot2","plotly","leaflet","ROAuth","tm")
check.packages(packages)


library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(leaflet)
library(rgdal)
library(twitteR)
library(ROAuth)
library(tm)
library(forecast)

#loaded all libraries 
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

create_matrix <- function(textColumns, language="english", minDocFreq=1, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
  
  stem_words <- function(x) {
    split <- strsplit(x," ")
    return(stemDocument(split[[1]],language=language))
  }
  
  control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
  
  if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
  
  trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
  trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")
  
  corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
  matrix <- DocumentTermMatrix(corpus,control=control);
  if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
  
  gc()
  return(matrix)
}
#sparse matrix creation
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("database/emotions.csv",header=FALSE)
  
  counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  
  colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
  return(documents)
}
# end of classify emotion
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("database/subjectivity.csv",header=FALSE)
  
  counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(positive=0,negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }		
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
    if (verbose) {
      print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  
  colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
  return(documents)
}

# end of classify polarity
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ui <-dashboardPage(skin = "black",

  dashboardHeader(title = "DASHBOARD"),
  
  dashboardSidebar(
    
  fluidPage( 
    sidebarMenu( titlePanel(h3("CRIME ANALYTICS")),
                 menuItem("STATE WISE",tabName = "stat",icon = icon("stats",lib="glyphicon")),
                 menuItem("DISTRICT WISE ",tabName = "dist",icon = icon("stats",lib="glyphicon")),
                 menuItem("CRIME MAP",tabName = "maps",icon = icon("map-marker",lib = "glyphicon")),
                 menuItem("EVENTS",tabName = "twitter",icon = icon("screenshot",lib="glyphicon")),
                 menuItem("FORECAST CRIME",tabName = "forecast",icon = icon("sort-by-attributes",lib ="glyphicon" ))
   
    )
    
    
  )
)
,
  
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    fluidPage(
      tabItems(
        tabItem( tabName = "stat",
                 fluidRow(
                   tabBox(width = "100%", side = "left",
                          
                          tabPanel( title = "STATE OVERVIEW",
                                    leafletOutput("stateleaf"),
                                    plotlyOutput("stategraph")
                            
                          )
                          
                   )
                   
                 )
                 
        ),#end of state
        tabItem(tabName = "dist",
                
                fluidRow(
                  
                  tabBox(width = "100%", side = "left",
                         
                         tabPanel( title = "OVERIVEW OF DISTRICTS",
                                   uiOutput("dyear"),
                                   uiOutput("dstate"), 
                                   uiOutput("dlist"),
                                   plotlyOutput("dcrime"),
                                   plotlyOutput("scrime")
        
                         ),
                         
                         tabPanel(title = "TRENDS IN CRIMES",
                                
                                  uiOutput("tcrime"),
                                  uiOutput("tstate"),
                                  uiOutput("tdist"),
                                  plotlyOutput("lgraph")
   
                         )
                         
                         
                  )
                  
                )
                
        ), #end of tab district
        
        tabItem(tabName = "maps",
                
                fluidRow(
                  
                  tabBox(width = "100%", side = "left",
                    
                  tabPanel(title = "GENERAL CRIMES",            
                    uiOutput("Ccrime"),
                    uiOutput("Cstate"),
                    leafletOutput("map")
                   
                    
                  ),
                  tabPanel(title = "CRIME TOWARDS WOMAN",
                           uiOutput("Wcrime"),
                           uiOutput("Wstate"),
                           leafletOutput("womenmap")
                    
                  ),
                  tabPanel(title = "CRIMES TOWARDS CHILDREN",
                          uiOutput("Icrime"),
                          uiOutput("Istate"),
                        plotlyOutput("Iplot"),
                        leafletOutput("Imap")
                  ) #end of tab box
                )#end of fluid row
          
        ) #end of Maps
        
        
        ),
        tabItem(tabName = "twitter",
           
          fluidRow( tabBox(width = "100%", side = "left",
          tabPanel(title = "Sentiment Analysis",      uiOutput("twstate"),
                uiOutput('twdist'),#end of twitter sentimental analysis for events
                leafletOutput("Twleaf"),
              
              div( style = "display:inline-block;",
               uiOutput('long')),
              div(style= "display:inline-block;",uiOutput('lat')
                  ),
              div(style= "display:inline-block;",uiOutput('rad')
              ),
              
              div(style= "display:inline-block;",uiOutput('twText')
              ),
              uiOutput('no_twt'),
              actionButton(inputId = "analyze",label = "Analyse"),
              plotOutput("sentplot"),
              plotOutput("polarityplot")
              
              
              
          )#end of tweet check
           ) #end of tab box    
        )
    ),
    
    tabItem(tabName = "forecast",
            
      fluidRow(
        
        tabBox(width = "100%",side = "left",
               
              tabPanel( title = "FORECAST CRIME",
                        
                        uiOutput("ffdist"),
                        uiOutput("ffcrime"),
                        sliderInput(inputId = "period",label = "Choose years",min = 2014,max = 2017,step = 1,value = 2014),
                        actionButton(inputId = "predict",label = "FORECAST"),
                        plotlyOutput("foplot")
                        
                
              ) 
               
               )
        
      )      
            
            )
    )
    )#end of fluid page
  ) #end of dashboard body
)#end of UI
          
#end of UI
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output,session) {
  consumer_key <-"AfbYgEqPQXFS6DRvDuWvgKjtV"
  
  consumer_secret <- "vSjWzqf2cu0RZt3GitG5CYmzt4WcLiu6Ones4TnOn1Cow0kfu2"
  access_token<-"244811000-N4oRoVuKGU2pTf8uocu1ySs8caLz1O9MjI8TvwzL"
  access_secret <- "8kGiHVVapssls5yIewVMZxSWjQSQ6fNZnkUmGqdQ5u1zC"
  
  setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )
  
values <- reactiveValues()
shp <- readOGR("maps-master/2011_Dist.shp")
mt <- subset(shp, shp$ST_NM == "Maharashtra")
dfs  <- read.csv("database/M1.csv",sep = ",")
idfs <- read.csv("database/infant13.csv",sep = ",")
tdfs <- read.csv("database/distspecs.csv",sep = ",")
catch.error = function(x){
  y = NA
  catch_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  return(y)
}



 
observe( { 
      
      values$df <- read.csv("database/Maharashtra.csv",sep = ',',header = TRUE)
      year <- unique(unlist(values$df[3]))
      output$dyear <- renderUI(selectInput(inputId = "Dyear",label = "Choose Year",choices = year ))
      
      
      #---------------------------District OVERVIEW ------------------------------------------------     
      observeEvent(input$Dyear,ignoreInit = FALSE,{
       
      states <- subset(values$df,values$df[3]== as.integer(input$Dyear), select = c(1))
    
      output$dstate <- renderUI(selectInput(inputId = "Dstate",label = "State",choices = unique(unlist(states))))
      
      
      observeEvent(input$Dstate,ignoreNULL = FALSE,{
     
        
        output$dlist <- renderUI(selectInput(inputId = "District",label = "Choose District",choices = unique(unlist(subset(values$df, values$df[3]==as.integer(input$Dyear) & values$df[1]== input$Dstate,select = c(2))))))
        
        observeEvent(input$District,ignoreNULL = FALSE,{
      
        output$dcrime <- renderPlotly({
          
          t <- subset(values$df , values$df[3]==as.integer(input$Dyear) & values$df[1]==input$Dstate & values$df[2] == input$District ,select= c(4,5,7,10,13,15,16,17,20))%>%gather(key="CRIMES",value="COUNT")
          
          t <- t[order(-t$COUNT),]
          t["CUMULATIVE"]<- cumsum(t["COUNT"])
          form <- list(categoryorder = "array",categoryarray = unique(unlist(t[1])),title = "",showticklabels = FALSE)
          p  <- plot_ly(t)%>%add_trace(x = ~CRIMES, y = ~COUNT, type = 'bar',color = ~CRIMES)%>%
                                 add_trace(x = ~CRIMES, y = ~CUMULATIVE, type = 'scatter', mode = 'lines+markers', name = 'CUMULATIVE', yaxis = 'y2',
                                                          line = list(color = '#45171D')) %>%
                            layout(title = 'PUBLIC CRIMES',
                                             xaxis = form,
                                               yaxis = list(title = "",side = 'left',showgrid = FALSE, zeroline = FALSE),
                                               yaxis2 = list(side = 'right', overlaying = "y", title = "", showgrid = FALSE, zeroline = FALSE),
                                               paper_bgcolor = 'rgba(245, 246, 249, 1)',
                                               plot_bgcolor = 'rgba(300, 246, 249, 1)',
                                               showlegend = TRUE)
                                }
                                      )

        
        output$scrime <- renderPlotly({
          
          kp <-subset(values$df , values$df[3]==as.integer(input$Dyear) & values$df[1]==input$Dstate & values$df[2] == input$District ,select= c(21,22,25,26,26,27,29,31))%>%gather(key="CRIMES",value="COUNT")
          
          kp <- kp[order(-kp$COUNT),]
          kp["CUMULATIVE"]<- cumsum(kp["COUNT"])
          sform <- list(categoryorder = "array",categoryarray = unique(unlist(kp["CRIMES"])),title = "",showticklabels = FALSE)
          pp  <- plot_ly(kp)%>%add_trace(x = ~CRIMES, y = ~COUNT, type = 'bar',color = ~CRIMES)%>%
            add_trace(x = ~CRIMES, y = ~CUMULATIVE, type = 'scatter', mode = 'lines+markers', name = 'CUMULATIVE', yaxis = 'y2',
                      line = list(color = '#45171D')) %>%
            layout(title = 'DOMESTIC CRIMES',
                   xaxis = sform,
                   yaxis = list(title = "",side = 'left',showgrid = FALSE, zeroline = FALSE),
                   yaxis2 = list(side = 'right', overlaying = "y", title = "", showgrid = FALSE, zeroline = FALSE),
                   paper_bgcolor = 'rgba(245, 246, 249, 1)',
                   plot_bgcolor = 'rgba(300, 246, 249, 1)',
                   showlegend = TRUE,autosize = TRUE)                            
          
          })

        })#end of choose District 
        })#end of choose state
  
      }
      )#end of choose year
      
      #---------------------------District OVERVIEW ------------------------------------------------
      #
      #
      #---------------------------District Crime Trend-------------------------------------------
      
      output$tcrime <- renderUI(selectInput(inputId = "TCRIME",label = "CHOOSE CRIME",choices = colnames(subset(values$df,select = c(-1,-2,-3,-33,-32)))))
      
      observeEvent(input$TCRIME,ignoreNULL = FALSE,{
        
      output$tstate <- renderUI(selectInput(inputId = "TSTATE",label = "STATE",choices = subset(values$df,select = c(1))%>%unlist()%>%unique()))
      
      observeEvent(input$TSTATE,ignoreNULL = FALSE,{
        
      output$tdist <- renderUI(selectInput(inputId = "TDISTRICT",label = "CHOOSE DISTRICT",choices = subset(values$df,values$df[1]== input$TSTATE,select = c(2))%>%unlist()%>%unique())) 
        
      observeEvent(input$TDISTRICT,ignoreNULL = FALSE,{
        
        
      output$lgraph <- renderPlotly(ggplotly(ggplot(data = subset(values$df, values$df[1]==input$TSTATE & values$df[2] == input$TDISTRICT ,select = c("YEAR",input$TCRIME)),aes_string(x = "YEAR",y=input$TCRIME,xlab="YEAR"))+geom_line(color='steelblue')+geom_point(color = 'steelblue')+theme(axis.title.y = element_blank())+ggtitle(input$TCRIME)))
      
      
      
      })  #end of district
      
      })  # end of state
      
      })

      #end of Crime Trend 
      
      #---------------------------District Crime Trend-------------------------------------------
      #
      #
      #---------------------------STATE OVERVIEW ------------------------------------------------
      
      
      observe({
      years <- unique(unlist(values$df[3]))
      Total_Crimes <- numeric()
      for(i in years)
      {
        tot <-  subset(values$df,values$df[3] == i,select= c(33))
        Total_Crimes = append(Total_Crimes,sum(tot))
      }
      trend <- data.frame(years,Total_Crimes)
      
      l <- subset(dfs,select=c(32))
      l <- as.integer(l$TOTAL.IPC.CRIMES)
      pal <- colorNumeric("Reds",domain = NULL)
      output$stateleaf <- renderLeaflet(leaflet(mt,options = leafletOptions(dragging = FALSE))%>%addTiles()%>%addPolygons( weight = 1, color = "Black",smoothFactor = 0.5, opacity = 1.0,fillOpacity = 0.5,
                                                                                                             fillColor = ~pal(l),highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),label = ~paste(DISTRICT,l))
      %>%addLegend(pal = pal,values = l,title = "TOTAL CRIMES PER DISTRICT :2013")
      
      ) #end of leaflet
      
      output$stategraph <- renderPlotly(ggplotly(ggplot(data = trend,aes_string(x = "years",y="Total_Crimes",xlab="years"))+geom_line(color='steelblue')+geom_point(color = 'steelblue')+theme(axis.title.y = element_blank())+ggtitle("TOTAL CRIMES TREND PER YEAR (2001-2013)")))
        
        
      })  #end of state Overview
   
    
      #---------------------------STATE OVERVIEW ------------------------------------------------
      #
      #
      #---------------------------CRIME MAPS ----------------------------------------------------
      
      output$Ccrime <- renderUI(selectInput(inputId = "CC",label = "CHOOSE CRIME",choices = colnames(subset(values$df,select = c(-1,-2,-3,-33,-7,-8,-9,-11,-26,-27,-28,-29,-30)))))
      
      observeEvent(input$CC,ignoreNULL = FALSE,{
        
      output$Cstate <- renderUI(selectInput(inputId = "CS",label = "STATE",choices = c("Maharashtra")))
      
      observeEvent(input$CS,ignoreNULL = FALSE,{
        
  
       # -----------------LEAFLET---------------------
        
        k <- dfs[input$CC]
        qpal <- colorNumeric("Reds",domain = NULL)
        output$map <- renderLeaflet(
          leaflet(mt,options = leafletOptions(dragging = FALSE))%>%addTiles()%>%addPolygons( weight = 1, color = "Black",smoothFactor = 0.5, opacity = 1.0,fillOpacity = 0.5,
          fillColor = ~qpal((unlist(k[1]))),highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),label = ~paste(DISTRICT,unlist(k[1])))
          %>%addLegend(pal = qpal,values = unlist(k[1]),title = input$CC)
        )
        
    
       #--------------------- LEAFLET ______________
     
      }) #end of crime maps
      
      }
      )
      
   ################ W  O  M  A  N ##############################
          
      output$Wcrime <- renderUI(selectInput(inputId = "WC",label = "CHOOSE CRIME",choices = colnames(subset(values$df,select = c(7,8,11,26,27,28,29,30)))))
      
      observeEvent(input$WC,ignoreNULL = FALSE,{
        
        output$Wstate <- renderUI(selectInput(inputId = "WS",label = "STATE",choices = c("Maharashtra")))
        
        observeEvent(input$WS,ignoreNULL = FALSE,{
          
          
          # -----------------LEAFLET---------------------
          
          w <- dfs[input$WC]
          wpal <- colorNumeric("Reds",domain = NULL)
          output$womenmap <- renderLeaflet(
            leaflet(mt,options = leafletOptions(dragging = FALSE))%>%addTiles()%>%addPolygons( weight = 1, color = "Black",smoothFactor = 0.5, opacity = 1.0,fillOpacity = 0.5,
                                                                                               fillColor = ~wpal((unlist(w[1]))),highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),label = ~paste(DISTRICT,unlist(w[1])))
            %>%addLegend(pal = wpal,values = unlist(w[1]),title = input$WC)
          )
          # -----------------LEAFLET---------------------
          
        })
    
        })#end of crime maps
      
  ################ W  O  M  A  N ##############################
      
 # ---------------- C H I L D R E N -----------------------------------------
  
     
      output$Icrime <- renderUI(selectInput(inputId = "IC",label = "CHOOSE CRIME",choices = colnames(subset(idfs,select=c(-1,-2,-15)))))
      
      observeEvent(input$IC,ignoreNULL = FALSE,{
        
        output$Istate <- renderUI(selectInput(inputId = "IS",label = "STATE",choices = c("Maharashtra")))
        
        observeEvent(input$IS,ignoreNULL = FALSE,{
          
          crime <- idfs[input$IC]
          district <- idfs["DISTRICT"]
           output$Iplot <- renderPlotly(plot_ly( x = unlist(district), y = unlist(crime), type = 'bar',color = unlist(district),orientation = 'v') %>%
                                          
                                          layout(title = input$IC,
                                                 xaxis = list(title = "",showticklabels = FALSE),
                                                 yaxis = list(title = ""),
                                                 paper_bgcolor = 'rgba(245, 246, 249, 1)',
                                                 plot_bgcolor = 'rgba(300, 246, 249, 1)',
                                                 showlegend = TRUE))
          
        })
        
      })
      
      
      
      i <- subset(idfs,select = c(15))
      ipal <- colorNumeric("Blues",domain = NULL)
      
      output$Imap <- renderLeaflet(
        leaflet(mt,options = leafletOptions(dragging = FALSE))%>%addTiles()%>%addPolygons( weight = 1, color = "Black",smoothFactor = 0.5, opacity = 1.0,fillOpacity = 0.5,
                                                                                           fillColor = ~ipal((unlist(i[1]))),highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),label = ~paste(DISTRICT,unlist(i[1])))
        %>%addLegend(pal = ipal,values = unlist(i[1]),title = "TOTAL CHILD RELATED CRIMES")
      )
            
  
      #---------------------------CRIME MAPS ----------------------------------------------------
      
      
      #---------------------------Event Sentiment  ----------------------------------------------------
      
      
      output$twstate <- renderUI(selectInput(inputId = "Twstate",label = "STATE",choices = c("MAHARASHTRA")))
      output$twdist <- renderUI(selectInput(inputId = "TwD",label = "Choose District",choices = unique(unlist(tdfs[1]))))
      observeEvent(input$TwD,ignoreInit = FALSE,{
      
      distspecs <- reactive(subset(tdfs,tdfs[1]==input$TwD,select = c(2,3,5)))
      Lng <- reactive(return(as.numeric(distspecs()$LONG)))
      Lat <- reactive(return(as.numeric(distspecs()$LAT)))
      output$Twleaf <- renderLeaflet(leaflet(options = leafletOptions(dragging = FALSE))%>%addTiles()%>%addMarkers(lng=Lng() ,lat = Lat(),label = input$TwD)%>%setView(lng=Lng() ,lat = Lat(),zoom = 12))
        
      radius <- reactive(return(paste0(distspecs()$RADIUS,"km")))
      
      output$long <- renderUI(selectInput(inputId = "Longitude",label = "Longitude",choices= Lng()))
      output$lat <- renderUI(selectInput(inputId = "Latitude",label = "Latitude",choices= Lat()))
      output$rad <- renderUI(selectInput(inputId = "cradius",label = "City Radius",choices= radius()))
      output$twText <- renderUI(textInput("tweet",label = "Enter Keyword",value = "#BJP"))
      output$no_twt <- renderUI(sliderInput(inputId = "n",label = "No. of Tweets",min = 25,max = 1000,value = 50))
     
      
      
     observeEvent(input$analyze,{
        
        geo <- paste0(input$Latitude,",",input$Longitude,",",input$cradius)
        
        withProgress(message = "Anlayzing Tweets",detail = "This May Take a while",value = 0,{
        
        tweets <- twListToDF(searchTwitter(input$tweet, n = as.integer(input$n), geocode = geo,lang = "en"))
        
        tweets_result = ""
        tweets_result = tweets$text
        tweets_result = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_result) #retweets 
        incProgress(amount = 0.20)
        tweets_result = gsub("@\\w+", "", tweets_result)  # all "@people"
        tweets_result = gsub("[[:punct:]]", "", tweets_result) 
        tweets_result = gsub("[[:digit:]]", "", tweets_result)
        tweets_result = gsub("http\\w+", "", tweets_result) #html links
        tweets_result = gsub("[ \t]{2,}", "", tweets_result) #white spaces
        tweets_result = gsub("^\\s+|\\s+$", "", tweets_result) #"slang words"
        tweets_result = sapply(tweets_result, catch.error)
        tweets_result = tweets_result[!is.na(tweets_result)]
        names(tweets_result) = NULL
        incProgress(amount = 0.40)
        twt_class_emo = classify_emotion(tweets_result, algorithm="bayes", prior=1.0)
        emotion = twt_class_emo[,7]
        emotion[is.na(emotion)] = "unknown"
        twt_class_pol = classify_polarity(tweets_result, algorithm="bayes")
        polarity = twt_class_pol[,4]
        incProgress(amount = 0.60)
        
       # output$test<-renderDataTable(head(tweets,10))
        sentiment_dataframe<- reactive({
          input$analyze
          sent <- isolate({
            sd <- data.frame(text=tweets_result, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
            sd <- within(sd, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
            
          })
          
        })
        Ttitle <- reactive({
          input$analyze
          tt <- isolate(as.String(input$tweet))
        })
       # sentiment_dataframe = data.frame(text=tweets_result, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
        #sentiment_dataframe = within(sentiment_dataframe, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
        
        incProgress(amount = 0.80)
        output$sentplot <- renderPlot( ggplot(sentiment_dataframe(), aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
                                         scale_fill_brewer(palette="Dark2") +
                                         ggtitle(paste("Sentiments of people on :",Ttitle())) +
                                         theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories'))
        
        output$polarityplot <- renderPlot( ggplot(sentiment_dataframe(), aes(x=polarity)) +
                                             geom_bar(aes(y=..count.., fill=polarity)) +
                                             scale_fill_brewer(palette="RdGy") +
                                             ggtitle(paste("Views of people on :",Ttitle())) +
                                             theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories'))
        incProgress(amount = 1)
        
        })

      })
    
      })
      
    
      #---------------------------Event Sentiment  ----------------------------------------------------
    
      #----------------------------Crime Forecast-------------------------------------------------------
      
      output$ffdist <- renderUI(selectInput(inputId = "Fodist",label = "Choose District",choices = subset(values$df,select = c(2))%>%unlist()%>%unique()))
      output$ffcrime <- renderUI(selectInput(inputId = "Focrime",label = "Choose Crime",choices = colnames(subset(values$df,select = c(-1,-2,-3)))))
      v <- reactiveValues(doplot = FALSE)
      
      observeEvent(input$predict,{
        
        v$doPlot <- input$predict
      })
      
      observeEvent(input$forecast,{
        v$doPlot <- FALSE
      })
      
      output$foplot<- renderPlotly(
        {
         if (v$doPlot == FALSE) return()
          
       
          isolate({
            
            pdat <- unlist(subset(values$df,values$df[2]== input$Fodist,select = c(input$Focrime)),use.names = FALSE)
            
            cts <- ts(data = pdat,start = c(2001),end=c(2013),frequency = 1)
            Hvalue <-  as.integer(input$period) - 2013
            withProgress(message = "Forecasting....",detail = "This May Take a while",value = 0,{  
            lower <- pdat
            upper <- pdat
            pred <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
            app_val <- numeric()
            for(i in 1:Hvalue){
              app_val <- append(app_val,i)
            }
            app_val[app_val%%5 != 0] <- NA
            dat <- append(pdat,app_val)
            incProgress(amount = 0.30,message = "Processing")
            f <- forecast(auto.arima(cts,stepwise = FALSE,ic=c('aic'),test = c('adf'),seasonal = FALSE),h = Hvalue,bootstrap = TRUE,level = c(95))
            incProgress(amount = 0.60,message = "Finalising")
            lower <- append(lower,round(f$lower[1:Hvalue]))
            upper <- append(upper,round(f$upper[1:Hvalue]))
            pred <- append(pred,round(f$mean[1:Hvalue]))
            pred[13] <- dat[13]
            lower[lower<0] <- 0
            start <- 2001
      
            ds <- numeric()
            for(i in 1:(13+Hvalue)){
               ds <- append(ds,start)
               start = start +1
               }
            incProgress(amount = 1,message = "Done")
           })
            
            p <- plot_ly(x = ds, y = dat,type = 'scatter',mode='lines+markers',name = 'History')%>%add_trace(y = pred,type = 'scatter', mode = 'lines+markers',name='forecast')%>%add_trace(y = upper,type = 'scatter', mode = 'lines',line = list(color = 'transparent'),name= 'Upper Bound',showlegend=FALSE)%>%add_trace(y = lower,type = 'scatter', mode = 'lines',fill = 'tonexty',line = list(color = 'transparent'),name ='Lower Bound',showlegend=FALSE)%>%layout(title = paste("Forecast till the year",(2013+Hvalue)),
                     paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                     xaxis = list(title ="YEAR",
                                  gridcolor = 'rgb(255,255,255)',
                                  showgrid = TRUE,
                                  showline = FALSE,
                                  showticklabels = TRUE,
                                  tickcolor = 'rgb(127,127,127)',
                                  ticks = 'outside',
                                  zeroline = FALSE),
                     yaxis = list(title = "CRIME COUNT",
                                  gridcolor = 'rgb(255,255,255)',
                                  showgrid = TRUE,
                                  showline = FALSE,
                                  showticklabels = TRUE,
                                  tickcolor = 'rgb(127,127,127)',
                                  ticks = 'outside',
                                  zeroline = FALSE))
            
            
            
          })
          
        }
      )
      
   

      
    }#end of operations
    
  
) 

  
}#end of server

# Run the application 
shinyApp(ui = ui, server = server)

