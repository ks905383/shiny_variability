library(shiny)
library(ggplot2)
library(ncdf4)

#load regions
load('./data/regions_r.RData')

#get models
file_list<- list.files(path='./data/',pattern='pr.*nc')
file_chars <- sapply(lapply(file_list,strsplit,split='_'),'[[',1)
#browser()
model_list <<- unique(file_chars[3,])

#file characteristics
filevarFN <- 'pr'

#Define UI 
shinyUI(fluidPage(
  #Application title
  headerPanel("variability vs. mean change, precipitation"),
  
  #Main layout
  sidebarLayout( 
    
    #Panel to set frequency, model, region
    sidebarPanel(
      selectInput("freq_set","Frequency:",
                  c("month","day"),selected="month"),
      selectInput("model_set","Model:",
                  model_list,selected="CCSM3"),
      selectInput("region_id","Region:",
                  regions.df$Name)
    ),
    
    #Scatterplot
    mainPanel(
      uiOutput("ui_set"),
      plotOutput("log_plot"))
      
  )))