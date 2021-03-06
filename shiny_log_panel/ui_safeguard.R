library(shiny)
library(ggplot2)
library(ncdf4)

#load regions
load('./data/regions_r.RData')

#file characteristics
filevarFN <- 'pr'
freq <- 'day'
#model <- 'NorESM1-M'
#expArray <- c('piControl','rcp85')
model <- 'CCSM3'
expArray <- c('1870control','i1400')

#get long names of frequency bands
vars_1 <- nc_open(paste0("./data/",list.files(path = './data/',pattern=paste0(filevarFN,'_',freq,'_',model,'_',expArray[[1]],'.*nc'))))
long.names <- character(vars_1$nvars)
#add ncdf variables to data frame
for (var_idx in (1:vars_1$nvars)) {
  long.names[var_idx] <- vars_1$var[[var_idx]]$longname
}

#Define UI 
shinyUI(pageWithSidebar(
  
  #Application title
  headerPanel(paste(model,freq,'precipitation')),
  
  #Sidebar which controls variables to plot
  sidebarPanel(
    selectInput("region_id","Region:",
                regions.df$Name),
    selectInput("graph_var","Frequency Band:",
                long.names)
  ),
  
  #show caption and plot
  mainPanel(
    plotOutput("log_plot")
  )
  
))