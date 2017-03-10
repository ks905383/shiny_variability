library(shiny)
library(package.test)
library(ggplot2)

#file characteristics
filevarFN <- 'pr'
freq <- 'day'
model <- 'NorESM1-M'
expArray <- c('piControl','rcp85')

#load regions
load("~/Documents/R/regions_r.RData")

#load files
vars_1 <- nc_open(paste0("~/Documents/R/",list.files(path = '~/Documents/R/',pattern=paste0(filevarFN,'_',freq,'_',model,'_',expArray[[1]],'.*nc'))))
vars_2 <- nc_open(paste0("~/Documents/R/",list.files(path = '~/Documents/R/',pattern=paste0(filevarFN,'_',freq,'_',model,'_',expArray[[2]],'.*nc'))))

#get lat, lon
lat <- as.vector(ncvar_get(vars_1,'lat'))
lon <- as.vector(ncvar_get(vars_1,'lon'))

#initalize data frame
spectral_ratios <- data.frame(matrix(,nrow = length(ncvar_get(vars_2,'mean')),ncol = 0))
#get variable names
ncdf_vars <- names(vars_1$var)
long.names <- character(vars_1$nvars)
#add ncdf variables to data frame
for (var_idx in (1:vars_1$nvars)) {
  spectral_ratios[[gsub("-",".",ncdf_vars[[var_idx]])]] <- log(as.vector(ncvar_get(vars_2,ncdf_vars[[var_idx]]))/as.vector(ncvar_get(vars_1,ncdf_vars[[var_idx]])))
  long.names[var_idx] <- vars_1$var[[var_idx]]$longname
}

#Define server logic required to plot variables 
shinyServer(function(input,output) {
  
  #get region idxs
  idx_region <- reactive({
    tmp.region <- which(as.character(regions.df$Name) == as.character(input$region_id))
    clip.region(regions.df$ID[[tmp.region]],'model',lat,lon)
  })
  
  #get freq band to graph
  graph.var <- reactive({
    which(long.names == input$graph_var)
  })
  
  #Get output plot
  output$log_plot <- renderPlot({
    ggplot(spectral_ratios,
           aes_string(x="mean",y=gsub("-",".",ncdf_vars[[graph.var()]]))) +
      geom_point(size=0.5,color="grey") +
      geom_point(data=spectral_ratios[idx_region(),],
                 aes_string(x="mean",y=gsub("-",".",ncdf_vars[[graph.var()]])),
                 color="red",size=1) + 
      labs(x = "Mean",y = long.names[[graph.var()]]) +
      xlim(-1,1) + ylim(-1,1)
  })
  
})