library(shiny)
library(ggplot2)
library(ncdf4)
#define function clip.region
clip.region <- function(region.id,model,lat,lon){
  library(R.matlab)
  #load regions data
  load('./data/regions_r.RData')
  
  #load surface-land fraction data
  sftlf_file <- readMat(paste0('./data/sftlf_',model,'.mat'))
  
  #Turn W / E longitude into single, east-counting coordinates (to support both 0 to 360 and -180 to 180 longitude standards)
  if (any(lon < 0)){
    lon[lon < 0] = 360 + lon[lon < 0];
  }
  
  region_set <- regions.df[regions.df$ID==region.id,]
  
  lat_idxs <- which(lat>=region_set$lat.min & lat<=region_set$lat.max)
  if (region_set$lon.max < region_set$lon.min){
    #If crosses prime meridian, make sure region wraps over correctly
    lon_idxs <- union(which(lon>=region_set$lon.min),which(lon<=region_set$lon.max))  
  } else {
    lon_idxs <- which(lon>=region_set$lon.min & lon<=region_set$lon.max)
  }
  
  #Change this to only do if vector lat/lon
  if (is.null(ncol(lat))) {
    idxs <- matrix(1,length(lat_idxs),length(lon_idxs))*((lat_idxs-1)*length(lon))
    idxs <- idxs + t(matrix(1,length(lon_idxs),length(lat_idxs))*lon_idxs)
  } else {}
  
  
  
  #Find land and ocean indices (defined as >50 land <50 ocean)
  if (region_set$Type == "LAND") {
    land_idxs <- which(sftlf_file$sftlf>50)
    idxs <- intersect(as.vector(idxs),land_idxs)
  } else if (region_set$Type == "OCEAN") {
    ocean_idxs <- which(sftlf_file$sftlf<=50)
    idxs <- intersect(as.vector(idxs),ocean_idxs)
  } else if (region_set$Type == "ALL") {
    idxs <- as.vector(t(idxs))
  }
  
  #output the idxs
  return(idxs)
  
}

#display settings
axis_limits <- c(-1,1)

#file characteristics
filevarFN <- 'pr'

#load regions
load("./data/regions_r.RData")

#Define server logic required to plot variables 
shinyServer(function(input,output) {
  #Set/update filenames (only when frequency or model is changed)
  filenames.load <- reactive({
    if (input$model_set=='CCSM3') {expArray <- c('1870control','i1400')
    } else {expArray <- c('piControl','rcp85')}
    freq <- switch(input$freq_set,
                   "day" = "day",
                   "month" = "Amon",
                   "year" = "yrr")
    filenames.load <- c(paste0("./data/",list.files(path = './data/',pattern=paste0(filevarFN,'_',freq,'_',input$model_set,'_',expArray[[1]],'.*nc'))),
                        paste0("./data/",list.files(path = './data/',pattern=paste0(filevarFN,'_',freq,'_',input$model_set,'_',expArray[[2]],'.*nc'))))
  })
  
  # #Get lat / lon if the filenames update
  # lat <- reactive({
  #   vars_1 <- nc_open(filenames.load()[1])
  #   lat <- as.vector(ncvar_get(vars_1,'lat'))
  # })
  # lon <- reactive({
  #   vars_1 <- nc_open(filenames.load()[1])
  #   lon <- as.vector(ncvar_get(vars_1,'lon'))
  # })
  
  #Set/update region idxs (only when region or model is changed)
  idx_region <- reactive({
    vars_1 <- nc_open(filenames.load()[1])
    lat <- as.vector(ncvar_get(vars_1,'lat'))
    lon <- as.vector(ncvar_get(vars_1,'lon'))
    tmp.region <- which(as.character(regions.df$Name) == as.character(input$region_id))
    clip.region(regions.df$ID[[tmp.region]],input$model_set,lat,lon)
  })

  
  #Set/update spectral ratios to plot and long names of frequency bands
  spec.ratios.data <- reactive({
    vars_1 <- nc_open(filenames.load()[1])
    vars_2 <- nc_open(filenames.load()[2])
    
    #initalize data frame
    spectral_ratios <- data.frame(matrix(,nrow = length(ncvar_get(vars_2,'mean')),ncol = 0))
    #get variable names
    ncdf_vars <- names(vars_1$var)
    long.names <- character(vars_1$nvars)
    #add ncdf variables to data frame
    for (var_idx in (1:vars_1$nvars)) {
      tmp <- log(as.vector(ncvar_get(vars_2,ncdf_vars[[var_idx]]))/as.vector(ncvar_get(vars_1,ncdf_vars[[var_idx]])))
      tmp[tmp<(axis_limits[1])] <- axis_limits[1]
      tmp[tmp>(axis_limits[2])] <- axis_limits[2]
      spectral_ratios[[gsub("-",".",ncdf_vars[[var_idx]])]] <- tmp
      long.names[var_idx] <- vars_1$var[[var_idx]]$longname
    }
    spec.ratios.data <- list("spectral_ratios"=spectral_ratios,
                          "long.names" = long.names,
                          "ncdf_vars" = ncdf_vars)
  })
  
  #Set/update spectral ratios to plot (whenever filenames change)
  output$ui_set <- renderUI({
    
    if (filenames.load()[1] != "./data/" & filenames.load()[2] != "./data/") { 
      # vars_1 <- nc_open(filenames.load()[1])
      # vars_2 <- nc_open(filenames.load()[2])
      # 
      # #get lat, lon
      # if (!exists('lat')) {lat <- as.vector(ncvar_get(vars_1,'lat'))}
      # if (!exists('lon')) {lon <- as.vector(ncvar_get(vars_1,'lon'))}
      # 
      # #initalize data frame
      # spectral_ratios <<- data.frame(matrix(,nrow = length(ncvar_get(vars_2,'mean')),ncol = 0))
      # #get variable names
      # ncdf_vars <<- names(vars_1$var)
      # long.names <<- character(vars_1$nvars)
      # #add ncdf variables to data frame
      # for (var_idx in (1:vars_1$nvars)) {
      #   tmp <- log(as.vector(ncvar_get(vars_2,ncdf_vars[[var_idx]]))/as.vector(ncvar_get(vars_1,ncdf_vars[[var_idx]])))
      #   tmp[tmp<(axis_limits[1])] <- axis_limits[1]
      #   tmp[tmp>(axis_limits[2])] <- axis_limits[2]
      #   spectral_ratios[[gsub("-",".",ncdf_vars[[var_idx]])]] <<- tmp
      #   long.names[var_idx] <<- vars_1$var[[var_idx]]$longname
      # }
      
      selectInput("graph_var","Frequency Band:",
                  spec.ratios.data()$long.names)
    } else {selectInput("graph_var","Frequency Band:",paste("Files for",filevarFN,input$model_set,freq,"don't exist for both",expArray[[1]]," and ",expArray[[2]]))}
  })
  
  #get freq band to graph
  graph.var <- reactive({
    which(spec.ratios.data()$long.names == input$graph_var)})
  
  #Get output plot
  output$log_plot <- renderPlot({
    ggplot(spec.ratios.data()$spectral_ratios,
           aes_string
           (x="mean",y=gsub("-",".",spec.ratios.data()$ncdf_vars[[graph.var()]]))) +
      geom_point(size=0.5,color="grey") +
      geom_point(data=spec.ratios.data()$spectral_ratios[idx_region(),],
                 aes_string(x="mean",y=gsub("-",".",spec.ratios.data()$ncdf_vars[[graph.var()]])),
                 color="red",size=1) + 
      labs(x = "Mean",y = spec.ratios.data()$long.names[[graph.var()]]) +
      xlim(axis_limits[1],axis_limits[2]) + ylim(axis_limits[1],axis_limits[2]) +
      geom_abline(intercept=0,slope=1,linetype='dashed') +
      geom_hline(yintercept=0) + geom_vline(xintercept=0)
  })
})