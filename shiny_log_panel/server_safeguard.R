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
freq <- 'day'
#model <- 'NorESM1-M'
model <- 'CCSM3'
if (model=='CCSM3') {expArray <- c('1870control','i1400')
} else {expArray <- c('piControl','rcp85')}

#load regions
load("./data/regions_r.RData")

#load files
vars_1 <- nc_open(paste0("./data/",list.files(path = './data/',pattern=paste0(filevarFN,'_',freq,'_',model,'_',expArray[[1]],'.*nc'))))
vars_2 <- nc_open(paste0("./data/",list.files(path = './data/',pattern=paste0(filevarFN,'_',freq,'_',model,'_',expArray[[2]],'.*nc'))))

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
  tmp <- log(as.vector(ncvar_get(vars_2,ncdf_vars[[var_idx]]))/as.vector(ncvar_get(vars_1,ncdf_vars[[var_idx]])))
  tmp[tmp<(axis_limits[1])] <- axis_limits[1]
  tmp[tmp>(axis_limits[2])] <- axis_limits[2]
  spectral_ratios[[gsub("-",".",ncdf_vars[[var_idx]])]] <- tmp
  #spectral_ratios[[gsub("-",".",ncdf_vars[[var_idx]])]] <- log(as.vector(ncvar_get(vars_2,ncdf_vars[[var_idx]]))/as.vector(ncvar_get(vars_1,ncdf_vars[[var_idx]])))
  long.names[var_idx] <- vars_1$var[[var_idx]]$longname
  
}

#Define server logic required to plot variables 
shinyServer(function(input,output) {
  
  #get region idxs
  idx_region <- reactive({
    tmp.region <- which(as.character(regions.df$Name) == as.character(input$region_id))
    clip.region(regions.df$ID[[tmp.region]],model,lat,lon)
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
      xlim(axis_limits[1],axis_limits[2]) + ylim(axis_limits[1],axis_limits[2]) +
      geom_abline(intercept=0,slope=1,linetype='dashed') +
      geom_hline(yintercept=0) + geom_vline(xintercept=0)
  })
  
})