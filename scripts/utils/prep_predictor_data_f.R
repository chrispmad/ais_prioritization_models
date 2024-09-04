prep_predictor_data = function(proj_path,
                               onedrive_path){
  
  print("Reading in rasters...")
  
  # Make {terra} vector of BC.
  bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
  
  # Pull in climate variables
  ph_NAM = terra::rast(paste0(onedrive_path,"CNF/ph-KR-208784-median_10km_ZN.tif")) |> terra::project("EPSG:4326")
  names(ph_NAM) <- "pH"
  Calc_NAM = terra::rast(paste0(onedrive_path,"CNF/calcium-KR-97648-median-10km-ZN.tif")) |> terra::project("EPSG:4326")
  names(Calc_NAM) <- "calc"
  
  # Pull in distance to road network
  roads = terra::rast(paste0(onedrive_path,"CNF/distance_to_road_raster.tif"))
  names(roads) <- "dist_to_highways"
  
  # Pull in population density raster
  pop_dens = terra::rast(paste0(onedrive_path,"CNF/population_density_raster.tif"))
  names(pop_dens) = "population_density"
  
  # Pull in stream order (of streams with stream order 3+), 2km resolution.
  # stream_ord = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif"))
  # names(stream_ord) = "stream_order"
  # stream_ord[] = factor(terra::values())
  # Read in variable names for the worldclim variables.
  renames<-c("Annual Mean Temperature", 
             "Mean Diurnal Range (temp)", 
             "Isothermality", 
             "Temperature Seasonality", 
             "Max Temperature of Warmest Month", 
             "Min Temperature of Coldest Month", 
             "Temperature Annual Range", 
             "Mean Temperature of Wettest Quarter",
             "Mean Temperature of Driest Quarter", 
             "Mean Temperature of Warmest Quarter", 
             "Mean Temperature of Coldest Quarter", 
             "Annual Precipitation", 
             "Precipitation of Wettest Month",
             "Precipitation of Driest Month", 
             "Precipitation Seasonality", 
             "Precipitation of Wettest Quarter", 
             "Precipitation of Driest Quarter",
             "Precipitation of Warmest Quarter", 
             "Precipitation of Coldest Quarter")
  renames<-gsub(" ", "_", renames)
  
  cmidata<-geodata::cmip6_world("ACCESS-CM2", ssp = "585", var = "bioc", res = 5, time = "2021-2040",path= paste0(proj_path,"/CMI/"))
  names(cmidata)<-renames
  
  # Elevation
  elev = terra::rast(paste0(onedrive_path,"CNF/elevation_BC.tif"))
  
  names(elev) = "elev"
  
  # Bring in waterbody connectivity
  wb_conn = terra::rast(paste0(onedrive_path,"CNF/stream_order_three_plus_raster.tif"))
  
  # Bring in masked rasters from the 'raster/' data folder.
  interpolated_raster_filepaths = list.files(path = paste0(onedrive_path,"raster/"),
             pattern = ".*masked_krig",
             full.names = T)
  
  interpolated_rasts = interpolated_raster_filepaths |> 
    lapply(terra::rast)
  
  print("Combining rasters...")
  
  rasters = list(cmidata$Annual_Mean_Temperature,
                 cmidata$Annual_Precipitation,
                 ph_NAM,Calc_NAM,roads,elev,pop_dens)
  
  rasters = append(rasters, interpolated_rasts)
  
  # Cut our rasters down to just BC.
  rasters = rasters |> 
    lapply(\(x) {
      terra::mask(terra::crop(x, bc_vect), bc_vect)
    })
  
  # Resample to ensure same resolution as bioclim variables.
  rasters = rasters |> 
    lapply(\(x) {
      terra::resample(x, rasters[[1]])
    })
  
  rasters_c = Reduce(x = rasters, f = 'c')
  
  return(rasters_c)
}
