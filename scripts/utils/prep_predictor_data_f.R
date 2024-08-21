prep_predictor_data = function(proj_path,
                               onedrive_path){
  
  print("Reading in rasters...")
  
  # Make {terra} vector of BC.
  bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
  
  # Pull in climate variables
  ph_NAM = terra::rast(paste0(onedrive_path,"ph-KR-208784-median_10km_ZN.tif")) |> terra::project("EPSG:4326")
  names(ph_NAM) <- "pH"
  Calc_NAM = terra::rast(paste0(onedrive_path,"calcium-KR-97648-median-10km-ZN.tif")) |> terra::project("EPSG:4326")
  names(Calc_NAM) <- "calc"
  
  # Pull in distance to road network
  roads = terra::rast(paste0(onedrive_path,"distance_to_road_raster.tif"))
  names(roads) <- "dist_to_highways"
  
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
  # pred_bioc = terra::rast(paste0(proj_path,"/CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif"))
  # names(pred_bioc)<-renames
  
  # Pull in elevation raster with {elevatr}
  elev = suppressMessages(elevatr::get_elev_raster(locations = sf::st_as_sf(bc_vect), z = 4)) |>
    terra::rast() |> 
    terra::crop(bc_vect) |> 
    terra::mask(bc_vect)
  names(elev) = "elev"
  
  print("Combining rasters...")
  
  rasters = list(cmidata$Annual_Mean_Temperature,
                 cmidata$Annual_Precipitation,
                 ph_NAM,Calc_NAM,roads,elev)
  
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
