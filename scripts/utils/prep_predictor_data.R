prep_predictor_data = function(proj_path,
                               onedrive_path){
  
  # Pull in climate variables
  ph_NAM = terra::rast(paste0(onedrive_path,"ph-KR-208784-median_10km_ZN.tif")) |> terra::project("EPSG:4326")
  names(ph_NAM) <- "pH"
  Calc_NAM = terra::rast(paste0(onedrive_path,"calcium-KR-97648-median-10km-ZN.tif")) |> terra::project("EPSG:4326")
  names(Calc_NAM) <- "calc"
  
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
  pred_bioc = terra::rast(paste0(proj_path,"/CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif"))
  names(pred_bioc)<-renames
  
  # Cut our rasters down to just BC.
  bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
  ph_clipped = terra::mask(terra::crop(ph_NAM, bc_vect), bc_vect)
  calc_clipped = terra::mask(terra::crop(Calc_NAM, bc_vect), bc_vect)
  pred_bioc_clipped = terra::mask(terra::crop(pred_bioc, bc_vect), bc_vect)
  
  # Pull in elevation raster with {elevatr}
  elev = suppressMessages(elevatr::get_elev_raster(locations = sf::st_as_sf(bc_vect), z = 4)) |>
    terra::rast() |> 
    terra::crop(bc_vect) |> 
    terra::mask(bc_vect)
  names(elev) = "elev"
  
  NA_ph_res = terra::resample(ph_clipped, pred_bioc_clipped)
  NA_calc_res = terra::resample(calc_clipped, pred_bioc_clipped)
  
  elev_res = terra::resample(elev, pred_bioc_clipped)
  
  all_rasters = c(pred_bioc_clipped, NA_ph_res, NA_calc_res, elev_res)
  
  names(all_rasters)[c(1,12)] <- c("temp","precip")
  temps<-pred_bioc_clipped[[1]]
  precip<-pred_bioc_clipped[[2]]
  
  stack_data<-c(NA_ph_res, NA_calc_res, elev_res, temps, precip)
  names(stack_data)<-c("pH", "Calc", "elev", "temp", "precip")
  
  return(stack_data)
}
