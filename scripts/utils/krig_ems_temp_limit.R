library(DBI)
library(RSQLite)
library(sf)
library(ggplot2)
library(dplyr)
library(bcmaps)
library(raster)
library(automap)
library(gstat)
library(stringr)
library(terra)
library(raster)
library(lubridate)

#krig_ems<-function(var_name, confidence_interval = 0.99){
  var_name<-"Temperature"
  Months<-c(2,3,4)
  confidence_interval<-0.99
  bc = bcmaps::bc_bound() |> 
    dplyr::summarise() |> 
    terra::vect()
  
  path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
  onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/CNF/")
  
  bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
  bc_vect_alb = terra::vect(sf::st_transform(bcmaps::bc_bound(),3005))
  
  conn<-dbConnect(RSQLite::SQLite(),"../EMS/output/EMS.sqlite")
  
  raw_data<-dbGetQuery(conn, paste0("select * from results where parameter like '%",var_name,"%'"))
  
  dbDisconnect(conn)
  
  raw_data<-raw_data[!is.na(raw_data$LATITUDE),]
  raw_data<-raw_data[!is.na(raw_data$LONGITUDE),]
  
  data_sf<-st_as_sf(raw_data, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
  data_sf$COLLECTION_DATE<-as.Date(data_sf$COLLECTION_DATE)
  
  results<-data_sf %>%
    dplyr::select(c(RESULT,COLLECTION_DATE,LOCATION_TYPE,LOCATION_PURPOSE,MONITORING_LOCATION, geometry)) %>% 
    dplyr::filter(!is.na(RESULT)) %>% 
    dplyr::filter(LOCATION_TYPE == "RIVER,STREAM OR CREEK" |
                    LOCATION_TYPE == "MONITORING WELL" |
                    LOCATION_TYPE == "LAKE OR POND")
  results_hold<-results
  
   results$season <- quarter(results$COLLECTION_DATE)
   results$season2<- factor(results$season, labels = c("Winter", "Spring", "Summer", "Autumn"))
  
   if(confidence_interval != 1){
    # Find confidence interval for values.
    conf_int_bounds <- t.test(results$RESULT, conf.level = confidence_interval)$conf.int[c(1:2)]
    
    # calculate first quantile
    Quantile1 <- quantile(results$RESULT, probs=1-confidence_interval)
    
    # calculate third quantile
    Quantile3 <- quantile(results$RESULT, probs=1-(1-confidence_interval))
    
    # calculate inter quartile range
    IQR = Quantile3-Quantile1
    
    outliers = results[results$RESULT > Quantile3 + (IQR*1.5) | results$RESULT < Quantile1 - (IQR*1.5),]
    
    # Tell user about the number of rows about to be dropped.
    cat(paste0("\nNote: ",nrow(outliers)," (",round(100*nrow(outliers)/nrow(results),3),
               "% of all records) outlier data points found outside of the 1st and 99th quantiles (",
               round(Quantile1,2)," and ",round(Quantile3,2),", respectively)... removed!"))
    
    # Find and drop outlier rows
    results = results[!(results$RESULT > Quantile3 + (IQR*1.5) | results$RESULT < Quantile1 - (IQR*1.5)),]
  }
  
  # Simplify our input data so that there is one point of data per 100km^2 raster cell.
  interp_grid = sf::st_make_grid(sf::st_as_sf(bc_vect_alb), cellsize = c(10000,10000)) |> 
    sf::st_as_sf() |> 
    sf::st_filter(sf::st_as_sf(bc_vect_alb))
  
  results_albers = sf::st_transform(results, 3005)
  
  results_albers_overlap = interp_grid |> 
    dplyr::mutate(row_id = row_number()) |> 
    sf::st_join(results_albers)
  
  
  for (i in season2) {
    
  }
  
  results_albers_overlap = results_albers_overlap |> 
    dplyr::group_by(season2, MONITORING_LOCATION) |> 
    #dplyr::group_by() |> 
    dplyr::mutate(medianVal = median(RESULT)) |> 
    dplyr::ungroup()# |> 
    #dplyr::filter(!duplicated(row_id))
  
  results_albers_as_centroids = results_albers_overlap |> 
    sf::st_centroid()
  

    ggplot() +
      geom_sf(data = results_albers_as_centroids, aes(fill = medianVal, 
                                                    col = medianVal))+
        facet_wrap(~ season2)
  
  #for turbidity
  #results_albers_as_centroids <- results_albers_as_centroids %>% filter(medianVal < 500)
  
  results_albers_as_centroids_no_na = results_albers_as_centroids |> dplyr::filter(!is.na(medianVal))
  
  toInterp<-st_transform(results_albers_as_centroids_no_na, 3005)
  
  tointerpPt<- toInterp %>% 
    dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                  lat = sf::st_coordinates(.)[,2])
  
  #create interpolation grid encompassing Canada and USA
  bbox <- sf::st_bbox(st_as_sf(bc))
  
  
  # Bring in raster for reference (dimensions, extent, etc.)
  # pred_bioc = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")
  bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
  # pred_bioc_clipped = mask(crop(pred_bioc, bc_vect), bc_vect)
  
  #terra::plot(pred_bioc_clipped$bio01)
  
  # ref = pred_bioc_clipped$bio01
  
  ref = terra::rast(paste0(onedrive_path, "reference_raster_wgs84.tif"))
  
  # ext_ref <- ext(ref)
  # res_ref <- res(ref)
  # x_seq <- seq(from = ext_ref[1], to = ext_ref[2], by = res_ref[1])
  # y_seq <- seq(from = ext_ref[3], to = ext_ref[4], by = res_ref[2])
  # grid10km <- expand.grid(x = x_seq, y = y_seq)
  # grid10km <- rast(grid10km, crs = crs(ref))
  # grid10km$nlyr<-1
  
  # grid10km = terra::resample(grid10km, ref)
  
  #grid10km<-raster(grid10km)
  # st_crs(results_albers_as_centroids_no_na)
  pointscrs<-st_transform(results_albers_as_centroids_no_na,4326)
  # st_crs(grid10km)
  # st_crs(pointscrs)
  
  pointscrs<-pointscrs[!is.na(pointscrs$season2),]
  
  
  KRgrid10km <- as(raster(ref), "SpatialGrid")
  
  data_list<-split(pointscrs, pointscrs$season2)
  
  library(purrr)
  library(snow)
  
  raster_list<-map(data_list, function(data){
    
    varKRVar <- autofitVariogram(medianVal ~ 1,
                                 as(data, "Spatial"),
                                 verbose=TRUE,
                                 fix.values = c(0,NA,NA))
    
    # Kriging
    KRvarmod <- gstat(formula=medianVal~1,
                      locations=as(data,"Spatial"),
                      model=varKRVar$var_model)
    beginCluster(n = 6)
    
    krig_rast<-clusterR(raster(ref), interpolate, args = list(KRvarmod))
    
    endCluster()
    spatRast<-rast(krig_rast)
    
    testrast<-crop(spatRast, bc_vect)
    maskrast<-mask(testrast, bc_vect)
    return(maskrast)
  })
  
  plot(raster_list$Winter)
  plot(raster_list$Summer)
  
  for (i in 1:length(raster_list)) {
    raster <- raster_list[[i]]
    nm_raster<-names(raster_list[i])
    raster_name <- paste0("raster_temp_", nm_raster, ".tif")  # Unique filename based on index
    writeRaster(raster, paste0("rasters/test/", raster_name), overwrite = TRUE)
  }
