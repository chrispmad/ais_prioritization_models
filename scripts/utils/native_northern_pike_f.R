remove_native_nPike<-function(x){
  bc_watersheds <- st_read(paste0(onedrive_wd,"CNF/watershed_groups.gpkg"), quiet = TRUE)
  bc_watersheds <- bc_watersheds |> st_transform(4326)
  pike_watersheds<- bc_watersheds |> 
    filter(str_detect(WATERSHED_GROUP_NAME, c("Peace|Liard|Yukon|Alsek|Taku|Makenzie")))
  bbox <- st_bbox(pike_watersheds)
  
  x <- x %>%
    mutate(X = st_coordinates(.)[,1],  # Extract longitude
           Y = st_coordinates(.)[,2]) %>%  # Extract latitude
    filter(!(Species == "Northern pike" & Y > bbox["ymin"] & X > bbox["xmin"])) %>%
    dplyr::select(-X, -Y)  # Remove temporary columns  
  rm(bc_watersheds, pike_watersheds, bbox)
  return(x)
}


