thin_occ<-function(data){
  
  spp_df<- data %>% 
    dplyr::mutate(x = st_coordinates(geometry)[,1],
                 y = st_coordinates(geometry)[,2]) %>% 
    sf::st_drop_geometry() %>% 
    #dplyr::select(x,y,Date,Species, Location, Data) %>% 
    mutate(present = 1)
    
    ## transform
  
  thinned_data_full<-spThin::thin(loc.data = spp_df,
                                  lat.col = "y", long.col = "x",
                                  spec.col = "Species",
                                  thin.par = 10,
                                  reps = 100,
                                  locs.thinned.list.return = T,
                                  write.files = F,
                                  write.log.file = F)

  max_rows_index <- which.max(sapply(thinned_data_full, nrow))
  most_rows_df <- thinned_data_full[[max_rows_index]]
  
  ## index match
  merged_rows<-data[match(row.names(data), row.names(most_rows_df)),]
  complete_rows<-merged_rows[!is.na(merged_rows$Species),]
  

  
  return(complete_rows)
}
