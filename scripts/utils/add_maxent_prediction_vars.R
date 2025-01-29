add_maxent_prediction_vars = function(d,onedrive_wd,maxent_output_folder){
  # Snag the predictions_r object from each element of the list.
  d$wb_maxent_suitability_mean = 0
  d$wb_maxent_suitability_median = 0
  d$wb_maxent_suitability_max = 0
  d$wb_maxent_training_AUC = 0
  d$wb_maxent_suitability_fig = "need link"
  
  # Ensure we have a local copy of the MaxEnt outputs, fresh off the press!
  # file.copy(from = maxent_output_folder, to = paste0(onedrive_wd), recursive = T)
  
  for(i in 1:nrow(d)){
    
    print(i)
    the_species = d[i,]$Species
    the_species_snake = snakecase::to_snake_case(the_species)
    species_folder = paste0(onedrive_wd,"MaxEnt_predictions/",the_species_snake,"/")
    # Temporary fix for pumpkinseed sunfish... will have to figure this out.
    # species_folder = stringr::str_replace(species_folder,"pumpkinseed\\/","pumpkinseed_sunfish\\/")
    
    # Read maxent results in for species
    
    if(file.exists(paste0(species_folder,"MaxEnt_prediction_raster.tif"))){
      the_pred_r = terra::rast(paste0(species_folder,"MaxEnt_prediction_raster.tif"))
      
      if(!sf::st_is_empty(d[i,])){
        the_pred_about_wb = terra::crop(the_pred_r, sf::st_buffer(d[i,], 5000))
        
        the_pred_about_wb_poly = sf::st_as_sf(terra::as.polygons(the_pred_about_wb, round = FALSE))
        
        raster_var_name = names(the_pred_about_wb_poly)[1]
        
        # ggplot() +
        #   geom_sf(data = the_pred_about_wb_poly,aes(fill = !!rlang::sym(raster_var_name)),col = 'transparent') +
        #   geom_sf(data = d[i,], col = 'red', fill = 'transparent')
        
        # Do overlap.
        summary_values = the_pred_about_wb_poly |> 
          sf::st_filter(d[i,]) |> 
          sf::st_drop_geometry() |> 
          dplyr::summarise(mean_val = mean(!!rlang::sym(raster_var_name)),
                           median_val = median(!!rlang::sym(raster_var_name)),
                           max_val = max(!!rlang::sym(raster_var_name)))
        
        # Pull out average values for the waterbody.
        mean_pred_val = summary_values$mean_val
        median_pred_val = summary_values$median_val
        max_pred_val = summary_values$max_val
        
        d[i,]$wb_maxent_suitability_mean = round(mean_pred_val,3)
        d[i,]$wb_maxent_suitability_median = round(median_pred_val,3)
        d[i,]$wb_maxent_suitability_max = round(max_pred_val,3)
      }
    } else {
      warning(paste0("MaxEnt prediction raster does not exist for ",the_species,"; you should probably pause this and make sure to generate that MaxEnt raster first!"))
    }
    
    if(file.exists(paste0(species_folder,"MaxEnt_key_metrics.csv"))){
      maxent_key_metrics = read.csv(paste0(species_folder,"MaxEnt_key_metrics.csv"))
      
      d[i,]$wb_maxent_training_AUC = maxent_key_metrics[maxent_key_metrics$metric == "training_auc",]$value
    } else {
      warning(paste0("MaxEnt Key Metrics CSV file does not exist for ",the_species,"; you should probably pause this and make sure to generate that file first!"))
    }
    
    # the_wb = d[i,]
    # 
    # # Does the background image exist?
    # background_img_folder = paste0(species_folder,"background_images/",the_wb$Waterbody)
    # backdrop_path = paste0(background_img_folder,"/",the_wb$Waterbody,"/MaxEnt_prediction_plot_no_occ.jpg")
    # 
    # browser()
    # if(!dir.exists(background_img_folder)){
    #   dir.create(background_img_folder)
    # }
    # if(!file.exists(backdrop_path)){
    # 
    #   unaltered_backdrop_image = paste0(species_folder,"MaxEnt_prediction_plot_no_occ.jpg") 
    #   
    #   backdrop = tryCatch(
    #     expr = {
    #       ggplot() + 
    #         ggimage::geom_image(aes(x=1,y=1,image = unaltered_backdrop_image), size = 1) +
    #         # tidyterra::geom_spatraster(data = the_pred_r) +
    #         theme(axis.text = element_blank())
    #     },
    #     error = function(e) return(NULL)
    #   )
    #   
    #   inset = ggplot() + 
    #     tidyterra::geom_spatraster(data = the_pred_about_wb, aes(fill = !!rlang::sym(raster_var_name))) + 
    #     geom_sf(data = d[i,], col = 'red', fill = 'transparent') + 
    #     ggthemes::theme_map() + 
    #     scale_fill_viridis_c(limits = c(0,1)) +
    #     labs(fill = 'Predicted\nSuitability') + 
    #     coord_sf(expand = F) +
    #     theme(legend.position = 'none',
    #           plot.background = element_rect(fill = 'transparent', color = 'black'))
    #   
    #   combo_plot = tryCatch(
    #     expr = {
    #       backdrop + 
    #         patchwork::inset_element(inset, 
    #                                  left = 0.1,
    #                                  bottom = 0.15,
    #                                  right = 0.3,
    #                                  top = 0.5)
    #     }, 
    #     error = function(e) return(NULL)
    #   )
    #   
    #   try(
    #     ggsave(filename = paste0(species_folder,"MaxEnt_prediction_plot_no_occ_w_inset.jpg"),
    #            plot = combo_plot,
    #            dpi = 300,
    #            width = 8, height = 8)
    #   )
    #   
    #   d[i,]$wb_maxent_suitability_fig = paste0(
    #     "HYPERLINK(\"",
    #     paste0(species_folder,"MaxEnt_prediction_plot_no_occ_w_inset.jpg"),
    #     "\", \"",
    #     "LINK",
    #     "\")"
    #   )
    # }
  }
  return(d)
}
