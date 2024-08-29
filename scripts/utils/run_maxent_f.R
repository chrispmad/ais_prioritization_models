run_maxent = function(species,
                      predictor_data,
                      vars_to_use = c(),
                      onedrive_path,
                      seed = 12345,
                      number_pseudoabsences = 5000,
                      number_folds = 5,
                      habitat_threshold_var = "equal_training_sensitivity_and_specificity_cloglog_threshold"
                      
){
  
  if(!is.null(seed)) set.seed(seed)
  
  if(is.data.frame(species)){
    # User fed in dataframe of species occurrences.
    dat = species |> 
      dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                    y = sf::st_coordinates(geometry)[,2])
    species_name = dat$Species[1]
  } else {
    if(is.character(species)){
      # User fed in species common name. Search for records.
      dat = suppressMessages(grab_aq_occ_data(species))
      
      species_name = species
      
      dat = dat |> 
        dplyr::select(-c(DataSource,Date,Species,Location,iNat_user,iNat_report_id))
      
      dat$x = sf::st_coordinates(dat$geometry)[,1]
      dat$y = sf::st_coordinates(dat$geometry)[,2]
      
      dat = sf::st_drop_geometry(dat)
    }
  }
  
  if(is.null(predictor_data)){
    stop("No predictor data entered; please supply at least one {terra} spatRaster.")
  }
  
  # Make {terra} vector of BC.
  bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
    # Bring in watercourses to constrain randomly sampled pseudoabsences to biologically meaningful locations
  # for aquatic organisms.
  # watercourses = terra::rast(paste0(onedrive_path,"stream_density_6_plus_order_raster.tif")) |> 
  watercourses = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif")) |> 
    # terra::resample(predictor_data[[1]]) |> 
    terra::mask(bc_vect)
  
  # If the user has specified a list of predictor variables to use, just keep those.
  if(!is.null(vars_to_use)){
    print(paste0("Constraining predictor raster variables to just: ",paste0(vars_to_use, collapse = ', ')))
    predictor_data = predictor_data[[c(vars_to_use)]]
  }
  
  print("Pulling predictor raster values for presence points...")
  
  for(raster_var in unique(names(predictor_data))){
    dat[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                        dat[,c("x","y")], ID = FALSE)[[raster_var]]
  }
  
  dat_just_pred_vars = sf::st_drop_geometry(dat[,c(names(predictor_data))])
  
  # Remove samples lacking predictor raster values?
  keep_ind = complete.cases(dat_just_pred_vars)
  dat = dat[keep_ind,]
  
  # Test collinearity
  # pred_vals = dat[,c(names(predictor_data))]
  
  pairs(dat_just_pred_vars, lower.panel = panel.smooth2, upper.panel = panel.cor, diag.panel = panel.hist)
  
  cor_res = cor(dat_just_pred_vars) |> 
    as.data.frame()
  
  # Pull out highly correlated variables.
  highly_correlated_vars = cor_res |>   
    tidyr::as_tibble() |> 
    dplyr::mutate(var_2  = row.names(cor_res)) |> 
    # Gather table long so we have a column for each of the two-variable comparisons
    tidyr::pivot_longer(cols = -c(var_2)) |> 
    # Drop rows where a variable was being compared with itself
    dplyr::filter(var_2 != name) |> 
    # Filter by some arbitrary cut-off - what is 'too' correlated??
    dplyr::filter(abs(value) >= 0.8) |> 
    # Switch into 'rowwise' mode - this is like looping by row.
    dplyr::rowwise() |> 
    # Make a column that lists which two variables are being compared, 
    # sorted alphabetically.
    dplyr::mutate(variable_combo = list(c(var_2, name))) |>
    dplyr::mutate(variable_combo = paste0(variable_combo[order(variable_combo)], collapse = '-')) |> 
    # Exit 'rowwise' mode with an ungroup()
    dplyr::ungroup() |> 
    # Filter using that alphabetical var name column; this prevents duplication of results.
    dplyr::filter(duplicated(variable_combo))
  
  # predictor_data_low_cor = predictor_data[[names(dat)[-c(1,2)]]]
  predictor_data_low_cor = predictor_data
  
  # Pull out x and y coordinates for presences
  presences = sf::st_drop_geometry(dat[,c('x','y')])
  
  # Sample watercourses' locations for a collection of pseudoabsences; combine with data and then split into testing and training.
  pseudoabsences <-randomPoints(raster::raster(watercourses), n = number_pseudoabsences) %>% 
    as.data.frame()
  
  presabs = dplyr::bind_rows(
    presences,
    pseudoabsences
  )
  
  fold <- folds(presabs, k = number_folds)
  pa_test <- presabs[fold == 1, ]
  pa_train <- presabs[fold != 1, ]
  
  # Initialize list to store maxent model results.
  maxent_results_l = list()
  
  # Make MaxEnt model
  print("Making MaxEnt Model...")
  me <- MaxEnt(predictor_data_low_cor, p = presences, a = pseudoabsences)
  
  # Check out results - this dataframe could be simplified to just hone in 
  # on the particular metrics we are curious about!
  me_res = me@results |> 
    as.data.frame()
  
  me_res = me_res |> 
    dplyr::mutate(metric = snakecase::to_snake_case(rownames(me_res))) |> 
    dplyr::rename(value = V1) |> 
    tidyr::as_tibble() |> 
    dplyr::select(metric, value)
  
  key_metrics = me_res |> 
    dplyr::filter(metric %in% c("x_training_samples","training_auc",habitat_threshold_var) | str_detect(metric,".*_contribution") | str_detect(metric,".*permutation_imp.*"))
  
  # Use MaxEnt model to predict to entire dataset
  predictions <- predict(me, predictor_data_low_cor)
  
  pres_sf = sf::st_as_sf(presences, coords = c("x","y"), crs = 4326)
  absences_sf = sf::st_as_sf(pseudoabsences, coords = c("x","y"), crs = 4326)
  
  points_sf = dplyr::bind_rows(
    pres_sf |> dplyr::mutate(type = 'presence'), 
    absences_sf |> dplyr::mutate(type = 'pseudoabsence')
  )
  
  # Calculate some values to use as labels and captions in the figure.
  train_samp = key_metrics[key_metrics$metric == 'x_training_samples',]$value
  train_auc = key_metrics[key_metrics$metric == 'training_auc',]$value
  
  metrics_caption = key_metrics |> dplyr::filter(stringr::str_detect(metric,"contribution")) |>
    dplyr::arrange(dplyr::desc(value)) |> 
    dplyr::mutate(title_metric = stringr::str_replace_all(metric,"_"," ")) |> 
    dplyr::rowwise() |> 
    dplyr::summarise(v = paste0(stringr::str_to_title(title_metric),': ',round(value,2),"%")) |> 
    dplyr::ungroup() |> 
    dplyr::summarise(paste0(v, collapse = '<br>'))
  
  predictions_plot = ggplot() + 
    tidyterra::geom_spatraster(data = predictions) + 
    geom_sf(data = points_sf, aes(col = type, alpha = type)) +
    scale_colour_manual(values = c('presence' = "red",
                                   'pseudoabsence' = "purple")) +
    scale_alpha_manual(values = c('presence' = 1,
                                  'pseudoabsence' = 0.1)) +
    scale_fill_viridis_c() + 
    labs(title = paste0(stringr::str_to_title(species_name)),
         subtitle = paste0("Training Samples: ",train_samp,
                           "<br>",
                           "Training Area-Under-Curve: ",train_auc),
         caption = metrics_caption) + 
    theme(
      plot.subtitle = ggtext::element_markdown(),
      plot.caption = ggtext::element_markdown()
    )
  
  #simplest way to use 'evaluate'
  e1 <- pa_evaluate(me, p=pa_test, a=pseudoabsences, x=predictor_data_low_cor)
  
  e1
  
  plot(e1, 'ROC')
  
  # Predicted habitat vs. not habitat plot, using 
  # whichever threshold approach selected in function call.
  habitat_or_not = predictions
  
  threshold_value = key_metrics |> 
    dplyr::filter(metric == habitat_threshold_var) |> 
    dplyr::pull(value)
  
  habitat_or_not[habitat_or_not < threshold_value] = FALSE
  habitat_or_not[habitat_or_not >= threshold_value] = TRUE
  
  list(model_fit = me,
       key_metrics = key_metrics,
       predictions_r = predictions,
       predictions_plot = predictions_plot,
       evaluation_output = e1,
       habitat_predictions = habitat_or_not
  )
}
