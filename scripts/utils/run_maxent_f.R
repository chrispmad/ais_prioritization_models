run_maxent = function(species,
                      predictor_data,
                      vars_to_use = c(),
                      onedrive_path,
                      seed = 12345,
                      number_pseudoabsences = 5000,
                      number_folds = 5,
                      regularisation_levels = c(1:2),
                      feature_classes = c("L","LQ"),
                      habitat_threshold_var = "equal_training_sensitivity_and_specificity_cloglog_threshold",
                      output_folder = NULL
){
  
  if(is.null(output_folder)) output_folder = getwd()
  
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
  watercourses = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif")) 
  
  # If the user has specified a list of predictor variables to use, just keep those.
  if(!is.null(vars_to_use)){
    print(paste0("Constraining predictor raster variables to just: ",paste0(vars_to_use, collapse = ', ')))
    predictor_data = predictor_data[[c(vars_to_use)]]
  }
  
  cat("\nPulling predictor raster values for presence points...")
  
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
  
  # Make sure presences are distinct.
  presences = dplyr::distinct(presences)
  
  # Sample watercourses' locations for a collection of pseudoabsences; combine with data and then split into testing and training.
  pseudoabsences <- predicts::backgroundSample(watercourses, p = terra::vect(dat), n = number_pseudoabsences,
                                extf = 0.9) %>% 
    as.data.frame()
  
  # Make MaxEnt model
  cat("\nMaking MaxEnt Model...")

  # John and I followed this website as a guide: https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html#eval
  # Consult if necessary!
  # ENMeval method of running maxent. Great because this includes lots of 
  # parameter tests etc. inside the function call.
  me = ENMevaluate(occs = presences, 
                   envs = predictor_data_low_cor, 
                   bg = pseudoabsences, 
                   algorithm = 'maxent.jar', 
                   partitions = 'block', 
                   tune.args = list(fc = feature_classes, 
                                    rm = regularisation_levels))
  
  # Find which model had the lowest AIC; we'll use this for now.
  opt.aicc = eval.results(me) |> dplyr::filter(delta.AICc == 0)
  
  var_importance = me@variable.importance[[opt.aicc$tune.args]]
  
  predictions = terra::rast(eval.predictions(me)[[opt.aicc$tune.args]])
  
  eval_model<- eval.models(me)[[opt.aicc$tune.args]]
  
  eval_plot<-eval_model
  # Pull out maxent's predictions for occurrence locations.

  # Check out results - this dataframe could be simplified to just hone in 
  # on the particular metrics we are curious about!
  maxent_results = me@results |> 
    dplyr::filter(tune.args == opt.aicc$tune.args) |> 
    tidyr::as_tibble() |> 
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) as.character(x))) |> 
    tidyr::pivot_longer(cols = dplyr::everything()) |> 
    dplyr::add_row(name = "regularisation_levels_tested", value = paste0(regularisation_levels, collapse = ', ')) |> 
    dplyr::add_row(name = "feature_classes_tested", value = paste0(feature_classes, collapse = ', '))
  
  maxent_results.partitions = me@results.partitions |> 
    dplyr::filter(tune.args == opt.aicc$tune.args) |> 
    tidyr::as_tibble()
  
  maxent_html = me@models[[opt.aicc$tune.args]]@html
  
  single_model_metrics = me@models[[opt.aicc$tune.args]]@results[,1] |> 
    as.matrix() |> 
    as.data.frame()
  
  single_model_metrics = single_model_metrics |> 
    dplyr::mutate(metric = snakecase::to_snake_case(rownames(single_model_metrics))) |>
    dplyr::rename(value = V1) |>
    tidyr::as_tibble() |>
    dplyr::select(metric, value)
  
  key_metrics = single_model_metrics |>
    dplyr::filter(metric %in% c("x_training_samples","training_auc",habitat_threshold_var) | str_detect(metric,".*_contribution") | str_detect(metric,".*permutation_imp.*"))
  
  pres_sf = sf::st_as_sf(me@occs, coords = c("x","y"), crs = 4326)
  pres_sf$groups = me@occs.grp
  absences_sf = sf::st_as_sf(me@bg, coords = c("x","y"), crs = 4326)
  
  points_sf = dplyr::bind_rows(
    pres_sf |> dplyr::mutate(type = 'presence'), 
    absences_sf |> dplyr::mutate(type = 'pseudoabsence')
  )
  
  # Calculate some values to use as labels and captions in the figure.
  train_samp = key_metrics[key_metrics$metric == 'x_training_samples',]$value
  train_auc = maxent_results$auc.train
  
  metrics_caption = var_importance |> 
    dplyr::select(variable, percent.contribution) |> 
    dplyr::arrange(dplyr::desc(percent.contribution)) |> 
    dplyr::mutate(title_metric = stringr::str_replace_all(variable,"_"," ")) |>
    dplyr::rowwise() |>
    dplyr::summarise(v = paste0(stringr::str_to_title(title_metric),': ',round(percent.contribution,2),"%")) |>
    dplyr::ungroup() |>
    dplyr::summarise(paste0(v, collapse = '<br>'))
  
  predictions_plot = ggplot() + 
    tidyterra::geom_spatraster(data = predictions) + 
    geom_sf(data = points_sf, aes(col = type, alpha = type)) +
    scale_colour_manual(values = c('presence' = "red",
                                   'pseudoabsence' = "purple")) +
    scale_alpha_manual(values = c('presence' = 1,
                                  'pseudoabsence' = 0.1),
                       guide = 'none') +
    scale_fill_viridis_c() + 
    labs(title = paste0(stringr::str_to_title(species_name)),
         subtitle = paste0("Number of Training Data Points: ",train_samp,
                           "<br>Training Area-Under-Curve: ",round(as.numeric(train_auc),4)),
         caption = metrics_caption,
         fill = "Predicted \nRelative \nSuitability",
         color = "Sample Type") + 
    theme(
      plot.subtitle = ggtext::element_markdown(),
      plot.caption = ggtext::element_markdown()
    )
  
  # Predicted habitat vs. not habitat plot, using 
  # whichever threshold approach selected in function call.
  habitat_or_not = me@predictions[[opt.aicc$tune.args]]
  
  threshold_value = key_metrics |> 
    dplyr::filter(metric == habitat_threshold_var) |> 
    dplyr::pull(value)
  
  habitat_or_not[habitat_or_not < threshold_value] = FALSE
  habitat_or_not[habitat_or_not >= threshold_value] = TRUE
  
  fit = terra::extract(
    predictions,
    points_sf |>
    terra::vect()
    )
  
  names(fit)[2] = 'maxent'
  
  obs = terra::extract(
    predictions,
    points_sf |>
      dplyr::filter(type == "presence") |> 
      terra::vect()
  )
  
  # Check for output folder; if it exists already, delete contents.
  output_fn = paste0(output_folder,"/",species_name,"_results/")
  
  if(!dir.exists(output_fn)){
    dir.create(output_fn)
  } else {
    old_files = list.files(path = output_fn, full.names = T)
    cat("\nDeleting old contents of results folder...\n")
    old_files |> 
      lapply(\(x) file.remove(x))
  }
  
  file.copy(from = maxent_html, to = paste0(output_fn,"MaxEnt_results.html"))
  write.csv(key_metrics, paste0(output_fn,"MaxEnt_key_metrics.csv"), row.names = F)
  write.csv(maxent_results, paste0(output_fn,"MaxEnt_Detailed_Model_Fitting_results.csv", row.names = F))
  terra::writeRaster(predictions, paste0(output_fn,"MaxEnt_prediction_raster.tif"))
  terra::writeRaster(habitat_or_not, paste0(output_fn,"MaxEnt_prediction_habitat_or_not.tif"))
  ggplot2::ggsave(filename = paste0(output_fn,"MaxEnt_prediction_plot.png"),
                  plot = predictions_plot,
                  dpi = 300, width = 8, height = 8)

  cat("\nFiles written to output folder...\n")
  
  list(model_fit = me,
       maxent_results = maxent_results,
       key_metrics = key_metrics,
       predictions_r = predictions,
       predictions_plot = predictions_plot,
       eval_plot = eval_plot,
       habitat_predictions = habitat_or_not
  )
}
