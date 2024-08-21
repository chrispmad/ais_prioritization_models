run_maxent = function(species,
                      predictor_data = list()
){
  if(is.data.frame(species)){
    # User fed in dataframe of species occurrences.
    dat = species
  } else {
    if(is.character(species)){
      # User fed in species common name. Search for records.
      dat = suppressMessages(grab_aq_occ_data(species))
    }
  }
  
  if(length(predictor_data) == 0){
    stop("No predictor data entered; please supply at least one {terra} spatRaster.")
  }
  
  
  samp$temp = terra::extract(pred_bioc_clipped[[1]], samp[,c("x","y")], ID = FALSE)
  samp$precip = terra::extract(pred_bioc_clipped$Annual_Precipitation, samp[,c("x","y")], ID = FALSE)
  samp$calc = terra::extract(calc_clipped$calc, samp[,c("x","y")], ID = FALSE)
  samp$pH = terra::extract(ph_clipped, samp[,c("x","y")], ID = FALSE)
  samp$elev = terra::extract(elev, samp[,c("x","y")], ID = FALSE)
  samp = as.data.frame(samp)
  
  # Run the maxent model here
  
  # Clean up the results; export a list of results.
}