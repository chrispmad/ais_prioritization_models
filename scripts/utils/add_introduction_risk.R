add_introduction_risk = function(d,lan_root){
  # Introduction risk!
  # intro_risk = terra::rast(paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/introduction_risk/introduction_risk.tif"))
  intro_risk_tifs = list.files(path = paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/introduction_risk"),
                               pattern = "introduction_risk_",
                               full.names = T) |> 
    lapply(\(x) terra::rast(x))
  
  names(intro_risk_tifs) = stringr::str_remove(
    stringr::str_remove(
      list.files(path = paste0(lan_root,"2 SCIENCE - Invasives/GENERAL/Budget/Canada Nature fund 2023-2026/Work Planning and modelling/MaxEnt_predictions/introduction_risk"),
                 pattern = "introduction_risk_",
                 full.names = F),
      ".tif"
    ),
    "introduction_risk_"
  )
  
  # Get average introduction risk for all pixels
  d$introduction_risk_mean = 0
  
  for(i in 1:nrow(d)){
    
    print(i)
    
    group_for_intro_risk = snakecase::to_snake_case(pr_sp[pr_sp$name == d[i,]$Species,]$group)
    
    # Pull out average values for the waterbody.
    mean_pred_val = terra::extract(intro_risk_tifs[[group_for_intro_risk]], terra::vect(d[i,]$geometry), 'mean', na.rm = T)
    # Is Median better??
    median_pred_val = terra::extract(intro_risk_tifs[[group_for_intro_risk]], terra::vect(d[i,]$geometry), 'median', na.rm = T)
    
    d[i,]$introduction_risk_mean = round(mean_pred_val[1,2],3)
  }
  return(d)
}
