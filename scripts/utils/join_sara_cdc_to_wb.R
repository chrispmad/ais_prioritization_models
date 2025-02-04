join_sara_cdc_to_wb = function(i, d, the_wb,ds_wb,wbs_overlap_sara_cdc,sara_sp,cosewic_risk_status_sp,previous_results){
  # Join on SARA and CDC that overlapped with the target waterbody.
  
  the_wb = the_wb |> 
    dplyr::left_join(
      wbs_overlap_sara_cdc |> 
        sf::st_drop_geometry() |> 
        dplyr::select(Waterbody = GNIS_NAME_,
                      Region,
                      FWA_WATERSHED_CODE = FWA_WAT,
                      starts_with("sara_"),
                      starts_with("cdc")),
      join_by(Waterbody, Region, FWA_WATERSHED_CODE)
    ) |> 
    dplyr::distinct()
  
  # Do the same for the downstream waterbody.
  if(nrow(ds_wb) > 0){
    if("Region" %in% names(ds_wb)){
      ds_wb = ds_wb |> 
        dplyr::left_join(
          wbs_overlap_sara_cdc |> 
            sf::st_drop_geometry() |> 
            dplyr::select(Waterbody = GNIS_NAME_,
                          Region,
                          FWA_WATERSHED_CODE = FWA_WAT,
                          starts_with("sara_"),
                          starts_with("cdc")),
          by = join_by(Waterbody, Region, FWA_WATERSHED_CODE)
        )
    } else {
      ds_wb = ds_wb |> 
        dplyr::left_join(
          wbs_overlap_sara_cdc |> 
            sf::st_drop_geometry() |> 
            dplyr::select(Waterbody = GNIS_NAME_,
                          FWA_WATERSHED_CODE = FWA_WAT,
                          starts_with("sara_"),
                          starts_with("cdc")),
          by = join_by(Waterbody, FWA_WATERSHED_CODE)
        )
    }
  }
  # sara_polys_in_wb = sara |> sf::st_filter(the_wb)
  # sara_polys_in_ds_wb = sara |> sf::st_filter(ds_wb)
  # cdc_polys_in_wb = cdc_f |> sf::st_filter(the_wb)
  # cdc_polys_in_ds_wb = cdc_f |> sf::st_filter(ds_wb)
  
  # Look up all species present in the waterbody, to find COSEWIC species
  # and native species. This looks only for taxa labelled "fishes","Actinopterygii", and "Mollusca",
  # but you can add more taxa if desired.
  species_in_wb = tryCatch(
    expr = bcinvadeR::find_all_species_in_waterbody(wb = sf::st_transform(the_wb,3005)),
    error = function(e) return(data.frame(DataSource = "",Date = "", Species = "", Location = "", geometry = 0)[0,])
  )
  # Same for downstream waterbody, unless there is none, as in the case of the Thompson-Okanagan string of lakes, which
  # we are treating as a single, connected unit.
  if(nrow(ds_wb) == 0){
    species_in_ds_wb = c()
  } else {
    species_in_ds_wb = tryCatch(
      expr = bcinvadeR::find_all_species_in_waterbody(wb = sf::st_transform(ds_wb,3005)),
      error = function(e) return(data.frame(DataSource = "",Date = "", Species = "", Location = "", geometry = 0)[0,])
    )
  }
  
  # Find all species present.
  unique_species_in_wb = unique(species_in_wb$Species)
  unique_species_in_ds_wb = unique(species_in_ds_wb$Species)
  
  # Clean up this vector a bit: delete anything in parentheses. Any other steps?
  unique_species_in_wb = stringr::str_squish(stringr::str_remove_all(unique_species_in_wb, " (\\(.*\\)|\\-.*)"))
  unique_species_in_ds_wb = stringr::str_squish(stringr::str_remove_all(unique_species_in_ds_wb, " (\\(.*\\)|\\-.*)"))
  
  # Drop empty elements.
  unique_species_in_wb = unique_species_in_wb[unique_species_in_wb != ""]
  unique_species_in_ds_wb = unique_species_in_ds_wb[unique_species_in_ds_wb != ""]
  
  # Find list of native species, aquatic invasive species, COSEWIC, SARA listed species.
  native_sp_in_wb = unique(unique_species_in_wb[!unique_species_in_wb %in% str_to_title(pr_sp$name)])
  ais_in_wb = unique(unique_species_in_wb[unique_species_in_wb %in% str_to_title(pr_sp$name)])
  cosewic_sp_in_wb = unique(unique_species_in_wb[unique_species_in_wb %in% cosewic_risk_status_sp])
  sara_sp_in_wb = unique(unique_species_in_wb[unique_species_in_wb %in% sara_sp])
  
  # if(i == 7) browser()
  # Remove slugs from native species; they're kind of not a fish?
  native_sp_in_wb = native_sp_in_wb[!stringr::str_detect(native_sp_in_wb, "Slug$")]
  
  # Remove things from cosewic that are present in sara.
  cosewic_sp_in_wb = cosewic_sp_in_wb[!cosewic_sp_in_wb %in% sara_sp_in_wb]
  
  # Same for downstream.
  # native_sp_in_ds_wb = unique(unique_species_in_ds_wb[!unique_species_in_ds_wb %in% str_to_title(pr_sp$name)]
  # ais_in_ds_wb = unique_species_in_ds_wb[unique_species_in_ds_wb %in% str_to_title(pr_sp$name)]
  cosewic_sp_in_ds_wb = unique(unique_species_in_ds_wb[unique_species_in_ds_wb %in% cosewic_risk_status_sp])
  sara_sp_in_ds_wb = unique(unique_species_in_ds_wb[unique_species_in_ds_wb %in% sara_sp])
  
  cosewic_sp_in_ds_wb = cosewic_sp_in_ds_wb[!cosewic_sp_in_ds_wb %in% sara_sp_in_ds_wb]
  
  # Now find all rows in our dataframe that have this waterbody as their target.
  # For each one, count up the above species groupings.
  for(y in 1:nrow(d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,])){
    # Ensure this gets applied to the right rows in d.
    
    print(paste0("current value of y:",y))
    
    the_species = d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$Species
    
    # Add on AIS and Native species
    other_ais_in_wb_info = unique(ais_in_wb[ais_in_wb != stringr::str_to_title(d[i,]$Species)])
    
    if(d[i,]$Species == "Rosy red fathead minnow"){
      other_ais_in_wb_info = other_ais_in_wb_info[other_ais_in_wb_info != "Fathead Minnow"]
    }
    
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$other_ais_in_wb = length(other_ais_in_wb_info)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$other_ais_in_wb_names = ifelse(length(other_ais_in_wb_info) > 0, paste0(other_ais_in_wb_info, collapse = ", "), NA)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$native_species_in_wb = length(unique(native_sp_in_wb))
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$native_species_in_wb_names = ifelse(length(native_sp_in_wb) > 0, paste0(native_sp_in_wb, collapse = ", "), NA)
    
    # Combine sara spatial overlaps with point data that is of a SARA-listed species name.
    sara_combo = unique(na.omit(c(unique(sara_sp_in_wb), unique(the_wb$sara_common_name))))
    sara_combo_ds = unique(na.omit(c(unique(sara_sp_in_ds_wb),unique(ds_wb$sara_common_name))))
    
    # CDC names. Make sure SARA and COSEWIC names aren't present here.
    # Drop anything in parentheses for cdc polys.
    # cdc_polys_in_wb = cdc_polys_in_wb |> dplyr::mutate(ENG_NAME = stringr::str_remove_all(ENG_NAME," \\(.*\\)"))
    # cdc_polys_in_ds_wb = cdc_polys_in_ds_wb |> dplyr::mutate(ENG_NAME = stringr::str_remove_all(ENG_NAME," \\(.*\\)"))
    
    # cdc_in_wb = unique(cdc_polys_in_wb$ENG_NAME)
    cdc_in_wb = unique(na.omit(the_wb$cdc_common_name))
    cdc_in_wb = cdc_in_wb[!cdc_in_wb %in% sara_sp]
    cdc_in_wb = cdc_in_wb[cdc_in_wb != "NA"]
    cdc_in_wb = cdc_in_wb[!cdc_in_wb %in% cosewic_risk_status_sp]
    cdc_in_ds_wb = unique(na.omit(ds_wb$cdc_common_name))
    cdc_in_ds_wb = cdc_in_ds_wb[!cdc_in_ds_wb %in% sara_sp]
    cdc_in_ds_wb = cdc_in_ds_wb[cdc_in_ds_wb != "NA"]
    cdc_in_ds_wb = cdc_in_ds_wb[!cdc_in_ds_wb %in% cosewic_risk_status_sp]
    
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_in_wb = length(unique(sara_combo))
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_in_wb_names = ifelse(length(sara_combo) > 0, paste0(unique(na.omit(sara_combo)), collapse = ", "), "none")
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_downstream = length(unique(sara_combo_ds))
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$sara_downstream_names = ifelse(length(sara_combo_ds) > 0, paste0(unique(na.omit(sara_combo_ds)), collapse = ", "), "none")
    
    # COSEWIC species
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_in_wb = length(cosewic_sp_in_wb)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_in_wb_names = ifelse(length(cosewic_sp_in_wb) > 0, paste0(unique(na.omit(cosewic_sp_in_wb)), collapse = ", "), "none")
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_downstream = length(cosewic_sp_in_ds_wb)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$COSEWIC_downstream_names = ifelse(length(cosewic_sp_in_ds_wb) > 0, paste0(unique(na.omit(cosewic_sp_in_ds_wb)), collapse = ", "), NA)
    
    # CDC species
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_in_wb = length(cdc_in_wb)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_in_wb_names = ifelse(length(cdc_in_wb) > 0, paste0(unique(na.omit(cdc_in_wb)), collapse = ", "), "none")
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_downstream = length(cdc_in_ds_wb)
    d[d$Waterbody == the_wb$Waterbody & d$Region == the_wb$Region,][y,]$cdc_listed_downstream_names = ifelse(length(cdc_in_ds_wb) > 0, paste0(unique(na.omit(cdc_in_ds_wb)), collapse = ", "), "none")
    
    
    # Now do the same thing to update the previous_results file!
    if(nrow(previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,]) >= y){
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$other_ais_in_wb = length(other_ais_in_wb_info)
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$other_ais_in_wb_names = ifelse(length(other_ais_in_wb_info) > 0, paste0(other_ais_in_wb_info, collapse = ", "), NA)
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$native_species_in_wb = length(unique(native_sp_in_wb))
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$native_species_in_wb_names = ifelse(length(native_sp_in_wb) > 0, paste0(native_sp_in_wb, collapse = ", "), NA)
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$sara_in_wb = length(unique(sara_combo))
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$sara_in_wb_names = ifelse(length(sara_combo) > 0, paste0(unique(na.omit(sara_combo)), collapse = ", "), "none")
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$sara_downstream = length(unique(sara_combo_ds))
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$sara_downstream_names = ifelse(length(sara_combo_ds) > 0, paste0(unique(na.omit(sara_combo_ds)), collapse = ", "), "none")
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$COSEWIC_in_wb = length(cosewic_sp_in_wb)
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$COSEWIC_in_wb_names = ifelse(length(cosewic_sp_in_wb) > 0, paste0(unique(na.omit(cosewic_sp_in_wb)), collapse = ", "), "none")
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$COSEWIC_downstream = length(cosewic_sp_in_ds_wb)
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$COSEWIC_downstream_names = ifelse(length(cosewic_sp_in_ds_wb) > 0, paste0(unique(na.omit(cosewic_sp_in_ds_wb)), collapse = ", "), NA)
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$cdc_listed_in_wb = length(cdc_in_wb)
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$cdc_listed_in_wb_names = ifelse(length(cdc_in_wb) > 0, paste0(unique(na.omit(cdc_in_wb)), collapse = ", "), "none")
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$cdc_listed_downstream = length(cdc_in_ds_wb)
      previous_results[previous_results$Waterbody == the_wb$Waterbody & previous_results$Region == the_wb$Region,][y,]$cdc_listed_downstream_names = ifelse(length(cdc_in_ds_wb) > 0, paste0(unique(na.omit(cdc_in_ds_wb)), collapse = ", "), "none")
    }
    
    # Save this new row to our query results!
    saveRDS(previous_results |> 
              dplyr::group_by(Region,Waterbody,Species) |> 
              dplyr::arrange(dplyr::desc(query_date)) |> 
              dplyr::slice(1) |> 
              dplyr::ungroup(), 
            file = paste0(onedrive_wd,"AIS_previous_query_results.rds"))
  }
  print("saved new waterbody row to the previous results RDS file")
  return(d)
}
