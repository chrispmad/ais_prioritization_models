summarise_columns_and_produce_excel_output_file = function(dat,output_folder,maxent_output_folder){
  
  d_bins = d |> 
    sf::st_drop_geometry() |> 
    rowwise() |> 
    # dplyr::mutate(records_in_wb_b = dplyr::case_when(
    #   records_in_wb / nrow(occ_species[occ_species$Species == Species,]) <= 0.33 ~ 1,
    #   records_in_wb / nrow(occ_species[occ_species$Species == Species,]) <= 0.66 ~ 2,
    #   records_in_wb / nrow(occ_species[occ_species$Species == Species,]) > 0.66 ~ 3,
    #   T ~ 0
    # )) |> 
    dplyr::mutate(number_inflows_b = dplyr::case_when(
      number_inflows <= 5 ~ 1,
      number_inflows <= 25 ~ 2,
      number_inflows > 25 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(other_ais_in_wb_b = dplyr::case_when(
      other_ais_in_wb <= 3 ~ 0,
      other_ais_in_wb <= 6 ~ -1,
      other_ais_in_wb > 6 ~ -2,
      T ~ 0
    )) |> 
    dplyr::mutate(other_ais_in_wb_b = dplyr::case_when(
      native_species_in_wb <= 3 ~ 0,
      native_species_in_wb <= 6 ~ -1,
      native_species_in_wb > 6 ~ -2,
      T ~ 0
    )) |> 
    dplyr::mutate(sara_in_wb_b = dplyr::case_when(
      sara_in_wb == 0 ~ 0,
      sara_in_wb == 1 ~ 1,
      sara_in_wb == 2 ~ 2,
      sara_in_wb >= 3 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(sara_downstream_b = dplyr::case_when(
      sara_downstream == 0 ~ 0,
      sara_downstream == 1 ~ 1,
      sara_downstream == 2 ~ 2,
      sara_downstream >= 3 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(COSEWIC_in_wb_b = dplyr::case_when(
      COSEWIC_in_wb == 0 ~ 0,
      COSEWIC_in_wb == 1 ~ 1,
      COSEWIC_in_wb == 2 ~ 2,
      COSEWIC_in_wb >= 3 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(COSEWIC_downstream_b = dplyr::case_when(
      COSEWIC_downstream == 0 ~ 0,
      COSEWIC_downstream == 1 ~ 1,
      COSEWIC_downstream == 2 ~ 2,
      COSEWIC_downstream >= 3 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(cdc_listed_in_wb_b = dplyr::case_when(
      cdc_listed_in_wb == 0 ~ 0,
      cdc_listed_in_wb == 1 ~ 1,
      cdc_listed_in_wb == 2 ~ 2,
      cdc_listed_in_wb >= 3 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(cdc_listed_downstream_b = dplyr::case_when(
      cdc_listed_downstream == 0 ~ 0,
      cdc_listed_downstream == 1 ~ 1,
      cdc_listed_downstream == 2 ~ 2,
      cdc_listed_downstream >= 3 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(maxent_suitability_max_b = dplyr::case_when(
      wb_maxent_suitability_max <= 0.33 ~ 1,
      wb_maxent_suitability_max <= 0.66 ~ 2,
      wb_maxent_suitability_max > 0.66 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(maxent_suitability_mean_b = dplyr::case_when(
      wb_maxent_suitability_mean <= 0.33 ~ 1,
      wb_maxent_suitability_mean <= 0.66 ~ 2,
      wb_maxent_suitability_mean > 0.66 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(m_suit_uncertainty_b = dplyr::case_when(
      wb_maxent_training_AUC >= 0.9 ~ 1,
      wb_maxent_training_AUC >= 0.8 ~ 2,
      wb_maxent_training_AUC < 0.8 ~ 3,
      T ~ 0
    )) |> 
    dplyr::mutate(introduction_risk_b = dplyr::case_when(
      introduction_risk_mean < 0.5 ~ 1,
      introduction_risk_mean >= 0.5 & introduction_risk_mean < 0.7 ~ 2,
      introduction_risk_mean >= 0.7 ~ 3,
      T ~ 0
    )) |>
    # dplyr::mutate(oldest_record_b = round(1/log(as.numeric(stringr::str_extract(Sys.Date(),'^[0-9]{4}')) - oldest_record + 2.71),3)) |> 
    ungroup()
  
  # Now split variables into their groupings:
  # 1. Introduction (GLM result, number inflows, number outflows)
  # 2. Habitat Suitability (MaxEnt prediction)
  # 3. Consequences (presence of SARA, COSEWIC, or CDC listed species either in waterbody
  # or in downstream waterbody)
  
  other_columns = d |> 
    sf::st_drop_geometry() |> 
    dplyr::select(Region:Established_in_Waterbody, new_to_waterbody, oldest_record,first_nations_cons_area_overlapped, 
                  wildlife_habitat_areas, wildlife_habitat_areas_hectares, other_ais_in_wb, other_ais_in_wb_names) |> 
    dplyr::mutate(oldest_record_b = round(1/log(as.numeric(stringr::str_extract(Sys.Date(),'^[0-9]{4}')) - oldest_record + 2.71),3))
  
  intro = d_bins |> 
    dplyr::select(Region:Established_in_Waterbody, number_inflows_b, introduction_risk_b)
  
  hab_suit = d_bins |> 
    dplyr::select(Region:Established_in_Waterbody, wb_maxent_suitability_max, wb_maxent_suitability_mean, wb_maxent_training_AUC, maxent_suitability_max_b, maxent_suitability_mean_b, m_suit_uncertainty_b, wb_maxent_suitability_fig)
  
  conseq = d_bins |> 
    dplyr::select(Region:Established_in_Waterbody, sara_in_wb:native_species_in_wb_names, other_ais_in_wb_b:cdc_listed_downstream_b)
  
  dat_l = list(other_columns, intro, hab_suit, conseq)
  names(dat_l) = c('other_columns', 'intro', 'hab_suit', 'conseq') 
  
  results = purrr::map2(dat_l,names(dat_l), ~ {
    
    binned_cols = names(.x)[stringr::str_detect(names(.x), "_b$")]
    all_other_cols = names(.x)[!names(.x) %in% binned_cols]
    number_cols = length(binned_cols)
    
    if(number_cols > 0){
      result = .x |> 
        tidyr::pivot_longer(cols = -all_other_cols) |> 
        dplyr::group_by(Region,Species,Waterbody) |> 
        dplyr::mutate(!!rlang::sym(paste0(.y,"_total")) := sum(value,na.rm=T) / number_cols) |> 
        dplyr::ungroup() |> 
        tidyr::pivot_wider()
      
      result = result |> dplyr::select(names(result)[names(result) != paste0(.y,"_total")], paste0(.y,"_total"))
    } else {
      result = .x
    }
  }) |> 
    purrr::reduce(dplyr::left_join)
  
  # Modify columns that will be hyperlinks
  for(i in 1:nrow(results)){
    
    the_species = results[i,]$Species
    the_species_snake = snakecase::to_snake_case(the_species)
    species_folder = paste0(maxent_output_folder,the_species_snake,"/")
    # Temporary fix for pumpkinseed sunfish... will have to figure this out.
    species_folder = stringr::str_replace(species_folder,"pumpkinseed\\/","pumpkinseed_sunfish\\/")
    
    # Link Species column to MaxEnt folder.
    results[i,]$Species = paste0(
      "HYPERLINK(\"",
      species_folder,
      "\", \"",
      results[i,]$Species,
      "\")"
    )
  }
  
  # # Add an overall summary column
  # results$priority_b = results$intro_total + results$hab_suit_total + results$conseq_total
  results$priority_b = round(results$intro_total/2 + results$hab_suit_total + results$conseq_total, 2)
  
  # Shuffle column order a bit.
  results = results |> 
    dplyr::select(Region:wildlife_habitat_areas_hectares,oldest_record_b:conseq_total,
                  other_ais_in_wb,other_ais_in_wb_b,other_ais_in_wb_names,priority_b)
  
  # # Make column names nicer.
  # results = results |> 
  #   dplyr::rename(
  #     # `Total Records` = records_in_wb,
  #     `Other AIS in WB` = other_ais_in_wb,
  #     `Other AIS in WB names` = other_ais_in_wb_names,
  #     `New to Waterbody` = new_to_waterbody,
  #     `Oldest Record` = oldest_record,
  #     `Distinct SARA in WB` = sara_in_wb,
  #     `Distinct SARA in WB names` = sara_in_wb_names,
  #     `CDC-listed species in WB` = cdc_listed_in_wb,
  #     `CDC-listed species in WB names` = cdc_listed_in_wb_names,
  #     `MaxEnt Habitat Suitability` = wb_maxent_suitability,
  #     `MaxEnt Model Performance` = wb_maxent_training_AUC,
  #     `First Nations Consultation Areas` = first_nations_cons_area_overlapped
  #   )
  # =========================================
  
  # specify column as formula per openxlsx::writeFormula option #2
  class(results$Species) <- "formula"
  class(results$wb_maxent_suitability_fig) <- "formula"
  
  # Create excel workbook
  my_wb = createWorkbook()
  
  # Add sheet(s)
  openxlsx::addWorksheet(my_wb, "model")
  
  # Add data to worksheet.
  openxlsx::writeData(my_wb, "model", results)
  
  red_text = openxlsx::createStyle(fontColour = 'red', fontSize = 14, borderColour = '#bdf0e6')
  blue_text = openxlsx::createStyle(fontColour = 'blue', fontSize = 12)
  
  green_fill = openxlsx::createStyle(fgFill = "#abe0b9")
  yellow_fill = openxlsx::createStyle(fgFill = "#ede695")
  purple_fill = openxlsx::createStyle(fgFill = "#d96aca")
  
  openxlsx::addStyle(my_wb, "model", style = red_text, rows = (2:(1+nrow(results))), cols = which(names(results)=="priority_b"))
  openxlsx::addStyle(my_wb, "model", style = blue_text, rows = (2:(1+nrow(results))), cols = which(names(results)=="Species"))
  openxlsx::addStyle(my_wb, "model", style = blue_text, rows = (2:(1+nrow(results))), cols = which(names(results)=="wb_maxent_suitability_fig"))
  
  c(names(intro)[-c(1:3)],'intro_total') |>
    lapply(\(x) openxlsx::addStyle(my_wb, "model", style = green_fill, rows = 1:(1+nrow(results)), cols = c(which(names(results) == x))))
  
  c(names(hab_suit)[-c(1:3)],'hab_suit_total') |>
    lapply(\(x) openxlsx::addStyle(my_wb, "model", style = yellow_fill, rows = 1:(1+nrow(results)), cols = c(which(names(results) == x))))
  
  c(names(conseq)[-c(1:3)],'conseq_total') |>
    lapply(\(x) openxlsx::addStyle(my_wb, "model", style = purple_fill, rows = 1:(1+nrow(results)), cols = c(which(names(results) == x))))
  
  openxlsx::setColWidths(my_wb, "model", cols = 1:ncol(results), widths = "auto")
  openxlsx::setColWidths(my_wb, "model", cols = which(names(results) == "Species"), widths = 20)
  openxlsx::setColWidths(my_wb, "model", cols = which(names(results) == "first_nations_cons_area_overlapped"), widths = 30)
  openxlsx::setColWidths(my_wb, "model", cols = which(names(results) == 'other_ais_in_wb_names'), widths = 20)
  openxlsx::setColWidths(my_wb, "model", cols = which(names(results) == 'native_species_in_wb_names'), widths = 30)
  
  # Add metadata.
  openxlsx::addWorksheet(my_wb, "metadata")
  
  metadata = tibble(
    variable = c("New to Waterbody",
                 "First Nations Consultation Areas",
                 "Oldest Record Bin",
                 "Other AIS in WB",
                 "MaxEnt Habitat Suitability",
                 "MaxEnt Model Performance"),
    notes = c("Is the oldest occurrence record for this species in this waterbody within the last six months (this time range updates each time this R script is re-run)",
              "The First Nations PIP Consultation Areas that are within 10 kilometers of the waterbody",
              "Records from this year receive a level of 1, while records from previous years receive an expoentially smaller priority number. 
            To calculate this, we take the reciprocal of the log of the current year minus the oldest record in the waterbody plus 2.71",
              "The number of other aquatic invasive species present in the waterbody, according to our occurrence records and iNaturalist. If this number is 0 to 3, it does not affect the priority ranking; 
            if it is 4 to 6, it reduces the priority by 1; if it is greater than 6, it reduces the priority by 2",
              "We use MaxEnt based on numerous predictor variables and known locations of species to predict the relative probability of finding the species in question at the chosen waterbody; the binned value
            is simply the range from 0 to 1 cut into thirds: if the predicted probability is from 0 to 0.33, the binned value is 1; from 0.34 to 0.66, 2; 0.67 to 1, 3",
              "The Area-Under-Curve of the MaxEnt model is used to evaluate how well a model can estimate known presence and background locations - 
            it is a score that describes model performance, where the maximum value is 1 and the minimum is 0. 
            To bin this value, I take anything over 0.9 as having the lowest uncertainty (1), anything between 0.8 and 0.9 as more uncertain (2), and anything lower than 0.8 to be much more uncertain (3)")
  )
  
  openxlsx::writeData(my_wb, "metadata", metadata)
  openxlsx::setColWidths(my_wb,"metadata",cols = 2:ncol(metadata), widths = 120)
  
  # Make new tab that shows the ranges for the binning...
  openxlsx::addWorksheet(my_wb, "binning_levels")
  
  binning_levels = data.frame(variable = c("number_inflows_b","other_ais_in_wb_b",
                                           "sara_in_wb_b","sara_downstream_b",
                                           "COSEWIC_in_wb_b","COSEWIC_downstream_b",
                                           "cdc_listed_in_wb_b","cdc_listed_downstream_b",
                                           "maxent_suitability_max_b","maxent_suitability_mean_b",
                                           "m_suit_uncertainty_b","introduction_risk_b"
  ),
  levels = c("<= 5 ~ 1, <= 25 ~ 2, > 25 ~ 3",
             "<= 3 ~ 0, <= 6 ~ -1, > 6 ~ -2",
             "0 ~ 0, 1 ~ 1, 2 ~ 2, >= 3 ~ 3",
             "0 ~ 0, 1 ~ 1, 2 ~ 2, >= 3 ~ 3",
             "0 ~ 0, 1 ~ 1, 2 ~ 2, >= 3 ~ 3",
             "0 ~ 0, 1 ~ 1, 2 ~ 2, >= 3 ~ 3",
             "0 ~ 0, 1 ~ 1, 2 ~ 2, >= 3 ~ 3",
             "0 ~ 0, 1 ~ 1, 2 ~ 2, >= 3 ~ 3",
             "<= 0.33 ~ 1, <= 0.66 ~ 2, > 0.66 ~ 3",
             "<= 0.33 ~ 1, <= 0.66 ~ 2, > 0.66 ~ 3",
             ">= 0.9 ~ 1, >= 0.8 ~ 2, < 0.8 ~ 3",
             "< 0.5 ~ 1, >= 0.5 & < 0.7 ~ 2, >= 0.7 ~ 3"
  )
  )
  
  openxlsx::writeData(my_wb, "binning_levels", binning_levels)
  
  openxlsx::saveWorkbook(my_wb, file = "output/example_ais_prioritization_results.xlsx",
                         overwrite = T)
  
  openxlsx::saveWorkbook(my_wb, file = paste0(output_folder,"example_ais_prioritization_results.xlsx"),
                         overwrite = T)
  
}
