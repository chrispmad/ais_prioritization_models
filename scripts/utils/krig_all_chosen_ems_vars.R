# EMS Variables to krig.

vars_to_krig = c(#"Temperature",
                 #"Phosphorus Total Dissolved",
                 #"Turbidity", 
                 "Nitrate_(NO3)_Dissolved",
                 "Nitrate (NO3)", 
                 "Nitrate(NO3) + Nitrite(NO2) Dissolved", 
                 "Carbon Dissolved Organic", 
                 "Nitrogen Total")

source("scripts/utils/krig_ems.R")

vars_to_krig |> 
  purrr::iwalk(
  ~ {
    krig_ems(.x)
  },
  .progress = T
)
