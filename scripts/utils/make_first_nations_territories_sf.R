library(bcdata)
library(tidyverse)

path_to_user = str_extract(getwd(), ".*Users/[A-Z]+")
onedrive_path = paste0(path_to_user,"/OneDrive - Government of BC/data/")

fn = bcdc_query_geodata("indian-reserves-and-band-names-administrative-boundaries") |> 
  collect()

sf::write_sf(fn, paste0(onedrive_path,"CNF/First_Nations_territories.gpkg"))
