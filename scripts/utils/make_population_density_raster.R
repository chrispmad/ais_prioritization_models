library(terra)
library(bcdata)
library(tidyverse)

proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/CNF/")

mun = bcmaps::municipalities()

pop = readr::read_csv("C:/Users/CMADSEN/Downloads/municipality-population.csv") |> 
  purrr::set_names(snakecase::to_snake_case)

# This includes the number of people by age! We probably just need the total, irrespective of age.
pop = pop |> dplyr::select(region:total)

cens = bcmaps::census_division()

# Clean up some of the municipality names: remove periods from St.,
# combine 'District Municipality' and 'City of'
pop = pop |> 
  dplyr::mutate(region_name = str_remove_all(region_name, '\\.')) |>
  dplyr::mutate(region_name = str_remove_all(region_name, ", (District|City of).*")) |> 
  dplyr::mutate(region_name = str_remove_all(region_name, 'Unincorporated Areas - ')) |> 
  dplyr::mutate(region_name = str_remove_all(region_name, ' \\(Census Division\\)')) |> 
  dplyr::mutate(region_name = ifelse(region_name == 'Metro Vancouver', 'Greater Vancouver', region_name)) |> 
  dplyr::mutate(region_name = ifelse(region_name == 'Valemount', 'Valemont',region_name)) |> 
  dplyr::mutate(region_name = ifelse(region_name == 'Sechelt District Municipality', 'Sechelt',region_name)) |> 
  dplyr::mutate(region_name = ifelse(region_name == 'Columbia Shuswap', 'Columbia-Shuswap',region_name))

# Same thing for municipalities
mun = mun |> 
  dplyr::mutate(region_name = str_remove_all(ADMIN_AREA_ABBREVIATION," - .*")) |> 
  # dplyr::mutate(region_name = str_remove_all(ADMIN_AREA_ABBREVIATION,"-")) |> 
  dplyr::group_by(region_name) |> 
  dplyr::summarise()

mun = dplyr::bind_rows(
  mun,
  cens |> dplyr::select(region_name = CENSUS_DIVISION_NAME)
)

# How many unique municipalities are in the population file?
length(unique(pop$region_name))
#190

# And how many of those are also in common with our spatial file of municipalities?
pop |> 
  dplyr::filter(region_name %in% mun$region_name) |> 
  dplyr::select(region_name) |> 
  dplyr::distinct()
# 154!

pop |> 
  dplyr::filter(!region_name %in% mun$region_name) |> 
  dplyr::select(region_name) |> 
  dplyr::distinct()


# Summarise the population data for this year, irrespective of gender.
pop_sum = pop |> 
  dplyr::filter(year == 2024) |> 
  dplyr::filter(gender == 'T')

pop_sum = pop_sum |> 
  dplyr::group_by(region_name, year) |> 
  dplyr::summarise(total = sum(total)) |> 
  dplyr::ungroup()

# Join the population data from the CSV to the spatial object.
mun_w_d = mun |> 
  dplyr::left_join(pop_sum,
                   join_by(region_name == region_name))

ggplot() + geom_sf(data = mun_w_d, aes(fill = total))

# Bring in raster for reference (dimensions, extent, etc.)
# pred_bioc = terra::rast("CMI/climate/wc2.1_5m/wc2.1_5m_bioc_ACCESS-CM2_ssp585_2021-2040.tif")
# bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
# pred_bioc_clipped = mask(crop(pred_bioc, bc_vect), bc_vect)
# 
# terra::plot(pred_bioc_clipped$bio01)
# 
# ref = pred_bioc_clipped$bio01

ref = terra::rast(paste0(onedrive_wd, "reference_raster_wgs84_500_by_1000m_res.tif"))

# Rasterize the population density.
mun_w_d = sf::st_transform(mun_w_d, 4326) # Transform
mun_w_d = mun_w_d |> dplyr::mutate(total = tidyr::replace_na(total, 0)) # Replace NA with 0

pop_dens_r = terra::rasterize(mun_w_d, ref, field = 'total', fun = 'max')

terra::plot(pop_dens_r)

terra::writeRaster(x = pop_dens_r, filename = paste0(onedrive_wd,"population_density_raster.tif"))
