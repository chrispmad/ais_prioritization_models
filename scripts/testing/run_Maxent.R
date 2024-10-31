library(tidyverse)
library(ggplot2)
library(DBI)
library(bcinvadeR)
library(terra)
library(sf)
library(geodata)
library(predicts)
library(ggpubr)
library(dismo)
library(rJava)
source("ZuurFuncs.R")
library(ecospat)
library(ENMeval)
library(bcmaps)


sppOI<-"Asian clam"
regularisation_levels = c(1:2); feature_classes = c("L","LQ"); number_pseudoabsences<-1000

habitat_threshold_var = "equal_training_sensitivity_and_specificity_cloglog_threshold"


#set locations
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
onedrive_path = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

source("scripts/utils/prep_predictor_data_f.R")
source("scripts/utils/run_maxent_f.R")

extentvect<-terra::vect("./vect/fras_colum.gpkg")
extentvect<-project(extentvect, "EPSG:4326")

bc<-bc_bound()
# ggplot() +
#   geom_sf(data = bc, fill = "lightblue") +
#   theme_minimal() +
#   labs(title = "British Columbia Geometry")
extentvect<- project(vect(bc),"EPSG:4326")

predictor_data = prep_predictor_data(proj_path = proj_wd,
                                     onedrive_path = paste0(onedrive_wd),
                                     extentvect)

predictor_data <- predictor_data[[c("pH", "calc", "dist_to_highways", "population_density", 
                                    "TotalInspections", "days_fished", "Carbon_Dissolved_Organic", "Chlorophyll", 
                                    "Conductivity", "Oxygen_Dissolved", "Turbidity", 
                                    "temp_Autumn", "temp_Spring", "temp_Summer", "temp_Winter")]]

#predictor_data<-terra::subset(predictor_data, c(10,5,9,7,8))
#plot(predictor_data)
#if(!file.exists("data/goldfish_example_data.rds")){
  goldfish = bcinvadeR::grab_aq_occ_data('goldfish')


  # Ensure unique coordinates by adding
  goldfish_jitter = goldfish |>
    dplyr::filter(duplicated(paste0(geometry))) |>
    sf::st_jitter(factor = 0.0001)

  not_jittered_goldfish = goldfish |>
    dplyr::filter(!duplicated(paste0(geometry)))

  goldfish_j = dplyr::bind_rows(goldfish_jitter,
                                not_jittered_goldfish)
  # library(leaflet)
  # leaflet() |>
  #   addTiles() |>
  #   addCircleMarkers(data = goldfish, color = 'black', fillColor = 'transparent') |>
  #   addCircleMarkers(data = goldfish_jitter, color = 'red', fillColor = 'red')


#   saveRDS(goldfish_j, "data/goldfish_example_data.rds")
# } else {
#   goldfish = readRDS("data/goldfish_example_data.rds")
# }


dat = goldfish |> 
  dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                y = sf::st_coordinates(geometry)[,2])
species_name = dat$Species[1]

# Make {terra} vector of BC.
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))

# Bring in watercourses to constrain randomly sampled pseudoabsences to biologically meaningful locations
# for aquatic organisms.
watercourses = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif")) 

plot(watercourses)



predictor_data$TotalInspections<-subst(predictor_data$TotalInspections, NA, 0)
predictor_data$days_fished<-subst(predictor_data$days_fished, NA, 0)

watercourses<-crop(watercourses, extentvect)
watercourses<-mask(watercourses, extentvect)
for(raster_var in unique(names(predictor_data))){
  dat[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                      dat[,c("x","y")], ID = FALSE)[[raster_var]]
}

dat_just_pred_vars = sf::st_drop_geometry(dat[,c(names(predictor_data))])
#Remove samples lacking predictor raster values?
keep_ind = complete.cases(dat_just_pred_vars)
dat = dat[keep_ind,]

cor_res = cor(dat_just_pred_vars) |> 
  as.data.frame()

predictor_data_low_cor = predictor_data
presences = sf::st_drop_geometry(dat[,c('x','y')])
# Make sure presences are distinct.
presences = dplyr::distinct(presences)

# Sample watercourses' locations for a collection of pseudoabsences; combine with data and then split into testing and training.
pseudoabsences <- predicts::backgroundSample(watercourses, p = terra::vect(dat), n = number_pseudoabsences,
                                             extf = 0.9) %>% 
  as.data.frame()


## Splits data by localities
the_block <- get.block(presences,bg=pseudoabsences, orientation = "lat_lon")
table(the_block$occs.grp)

# evalplot.grps(pts = presences, pts.grp = the_block$occs.grp, envs = raster(predictor_data_low_cor$Annual_Mean_Temperature))+
#   ggplot2::ggtitle("Spatial block partitions: occurrences")
# 
# evalplot.grps(pts = pseudoabsences, pts.grp = the_block$bg.grp, envs = raster(predictor_data_low_cor$Annual_Mean_Temperature))+
#   ggplot2::ggtitle("Spatial block partitions: background")

r_stack<-raster::stack(predictor_data)

occs.z<- cbind(presences, raster::extract(r_stack, presences))
bg.z <- cbind(pseudoabsences, raster::extract(r_stack, pseudoabsences))



# evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
#                      occs.grp = the_block$occs.grp, bg.grp = the_block$bg.grp, categoricals = NULL)
# 
# evalplot.envSim.hist(sim.type = "most_diff", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
#                      occs.grp = the_block$occs.grp, bg.grp = the_block$bg.grp, categoricals = NULL)
# 
# evalplot.envSim.hist(sim.type = "most_sim", ref.data = "occs", occs.z = occs.z, bg.z = bg.z, 
#                      occs.grp = the_block$occs.grp, bg.grp = the_block$bg.grp, categoricals = NULL)
# 
# 
# evalplot.envSim.map(sim.type = "mess", ref.data = "presences", envs = r_stack, occs.z = occs.z, 
#                     bg.z = bg.z, occs.grp = the_block$occs.grp, bg.grp = the_block$bg.grp, 
#                     categoricals = NULL, bb.buf = 7)
# 
# evalplot.envSim.map(sim.type = "most_diff", ref.data = "presences", envs = r_stack, occs.z = occs.z, 
#                     bg.z = bg.z, occs.grp = the_block$occs.grp, bg.grp = the_block$bg.grp, 
#                     categoricals = NULL, bb.buf = 7)

## Block seems most appropriate, as it splits occurrences as evenly as it can
me = ENMevaluate(occs = presences, 
                 envs = predictor_data_low_cor, 
                 bg = pseudoabsences, 
                 algorithm = 'maxent.jar', 
                 partitions = 'block', 
                 tune.args = list(fc = feature_classes, 
                                  rm = regularisation_levels))

me



# Find which model had the lowest AIC; we'll use this for now.
opt.aicc = eval.results(me) |> dplyr::filter(delta.AICc == 0)

var_importance = me@variable.importance[[opt.aicc$tune.args]]

predictions = terra::rast(eval.predictions(me)[[opt.aicc$tune.args]])

eval_model<- eval.models(me)[[opt.aicc$tune.args]]
#eval_model1<-eval_model
dismo::response(eval_model)
r1<-response(eval_model, var = "Annual_Mean_Temperature")

plot(r1)
axis(side = 1, at = eval_model@presence$Annual_Mean_Temperature, col = "blue", las = 2, pos = -0.02)

# Replace parentheses in predictor data names with periods. 
# This happens in the depths of MaxEnt to our variable names.
predictor_data_names = stringr::str_replace_all(names(predictor_data),"(\\(|\\))","\\.")

longList <- predictor_data_names %>% 
  purrr::map( ~ {
    # if(.x == "Nitrate(NO3)_plus_Nitrite(NO2)_Dissolved") browser()
    xy_points = dismo::response(eval_model, var = .x) %>% 
      as.data.frame() %>% 
      dplyr::mutate(variable = .x) %>% 
      dplyr::rename(X = V1, Y = p)
  }) %>% 
  dplyr::bind_rows() %>% 
  tidyr::as_tibble()

# Who are our presence points?

presence_data<-eval_model@presence
presence_data$index<-row.names(eval_model@presence)
library(data.table)
long_presence<-melt(setDT(presence_data), id.vars = "index")
names(long_presence)<-c("index", "variable", "X")

lim_vals<-long_presence %>% 
  group_by(variable) %>% 
  summarise(max = max(X),
            min = min(X))



presence_indices = row.names(eval_model@presence)
longList$is_presence = FALSE
longList[presence_indices,]$is_presence = TRUE

source("scripts/utils/response_plot.r")

plotlist<-list()
for (i in longList$variable){
  dt_to_plot<-longList[longList$variable == i,]
  lng_pres<-long_presence[long_presence$variable == i,]
  
  plotlist[[i]]<-response_plot(dt_to_plot, lng_pres, i)
}



plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
  
  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols)
}

plot_a_list(plotlist, 4,5)


longList |> 
  ggplot() + 
  geom_line(aes(x=X, y=Y,group=variable), col = 'red', linewidth = 1) +
  geom_segment(data = long_presence, aes(x=X,xend=X,y=0,yend=0.1, group = variable), col = 'blue') + 
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
  xlim(aes(min(X[variable == ..PANEL..]), max(X[variable == ..PANEL..]))) +
  #xlim(c(lim_vals$min, lim_vals$max))+
  facet_wrap( ~ variable, scales = 'free_x') +
  ggthemes::theme_clean() +
  theme(legend.position = 'none')


ggplot(data = longList)+
  geom_line(aes(x = as.numeric(X), y = Y), col = "red")+
  #geom_segment(data = eval  aes(x = as.numeric(X), y = 0, xend = as.numeric(X), yend = .01 , col = variable))+
  geom_segment(data = eval_model@presence, aes(x = c))
  facet_wrap(~variable, scales = "free_x")






# evalplot.envSim.hist(me, occs.grp = NULL, bg.grp = NULL)
# evalplot.grps(e=me, envs = raster::stack(predictor_data_low_cor), pts = presences)

dat_sim<-similarity(predictor_data, presences)
dat_mess<-dat_sim$similarity_min





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
  dplyr::slice(1:5) |> 
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

predictions_plot
#rayshader::plot_gg(predictions_plot)
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
