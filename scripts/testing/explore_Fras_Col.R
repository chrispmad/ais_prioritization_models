library(tidyverse)
library(DBI)
library(bcinvadeR)
library(terra)
library(sf)
library(geodata)
library(predicts)
library(ggpubr)
library(dismo)
library(rJava)
library(spatstat)
library(ape)
library(bcmaps)
library(data.table)
library(ENMeval)
source("ZuurFuncs.R")
source("scripts/utils/thin_occ.R")

num_bg<-10000
regularisation_levels = c(1:2); feature_classes = c("L","LQ"); number_pseudoabsences<-1000
#set locations
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
onedrive_path = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

sppOI<-"asian clam"

# Get occurrence data for species (if we haven't done this yet!)
#if(!exists("d")){
  d = suppressMessages(grab_aq_occ_data(sppOI))
#}

bc<-bc_bound()


d_alb<-st_transform(d, 3005)
d_thin<-thin_occ(d_alb)

d_thin$thinned<-"Y"
d_alb$thinned<-"N"
d_comp<-rbind(d_thin, d_alb)

d_alb<- d_alb %>% 
  dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                y = sf::st_coordinates(geometry)[,2])

d_thin <- d_thin %>% 
  dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                y = sf::st_coordinates(geometry)[,2])


p1<-ggplot()+
  geom_sf(data = bc, color = "lightgrey", alpha = 0.5)+
  geom_sf(data = d_alb, color = "blue")+
  ggthemes::theme_map()+
  ggtitle(paste0("Full obs:", nrow(d_alb)))+
  theme(plot.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(1.8)),
        legend.text = element_text(size = rel(1.2)),
        panel.background = element_blank()
  ) 

p2<-ggplot()+
  geom_sf(data = bc, color = "lightgrey", alpha = 0.5)+
  geom_sf(data = d_thin, color = "darkgreen")+
  ggthemes::theme_map()+
  ggtitle(paste0("Thinned obs:", nrow(d_thin)))+
  theme(plot.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(1.8)),
        legend.text = element_text(size = rel(1.2)),
        panel.background = element_blank()
  )

library(patchwork)
  p1 | p2


source("scripts/utils/prep_predictor_data_f.R")
  extentvect<-terra::vect("./vect/fras_colum.gpkg")
  extentvect<-project(extentvect, "EPSG:4326")
predictor_data <- prep_predictor_data(proj_path = proj_wd,
                                     onedrive_path = paste0(onedrive_wd),
                                     extentvect)
# plot(predictor_data)
# predictor_data[[1]]
# predictor_data[[16]]


for(raster_var in unique(names(predictor_data))){
  d_alb[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                        d_alb[,c("x","y")], ID = FALSE)[[raster_var]]
}

for(raster_var in unique(names(predictor_data))){
  d_thin[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                         d_thin[,c("x","y")], ID = FALSE)[[raster_var]]
}

d_alb_DT<-setDT(d_alb)
d_thin_DT<-setDT(d_thin)
melt_d_alb<-melt(d_alb_DT, id.vars = 1:11, measure.vars = 11:ncol(d_alb_DT))
melt_d_thin<-melt(d_thin_DT, id.vars = 1:11, measure.vars = 11:ncol(d_thin_DT))

p<-ggplot(data = melt_d_alb, aes(factor(variable),y = value))+
  theme(plot.title = element_text(size = rel(2), face = "bold"),
                plot.subtitle = element_text(size = rel(1.8)),
                legend.title = element_text(size = rel(1.8)),
                legend.text = element_text(size = rel(1.2)),
                legend.position = 'bottom',
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                                color = "grey"),
                axis.text = element_text(size = rel(1.1), face = "bold"),
                axis.title = element_text(face = "bold"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                )

p + geom_boxplot() + facet_wrap(~variable, scale = "free")+
  ggtitle("Predictor values at all observations")

#terra::plot(log10(predictor_data$Oxygen_Dissolved))

p1<-ggplot(data = melt_d_thin, aes(factor(variable),y = value))+
  theme(plot.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(1.8)),
        legend.title = element_text(size = rel(1.8)),
        legend.text = element_text(size = rel(1.2)),
        legend.position = 'bottom',
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                        color = "grey"),
        axis.text = element_text(size = rel(1.1), face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
  )

p1 + geom_boxplot() + facet_wrap(~variable, scale = "free")+
  ggtitle("Predictor values at thinned observations")

library(GGally)
# Z<-cbind(samp$present, samp$temp, samp$precip, samp$calc, samp$pH, samp$elev)
# colnames(Z)<-c("Present", "Temp", "Precip", "Calcium", "pH", "Elevation")
# pairs(Z[,-1], lower.panel = panel.smooth2, upper.panel = panel.cor, diag.panel = panel.hist)

# col_start<-grep("^y$", colnames(d_alb))+1
# ggpairs(d_alb,columns = col_start:ncol(d_alb))+
#   ggtitle("All observations for predictor data correlation")
# 
# ggpairs(d_thin,columns = col_start:ncol(d_alb))+
#   ggtitle("Thinned observations for predictor data correlation")


library(ENMTools)

cor<-raster.cor.matrix(predictor_data)
thresh<-0.6
dists<-as.dist(1-abs(cor))
clust <- hclust(dists, method = "single")
groups <- cutree(clust, h = 1 - thresh)
#jpeg("./images/cluterDendogram.jpg", width = 1200, height = 800)
## Visualize groups:
plot(clust, hang = -1)
rect.hclust(clust, h = 1 - thresh)
#dev.off()

vif2<-usdm::vif(predictor_data)
vif2

# Bring in watercourses to constrain randomly sampled pseudoabsences to biologically meaningful locations
# for aquatic organisms.
watercourses = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif")) 

plot(watercourses)

watercourses<-crop(watercourses, extentvect)
watercourses<-mask(watercourses, extentvect)
#watercourses<-project(watercourses, "EPSG:3005")
plot(watercourses)
d <- d |> 
  dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                y = sf::st_coordinates(geometry)[,2])

presences <- d |> 
  sf::st_drop_geometry(c('x','y')) |> 
  select(c("x", "y"))
presences_thin<- d_thin |>
  st_as_sf() |> 
  st_transform(4326) |> 
  dplyr::mutate(x1 = sf::st_coordinates(geometry)[,1],
                y1 = sf::st_coordinates(geometry)[,2]) |> 
  select(c("x1", "y1")) |> 
  st_drop_geometry()

  names(presences_thin)<-c("x", "y")
  
  #sf::st_drop_geometry(d_thin[,c('x','y')])


# Sample watercourses' locations for a collection of pseudoabsences; combine with data and then split into testing and training.
background_points <- predicts::backgroundSample(watercourses, p = terra::vect(d), n = num_bg,
                                             extf = 0.9)

## Splits data by localities
the_block <- get.block(presences,bg=background_points, orientation = "lat_lon")
table(the_block$occs.grp)

evalplot.grps(pts = presences, pts.grp = the_block$occs.grp, envs = raster(predictor_data$Annual_Mean_Temperature))+
  geom_sf(data= st_transform(bc,3005), alpha = 0.1)+
  ggplot2::ggtitle("Spatial block partitions: occurrences")

evalplot.grps(pts = background_points, pts.grp = the_block$bg.grp, envs = raster(predictor_data$Annual_Mean_Temperature))+
  geom_sf(data= st_transform(bc,3005), alpha = 0.1)+
  ggplot2::ggtitle("Spatial block partitions: background")

r_stack<-raster::stack(predictor_data)

occs.z<- cbind(presences, raster::extract(r_stack, presences))
bg.z <- cbind(background_points, raster::extract(r_stack, background_points))

# predictor_data = predictor_data %>% 
#   lapply(\(x) {
#     terra::project(x, "EPSG:3005")
#   }) %>% 
#   unlist()
# 
# predictor_data<-terra::rast(predictor_data)

presences


library(tidyterra)
ggplot()+
  geom_spatraster(data= predictor_data[[1]])+
  geom_point(data = presences, aes(x=x,y=y))


me = ENMevaluate(occs = presences, 
                 envs = predictor_data, 
                 bg = background_points, 
                 algorithm = 'maxent.jar', 
                 partitions = 'block', 
                 tune.args = list(fc = feature_classes, 
                                  rm = regularisation_levels))

me


# Find which model had the lowest AIC; we'll use this for now.
opt.aicc = eval.results(me) |> dplyr::filter(delta.AICc == 0)
opt.aicc

var_importance = me@variable.importance[[opt.aicc$tune.args]]
var_importance

predictions = terra::rast(eval.predictions(me)[[opt.aicc$tune.args]])
plot(predictions)


eval_model<- eval.models(me)[[opt.aicc$tune.args]]
eval_model

dismo::response(eval_model)
#r1<-response(eval_model, var = "Annual_Mean_Temperature")

#plot(r1)
#axis(side = 1, at = eval_model@presence$Annual_Mean_Temperature, col = "blue", las = 2, pos = -0.02)

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

plot_a_list(plotlist, 5,5)

plot(predictor_data$asian_clam_temperature_limits)

dat_sim<-similarity(predictor_data, presences)
dat_mess<-dat_sim$similarity_min

# Check out results - this dataframe could be simplified to just hone in 
# on the particular metrics we are curious about!
maxent_results = me@results |> 
  dplyr::filter(tune.args == opt.aicc$tune.args) |> 
  tidyr::as_tibble() |> 
  dplyr::mutate(dplyr::across(dplyr::everything(), \(x) as.character(x))) |> 
  tidyr::pivot_longer(cols = dplyr::everything()) |> 
  dplyr::add_row(name = "regularisation_levels_tested", value = paste0(regularisation_levels, collapse = ', ')) |> 
  dplyr::add_row(name = "feature_classes_tested", value = paste0(feature_classes, collapse = ', '))
maxent_results

maxent_results.partitions = me@results.partitions |> 
  dplyr::filter(tune.args == opt.aicc$tune.args) |> 
  tidyr::as_tibble()
maxent_results.partitions

maxent_html = me@models[[opt.aicc$tune.args]]@html
maxent_html

single_model_metrics = me@models[[opt.aicc$tune.args]]@results[,1] |> 
  as.matrix() |> 
  as.data.frame()
single_model_metrics

single_model_metrics = single_model_metrics |> 
  dplyr::mutate(metric = snakecase::to_snake_case(rownames(single_model_metrics))) |>
  dplyr::rename(value = V1) |>
  tidyr::as_tibble() |>
  dplyr::select(metric, value)
single_model_metrics

key_metrics = single_model_metrics |>
  dplyr::filter(metric %in% c("x_training_samples","training_auc","habitat_threshold_var") | str_detect(metric,".*_contribution") | str_detect(metric,".*permutation_imp.*"))
key_metrics

pres_sf = sf::st_as_sf(me@occs, coords = c("x","y"), crs = 3005)
pres_sf$groups = me@occs.grp
absences_sf = sf::st_as_sf(me@bg, coords = c("x","y"), crs = 3005)

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

species_name<-sppOI
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


########################
########################
########################

cor<-raster.cor.matrix(predictor_data)
thresh<-0.6
dists<-as.dist(1-abs(cor))
clust <- hclust(dists, method = "single")
groups <- cutree(clust, h = 1 - thresh)
#jpeg("./images/cluterDendogram.jpg", width = 1200, height = 800)
## Visualize groups:
plot(clust, hang = -1)
rect.hclust(clust, h = 1 - thresh)
test<-predictor_data

to_keep<-c("asian_clam_temperature_limit", "Phosphorus_Total_Dissolved", "Oxygen_Dissolved",
           "TotalInspections", "days_fished", "population_density", "Water_Temperature",
           "Carbon_Dissolved_Organic", "calc", "dist_to_highways", "Annual_Mean_Temperature",
           "Nitrogen_Total")
test<-subset(predictor_data, names(predictor_data) == to_keep)
predictor_data
groups

