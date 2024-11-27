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
#source("ZuurFuncs.R")
library(ecospat)
library(ENMeval)
library(bcmaps)
library(data.table)
library(here)
library(viridis)
library(spatialEco)
library(nortest)
source(here("scripts/utils/prep_predictor_data_f.R"))
source(here("scripts/utils/zuurFuncs.R"))


sppOI<-"asian clam"

#set locations
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
onedrive_path = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

bc<-bc_bound()
extentvect<- project(vect(bc),"EPSG:4326")
#extentvect<-terra::vect("./vect/fras_colum.gpkg")
extentvect<-project(extentvect, "EPSG:4326")
bc_vect = terra::vect(sf::st_transform(bcmaps::bc_bound(),4326))
bc_wgs<-sf::st_transform(bcmaps::bc_bound(),4326)


predictor_data = prep_predictor_data(proj_path = proj_wd,
                                     onedrive_path = paste0(onedrive_wd),
                                     extentvect)


predictor_data <- predictor_data[[c("population_density",
                                    "TotalInspections", "days_fished","dist_to_highways")]]

# sppDF = bcinvadeR::grab_aq_occ_data(sppOI)

sppDF<- sf::st_read(paste0(onedrive_path,"invasive_species_records_2024-11-20.gpkg"))

sppDF_jitter = sppDF |>
  dplyr::filter(duplicated(paste0(geom))) |>
  sf::st_jitter(factor = 0.0001)

not_jittered_sppDF = sppDF |>
  dplyr::filter(!duplicated(paste0(geom)))

sppDF_j = dplyr::bind_rows(sppDF_jitter,
                           not_jittered_sppDF)

dat = sppDF |> 
  dplyr::mutate(x = sf::st_coordinates(geom)[,1],
                y = sf::st_coordinates(geom)[,2])


for(raster_var in unique(names(predictor_data))){
  dat[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                      dat[,c("x","y")], ID = FALSE)[[raster_var]]
}


modelData <- dat |> 
  dplyr::select(x, y, Species, population_density, TotalInspections, days_fished, dist_to_highways) 

modelData$presence<-"Present"


###########
##########

# Create pseudo-absences for use in GLM

###############
##############
watercourses = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif")) 

#predictor_data$TotalInspections<-subst(predictor_data$TotalInspections, NA, 0)
#predictor_data$days_fished<-subst(predictor_data$days_fished, NA, 0)

watercourses<-crop(watercourses, extentvect)
watercourses<-mask(watercourses, extentvect)

#plot(watercourses)

# Generate the same number of absences as seen in what we are predicting - see Massin et al. (2012)
number_pseudoabsences <- nrow(modelData)
pseudoabsences <- predicts::backgroundSample(watercourses, p = terra::vect(modelData), n = number_pseudoabsences, extf = 0.9) |> 
  as.data.frame()



for(raster_var in unique(names(predictor_data))){
  pseudoabsences[[raster_var]] <- terra::extract(predictor_data[[raster_var]], 
                                                 pseudoabsences[,c("x","y")], ID = FALSE)[[raster_var]]
}

modelDataAbs <- pseudoabsences %>%
  dplyr::select(x, y, population_density, TotalInspections, days_fished, dist_to_highways)


#modelDataAbs_sf <- st_as_sf(modelDataAbs, coords = c("x", "y"), crs = 4326)

# modelDataAbs_sf <- modelDataAbs_sf |> 
#   dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
#                 y = sf::st_coordinates(geometry)[,2]) |> 
#   dplyr::select(-geometry)

modcrs<-crs(modelData)  



modelDataAbs$presence<-"Absent"
modelDataAbs$Species<-"None"
#names(modelData)[names(modelData) == "geom"] <- "geometry"
#st_geometry(modelData) <- "geometry"
totModelData<-bind_rows(st_drop_geometry(modelData), modelDataAbs) 

totModelData<-totModelData[complete.cases(totModelData),]


long_data <- totModelData  |> 
  pivot_longer(cols = c(population_density, TotalInspections, days_fished, dist_to_highways),
               names_to = "variable", values_to = "value")

ggplot(long_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(x = "Variable", y = "Value") +
  ggtitle("Boxplots of Predictor Variables") +
  theme_minimal()


# ggplot() +
#        geom_sf(data = bc, color = "lightgrey", alpha = 0.5) +
#        geom_sf(data = modelData[!modelData$Species == "None",], aes(color = Species), size = 3, alpha = 0.7) +
#        scale_color_viridis_d(option = "A") + 
#       labs(title = "Distribution of Invasive Species") +
#        theme_minimal()
  
split_poly <- function(sf_poly, n_areas) {
  # Create random points
  points_rnd <- st_sample(sf_poly, size = 10000)
  # k-means clustering
  points <- do.call(rbind, st_geometry(points_rnd)) %>%
    as_tibble() %>% setNames(c("lon","lat"))
  k_means <- kmeans(points, centers = n_areas)
  # Create voronoi polygons
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
  # Clip to sf_poly
  crs(voronoi_polys) <- crs(sf_poly)
  voronoi_sf <- st_as_sf(voronoi_polys)
  equal_areas <- st_intersection(voronoi_sf, sf_poly)
  equal_areas$area <- st_area(equal_areas)
  return(equal_areas)
}

grid<-st_make_grid(bc, cellsize = 100000)
intersected_grid <- st_intersection(grid, bc)
plot(intersected_grid)
intersected_grid<-st_as_sf(intersected_grid)
intersected_grid$id<-1:nrow(intersected_grid)

intersected_grid<-st_transform(intersected_grid, st_crs(modcrs))

r <- sf::st_join(modelData, intersected_grid, join = st_intersects)



point_count<- r |> 
  group_by(id) |> 
  summarise( point_count = n())

bc_areas_counts<- st_join(intersected_grid, point_count)

plot(bc_areas_counts)

bc_areas_counts_clean <- bc_areas_counts |> 
  filter(!is.na(point_count))

breaks <- classInt::classIntervals(bc_areas_counts_clean$point_count, n = 6, style = "equal")
bc_areas_counts_clean$color_category <- as.factor(cut(bc_areas_counts_clean$point_count, breaks$brks, labels = FALSE, na.rm = TRUE))
c_areas_counts_clean <- bc_areas_counts_clean %>%
  filter(!is.na(point_count))  # Filter before data is used for ggplot


ggplot() +
  geom_sf(data = st_transform(bc, 4326), color = "lightgrey", alpha = 0.8) +
  geom_sf(data = bc_areas_counts_clean, aes(fill = point_count), color = "darkgrey", size = 0.2) +
  #geom_point(data = modelData, aes(x =x , y =y),color = "black", size = 0.2) +
  scale_fill_gradientn(colors = viridis(9)[3:9], name = "Point Count")+
  labs(title = "Point Counts per Polygon") +
  theme_minimal()





modDT<-setDT(totModelData)
#Fix the melt
melt_dat<-melt(modDT, id.vars = c(3,8), measure.vars = c(4:7))

melt_dat$presence <- as.factor(melt_dat$presence)

ggplot(melt_dat, aes(x = as.factor(presence), y = value, fill = variable)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Absence and Presence by Predictor") +
  ylab("")+
  xlab("")+
  theme_minimal()+
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
    legend.position = 'none'
  )

#Outliers
totModelData$presence<-factor(totModelData$presence)
dotchart(totModelData$population_density, group = totModelData$presence, main = "Population density")
dotchart(totModelData$TotalInspections, group = totModelData$presence, main = "Total Inspections")
dotchart(totModelData$days_fished, group = totModelData$presence, main = "Days fished")
dotchart(totModelData$dist_to_highways, group = totModelData$presence, main = "Distance to highways")

# nothing that stands out as needing fixing. Some large values - may need transformations


# Co-linearity
Z<- cbind(totModelData$population_density, totModelData$TotalInspections, 
          totModelData$days_fished, totModelData$dist_to_highways)
colnames(Z)<-c("Population density", "Total inspections", "Days fished", "Distance to highways")
pairs(Z, lower.panel = panel.smooth2, upper.panel = panel.cor, diag.panel = panel.hist )

# Looking at co-linearity too
ggscatter(data = totModelData, x = "TotalInspections", y = "days_fished",
            add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
            xlab= "Total Inspections", ylab = "Days fished")


ad.test(totModelData$population_density)
ad.test(totModelData$TotalInspections)
ad.test(totModelData$days_fished)
ad.test(totModelData$dist_to_highways)
hist(totModelData$population_density)

hist(totModelData$TotalInspections)

hist(totModelData$days_fished)

hist(totModelData$dist_to_highways+0.01)



totModelData$l.population_density<-log10(totModelData$population_density+10)
totModelData$l.TotalInspections<-log10(totModelData$TotalInspections+10)
totModelData$l.days_fished<-log10(totModelData$days_fished+10)
totModelData$l.dist_to_highways<-log10(totModelData$dist_to_highways+10)

hist(totModelData$l.population_density)
hist(totModelData$l.TotalInspections)
hist(totModelData$l.days_fished)
hist(totModelData$l.dist_to_highways)

#totModelData[!totModelData$dist_to_highways == 0,] 

#########################################################################################
library(lme4)

totModelData$presence<-as.factor(totModelData$presence)
#### Locations were an issue when predicting - goung to round them
totModelData$location <- paste(round(totModelData$x, digits = 4), round(totModelData$y, digit = 4), sep = "_")


# Fit the logistic mixed model
risk_model <- glm(
  presence ~ l.population_density + l.dist_to_highways + l.TotalInspections,
  #+ (1 | location),  # Random effect for spatial grouping by location
  data = totModelData,
  family = binomial(link = "logit")
)
summary(risk_model)
10^(summary(risk_model)$coefficients)

step(risk_model, test = 'LRT')
# https://r.qcbs.ca/workshop06/book-en/binomial-glm.html

plot(risk_model)


# use watercourses raster- save as introduction risk
introduction_risk = predictor_data[[1]]

predict_rast<-predictor_data
names(predict_rast) = c("l.population_density", "l.TotalInspections", "l.days_fished", "l.dist_to_highways")
values(predict_rast)<-log10(values(predict_rast)+10)  
newData<-terra::as.data.frame(predictor_data, xy= TRUE)
dodata<-newData[,3:ncol(newData)]
names(dodata)<-c("l.population_density", "l.TotalInspections", "l.days_fished", "l.dist_to_highways")
dodata <- dodata |> mutate(across(everything(), \(x) log10(x+10)))

model_predict<-terra::predict( predict_rast, risk_model, na.rm = F)
model_predict
introduction_risk$prediction<-model_predict
plot(introduction_risk$prediction)
introduction_risk$population_density = NULL
introduction_risk

# Write out this perhaps shoddy prediction raster to our other data file folder to be used in the 
# AIS model!
terra::writeRaster(introduction_risk, paste0(onedrive_path,'raster/introduction_risk_prediction.tif'))

introduction_risk$prediction<-model_predict
plot(introduction_risk$prediction)
random_effects<-ranef(risk_model) |> 
  pluck(1) |> 
  rownames_to_column() |> 
  rename(location = rowname, Intercept = "(Intercept)")

random_effects |> as_tibble()

#lattice::dotplot(ranef(risk_model))
plot(risk_model)
hist(resid(risk_model))

risk_fitted<-predict(risk_model, newdata = totModelData, re.form = NA)
plot(risk_fitted)


#allFit(risk_model)


###########################################################################################

library(ENMeval)

pres_xy<- modelData |> 
  dplyr::select(x,y)
pseudo<- pseudoabsences |> 
  dplyr::select(x,y)
  

me = ENMevaluate(occs = pres_xy,
                 envs = predictor_data,
                 bg = pseudo,
                 algorithm = 'maxent.jar',
                 partitions = 'block',
                 tune.args = list(fc = c("L","LQ"),
                                  rm = c(1:5)))
me
me@results
opt.aicc = eval.results(me) |> dplyr::filter(delta.AICc == 0)
opt.aicc
plot(opt.aicc)
plot(me@predictions[[1]])
