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
source("ZuurFuncs.R")
source("scripts/utils/thin_occ.R")

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
plot(predictor_data)
predictor_data[[1]]
predictor_data[[16]]


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

terra::plot(log10(predictor_data$Oxygen_Dissolved))

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

col_start<-grep("^y$", colnames(d_alb))+1
ggpairs(d_alb,columns = col_start:ncol(d_alb))+
  ggtitle("All observations for predictor data correlation")

ggpairs(d_thin,columns = col_start:ncol(d_alb))+
  ggtitle("Thinned observations for predictor data correlation")


library(ENMTools)

cor<-raster.cor.matrix(predictor_data)
thresh<-0.6
dists<-as.dist(1-abs(cor))
clust <- hclust(dists, method = "single")
groups <- cutree(clust, h = 1 - thresh)
jpeg("./images/cluterDendogram.jpg", width = 1200, height = 800)
## Visualize groups:
plot(clust, hang = -1)
rect.hclust(clust, h = 1 - thresh)
dev.off()

vif2<-usdm::vif(predictor_data)
vif2

# Bring in watercourses to constrain randomly sampled pseudoabsences to biologically meaningful locations
# for aquatic organisms.
watercourses = terra::rast(paste0(onedrive_path,"fwa_streams/stream_order_three_plus_2km_res.tif")) 

plot(watercourses)

watercourses<-crop(watercourses, extentvect)
watercourses<-mask(watercourses, extentvect)
plot(watercourses)
