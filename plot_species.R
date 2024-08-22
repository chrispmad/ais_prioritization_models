
library(bcdata)
library(bcmaps)
library(ggplot2)
library(bcinvadeR)

plotspecies<-function(d){
  
  
  p1<-ggplot()+
    geom_sf(data = bc, color = "lightgrey", alpha = 0.7)+
    geom_point(data = d, aes (x = x, y = y),color = "purple",size = 1.3)+
    labs(
         title = paste0("Occurance of ",spp, " in British Columbia"),
         subtitle = paste0(nrow(d), " unique observations"))+
    ggthemes::theme_map()+
    #scale_color_manual(values = c(Present = "Purple"))+
    theme(plot.title = element_text(size = rel(2), face = "bold"),
          plot.subtitle = element_text(size = rel(1.8)),
          legend.title = element_text(size = rel(1.8)),
          legend.text = element_text(size = rel(1.2)),
          legend.position = 'right',
          panel.background = element_blank()
    )
  return(p1)
}

bc<-sf::st_transform(bcmaps::bc_bound(),4326)
spp<-"goldfish"
d<-suppressMessages(grab_aq_occ_data(spp))
samp<- d %>% 
  plyr::mutate(x = st_coordinates(geometry)[,1],
               y = st_coordinates(geometry)[,2]) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(x,y) %>% 
  mutate(present = 1)
spp_plot<-plotspecies(samp)
ggsave(spp_plot,filename = paste0("./images/",spp,"_occurances.jpeg"), height = 10, width = 12, dpi = 300)


