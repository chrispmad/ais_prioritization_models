response_plot<-function(long_x, long_p){
  x_min<-min(long_p$X)
  x_max<-max(long_p$X)
  
  l_nudge<-x_max*0.15
  
  
  p1<-ggplot(data = long_p)+
  geom_line(data = long_x, aes(x=X, y=Y), col = 'red', linewidth = 1) +
    geom_segment(aes(x=X,xend=X,y=0,yend=0.1), col = 'blue') + 
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
    xlim(c(x_min-l_nudge,x_max+l_nudge))+
    #facet_wrap( ~ variable, scales = 'free_x') +
    #ggthemes::theme_clean() +
    theme(legend.position = 'none')
  
  return(p1)
}
