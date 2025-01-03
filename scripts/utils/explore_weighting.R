library(plotly)


number_inflows_b <- seq(1, 10, by = 1)
introduction_risk_b <- seq(1, 10, by = 1)


calculate_average <- function(inflow, risk) {
  return((inflow + risk) / 2)
}


inflows <- rep(number_inflows_b, each = length(introduction_risk_b))
risks <- rep(introduction_risk_b, times = length(number_inflows_b))


average_values <- mapply(function(inflow, risk) (inflow + risk) / 2, inflow = inflows, risk = risks)


average_matrix <- matrix(average_values, nrow = length(number_inflows_b), ncol = length(introduction_risk_b), byrow = TRUE)


fig <- plot_ly(
  x = number_inflows_b, 
  y = introduction_risk_b, 
  z = average_matrix, 
  type = "surface",
  colorscale = "Viridis",   
  showscale = TRUE          
) %>%
  layout(
    title = '3D Surface Plot of Average of Inflows and Risk',
    scene = list(
      xaxis = list(title = 'Number of Inflows'),
      yaxis = list(title = 'Introduction Risk'),
      zaxis = list(title = 'Average')
    ),
    autosize = TRUE,
    margin = list(l = 50, r = 50, b = 50, t = 50)  # Adjust plot margins
  )

surface_data <- data.frame(
  inflows = inflows,
  risks = risks,
  average = average_values
)


fig

wb_maxent_suitability_max <- seq(0,1, by = 0.01)
wb_maxent_suitability_mean	 <- seq(0,1, by = 0.01)
wb_maxent_training_AUC <- seq(0,1, by = 0.01)	

maxent_suitability_max_b <- seq(0,10, by = 1)
maxent_suitability_mean_b<- seq(0,10, by = 1)
m_suit_uncertainty_b<- seq(0,10, by = 1)

df <- expand.grid(
  maxent_suitability_max_b = maxent_suitability_max_b,
  maxent_suitability_mean_b = maxent_suitability_mean_b,
  m_suit_uncertainty_b = m_suit_uncertainty_b
)

df <- df |> mutate(average_value = (maxent_suitability_max_b + maxent_suitability_mean_b + m_suit_uncertainty_b) / 3)

suitable_habitat<-df

heatmap1<- ggplot(df, aes(x = maxent_suitability_max_b, y = maxent_suitability_mean_b, fill = average_value)) +
  geom_tile() +
  facet_wrap(~ m_suit_uncertainty_b, ncol = 3) +
  scale_fill_viridis_c() +
  labs(
    title = "Faceted Heatmap: Averaged Values Across Combinations",
    x = "Maxent Suitability Max",
    y = "Maxent Suitability Mean",
    fill = "Average Value"
  ) +
  theme_minimal()



	
other_ais_in_wb_b	<- seq(0,10, by = 1)
sara_in_wb_b	<- seq(0,10, by = 1)
sara_downstream_b	<- seq(0,10, by = 1)
COSEWIC_in_wb_b	<- seq(0,10, by = 1)
COSEWIC_downstream_b	<- seq(0,10, by = 1)
cdc_listed_in_wb_b	<- seq(0,10, by = 1)
cdc_listed_downstream_b <- seq(0,10, by = 1)

df <- expand.grid(
  other_ais_in_wb_b = other_ais_in_wb_b,
  sara_in_wb_b = sara_in_wb_b,
  sara_downstream_b = sara_downstream_b,
  COSEWIC_in_wb_b = COSEWIC_in_wb_b,
  COSEWIC_downstream_b = COSEWIC_downstream_b,
  cdc_listed_in_wb_b = cdc_listed_in_wb_b,
  cdc_listed_downstream_b = cdc_listed_downstream_b
)

df <- df |> 
  mutate(average_value = rowMeans(across(everything())))

species<-df

summary_df <- df |> 
  group_by(other_ais_in_wb_b, sara_in_wb_b) %>%
  summarize(avg_value = mean(average_value), .groups = 'drop')

heatmap_plot <- ggplot(summary_df, aes(x = other_ais_in_wb_b, y = sara_in_wb_b, fill = avg_value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Heatmap of Averaged Values",
    x = "Other AIS in WB",
    y = "SARA in WB",
    fill = "Average Value"
  ) +
  theme_minimal()


head(surface_data)
head(suitable_habitat)
head(species)

part1<- seq(min(surface_data$average), max(surface_data$average), by = 0.1)
part2<- seq(min(suitable_habitat$average_value), max(suitable_habitat$average_value), by = 0.1)
part3<- seq(min(species$average_value), max(species$average_value), by = 0.1)

