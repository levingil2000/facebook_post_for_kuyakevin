#install.packages("haven")
#library(haven)
library(dplyr)
library(tidyr)

gps_dataset_individual <- read_dta("individual_new.dta")
gps_dataset_individual_v11 <-read_dta("individual_v11_new.dta")

#Rescaled data for visualization
## Goal is to standardize the following variables: wgt, patience, risktaking, posrecip, negrecip,
## altruism, trust. 
## By standardizing the variables we get to visualize how the philippines compare to the mean/globally with 0, positives and negatives
## To make it more understandable for a layman, i'll scale the normalized values to 100.

##Calculate the mean and the sd's for easier computing
mean_patience = mean(gps_dataset_individual$patience, na.rm = TRUE)
mean_risktaking = mean(gps_dataset_individual$risktaking, na.rm = TRUE)
mean_posrecip = mean(gps_dataset_individual$posrecip, na.rm = TRUE)
mean_negrecip = mean(gps_dataset_individual$negrecip, na.rm = TRUE)
mean_altruism = mean(gps_dataset_individual$altruism, na.rm = TRUE)
mean_trust = mean(gps_dataset_individual$trust, na.rm = TRUE)
sd_patience = sd(gps_dataset_individual$patience, na.rm = TRUE)
sd_risktaking = sd(gps_dataset_individual$risktaking, na.rm = TRUE)
sd_posrecip = sd(gps_dataset_individual$posrecip, na.rm = TRUE)
sd_negrecip = sd(gps_dataset_individual$negrecip, na.rm = TRUE)
sd_altruism = sd(gps_dataset_individual$altruism, na.rm = TRUE)
sd_trust = sd(gps_dataset_individual$trust, na.rm = TRUE)

scaled_df <- gps_dataset_individual %>%
  mutate(patience.scale = (10/3)*(patience-mean_patience)/sd_patience) %>%
  mutate(risktaking.scale = (10/3)*(risktaking - mean_risktaking)/sd_risktaking) %>%
  mutate(posrecip.scale = (10/3)*(posrecip- mean_posrecip)/sd_posrecip) %>%
  mutate(negrecip.scale = (10/3)*(negrecip - mean_negrecip)/sd_negrecip) %>%
  mutate(altruism.scale = (10/3)*(altruism - mean_altruism)/sd_altruism) %>%
  mutate(trust.scale = (10/3)*(trust - mean_trust)/sd_trust)

# This prep allows me to compare philippines vs the rest of the world

### create a dataset limited to the philippines
gps_individual_philippines <- scaled_df %>%
  filter(country == "Philippines")

## Comparing the variables via barchart and since i've standardized the values, we can easily compare how 
## the country performs vs the global standards
#library(ggplot2)

mean_scores_ph <- gps_individual_philippines %>%
  summarise(across(ends_with("scale"), ~mean(.x, na.rm=TRUE), .names = "{.col}_mean")) %>%
  pivot_longer(
    col = ends_with("_mean"),
    names_to = "variable",
    values_to = "mean_value",
    names_pattern = "(.*)_mean")

#plot
ggplot(mean_scores_ph, aes(y = variable, x = mean_value)) +
  # Create horizontal bars.
  # Use `fill` to distinguish positive from negative deviations for better visual clarity.
  geom_bar(stat = "identity", aes(fill = ifelse(mean_value > 0, "Positive Deviation", "Negative Deviation"))) +
  
  # Add a vertical line at x = 0 to serve as the centered axis
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.8) +
  
  # Add titles and labels
  labs(
    title = paste0("Mean scores PH "),
    subtitle = "Deviation from the Global Mean (0) for Standardized Variables",
    x = "Scores",
    y = NULL, # No label needed for the y-axis if it just lists variables
    fill = "Deviation Direction" # Legend title for bar color
  ) +
  
  # Use a clean theme
  theme_minimal() +
  
  # Customize theme elements for better readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray30"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  
  # Manually set colors for positive/negative deviations
  scale_fill_manual(values = c("Positive Deviation" = "#4CAF50", "Negative Deviation" = "#F44336")) + # Green for positive, Red for negative
  
  # Adjust x-axis limits to ensure zero is clearly visible and bars don't extend too far.
  # This makes sure the plot expands enough to show negative bars and the zero line well.
  coord_cartesian(
    xlim = c(min(mean_scores_ph$mean_value) - 0.2, # Add some padding
             max(mean_scores_ph$mean_value) + 0.2)  # Add some padding
  )
