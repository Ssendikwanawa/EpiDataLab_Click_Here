rm(list = ls())
#######
# Load required libraries
library(ggplot2)
library(ggridges)
library(viridis)
library(readxl)

# Load your dataset
library(readxl)
reporting_rates2023_Timeliness <- read_excel("C:/Users/Administrator/Desktop/reporting_rates2023_Timeliness.xls")
View(reporting_rates2023_Timeliness)

sendi <- reporting_rates2023_Timeliness

# Create the density ridge plot
ggplot(
  sendi, 
  aes(x = Timeliness, y = District, fill = stat(x))  # Map fill to stat(x)
) +
  geom_density_ridges_gradient(
    scale = 3, rel_min_height = 0.01  # Gradient density ridge settings
  ) +
  scale_fill_gradient(
    name = "Timeliness",  # Label for the color scale
    low = "red",          # Low scores in red
    high = "darkgreen"        # High scores in green
  ) +
  labs(
    title = "Timeliness Over Periods Across Districts", 
    x = "Timeliness", 
    y = "District"
  ) +
  theme_minimal(base_size = 12) +  # Clean theme for minimal distractions
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered title
    axis.title = element_text(face = "bold"),  # Bold axis titles
    axis.text.y = element_text(size = 10)      # Adjust y-axis text size
  )

#####################
#########################
#################################COMPLETENESS
rm(list = ls())
#######
# Load required libraries
library(ggplot2)
library(ggridges)
library(viridis)
library(readxl)

# Load your dataset
library(readxl)
reporting_rates2023_Completeness_xls <- read_excel("C:/Users/Administrator/Desktop/reporting_rates2023_Completeness.xls.xlsx")

sendi <- reporting_rates2023_Completeness_xls

# Create the density ridge plot
ggplot(
  sendi, 
  aes(x = Completeness, y = District, fill = stat(x))  # Map fill to stat(x)
) +
  geom_density_ridges_gradient(
    scale = 3, rel_min_height = 0.01  # Gradient density ridge settings
  ) +
  scale_fill_gradient(
    name = "Completeness",  # Label for the color scale
    low = "red",          # Low scores in red
    high = "darkgreen"        # High scores in green
  ) +
  labs(
    title = "overall distribution of Completeness scores within each District during the year 2024", 
    x = "Completeness", 
    y = "District"
  ) +
  theme_minimal(base_size = 12) +  # Clean theme for minimal distractions
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered title
    axis.title = element_text(face = "bold"),  # Bold axis titles
    axis.text.y = element_text(size = 10)      # Adjust y-axis text size
  )

####FACCET THE PLOTS
rm(list = ls())
# Load required libraries
library(ggplot2)
library(ggridges)
library(viridis)
library(readxl)
library(dplyr)

# Load the datasets
data_timeliness <- read_excel("C:/Users/Administrator/Desktop/reporting_rates2023_Timeliness.xls")
data_completeness <- read_excel("C:/Users/Administrator/Desktop/reporting_rates2023_Completeness.xls.xlsx")

# Add a 'Metric' column to differentiate between the datasets
data_timeliness <- data_timeliness %>%
  rename(Score = Timeliness) %>%
  mutate(Metric = "Timeliness")

data_completeness <- data_completeness %>%
  rename(Score = Completeness) %>%
  mutate(Metric = "Completeness")

# Combine the two datasets
combined_data <- bind_rows(data_timeliness, data_completeness)

# Create the facetted density ridge plot
ggplot(
  combined_data, 
  aes(x = Score, y = District, fill = stat(x))  # Map fill to stat(x)
) +
  geom_density_ridges_gradient(
    scale = 3, rel_min_height = 0.01  # Gradient density ridge settings
  ) +
  scale_fill_gradient(
    name = "KEY",  # Label for the color scale
    low = "red",     # Low scores in red
    high = "darkgreen"  # High scores in green
  ) +
  labs(
    title = "Probability density plot of Timeliness and Completeness  rates for Lango districts, Jan-Dec 2023.", 
    x = "Performance metric score (%)", 
    y = ""
  ) +
  theme_minimal(base_size = 12) +  # Clean theme for minimal distractions
  theme(
    plot.title = element_text(hjust = -6, face = "bold", size = 18),  # Centered title
    axis.title = element_text(face = "bold"),  # Bold axis titles
    axis.text.y = element_text(size = 10, face = "bold"),  # Bold and adjust y-axis text size
    strip.text = element_text(face = "bold", size = 16),   # Bold text for facet labels (Timeliness/Completeness)
    legend.position = "bottom"                  # Position the legend at the bottom
  ) +
  facet_wrap(~ Metric, scales = "free_x")  # Facet by Metric, allow independent x-scales
