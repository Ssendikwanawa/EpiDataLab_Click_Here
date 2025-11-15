rm(list = ls())
# Load necessary libraries
library(sf)         # For handling spatial data
library(dplyr)      # For data manipulation
library(ggplot2)    # For creating the plot
library(RColorBrewer) # For color palettes
library(ggfx)

library(ggspatial)
# Path to your shapefile - adjust the path as necessary
shapefile_path <- "C:/Users/Administrator/Desktop/Ssendi/Shape files 2020/Districts" # Change this to your shapefile's path

# Read the shapefile
districts <- st_read("C:/Users/Administrator/Desktop/Ssendi/Shape files 2020/Districts")

# Filter for Lango region districts
lango_districts <- districts %>%
  filter(District %in% c("ALEBTONG", "AMOLATAR", "APAC", "DOKOLO", "KOLE", 
                         "LIRA CITY", "LIRA", "KWANIA", "OTUKE", "OYAM"))

# Performance data (Added 2 cases category)
performance_data <- data.frame(
  District = c("LIRA CITY", "DOKOLO", "OYAM", "APAC", "ALEBTONG", "AMOLATAR", "KOLE", "LIRA", "KWANIA", "OTUKE"),
  Performance = c(3, 2, 1, 1, 0, 0, 0, 0, 0, 0)
)

# Merge performance data with spatial data
lango_performance <- lango_districts %>%
  left_join(performance_data, by = "District")

# Create a color palette (using a discrete palette for the four categories)
performance_colors <- c("#FFFFFF", "#FF5C5C", "red", "#660000") # White for 0, light red for 1, orange for 2, dark red for 3

# Plot the map
ggplot(lango_performance) +
  geom_sf(aes(fill = factor(Performance)), color = "black", size = 0.25) +
  geom_sf_text(aes(label = District), size = 3, color = "black", check_overlap = TRUE, 
               family = "Times", fontface = "bold") +
  scale_fill_manual(values = performance_colors,
                    breaks = c(0, 1, 2, 3),
                    labels = c("Other districts", "1 confirmed case", "2 confirmed cases", "3 confirmed cases"),
                    guide = guide_legend(title = "Mpox cases confirmed")) +
  labs(title = "Mpox affected districts in Lango by 20th Nov 2024") +
  theme_classic() +
  theme(legend.position = "right",
        axis.title.x = element_blank(), #removes x axis and y axis labels
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

###############################
#######################################
####################################
############################
####################
#######################################Add data labels
# Merge performance data with spatial data
lango_performance <- lango_districts %>%
  left_join(performance_data, by = "District")

# Create labels with performance in brackets for selected districts
lango_performance$Label <- ifelse(lango_performance$District %in% c("DOKOLO", "OYAM", "LIRA CITY", "APAC"),
                                  paste(lango_performance$District, "(", lango_performance$Performance, ")"),
                                  lango_performance$District)

# Create a color palette for performance levels
performance_colors <- c("white", "#FFCCCC", "#FF6347", "#660000")  # Customize based on your needs("#D3D3D3", "#FFCCCC", "#FF6347", "#FF0000", "#FFFFFF")
ggplot(lango_performance) +
  geom_sf(aes(fill = factor(Performance)), color = "black", size = 0.25) +  # Fill by performance
  geom_sf_text(aes(label = Label), size = 4.5, color = "black", check_overlap = TRUE, 
               family = "calibri", fontface = "bold") +  # Use Calibri font
  scale_fill_manual(values = performance_colors,
                    breaks = c(0, 1, 2, 3),
                    labels = c("Other districts", "1 confirmed case", "2 confirmed cases", "3 confirmed cases"),
                    guide = guide_legend(title = "Mpox cases confirmed")) +  # Color scale for performance
  labs(title = "Spatial distribution of Mpox cases in Lango as of 17th Dec, 2024") +  # Title
  theme_classic() +
  theme(
    legend.position = "right",  # Position of the legend
    axis.title.x = element_blank(),  # Remove axis labels
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),  # Remove the axis lines
    plot.margin = margin(30, 0, 0, 0),  # Increase top margin for the title (30 units)
    panel.margin = margin(0, 0, 0, 0),  # Remove the margin inside the panel
    plot.title = element_text(face = "bold", size = 18, vjust = 2),  # Adjust the title size and move it down (vjust = 2)
    legend.title = element_text(face = "bold", size = 14),  # Increase size and make legend title bold
    legend.text = element_text(face = "bold", size = 14)  # Increase size and make legend text bold
  ) + 
  coord_sf(expand = FALSE) +  # Adjust map limits to give more space
  annotation_scale(location = "bl", width_hint = 0.2) +  # Place scale bar at bottom-left
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(1, "in"), pad_y = unit(2.5, "in"),  # Adjust padding to move it outside
                         height = unit(0.81, "in"), width = unit(0.31, "in"), 
                         style = north_arrow_fancy_orienteering)  # Style for a classic north arrow

#############

