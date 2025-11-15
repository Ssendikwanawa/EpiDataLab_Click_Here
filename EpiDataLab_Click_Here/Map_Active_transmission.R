# Load necessary libraries
library(sf)         # For handling spatial data
library(dplyr)      # For data manipulation
library(ggplot2)    # For creating the plot
library(RColorBrewer) # For color palettes
library(ggfx)

# Install ggspatial if not already installed
if (!requireNamespace("ggspatial", quietly = TRUE)) {
  install.packages("ggspatial")
}


library(ggspatial)
# Path to your shapefile - adjust the path as necessary
shapefile_path <- "C:/Users/Administrator/Desktop/Ssendi/Shape files 2020/Districts" # Change this to your shapefile's path

# Read the shapefile
districts <- st_read("C:/Users/Administrator/Desktop/Ssendi/Shape files 2020/Districts")

# Filter for Lango region districts
lango_districts <- districts %>%
  filter(District %in% c("ALEBTONG", "AMOLATAR", "APAC", "DOKOLO", "KOLE", 
                         "LIRA CITY", "LIRA", "KWANIA", "OTUKE", "OYAM"))

# Performance data
performance_data <- data.frame(
  District = c("LIRA CITY", "DOKOLO", "OYAM", "APAC", "ALEBTONG", "AMOLATAR", "KOLE", "LIRA", "KWANIA", "OTUKE"),
  Performance = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0)
)

# Merge performance data with spatial data
lango_performance <- lango_districts %>%
  left_join(performance_data, by = "District")

# Create a color palette
performance_colors <- c("#FFFFFF", "brown", "brown") # White for 0, light red for 1, dark red for 3

# Plot the map
ggplot(lango_performance) +
  geom_sf(aes(fill = Performance), color = "black", size = 0.25) +
  geom_sf_text(aes(label = District), size = 3, color = "black", check_overlap = TRUE, 
               family = "Times", fontface = "bold")+
  scale_fill_gradientn(colors = performance_colors, 
                       values = scales::rescale(c(0, 1, 3)),
                       breaks = c(0, 1, 3),
                       labels = c("Other districts", "DIstricts with active transmission", "3 confirmed cases"),
                       guide = guide_legend(title = "Key")) +
  labs(title = "Mpox affected districts in Lango by 20th Nov 2024") +
  theme_classic()+
  theme(legend.position = "right",
        axis.title.x = element_blank(), #removes x axix and y axis labels
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  annotation_scale(location = "br", width_hint = 0.5) +  # Add a scale bar at the bottom right
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"), # Adjust padding to move outside the map
                         height = unit(0.81, "in"), width = unit(0.31, "in"), 
                         style = north_arrow_fancy_orienteering)  # Style for a classic north arrow


#######LABEL PERFORMANCE
# Filter for Lango region districts
lango_districts <- districts %>%
  filter(District %in% c("ALEBTONG", "AMOLATAR", "APAC", "DOKOLO", "KOLE", 
                         "LIRA CITY", "LIRA", "KWANIA", "OTUKE", "OYAM"))

# Performance data
performance_data <- data.frame(
  District = c("LIRA CITY", "DOKOLO", "OYAM", "APAC", 
               "ALEBTONG", "AMOLATAR", "KOLE", "LIRA", "KWANIA", "OTUKE"),
  Performance = c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0)
)

# Merge performance data with spatial data
lango_performance <- lango_districts %>%
  left_join(performance_data, by = "District")

# Create labels with performance in brackets for selected districts
lango_performance$Label <- ifelse(lango_performance$District %in% c("DOKOLO", "OYAM", "LIRA CITY", "APAC"),
                                  paste(lango_performance$District, "(", lango_performance$Performance, ")"),
                                  lango_performance$District)

# Plot the map
ggplot(lango_performance) +
  geom_sf(aes(fill = Performance), color = "black", size = 0.25) +
  geom_sf_text(aes(label = Label), size = 3, color = "black", check_overlap = TRUE, 
               family = "Times", fontface = "bold") + 
  scale_fill_gradientn(colors = performance_colors, 
                       values = scales::rescale(c(0, 1, 3)),
                       breaks = c(0, 1, 3),
                       labels = c("No cases confirmed", "1 confirmed case", "3 confirmed cases"),
                       guide = guide_legend(title = "Mpox cases confirmed")) +
  labs(title = "Mpox affected districts in Lango by 20th Nov 2024") +
  theme_classic() +
  theme(legend.position = "right",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  annotation_scale(location = "br", width_hint = 0.5) +  # Add a scale bar at the bottom right
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"), # Adjust padding to move outside the map
                         height = unit(0.81, "in"), width = unit(0.31, "in"), 
                         style = north_arrow_fancy_orienteering)  # Style for a classic north arrow

#####################################
############################################ADDING GLOWING EFFECTS
# Create the plot with Arial to avoid font issues
library(ggplot2)
library(ggfx) 

plot <- ggplot(lango_performance) +
  geom_sf(aes(fill = as.factor(Performance)), color = "black", size = 0.5) %>%
  with_outer_glow(width = 10, colour = "grey", alpha = 5) + 
  geom_sf_text(aes(label = District), size = 3, color = "black", family = "Arial", fontface = "bold", check_overlap = TRUE) +
  scale_fill_manual(values = c("white", "#FFCCCC", "#FF0000"),
                    labels = c("No cases confirmed", "1 confirmed case", "3 confirmed cases"),
                    guide = guide_legend(title = "Mpox cases confirmed")) +
  theme_classic() +
  theme(legend.position = "right",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  annotation_scale(location = "br", width_hint = 0.5) +  # Add a scale bar at the bottom right
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         height = unit(1, "in"), width = unit(1, "in"), 
                         style = north_arrow_fancy_orienteering)  # Add a north arrow at the top left

# Print the plot
print(plot)

