# Clear Environment
rm(list = ls())

# Load Required Libraries
library(sf)          # For working with shapefiles
library(ggplot2)     # For plotting
library(dplyr)       # For data manipulation
library(ggspatial)   # For adding spatial elements
library(ggrepel)     # For better text placement
library(jsonlite)    # For handling GeoJSON files

# Convert to Spatial Object:Use the sf package for spatial operations. Convert the GeoJSON structure into a simple features (sf) object:
library(sf)
sf_object <- st_read("C:/Users/Administrator/Desktop/Ssendi/Shape files 2020/subcounty_boundaries.geojson")

# Filter the Lango region by district
lango_subcounties <- sf_object %>%
  filter(DNAME2014 %in% c("LIRA"))

head(sf_object)

# Create subcounty-level case data as a data frame
case_data_subcounties <- data.frame(
  Subcounty = c("ADYEL DIVISION",
                "RAILWAY DIVISION", "OJWINA DIVISION"),
  Cases = c(1, 1, 2),  # Number of cases in each subcounty
  District = c("LIRA")  # Districts corresponding to subcounties
)

names(case_data_subcounties)
names(sf_object)
# Rename Columns in Case Data to Match Spatial Object
case_data_subcounties <- case_data_subcounties %>%
  rename(SNAME2014 = Subcounty, DNAME2014 = District)

names(case_data_subcounties)
# Merge Case Data with Spatial Data
lango_subcounties <- lango_subcounties %>%
  left_join(case_data_subcounties, by = c("SNAME2014", "DNAME2014"))

# Replace NA with 0 for Subcounties with No Reported Cases
lango_subcounties$Cases[is.na(lango_subcounties$Cases)] <- 0

# Updated Function to Create a Map with Callouts
create_map <- function(data) {
  # Compute centroids for labeling (automatically derived from spatial data)
  data_centroids <- st_centroid(data)
  
  ggplot(data = data) +
    geom_sf(aes(fill = Cases), color = "black") +                      # Subcounty polygons with color gradient
    scale_fill_gradient(low = "lightyellow", high = "red", name = "Cases") +
    geom_label_repel(                                                 # Add callouts for subcounties with cases
      data = data_centroids %>% filter(Cases > 0),                    # Only label subcounties with cases
      aes(
        x = st_coordinates(geometry)[, 1], 
        y = st_coordinates(geometry)[, 2], 
        label = SNAME2014
      ),
      size = 3,                                                      # Font size for labels
      color = "black",                                               # Label text color
      box.padding = 0.5,                                             # Padding around label box
      point.padding = 0.5,                                           # Distance from point to label
      segment.color = "grey50",                                      # Line color
      segment.size = 0.5                                             # Line thickness
    ) +
    labs(
      title = "Mpox Case Distribution",
      subtitle = "Data as of December 17, 2024",
      caption = "Source: DHIS2"
    ) +
    annotation_scale(location = "bl", width_hint = 0.15) +             # Scale bar at bottom-left
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering()) + # Compass at top-left
    theme_minimal() +
    theme(
      panel.grid = element_blank(),                                   # Remove grid lines
      panel.background = element_rect(fill = "white", color = NA),    # White background
      axis.text = element_blank(),                                    # Remove axis text
      axis.ticks = element_blank(),                                   # Remove axis ticks
      axis.title = element_blank()                                    # Remove axis titles
    )
}

# Plot Map for All Lango Subcounties
map_all_lango <- create_map(lango_subcounties)
print(map_all_lango)  # View the map


