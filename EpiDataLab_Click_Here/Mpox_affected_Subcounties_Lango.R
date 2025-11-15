rm(list = ls())

# Load Required Libraries
library(sf)          # For working with shapefiles
library(ggplot2)     # For plotting
library(dplyr)       # For data manipulation
library(ggspatial)   # For adding spatial elements
library(ggrepel)     # For better text placement
#let us have a continuous legend scale from 0 to 5 but also include the number of cases for example MYENE(2), CHEGERE(1), LIRA WEST DIVISION (5), LIRA EAST DIVISION(1), KANGAI(1), DOKOLO TOWN COUNCIL(1), OTUKE TOWN COUNCIL(4)
# Load Shapefiles 
districts <- st_read("C:/Users/Administrator/Desktop/uganda_districts.shp")
subcounty_shapefile <- st_read("C:/Users/Administrator/Desktop/Uganda-Subcounties-2021.shp")

# Filter Subcounties for the Lango Region and remove empty geometries
lango_subcounties <- subcounty_shapefile %>%
  filter(District %in% c("APAC", "DOKOLO", "LIRA CITY", "OYAM", "AMOLATAR", "KWANIA", "LIRA",
                         "KOLE", "ALEBTONG", "OTUKE")) %>%
  filter(!st_is_empty(geometry))

# Ensure consistent CRS
lango_subcounties <- st_transform(lango_subcounties, crs = st_crs(districts))

# Create Subcounty-Level Case Data
case_data_subcounties <- data.frame(
  Subcounty = c("CHEGERE", "DOKOLO TOWN COUNCIL", "KANGAI","ADOK",
                "LIRA EAST DIVISION", "LIRA WEST DIVISION", "MYENE", "KAMDINI",
                "OYAM TOWN COUNCIL", "OTUKE TOWN COUNCIL"),
  Cases = c(1, 1, 1, 1, 1, 6, 2, 1, 1, 4),  # Number of cases in each subcounty
  District = c("APAC", "DOKOLO", "DOKOLO", "DOKOLO",
               "LIRA CITY", "LIRA CITY", "OYAM", "OYAM", "OYAM", "OTUKE")  # Districts corresponding to subcounties
)

# Merge Case Data with Subcounty Spatial Data
merged_data <- lango_subcounties %>%
  left_join(case_data_subcounties, by = c("Subcounty", "District"))

# Replace NA with 0 for Subcounties with No case
merged_data$Cases[is.na(merged_data$Cases)] <- 0

# Convert Cases to a Factor for Categorized Legend
merged_data$Cases <- factor(
  merged_data$Cases,
  levels = c(0, 1, 2, 4, 6),
  labels = c("No case", "1 Case", "2 Cases", "4 Cases", "6 Cases")
)

# Filter out districts with affected subcounties for text labels
districts_to_label <- c("APAC", "DOKOLO", "LIRA CITY", "OYAM", "OTUKE")

# Group by district, merge geometries, and calculate centroids
district_centroids <- lango_subcounties %>%
  filter(District %in% districts_to_label) %>%
  group_by(District) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_point_on_surface() %>%
  st_as_sf() %>%
  mutate(label = District)

# Plot the Map
ggplot(data = merged_data) +
  geom_sf(aes(fill = Cases), color = "black") +  # Subcounty polygons colored by cases
  scale_fill_manual(
    values = c("white","gold","pink", "red","#8B0000"),
    name = "No of Cases"
  ) +
  geom_label_repel(
    data = merged_data %>% filter(Cases != "No case"),
    aes(label = Subcounty, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.5,
    fontface = "bold",
    color = "black",
    box.padding = 0.97,
    point.padding = 0.15,
    segment.color = "black",
    fill = "white",
    max.overlaps = 200
  ) +
  geom_text_repel(
    data = district_centroids,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    size = 4,
    fontface = "bold",
    color = "blue",
    segment.color = NA,
    max.overlaps = 200,
    nudge_x = 0.005,
    nudge_y = 0.008
  ) +
  geom_sf(
    data = lango_subcounties %>% filter(District %in% districts_to_label),
    aes(geometry = geometry),
    fill = NA,
    color = "brown",
    size = 3,
    alpha = 0.7
  ) +
  annotation_scale(location = "bl", width_hint = 0.15) +  # Scale bar at bottom-left
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering()) +  # Compass at top-left
  labs(
    title = "Affected Subcounties by 12/Jan/2025.",
    subtitle = "",
    caption = ""
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    plot.caption = element_text(face = "bold.italic", color = "tomato3", hjust = 0.5)
  )

############### i want to view my shape file
library (sf)
shapefile <- st_read("C:/Users/Administrator/Desktop/Uganda-Subcounties-2021.shp")
View(shapefile)

library (sf)
shapefile5 <- st_read("C:/Users/Administrator/Desktop/uganda_districts.shp")
View(shapefile5)

######################## LETS USE A CONTINOUS LEGEND
rm(list = ls())
# Load Required Libraries
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(ggrepel)

# Load Shapefiles
districts <- st_read("C:/Users/Administrator/Desktop/uganda_districts.shp")
subcounty_shapefile <- st_read("C:/Users/Administrator/Desktop/Uganda-Subcounties-2021.shp")

# Filter Subcounties for the Lango Region and remove empty geometries
lango_subcounties <- subcounty_shapefile %>%
  filter(District %in% c("APAC", "DOKOLO", "LIRA CITY", "OYAM", "AMOLATAR", "KWANIA", "LIRA",
                         "KOLE", "ALEBTONG", "OTUKE")) %>%
  filter(!st_is_empty(geometry))

# Ensure consistent CRS
lango_subcounties <- st_transform(lango_subcounties, crs = st_crs(districts))

# Create Subcounty-Level Case Data
case_data_subcounties <- data.frame(
  Subcounty = c("CHEGERE", "DOKOLO TOWN COUNCIL", "KANGAI","ADOK",
                "LIRA EAST DIVISION", "LIRA WEST DIVISION", "MYENE", "KAMDINI",
                "OYAM TOWN COUNCIL", "OTUKE TOWN COUNCIL"),
  Cases = c(1, 1, 1, 1, 1, 6, 2, 1, 1, 4),  # Number of cases in each subcounty
  District = c("APAC", "DOKOLO", "DOKOLO", "DOKOLO",
               "LIRA CITY", "LIRA CITY", "OYAM", "OYAM", "OYAM", "OTUKE")  # Districts corresponding to subcounties
)
# Merge Case Data with Subcounty Spatial Data
merged_data <- lango_subcounties %>%
  left_join(case_data_subcounties, by = c("Subcounty", "District"))

# Replace NA with 0 for Subcounties with No case
merged_data$Cases[is.na(merged_data$Cases)] <- 0

# Create Labels with Subcounty Names and Case Counts
merged_data$Label <- paste0(merged_data$Subcounty, " (", merged_data$Cases, ")")

# Filter out districts with no affected subcounties for text labels
districts_to_label <- c("APAC", "DOKOLO", "LIRA CITY", "OYAM", "OTUKE")

# Group by district, merge geometries, and calculate centroids
district_centroids <- lango_subcounties %>%
  filter(District %in% districts_to_label) %>%
  group_by(District) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_point_on_surface() %>%
  st_as_sf() %>%
  mutate(label = District)

# Plot the Map
ggplot(data = merged_data) +
  geom_sf(aes(fill = Cases), color = "black", size = 4) +  # Increase size for boundary lines
  scale_fill_gradient(
    low = "white",
    high = "blue4",# labeling affected subcounties with red4 color
    name = "",
    limits = c(0, 6),
    breaks = seq(0, 6, 1)
  ) +
  geom_label_repel(
    data = merged_data %>% filter(Cases > 0),  # Add labels only for subcounties with cases
    aes(label = Label, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.8,
    color = "blue",
    box.padding = 1.5,
    point.padding = 1.2,
    segment.color = "blue",
    direction="both",
    fill = "white",
    max.overlaps = 100
  ) +
  geom_text_repel(
    data = district_centroids,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    size = 4.8,
    fontface = "bold",
    color = "black",## this is labeling district names in black
    segment.color = NA,
    max.overlaps = 50,
    nudge_x = 0.5,
    nudge_y = 8
  ) +
  geom_sf(
    data = lango_subcounties %>% filter(District %in% districts_to_label),
    aes(geometry = geometry),
    fill = NA,
    color = "red2", #removed #343434 color//this is labeling subcounty boundaries with color red
    size = 14,  # Increase the inherent size of subcounty boundaries
    alpha = 0.9
  ) +
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 22, margin = margin(0, 0, -10, 0)),  # Remove space below title
    plot.title.position = "plot",  # Align title tightly with the plot panel
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",  # Legend on the right
    legend.title = element_text(face = "bold", size = 18),
    legend.text = element_text(size = 18),
    legend.spacing = unit(-1, "cm"),  # Reduce space between legend and map
    legend.margin = margin(0, 0, 0, 0),  # Remove padding inside legend box
    plot.caption = element_text(face = "bold.italic", color = "tomato3", hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)  # Remove overall plot margins
  )

##### other padding options try n see
# Plot the Map
ggplot(data = merged_data) +
  geom_sf(aes(fill = Cases), color = "black", size = 4) +  # Increase size for boundary lines
  scale_fill_gradient(
    low = "white",
    high = "blue4",  # Labeling affected subcounties with red4 color
    name = "Key",
    limits = c(0, 6),
    breaks = seq(0, 6, 1)
  ) +
  geom_label_repel(
    data = merged_data %>% filter(Cases > 0),  # Add labels only for subcounties with cases
    aes(label = Label, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.8,
    color = "blue",
    box.padding = 5,        # Increase to push labels further from the box
    point.padding = 0.11,      # Increase to separate labels from points
    segment.color = "blue",
    fill = "white",
    max.overlaps = 200,
    direction = "y",          # Allow vertical adjustment for cleaner placement
    force_pull = 0.5          # Adjust force to control placement tightness
  ) +
  geom_text_repel(
    data = district_centroids,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    size = 4.8,
    fontface = "bold",
    color = "black",  # This is labeling district names in black
    segment.color = NA,
    max.overlaps = 50,
    nudge_x = 0.01,  # Nudge district labels slightly
    nudge_y = 0.01
  ) +
  geom_sf(
    data = lango_subcounties %>% filter(District %in% districts_to_label),
    aes(geometry = geometry),
    fill = NA,
    color = "red2",  # Removed #343434 color, this is labeling subcounty boundaries with color red
    size = 14,  # Increase the inherent size of subcounty boundaries
    alpha = 0.9
  ) +
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 22, margin = margin(0, 0, -10, 0)),  # Remove space below title
    plot.title.position = "plot",  # Align title tightly with the plot panel
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",  # Legend on the right
    legend.title = element_text(face = "bold", size = 18),
    legend.text = element_text(size = 18),
    legend.spacing = unit(-1, "cm"),  # Reduce space between legend and map
    legend.margin = margin(0, 0, 0, 0),  # Remove padding inside legend box
    plot.caption = element_text(face = "bold.italic", color = "tomato3", hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)  # Remove overall plot margins
  )
####

