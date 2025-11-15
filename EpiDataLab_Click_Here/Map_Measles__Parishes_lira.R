# Clear the workspace
rm(list = ls())

# Load required libraries
library(sf)
library(dplyr)

# Load the Subcounty shapefile
subcounty_shapefile <- st_read("C:/Users/Administrator/Desktop/Uganda-Subcounties-2021.shp")

# Filter for the Subcounty "ITEK""
ITEK_Subcounty <- subcounty_shapefile %>%
  filter(Subcounty=="BARR") 

# Load the Parishes shapefile
Parishes_shapefile <- st_read("C:/Users/Administrator/Desktop/PARISHES_2016_UTM_36N.shp")

# Filter subcounty shapefile for subcounties in LIRA district
LIRAparishes <- Parishes_shapefile  %>%
  filter(SName2016 == "BARR")

# Extract and list the Parish names
Parish_names <- LIRAparishes$PName2016

# Print the subcounty names
print(Parish_names)

####print(Parish_names) "AYAMO"   "OLILO"   "TETYANG" "ALEBERE" "AJIA"    "OBOT"    "AYIRA"   "ABUNGA" 
### "ONYWAKO" "OBER" 

###########
# Load District Shapefile
districts <- st_read("C:/Users/Administrator/Desktop/uganda_districts.shp")

# C:\Users\Administrator\Desktop\uganda_districts.shp
# Load Subcounty Shapefile
# Load the Parishes shapefile
Parishes_shapefile <- st_read("C:/Users/Administrator/Desktop/PARISHES_2016_UTM_36N.shp")

# Filter Subcounties for the Lango Region
lango_parishesaffected <- Parishes_shapefile %>%
  filter(SName2016 %in% c("BARR"))


# Create parish-Level Case Data
case_data_parishes <- data.frame(
  PName2016 = c("TETYANG", "ONYWAKO"),
  Cases = c(16, 19),  # Number of cases in each subcounty
  SName2016 = c("BARR")  # Subcounty corresponding to parish
)
# in my Legend, I want to show that Tetyang has 16 cases awhile Onywako 19 Cases then Other Parishes No Cases
# Merge Case Data with Subcounty Spatial Data
merged_data <- left_join(
  lango_parishesaffected,
  case_data_parishes,
  by = c("PName2016" = "PName2016", "SName2016" = "SName2016")
)

# Replace NA with 0 for other subcounties with no affected parishes
merged_data$Cases[is.na(merged_data$Cases)] <- 0


# Filter out districts with no affected subcounties for text labels
districts_to_label <- c("BARR")  # Only these districts will have names on the map

# Group by Subcounty, merge geometries, and calculate centroids
Subcounty_centroids <- lango_parishesaffected %>%
  filter(SName2016 %in% c("BARR")) %>%
  group_by(SName2016) %>%  # Group by Subcounty
  summarise(geometry = st_union(geometry)) %>%  # Merge geometries into one per district
  st_centroid() %>%  # Calculate centroids
  st_as_sf() %>%  # Convert to sf object to retain attributes
  mutate(label = SName2016)  # Add a new column for subcounty names

####### Plot map
library(ggplot2)
library(ggrepel)
# Categorize Cases for Simple Legend
merged_data <- merged_data %>%
  mutate(
    Case_Category = case_when(
      Cases == 19 ~ "19 Cases",   # Specific category for 19 Cases
      Cases == 16 ~ "16 Cases",   # Specific category for 16 Cases
      Cases == 0 ~ "No Cases"     # Category for no cases
    )
  )

# Plot map with Simple Legend
ggplot(data = merged_data) +
  # Add parish polygons with fill based on Case_Category
  geom_sf(aes(fill = Case_Category), color = "black") +
  
  # Define discrete fill scale for the simplified legend
  scale_fill_manual(
    values = c(
      "19 Cases" = "#8B0000",       # Red for 19 Cases
      "16 Cases" = "#c2b", # Light blue for 16 Cases
      "No Cases" = "lightgreen"        # Tan for No Cases
    ),
    name = "Measles Case Load" # Legend title
  ) +
  
  # Highlight affected parishes with labels
  geom_label_repel(
    data = merged_data %>% filter(Cases > 0),  # Add labels only for affected parishes
    aes(label = PName2016, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    fontface = "bold",
    color = "black",
    box.padding = 0.07,
    point.padding = 0.15,
    segment.color = NA,
    fill = "white",
    max.overlaps = 200
  ) +
  
  # Add Subcounty names for affected subcounties only (once per subcounty using centroids)
  geom_text_repel(
    data = Subcounty_centroids, 
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    size = 6,
    fontface = "bold",
    segment.color = NA,
    nudge_x = 0.05,
    nudge_y = 0.08
  ) +
  
  # Highlight borders of affected subcounties with thicker lines and contrasting color
  geom_sf(data = lango_parishesaffected %>% filter(SName2016 %in% c("BARR")), 
          aes(geometry = geometry), fill = NA, color = "red4", size = 1.8) +  
  
  # Customize the theme and labels
  labs(
    title = "Measles Case Load in Lira Affected Subcounty BARR (Currently Called ITEK S/C)",
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
    legend.title = element_text(face = "bold", size = 19),
    legend.text = element_text(size = 16),
    legend.position = "right"
  )




















colors()
###################
######################
#########################
rm(list =ls()) 
library(sf)           # For working with shapefiles
library(ggplot2)      # For plotting
library(dplyr)        # For data manipulation
library(ggspatial)    # For adding spatial elements
library(ggrepel)      # For better text placement

# Load the full Uganda District Shapefile
uganda_districts <- st_read("C:/Users/Administrator/Desktop/uganda_districts.shp")

# Filter for LIRA CITY and the other highlighted districts for labeling (without labels)
highlight_districts <- c("APAC", "DOKOLO", "LIRA CITY", "OYAM", "LIRA", "KOLE", "LIRA CITY")

# Create a new column for color based on the district name
uganda_districts <- uganda_districts %>%
  mutate(color = ifelse(District == "LIRA CITY", "tomato2", "white"))

# Plot the map with boundaries for all districts, LIRA CITY highlighted and no labels for the others
ggplot() +
  # Plot the entire Uganda map with all districts' boundaries
  geom_sf(data = uganda_districts, aes(fill = color), color = "black", size = 10.3) +
  scale_fill_identity() +  # Use colors directly from the 'color' column
  # Add the label for LIRA CITY only
  geom_sf_text(data = uganda_districts %>% filter(District == "LIRA CITY"), 
               aes(label = District), size = 4, color = "black", fontface = "bold") +
  theme_classic() +
  ggtitle("Uganda Map with Highlighted LIRA CITY") +
  theme(legend.position = "none")  # Remove the legend


