# 1. Clear environment and load packages
rm(list = ls())
library(sf)
library(dplyr)
library(readxl)
library(stringr)
library(tmap)
library(spdep)
library(grid)
library(gridExtra)

# 2. Read Excel and shapefile, then join into merged_sf
anthrax_df <- read_excel("D:/EVD_Mbale/Vero/Anthrax_byplace.xlsx") %>%
  mutate(
    District_clean  = str_to_upper(str_trim(District)),
    Positivity_Rate = as.numeric(Positivity_Rate),
    Incidence_100k  = as.numeric(Incidencerate)
  )

districts_sf <- st_read(
  "C:/Users/Administrator/Desktop/uganda_districts.shp",
  stringsAsFactors = FALSE
) %>%
  mutate(DISTRICT_upper = str_to_upper(str_trim(District)))

merged_sf <- districts_sf %>%
  left_join(
    anthrax_df %>% select(District_clean, Positivity_Rate, Incidence_100k, Female, Male),
    by = c("DISTRICT_upper" = "District_clean")
  ) %>%
  mutate(
    total_cases = ifelse(is.na(Female) & is.na(Male), 0, Female + Male)
  )

# 3. Reproject to a projected CRS (UTM36N) for spatial analysis
merged_sf <- st_transform(merged_sf, crs = 32636)

# 4. Build a Queen‐contiguity neighbor list & row‐standardized weights
nb_queen    <- poly2nb(merged_sf, queen = TRUE)
listw_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

# 5. GLOBAL MORAN’S I

# 5a. On total_cases
y_cases     <- merged_sf$total_cases
moran_cases <- moran.test(y_cases, listw_queen, zero.policy = TRUE)
print(moran_cases)

# 5b. On Incidence_100k (exclude NAs)
y_incidence  <- merged_sf$Incidence_100k
keep_inc     <- !is.na(y_incidence)
nb_inc_sub   <- subset(nb_queen, subset = keep_inc)
listw_inc_sub <- nb2listw(nb_inc_sub, style = "W", zero.policy = TRUE)
moran_incidence <- moran.test(y_incidence[keep_inc], listw_inc_sub, zero.policy = TRUE)
print(moran_incidence)

# 6. LOCAL GETIS–ORD Gi*

# 6a. For total_cases (no NAs to exclude)
gi_cases      <- localG(y_cases, listw_queen, zero.policy = TRUE)
merged_sf$Gi_Cases_Z <- as.numeric(gi_cases)
merged_sf <- merged_sf %>%
  mutate(
    Gi_Cases_cat = case_when(
      Gi_Cases_Z >=  2.58                       ~ "Hotspot (p ≤ 0.01 : 99% CI)",
      Gi_Cases_Z >=  1.96 & Gi_Cases_Z <  2.58  ~ "Hotspot (p ≤ 0.05 : 95% CI)",
      Gi_Cases_Z <= -2.58                       ~ "Coldspot (p ≤ 0.01 : 99% CI)",
      Gi_Cases_Z <= -1.96 & Gi_Cases_Z >  -2.58  ~ "Coldspot (p ≤ 0.05 : 95% CI)",
      TRUE                                      ~ "Not significant"
    )
  )
merged_sf$Gi_Cases_cat <- factor(
  merged_sf$Gi_Cases_cat,
  levels = c(
    "Hotspot (p ≤ 0.01 : 99% CI)",
    "Hotspot (p ≤ 0.05 : 95% CI)",
    "Coldspot (p ≤ 0.05 : 95% CI)",
    "Coldspot (p ≤ 0.01 : 99% CI)",
    "Not significant"
  )
)

# 6b. For incidence (exclude NAs)
gi_inc <- localG(y_incidence[keep_inc], listw_inc_sub, zero.policy = TRUE)
merged_sf$Gi_Inc_Z <- NA_real_
merged_sf$Gi_Inc_Z[keep_inc] <- as.numeric(gi_inc)
merged_sf <- merged_sf %>%
  mutate(
    Gi_Inc_cat = case_when(
      Gi_Inc_Z >=  2.58                        ~ "Hotspot (p ≤ 0.01 : 99% CI)",
      Gi_Inc_Z >=  1.96 & Gi_Inc_Z <  2.58   ~ "Hotspot (p ≤ 0.05 : 95% CI)",
      Gi_Inc_Z <= -2.58                        ~ "Coldspot (p ≤ 0.01 : 99% CI)",
      Gi_Inc_Z <= -1.96 & Gi_Inc_Z >  -2.58   ~ "Coldspot (p ≤ 0.05 : 95% CI)",
      TRUE                                     ~ "Not significant"
    )
  )
merged_sf$Gi_Inc_cat <- factor(
  merged_sf$Gi_Inc_cat,
  levels = c(
    "Hotspot (p ≤ 0.01 : 99% CI)",
    "Hotspot (p ≤ 0.05 : 95% CI)",
    "Coldspot (p ≤ 0.05 : 95% CI)",
    "Coldspot (p ≤ 0.01 : 99% CI)",
    "Not significant"
  )
)

# 7. PLOT LOCAL Gi* MAPS

# 7a. total_cases Gi* map
tmap_mode("plot")
map_gi_cases <- tm_shape(merged_sf) +
  tm_polygons(
    col        = "Gi_Cases_cat",
    palette    = c(
      "Hotspot (p ≤ 0.01 : 99% CI)"   = "#ff0000",
      "Hotspot (p ≤ 0.05 : 95% CI)"   = "#FF1493",#FF33CC
      "Coldspot (p ≤ 0.05 : 95% CI)"  = "#6baed6",
      "Coldspot (p ≤ 0.01 : 99% CI)"  = "#084594",
      "Not significant"      = "gray80"
    ),
    title      = "Cluster Type(Cases)",
    border.col = "gray60",
    lwd        = 0.2,
    textNA     = ""      # hide NA
  ) +
  tm_title(
    text     = "By Total Case Counts",
    size     = 1.3,
    fontface = "bold",
    position = c("center", "top")
  ) +
  tm_layout(
    frame             = FALSE,
    legend.outside    = TRUE,
    legend.position   = c(x = 0.53, y = 0.43),    
    legend.title.size = 1.1,
    legend.text.size  = 1.2,
    legend.title.fontface= "bold",
    #legend.text.fontface = "bold",
    legend.frame         = FALSE,       
    legend.stack         = "vertical",         # side‐by‐side categories
    legend.width         = 14.4,                 # shrink legend width
    legend.height        = 7.5,                  # shrink legend height
    outer.margins     = c(0.02, 0.02, 0.02, 0.02)
  )

# 7b. incidence Gi* map
map_gi_inc <- tm_shape(merged_sf) +
  tm_polygons(
    col        = "Gi_Inc_cat",
    palette    = c(
      "Hotspot (p ≤ 0.01 : 99% CI)"   = "#FF0000",
      "Hotspot (p ≤ 0.05 : 95% CI)"   = "#FF1493",##F72585, #FF1493, neon pink #FF33CC
      "Coldspot (p ≤ 0.05 : 95% CI)"  = "#6baed6",
      "Coldspot (p ≤ 0.01 : 99% CI)"  = "#084594",
      "Not significant"      = "gray80"
    ),
    title      = "Cluster Type(Incidence)",
    border.col = "gray60",
    lwd        = 0.2,
    textNA     = ""      # hide NA
  ) +
  tm_title(
    text     = "By Incidence/100K",
    size     = 1.3,
    fontface = "bold",
    position = c("center", "top")
  ) +
  tm_layout(
    frame             = FALSE,
    legend.outside    = TRUE,
    legend.position      = c(x = 0.53, y = 0.43),    
    legend.title.size = 1.1,
    legend.text.size  = 1.2,
    legend.title.fontface= "bold",
    #legend.text.fontface = "bold",
    legend.frame         = FALSE,       
    legend.stack         = "vertical",         # side‐by‐side categories
    legend.width         = 14,                 # shrink legend width
    legend.height        = 7.5,                  # shrink legend height
    outer.margins     = c(0.02, 0.02, 0.02, 0.02)
  )

# 8. Convert tmap objects to grobs
g1 <- tmap_grob(map_gi_cases)
g2 <- tmap_grob(map_gi_inc)

# 9. Arrange side by side with a shared title
grid.arrange(
  g1,
  g2,
  ncol = 2,
  top = textGrob(
    "Local Getis–Ord Gi* Spatio-Temporal Analysis of Anthrax Clusters\n (Nov 2023 – Apr 2025), Uganda.",
    gp    = gpar(fontsize = 18, fontface = "bold"),
    hjust = 0.5
  )
)

