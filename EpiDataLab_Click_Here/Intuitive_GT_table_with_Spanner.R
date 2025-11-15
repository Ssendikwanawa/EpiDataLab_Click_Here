# Clear the workspace
rm(list = ls())

# Load required libraries
library(dplyr)
library(gt)
library(readxl)

# Step 1: Load EVD Info Data
EVD_Info_File <- "D:/EVD_Mbale/manuscript_RingSweepBombardment/Ringsweep_HH_Community_data.xlsx"
evd_data <- read_excel(EVD_Info_File)

#View(evd_data) HIDE THIS
# Step 2: Build GT Table
evd_table <- evd_data %>%
  gt() %>%
  
  # Add Title and Subtitle
  tab_header(
    title = md("**Table 1: Community Engagement and SVD Health Education Coverage During the Ring Sweep Strategy.**"),
    subtitle = md("Summary of Household Visits and SVD Health Education Coverage Across Selected Subcounties Within a 40km Radius, Bugisu Sub-Region.")
  ) %>%
            
  # Column Labels
  cols_label(
    District = md("**District**"),
    Subcounty = md("**Subcounty**"),
    HH_Visited = md("**Households<br>Visited**"),
    Total_Households_in_Subcounty = md("**No. of HHs<br>per SC**"),
    Percent._HH_Visited = md("**Percent.<br>HH Visited**"), 
    HH_Residents_Given_Ebola_Education = md("**HH Residents<br>Given SVD HE**"),
    Subcounty_Population = md("**Subcounty<br>Population**"),
    Percent._Residents_Given_Ebola_Education = md("**Percent.Residents<br>Given SVD HE**")
  ) %>%
  
  # Spanner Labels for Column Groups
  tab_spanner(
    label = md("**Geographic Area**"),
    columns = c(District, Subcounty),
    id = "loc_info"
  ) %>%
  tab_spanner(
    label = md("**Household Engagement**"),
    columns = c(HH_Visited, Total_Households_in_Subcounty, Percent._HH_Visited),
    id = "hh_visits"
  ) %>%
  tab_spanner(
    label = md("**SVD Health Education**"),
    columns = c(HH_Residents_Given_Ebola_Education, Subcounty_Population, Percent._Residents_Given_Ebola_Education),
    id = "he_edu"
  ) %>%
  
  # Style Spanner Headers (matching their groups)
  tab_style(
    style = list(
      cell_fill(color = "#f6faf4"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_spanners(spanners = c("loc_info", "he_edu"))
  ) %>%
  
  # Center align numeric columns
  cols_align(
    align = "center",
    columns = c(
      HH_Residents_Given_Ebola_Education, Subcounty_Population, 
      Percent._Residents_Given_Ebola_Education, HH_Visited,
      Total_Households_in_Subcounty, Percent._HH_Visited
    )
  ) %>%
  
  # Bold text for "40KM Radius" row
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = District == "40KM Radius")
  ) %>%
  
  # % columns styled: fill + border + bold
  tab_style(
    style = list(
      cell_fill(color = "#e9f7fd"),
      cell_borders(sides = "all", color = "#f8f9fa", weight = px(1.8)),
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = c(Percent._Residents_Given_Ebola_Education, Percent._HH_Visited))
  ) %>%
  
  # Color shading for Ebola Education block
  tab_style(
    style = cell_fill(color = "#f6faf4"),
    locations = cells_body(columns = c(
      HH_Residents_Given_Ebola_Education, Subcounty_Population, Percent._Residents_Given_Ebola_Education
    ))
  ) %>%
  
  # Bold all column labels
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  
  # Apply color gradients to percentage columns
  data_color(
    columns = c(Percent._HH_Visited),
    fn = scales::col_numeric(
      palette = c("#DCE2D5", "#8CA59A"),
      domain = c(0.1, 0.7)
    )
  ) %>%
  data_color(
    columns = c(Percent._Residents_Given_Ebola_Education),
    fn = scales::col_numeric(
      palette = c("#DCE2DF", "#8CA597"),
      domain = c(0.1, 0.5)
    )
  ) %>%
  
  # Row striping for better legibility
  tab_options(
    row.striping.include_table_body = TRUE,
    row.striping.include_stub = TRUE,
    row.striping.background = "#f8f9fa"
  ) %>%
  
  # Column Widths
  cols_width(
    District ~ px(120),
    Subcounty ~ px(139),
    HH_Visited ~ px(95),
    Total_Households_in_Subcounty ~ px(110),
    Percent._HH_Visited ~ px(135),
    HH_Residents_Given_Ebola_Education ~ px(125),
    Subcounty_Population ~ px(85),
    Percent._Residents_Given_Ebola_Education ~ px(135)
  ) %>%
  
  # Font and compact padding
  tab_options(
    table.font.size = "100%",
    data_row.padding = px(2.3)
  ) %>%
  
  # Source note
  tab_source_note(
    source_note = md("_**Note:** HH = Household, HE = Health Education, Percent = Percentage, SC = Sub County, SVD = Sudan Virus Disease._")
  ) %>%
  
  # Final touch: Roboto font
  opt_table_font(
    font = google_font("Roboto")
  )

# Step 3: Show the table in viewer
evd_table

