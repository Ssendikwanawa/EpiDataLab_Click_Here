rm(list = ls())
# Load required libraries
library(readxl)  # If your file is Excel
# library(readr)  # If your file is CSV
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Load the dataset from your desktop
library(readxl)
Maternal_Deaths_Yearly_Wk47 <- read_excel("C:/Users/Administrator/Desktop/Ssendi/Week47/Maternal Deaths Yearly Wk47.xls")
View(Maternal_Deaths_Yearly_Wk47)

# 2. Reshape the data to long format (from wide to long)
maternal_deaths_long <- Maternal_Deaths_Yearly_Wk47 %>%
  pivot_longer(cols = c("2022", "2023", "2024"), 
               names_to = "Year", 
               values_to = "Deaths")

# 3. Remove rows where 'Deaths' or 'Period' are NA
maternal_deaths_long <- maternal_deaths_long %>%
  filter(!is.na(Deaths) & !is.na(Period))  # Remove rows with NA in 'Deaths' or 'Period'

# 4. Ensure 'Period' is treated as an ordered factor with weeks from W1 to W47
maternal_deaths_long$Period <- factor(maternal_deaths_long$Period, 
                                      levels = paste0("W", 1:47), 
                                      ordered = TRUE)

# 5. Check if any NA values still remain in 'Period'
sum(is.na(maternal_deaths_long$Period))  # This should return 0

# 6. Create the line plot with increased line width, custom colors, and a caption
ggplot(maternal_deaths_long, aes(x = Period, y = Deaths, color = Year, group = Year)) +
  geom_line(size = 1.5) +  # Increase the line width (size = 1.5)
  geom_point(size = 3) +  # Add points with size 3
  labs(title = "Maternal deaths reported in recent years (2022-2024) by Epi Week 47, 2024.",
       x = "",
       y = "Number of Maternal Deaths",
       color = "",
       caption = "Source: DHIS2") +  # Add the caption
  scale_color_manual(values = c("2024" = "red", "2023" = "blue", "2022" = "green")) +  # Custom colors for each year
  theme_classic() +  # Minimal plot theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Bold x-axis labels
    axis.text.y = element_text(face = "bold"),  # Bold y-axis labels
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    plot.title = element_text(face = "bold", size = 16),  # Bold title
    plot.caption = element_text(hjust = 0.5, face = "italic", color = "grey30", size = 10),  # Style the caption
    legend.position = "bottom",#move legend to bottom
    legend.text = element_text(size = 13, face = "bold")  # Increase font size and make legend text bold
  )  # Bold everything related to the axes and title
