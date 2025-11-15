# Load necessary libraries
library(readxl)   # For reading Excel files
library(ggplot2)  # For plotting
library(dplyr)    # For data manipulation
library(tidyr)    # For reshaping the data

##IMPORT DATA SET
library(readxl)
Dysentry_Typhoid_thresholds <- read_excel("C:/Users/Administrator/Desktop/Ssendi/Week46/Dysentry_Typhoid thresholds.xls")
View(Dysentry_Typhoid_thresholds)

data <- Dysentry_Typhoid_thresholds #let date set imported be called data

names (data)
# Rename columns for easier referencing if needed (optional)
colnames(data) <- c("Period", "Typhoid_Alert_Threshold", "Dysentery_Alert_Threshold", 
                    "Typhoid_Cases_2024", "Dysentery_Cases_2024")

# Reshape the data for faceted plotting
long_data <- data %>%
  pivot_longer(cols = c(Typhoid_Alert_Threshold, Typhoid_Cases_2024, 
                        Dysentery_Alert_Threshold, Dysentery_Cases_2024),
               names_to = c("Disease", "Type"),
               names_sep = "_",
               values_to = "Count") %>%
  mutate(Disease = case_when(
    Disease == "Typhoid" ~ "Typhoid",
    Disease == "Dysentery" ~ "Dysentery"
  ))

# Ensure `Period` is treated as a factor for proper grouping
long_data <- long_data %>%
  mutate(Period = factor(Period, levels = unique(Period)))

# Inspect the Type column to match the color names
unique(long_data$Type)

# Replace missing values in Count with 0
long_data <- long_data %>%
  mutate(Count = ifelse(is.na(Count), 0, Count))

scale_color_manual(
  values = c(
    "Alert" = "green",
    "Cases" = "red"
  ),
  name = "Key"
)

# Rename values in the `Type` column
long_data <- long_data %>%
  mutate(Type = case_when(
    Type == "Alert" ~ "Alert Threshold",
    Type == "Cases" ~ "Cases 2024",
    TRUE ~ Type  # Keep other values unchanged (if any)
  ))

# Rename values in the `Type` column
long_data <- long_data %>%
  mutate(Type = case_when(
    Type == "Alert" ~ "Alert Threshold",
    Type == "Cases" ~ "Cases 2024",
    TRUE ~ Type  # Keep other values unchanged (if any)
  ))

# Create the faceted plot with explicit grouping and custom colors
ggplot(long_data, aes(x = Period, y = Count, color = Type, group = interaction(Type, Disease))) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Disease, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Alert Threshold" = "green",
      "Cases 2024" = "red"
    ),
    name = "Key"  # Rename the legend
  ) +
  scale_y_continuous(labels = function(x) as.integer(x)) +  # Remove decimals
  labs(
    title = "Suspected cases of Dysentery and Typhoid Fever reported by Epi Week 46, 2024",
    x = "Epi weeks in 2024", 
    y = "Number of cases suspected and reported"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1), 
    legend.position = "bottom"  # Move the legend to the bottom
  )

##########################################
##############################################
####################################allow plots in potrait format
# Rename values in the `Type` column
long_data <- long_data %>%
  mutate(Type = case_when(
    Type == "Alert" ~ "Alert Threshold",
    Type == "Cases" ~ "Cases 2024",
    TRUE ~ Type  # Keep other values unchanged (if any)
  ))

# Create the faceted plot with explicit grouping, custom colors, and portrait layout
ggplot(long_data, aes(x = Period, y = Count, color = Type, group = interaction(Type, Disease))) +
  geom_line(linewidth = 1) +
  facet_grid(Disease ~ ., scales = "free_y") +  # Use facet_grid for vertical alignment
  scale_color_manual(
    values = c(
      "Alert Threshold" = "green",
      "Cases 2024" = "red"
    ),
    name = "Key"  # Rename the legend
  ) +
  scale_y_continuous(labels = function(x) as.integer(x)) +  # Remove decimals
  labs(
    title = "Suspected cases of Dysentery and Typhoid Fever reported by Epi Week 46, 2024",
    x = "Epi weeks in 2024", 
    y = "Number of cases suspected and reported"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"  # Move the legend to the bottom
  )
