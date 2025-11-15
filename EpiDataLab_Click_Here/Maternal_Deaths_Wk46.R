###==
# Clear the environment
rm(list = ls())

# Load necessary libraries
library(readxl)   # For reading Excel files
library(ggplot2)  # For creating plots
library(dplyr)    # For data manipulation

# Import the dataset
data <- read_excel("C:/Users/Administrator/Desktop/Ssendi/wK1/Causes_MD_in2024.xlsx")

# Summarize the data by counting occurrences of each cause
cause_summary <- data %>%
  group_by(`Probable cause of death`) %>%
  summarise(Count = sum(number)) %>%  # Sum the 'number' column
  arrange(desc(Count))

# Create a bar chart for the top 10 causes
top_10_chart <- ggplot(cause_summary[1:10, ], aes(x = reorder(`Probable cause of death`, Count), y = Count, fill = `Probable cause of death`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Probable Causes of Maternal Deaths in 2024.",
    x = "Probable Cause",
    y = "Number of Maternal Deaths"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),  # Make causes bold
    legend.position = "none"  # Remove the legend
  ) +
  scale_fill_manual(values = c("#FF6347", "#FFA07A", "#FFD700", "#7CFC00", 
                               "#00BFFF", "#9370DB", "#FF69B4", "#40E0D0",
                               "#C0C", "#FF4500"))

# Display the top 10 chart
print(top_10_chart)


######
############ ALL CAUSES OF MATERNAL DEATHS
# Create a bar chart for all causes
# Load necessary libraries
library(ggplot2)
library(viridis)

# Create a bar chart for all causes with automatic colors
summary_chart <- ggplot(cause_summary, 
                        aes(x = reorder(`Probable cause of death`, Count), 
                            y = Count, fill = `Probable cause of death`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Summary of All Probable Causes of Maternal Deaths in 2024",
    x = "",
    y = "Number of Maternal Deaths "
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 60, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.position = "none"  # Remove legend for clarity
  ) +
  scale_fill_viridis_d()  # Automatically assign colors

# Display the summary chart
print(summary_chart)

##
# Manually create a larger color palette with 54 colors
colors <- colorRampPalette(c("#FF4500", "#DA70D6", "#32CD32", "#00CED1",
                             "#4682B4", "#FFD700", "#FF6347", "#8B008B",
                             "#7FFF00", "#FF8C00"))(54)

# Create a bar chart for all causes with a manual color palette
summary_chart <- ggplot(cause_summary, 
                        aes(x = reorder(`Probable cause of death`, Count), 
                            y = Count, fill = `Probable cause of death`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "All Probable Causes of Maternal Deaths in 2024.",
    x = "",
    y = "Number of Maternal Deaths", 
    caption = "Data Source: DHIS2 Event Reports_Maternal Death Review Form."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.position = "none"  # Remove legend for clarity
  ) +
  scale_fill_manual(values = colors)

# Display the summary chart
print(summary_chart)

