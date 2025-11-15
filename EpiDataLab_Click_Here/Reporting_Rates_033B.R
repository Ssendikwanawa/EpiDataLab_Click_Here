# Load necessary libraries
rm(list = ls())
# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)

# Read the data (adjust file path accordingly
file_path <- "D:/Ssendi/Week7/ReportingratesWK6.xlsx"
data <- read_excel(file_path)

# Check column names of the data
names(data)

# Reshape the data to long format for easier plotting
data_long <- melt(data, id.vars = c("Period", "District"), 
                  variable.name = "Metric", value.name = "Percentage")

# Create the bar plot
ggplot(data_long, aes(x = District, y = Percentage, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create grouped bars
  labs(
    title = "Reporting Rates, Wk6 2025",
    x = "",
    y = ""
  ) +
  scale_fill_manual(
    values = c("Completeness" = "cadetblue", "Timeliness" = "red"),  # Define colors for bars
    labels = c("Completeness", "Timeliness")  # Ensure proper legend labels
  ) +
  
  # Add the MOH threshold line
  geom_hline(yintercept = 80, linetype = "dashed", color = "green", size = 0.6) +
  
  # Annotate the MOH Threshold line
  annotate("text", x = length(unique(data_long$District)), y = 80, 
           label = "MOH Target", color = "green4", fontface = "bold", hjust = 11) +
  
  # Apply black background theme
  theme_dark() +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "blue"),       # Title in bold white
    axis.title.x = element_text(face = "bold", size = 21, color = "white"),    # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 26, color = "white"),    # Bold y-axis label
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white", size = 21), # X-axis labels rotated and white
    axis.text.y = element_text(color = "white", size = 19),                    # Y-axis text white
    legend.text = element_text(color = "white", size = 10),                    # Legend text white
    legend.background = element_rect(fill = "black"),                          # Black legend background
    legend.title = element_blank(),                                           # Remove legend title
    
    # Set the backgrounds of the x-axis, y-axis, and overall plot to black
    panel.background = element_rect(fill = "black", color = NA),               # Background inside plot area
    plot.background = element_rect(fill = "black", color = NA)                 # Outer background
  ) +
  
  # Add data labels on top of bars
  geom_text(aes(label = Percentage), position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 4, color = "white")  # Display percentages above bars

###############################

############################################### REPORTING RATES ALLL WEEKS#####################
#######################################################################################################
##################################################################################################################


