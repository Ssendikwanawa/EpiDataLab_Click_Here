
rm(list = ls())
library(ggplot2)
library(ggrepel)

# Create a data frame with the provided data
data <- data.frame(
  District = c("Apac", "Dokolo", "Other Districts"),
  Cases = c(2, 2, 0),
  Disease = c("Mpox", "Mpox", "")
)

# Clean data to ensure no missing values
data <- na.omit(data)

# Create the plot with adjusted font sizes and y-axis breaks
ggplot(data, aes(x = District, y = Cases, label = ifelse(Cases > 0, Disease, ""))) + 
  geom_point(size = 16, color = "greenyellow") +
  geom_text_repel(
    box.padding = 0.1, point.padding = 1.2, max.overlaps = Inf, 
    size = 8, # Adjust label size as needed
    fontface = "bold",
    nudge_x = 0.2  # Adjust nudge_x for better label positioning
  ) +
  labs(
    title = "Signals sent through the Alert 6767 SMS platform during Wk2",
    x = "",
    y = "Number of Signals sent"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 23, face = "bold"), 
    axis.text.y = element_text(size = 23), 
    axis.title.x = element_text(size = 23), 
    axis.title.y = element_text(size = 20), 
    plot.title = element_text(size = 27, face = "bold") 
  ) +
  scale_y_continuous(breaks = seq(0, max(data$Cases) + 1, by = 1),  # Set breaks at whole numbers
                     expand = expansion(mult = c(0.31, 0.31)))

##################
library(ggplot2)
library(ggrepel)

# Create a data frame with the provided data
data <- data.frame(
  District = c("Apac", "Dokolo", "Other Districts"),
  Cases = c(2, 2, 0),
  Disease = c("Mpox", "Mpox", "")
)

# Create the plot with adjusted font sizes and y-axis breaks
ggplot(data, aes(x = District, y = Cases, label = ifelse(Cases > 0, Disease, ""))) + 
  geom_point(size = 14, color = "green") +
  geom_text_repel(
    box.padding = 0.1, point.padding = 1.2, max.overlaps = Inf, 
    size = 8, # Adjust label size as needed
    fontface = "bold",
    nudge_x = 0.2  # Adjust nudge_x for better label positioning
  ) +
  labs(
    title = "",
    x = "",
    y = "Number of Signals sent"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 23, face = "bold"), 
    axis.text.y = element_text(size = 23), 
    axis.title.x = element_text(size = 23), 
    axis.title.y = element_text(size = 23), 
    plot.title = element_text(size = 26, face = "bold") 
  ) +
  scale_y_continuous(breaks = seq(0, max(data$Cases) + 1, by = 1),  # Set breaks at whole numbers
                     limits = c(0, max(data$Cases) + 1),      # Set limits to avoid displaying 0 on the y-axis
                     expand = expansion(mult = c(0.31, 0.1)))

