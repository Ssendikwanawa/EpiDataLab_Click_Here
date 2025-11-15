rm(list = ls())  
#################### MAKE X AND Y AXES BOLD
library(ggplot2)
library(dplyr)

# Data
data <- data.frame(
  AgeGroup = rep(c("10-19", "20-29", "30-39", "40-44"), 2),
  Sex = rep(c("Male", "Female"), each = 4),
  Count = c(2, 1, 2, 1, 0, 3, 2, 0)
)

# Transform data to have negative counts for males
data <- data %>%
  mutate(Count = ifelse(Sex == "Male", -Count, Count))

# Plot pyramid with data labels outside the bars
ggplot(data, aes(x = AgeGroup, y = Count, fill = Sex)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Count != 0, abs(Count), "")), 
            position = position_nudge(y = ifelse(data$Count > 0, 0.051, -0.051)), # Nudge labels outside bars
            color = "black", 
            size = 8) +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-4, 4)) +  # Adjust limits based on your data
  labs(title = "Age-Sex distribution for confirmed cases, 30/12/2024.",
       x = "Age", 
       y = "Number of confirmed Mpox cases", 
       fill = "") +
  scale_fill_manual(values = c("Male" = "violetred", "Female" = "turquoise1")) +
  theme_linedraw (base_size = 25) +
  theme(
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.background = element_rect(fill = "white", color = "grey80"),
    panel.grid = element_line(color = "grey85"), 
    plot.title.position = "plot",  # Position title outside plot area
    legend.position = "bottom"    # Move the legend to the bottom
  )
colors()  
