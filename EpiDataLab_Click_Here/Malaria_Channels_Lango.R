rm(list = ls())
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset (assuming it's in the working directory or specify full path)
library(readxl)
sendimalaria <- read_excel("C:/Users/Administrator/Desktop/Ssendi/Week46/sendimalaria.xls")
View(sendimalaria)


names(sendimalaria)

# Handle missing values by replacing them with NA, or filtering them out
# Here, we'll filter out rows with any missing values in essential columns
sendimalaria_clean <- sendimalaria %>%
  filter(!is.na(District), !is.na(Weeks), !is.na(LowerLimit), !is.na(UpperLimit), !is.na(cases2025))

# Define Lango Region districts to filter
lango_districts <- c("Alebtong", "Apac", "Amolatar", "Dokolo", "Kwania", "Otuke", "Kole", "Lira District", "Lira City", "Oyam")

# Filter data to include only the specified districts
data_filtered <- sendimalaria_clean %>% filter(District %in% lango_districts)

# Plot the faceted line graph with linewidth aesthetic
ggplot(data_filtered, aes(x = Weeks)) +
  geom_line(aes(y = LowerLimit, color = "Lower limit"), linewidth = 1) +
  geom_line(aes(y = UpperLimit, color = "Upper limit"), linewidth = 1) +
  geom_line(aes(y = cases2025, color = "Malaria Cases diagnosed in 2025"), linewidth = 1) +
  facet_wrap(~ District, scales = "free_y") +  # Facet by District
  scale_color_manual(values = c("Lower limit" = "blue4", "Upper limit" = "green4", "Malaria Cases diagnosed in 2025" = "tomato")) +
  labs(title = "Malaria normal channels for Lango Region per District by Epi Week 52, 2025.",
       x = "Epi Weeks in 2025",
       y = "Number of Malaria Cases",
       color = "") +
  theme_minimal() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        strip.text = element_text(face = "bold"))+
theme(legend.position = "bottom", 
      plot.title = element_text(hjust = 0.5, face = "bold"),
      strip.text = element_text(face = "bold"))  # Make District names bold in facet labels



colors()


###########################
##################################
################################################
############################################################### Load necessary library
library(ggplot2)
library(ggrepel)  # For better label placement

# Create a data frame with the provided data
data <- data.frame(
  District = c("Dokolo", "Dokolo","Other Districts"),
  Cases = c(10, 1,0),
  Disease = c("MPOX Cases", "Measles Case","")
)

# Plot
ggplot(data, aes(x = District, y = Cases, size = Cases, color = Disease)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(aes(label = paste(Cases, Disease)), # Labels with both cases and disease names
                  size = 4, color = "black", 
                  fontface = "bold", # Make bubble labels bold
                  box.padding = 0.1, point.padding = 1.2,  
                  direction = "both", hjust = -0.25, vjust = 0.25, 
                  max.overlaps = Inf) +
  scale_size_continuous(range = c(10, 20)) +  # Adjust bubble sizes
  labs(title = "Suspected Conditions Reported Through Signals & Triaged
       during Week 44, 2025 in Lango",
       x = "", y = "Number of Signals", face="bold") +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text.x = element_text(face = "bold"),  # Make x-axis labels (district names) bold
        strip.text = element_text(face = "bold")) +  # Make facet labels (other district labels) bold
  scale_color_manual(values = c("Measles Case" = "blue3", "MPOX Cases" = "red"))
