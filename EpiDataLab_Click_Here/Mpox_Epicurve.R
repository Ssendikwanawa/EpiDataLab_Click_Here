
rm(list = ls())

library(ggplot2)
library(dplyr)

# Define the exact date range with specified cases
cases <- data.frame(
  Date = seq(as.Date("2024-10-10"), as.Date("2025-01-26"), by = "day"),
  Cases = 0
)

# Mark specific dates with cases
cases$Cases[cases$Date == as.Date("2024-10-11")] <- 1
cases$Cases[cases$Date == as.Date("2024-10-29")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-09")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-12")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-13")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-15")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-02")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-08")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-19")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-29")] <- 4
cases$Cases[cases$Date == as.Date("2025-01-04")] <- 2
cases$Cases[cases$Date == as.Date("2025-01-07")] <- 1
cases$Cases[cases$Date == as.Date("2025-01-17")] <- 2


# Plot the Epi Curve with annotations
ggplot(cases, aes(x = Date, y = Cases)) +
  geom_bar(stat = "identity", fill = "blue4", width = 5) +  # Set width to make bars thicker
  
  # Set x-axis date breaks and labels, adjusting for exact range of data
  scale_x_date(
    breaks = seq(as.Date("2024-10-10"), as.Date("2025-01-26"), by = "2 day"),  # Show all dates
    date_labels = "%d %b",
    expand = c(0, 0)  # Remove extra padding on both sides
  ) +
  
  # Set y-axis limits and breaks
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4.5)) +  # Set y-axis breaks
  
  # Add titles and labels
  labs(
    title = "Epicurve. (n = 19)",
    x = "",
    y = "No_ of cases"
  ) +
  
  # Apply minimal theme and adjust font styles
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 80, vjust = 0.5, hjust = 0.3, face = "bold", family = "Times New Roman"),
    axis.title.x = element_text(face = "bold", family = "Times New Roman"),
    axis.title.y = element_text(face = "bold", family = "Times New Roman"),
    
    # Title positioning and alignment
    plot.title = element_text(hjust = 0, size = 36, face = "bold", family = "Times New Roman"),
    
    # Position the title outside plot area
    plot.title.position = "plot", 
    
    # Add a boundary around the plot
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )



colors()


#################
###############adding data labels
# Plot the Epi Curve with annotations
ggplot(cases, aes(x = Date, y = Cases)) +
  geom_bar(stat = "identity", fill = "red4", width = 3) +  # Set width to make bars thicker
  
  # Add data labels for dates with cases greater than zero
  geom_text(data = cases %>% filter(Cases > 0),
            aes(label = Cases),
            vjust = -0.5, color = "blue4", size = 5, fontface = "bold") +
  
  # Set x-axis date breaks and labels, adjusting for exact range of data
  scale_x_date(
    breaks = seq(as.Date("2024-10-10"), as.Date("2025-01-14"), by = "2 day"),  # Show all dates
    date_labels = "%d %b %Y",
    expand = c(0, 0)  # Remove extra padding on both sides
  ) +
  
  # Set y-axis limits and breaks
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4.5)) +  # Set y-axis breaks
  
  # Add titles and labels
  labs(
    title = "Case counts over time, (n = 16)",
    x = "Date confirmed",
    y = "Number of Mpox confirmed cases"
  ) +
  
  # Apply minimal theme and adjust font styles
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold", family = "Times New Roman"),
    axis.title.x = element_text(face = "bold", family = "Times New Roman"),
    axis.title.y = element_text(face = "bold", family = "Times New Roman"),
    
    # Title positioning and alignment
    plot.title = element_text(hjust = 0, size = 26, face = "bold", family = "Times New Roman"),  # Increased title font size
    
    # Position the title outside plot area
    plot.title.position = "plot", 
    
    # Add a boundary around the plot
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )



















































################# prediction model below

















































































































##########
rm(list = ls())
library(ggplot2)
library(dplyr)

# Define the exact date range with specified cases
cases <- data.frame(
  Date = seq(as.Date("2024-10-10"), as.Date("2025-01-08"), by = "day"),
  Cases = 0
)

# Mark specific dates with cases
cases$Cases[cases$Date == as.Date("2024-10-11")] <- 1
cases$Cases[cases$Date == as.Date("2024-10-29")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-09")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-12")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-13")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-15")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-02")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-08")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-19")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-29")] <- 2

# Plot the Epi Curve with a smooth line and annotations
ggplot(cases, aes(x = Date, y = Cases)) +
  geom_smooth(method = "loess", span = 0.3, color = "red2", size = 1.2) +  # Smooth curve using loess method
  
  # Add annotations for each case date
  annotate("text", x = as.Date("2024-10-11"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  annotate("text", x = as.Date("2024-10-29"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  annotate("text", x = as.Date("2024-11-09"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  annotate("text", x = as.Date("2024-11-12"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  annotate("text", x = as.Date("2024-11-13"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  annotate("text", x = as.Date("2024-11-15"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  annotate("text", x = as.Date("2024-12-02"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  annotate("text", x = as.Date("2024-12-08"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  annotate("text", x = as.Date("2024-12-19"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  annotate("text", x = as.Date("2024-12-29"), y = 1, label = "", size = 4, color = "blue4", angle = 75, hjust = 0) +
  
  # Set x-axis date breaks and labels
  scale_x_date(
    breaks = seq(as.Date("2024-10-10"), as.Date("2025-01-03"), by = "1 day"),  # Show every 5 days
    date_labels = "%d %b"
  ) +
  
  # Set y-axis limits and breaks
  scale_y_continuous(breaks = seq(0, 2, by = 1), limits = c(0, 2)) +  # Adjust y-axis breaks
  
  # Add titles and labels
  labs(
    title = "Mpox Epi Curve for Lango, 8 Oct - 30th Dec 2024.",
    x = "Date confirmed",
    y = "Number of Mpox confirmed cases"
  ) +
  
  # Apply classic theme and adjust font styles
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold", family = "Times New Roman"),
    axis.title.x = element_text(face = "bold", family = "Times New Roman"),
    axis.title.y = element_text(face = "bold", family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = "Times New Roman"),
    plot.title.position = "plot"  # Position title outside plot area
  )


#####non linear growth model
rm(list = ls())
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the exact date range with specified cases
cases <- data.frame(
  Date = seq(as.Date("2024-10-10"), as.Date("2025-01-08"), by = "day"),
  Cases = 0
)

# Mark specific dates with cases
cases$Cases[cases$Date == as.Date("2024-10-11")] <- 1
cases$Cases[cases$Date == as.Date("2024-10-29")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-09")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-12")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-13")] <- 1
cases$Cases[cases$Date == as.Date("2024-11-15")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-02")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-08")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-19")] <- 1
cases$Cases[cases$Date == as.Date("2024-12-29")] <- 2

# Convert Date to numeric (number of days since the first date)
cases$Days <- as.numeric(cases$Date - min(cases$Date))

# Check for any NA values
if (any(is.na(cases$Days)) | any(is.na(cases$Cases))) {
  stop("There are missing values in your data.")
}

# Fit Poisson regression model
poisson_model <- glm(Cases ~ Days, data = cases, family = poisson())

# Check model summary
summary(poisson_model)


# Make predictions (on the log scale)
cases$Poisson_Predicted <- predict(poisson_model, newdata = cases, type = "response")

# Plot the observed vs. predicted cases
ggplot(cases, aes(x = Date, y = Cases)) +
  geom_point(color = "red", size = 2) +
  geom_line(aes(y = Poisson_Predicted), color = "blue", size = 1.2) +
  labs(title = "Poisson Model Fit to Mpox Cases",
       x = "Date", y = "Number of Cases") +
  theme_minimal()

# Check for overdispersion
dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
dispersion  # Should be close to 1


library(MASS)
neg_binom_model <- glm.nb(Cases ~ Days, data = cases)

# Summary of the negative binomial model
summary(neg_binom_model)
