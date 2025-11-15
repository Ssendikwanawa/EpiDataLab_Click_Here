# Clear workspace
rm(list = ls())
# Load libraries
library(dplyr)
library(tidyr)
library(ggstatsplot)
library(rstatix)

# Create your data
data <- data.frame(
  Time = 1:11,
  ACS_Alerts = c(2.416667, 2.333333, 2.25, 2.166667, 2.083333, 2.0, 1.916667, 1.833333, 1.75, 1.666667, 1.583333),
  Early_Alerts = c(5.643443, 5.180328, 4.717213, 4.254098, 3.790984, 3.327869, 2.864754, 2.401639, 1.938525, 1.47541, 1.012295),
  Ring_Alerts = c(1.392857, 1.928571, 2.464286, 3.0, 3.535714, 4.071429, 4.607143, 5.142857, 5.678571, 6.214286, 6.75)
)
# Reshape to long format
long_data <- data %>%
  pivot_longer(cols = -Time, names_to = "Intervention", values_to = "Alerts")

# Perform Welch's ANOVA
welch_anova_result <- long_data %>%
  welch_anova_test(Alerts ~ Intervention)

# View the result
print(welch_anova_result)

#### SIGNIFICANT

# Perform Games-Howell post hoc test
games_howell_result <- long_data %>%
  games_howell_test(Alerts ~ Intervention)

# View the result
print(games_howell_result)
