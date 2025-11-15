# === Clear Environment ===
rm(list = ls())

# === Set Aesthetic Theme ===
theme_set(theme_minimal(base_family = "Times New Roman"))


# === Load Required Libraries ===
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(readxl)
library(showtext)

# === Optional: Load Google Font or System Font ===
# font_add_google("Roboto", "roboto")  # Example if internet is active
# showtext_auto()

# === Load Dataset ===
data <- read_excel("D:\\EVD_Mbale\\Mortality_Surveillance\\nEW_DR.MS_DATA.xlsx")

# === Prepare Data ===
data <- data %>%
  mutate(
    Date_of_Death = as.Date(Date_of_Death, format = "%d/%m/%Y"),
    Date_of_sample_collection = as.Date(Date_of_sample_collection, format = "%d/%m/%Y"),
    time_to_intervention = as.numeric(Date_of_sample_collection - Date_of_Death),
    Sample_collected = as.character(Sample_collected),
    status = ifelse(Sample_collected == "1", 1, 0)
  ) %>%
  filter(!is.na(time_to_intervention) & time_to_intervention >= 0)

# === Fit KM Model ===
km_fit <- survfit(Surv(time_to_intervention, status) ~ 1, data = data)

# === Plot KM Curve with Enhanced Aesthetics ===
plot <- ggsurvplot(
  km_fit,
  data = data,
  fun = "event",                            # Failure function: 1 - survival
  conf.int = FALSE,                          # from Death to Sample Collection (days)",
  xlab = "Time (Days from Death to Sample Collection)",
  ylab = "Cumulative Probability ",
  title = "Kaplan–Meier Failure Curve: 
  Time to Postmortem Sample Collection",
  surv.scale = "percent",
  palette = c("#F30000"),                   # Strong red line
  break.time.by = 1,
  risk.table = TRUE,
  risk.table.y.text.col = TRUE,
  risk.table.height = 0.2,
  censor = TRUE,                           # You can turn this TRUE if you want + marks
  ggtheme = theme(base_family = "Liberation Sans")
)

# === Beautify the Theme and Fonts ===
plot$plot <- plot$plot +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 14, family = "Times New Roman"),
    axis.line = element_line(size = 1, color = "#000000"),
    panel.grid.major = element_line(color = "grey80", size = 0.4),  # brings back soft main grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.2)   # adds faint minor lines
  ) #+
  #labs(caption = "Figure 1. Kaplan–Meier failure curve showing cumulative probability of postmortem sample collection over time. The number at risk table below indicates the number of deaths still eligible for sample collection at each time point. A majority of samples were collected on the same day of death (Day 0), with a sharp decline in individuals at risk thereafter.")

# === Thicken Survival Line and Confidence Ribbon ===
plot$plot$layers[[2]]$aes_params$size <- 3  # Increase line width
plot$plot$layers[[3]]$aes_params$alpha <- 2  # Confidence interval shading

# === Font Styling Options ===
# Change `base_family =` above to any of:
# "Times New Roman", "Arial", "Roboto", "Liberation Sans", "Calibri", etc.
# You can also install fonts using `extrafont`, `showtext`, or `systemfonts`

# === Print Final Plot ===
print(plot)

