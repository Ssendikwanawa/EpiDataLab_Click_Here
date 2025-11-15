# ---- Clear ----
rm(list = ls())
# ---- Libraries ----
library(tidyverse)
library(lubridate)
library(readxl)
library(ggrepel)
library(scales)

# ---- Load Data ----
xls_path <- "D:/EVD_Mbale/VHF Seasonality_Decades/phes_redo/seasonality_vhfs_phes.xlsx"
df_raw <- read_excel(xls_path)

# ---- Clean & Select Variables ----
df_clean <- df_raw %>%
  rename(
    Incident_Date = any_of(c("Incident_Dates", "Incident Dates", "Incident Date")),
    CCHF = any_of(c("CCHF", "CCCHF")),
    Ebola_SUVD = any_of(c("Ebola_SUVD", "Ebola SUVD")),
    YFV = any_of(c("YFV", "yfv")),
    RVF = any_of(c("RVF", "rvf")),
    ZIKV = any_of(c("ZIKV"))
  ) %>%
  mutate(
    year = year(Incident_Date),
    month_num = month(Incident_Date),
    year_month = floor_date(Incident_Date, "month"),
    year_month_label = format(year_month, "%b %Y")
  ) %>%
  mutate(across(c(CCHF, Ebola_SUVD, ZIKV, YFV, RVF), as.numeric)) %>%
  select(Incident_Date, year, month_num, year_month, year_month_label,
         CCHF, Ebola_SUVD, ZIKV, YFV, RVF)

# ---- Reshape to Long Format ----
VHF_Type <- c("CCHF", "Ebola_SUVD", "ZIKV", "YFV", "RVF")

df_long <- df_clean %>%
  pivot_longer(cols = all_of(VHF_Type),
               names_to = "VHF_Type",
               values_to = "Confirmed_Human_Cases") %>%
  filter(!is.na(Confirmed_Human_Cases), Confirmed_Human_Cases > 0)

# ---- Calculate date range BEFORE complete() ----
date_min <- as.Date(min(df_long$year_month, na.rm = TRUE))
date_max <- as.Date(max(df_long$year_month, na.rm = TRUE))

# ---- Summarize Monthly Counts (adjust date_max to June 2025 explicitly) ----
date_min <- as.Date("2024-07-01")
date_max <- as.Date("2025-06-30")  # explicitly June 2025

monthly <- df_long %>%
  group_by(year_month, year_month_label, VHF_Type) %>%
  summarise(total_cases = sum(Confirmed_Human_Cases), .groups = "drop") %>%
  complete(
    VHF_Type,
    year_month = seq.Date(date_min, date_max, by = "month"), # sequence ends on June 2025
    fill = list(total_cases = 0)
  ) %>%
  mutate(
    year_month = as.Date(year_month),
    year_month_label = format(year_month, "%b %Y"),
    total_cases = ifelse(total_cases == 0, 0.1, total_cases)
  )

# No additional filtering needed, since complete() already restricts data to Jul 2024-Jun 2025

# Prepare label data for points > 0.1
label_data <- monthly %>% filter(total_cases > 0.1)


# ---- Rename VHF Types ----
monthly <- monthly %>%
  mutate(VHF_Type = recode(VHF_Type,
                           "CCHF" = "CremeanCongo Hemorrhagic Fever (CCHF)",
                           "Ebola_SUVD" = "Ebola Sudan Virus",
                           "YFV" = "Yellow Fever Virus",
                           "RVF" = "Rift Valley Fever",
                           "ZIKV" = "Zika Virus"))

facet_labels <- c(
  "CremeanCongo Hemorrhagic Fever (CCHF)" = "CremeanCongo Hemorrhagic Fever (CCHF)",
  "Ebola Sudan Virus" = "Ebola Sudan Virus",
  "Yellow Fever Virus" = "Yellow Fever Virus",
  "Rift Valley Fever" = "Rift Valley Fever",
  "Zika Virus" = "Zika Virus"
)

VHF_Type_colors <- c(
  "CremeanCongo Hemorrhagic Fever (CCHF)" = "#7CAE00",
  "Ebola Sudan Virus" = "#B22222",
  "Yellow Fever Virus" = "#FFD700",
  "Rift Valley Fever" = "blue",
  "Zika Virus" = "#c77cff"
)

# ---- Restrict X-axis to Jul 2024 - Jun 2025 ----
x_min <- as.Date("2024-07-01")
x_max <- as.Date("2025-06-30")

# Filter data and labels within range (includes Jun 2025)
monthly <- monthly %>% 
  filter(year_month >= x_min & year_month <= x_max)

label_data <- monthly %>% filter(total_cases > 0.1)

# ---- Plot ----
p_monthly_facet <- ggplot(monthly,
                          aes(x = year_month, y = total_cases,
                              group = VHF_Type, color = VHF_Type)) +
  geom_line(linewidth = 2) +
  geom_point(data = monthly %>% filter(total_cases > 0.22), size = 6) +
  geom_text_repel(
    data = label_data,
    aes(label = total_cases),
    size = 5, fontface = "bold", color = "#000000",
    max.overlaps = 8
  ) +
  facet_wrap(~ VHF_Type, scales = "free_y", ncol = 2,
             labeller = labeller(VHF_Type = facet_labels)) +
  scale_color_manual(values = VHF_Type_colors) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "1 month",
    limits = c(x_min, x_max),  # strict cutoff at Jun 2025 inclusive
    expand = expansion(mult = c(0.01, 0))
  ) +
  scale_y_continuous(
    trans = "log1p",
    breaks = c(0, 1, 10, 50),
    labels = function(x) as.integer(round(x))
  ) +
  labs(
    title = "",
    x = "Incident Month during 2024/25 FY",
    y = "Confirmed Case Counts"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 21),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5,
                               face = "bold", size = 20),
    axis.text.y = element_text(face = "bold", size = 17),
    strip.text.x = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.position = "none"
  )

print(p_monthly_facet)

