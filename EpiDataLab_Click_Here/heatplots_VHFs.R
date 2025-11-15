# --- Clear workspace ---
rm(list = ls())

# --- Load libraries ---
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)

# --- Load data ---
file_path <- read_excel("D:/EVD_Mbale/VHF Seasonality_Decades/PHEs_2024_25/PHEs.xlsx")

# --- Debug: Check raw dates ---
cat("Raw Incident_Dates:\n")
print(unique(file_path[[1]]))

# --- Standardize Incident_Dates ---
file_path[[1]] <- trimws(as.character(file_path[[1]])) # Ensure character
# Map months to correct format
file_path$Incident_Dates <- case_when(
  file_path[[1]] == "Jun" ~ "June 2024",
  file_path[[1]] == "Jul" ~ "July 2024",
  file_path[[1]] == "Aug" ~ "Aug 2024",
  file_path[[1]] == "Sept" ~ "Sept 2024",
  file_path[[1]] == "Oct" ~ "Oct 2024",
  file_path[[1]] == "Nov" ~ "Nov 2024",
  file_path[[1]] == "Dec" ~ "Dec 2024",
  file_path[[1]] == "Jan" ~ "Jan 2025",
  file_path[[1]] == "Feb" ~ "Feb 2025",
  file_path[[1]] == "Mar" ~ "Mar 2025",
  file_path[[1]] == "Apr" ~ "Apr 2025",
  file_path[[1]] == "May" ~ "May 2025",
  file_path[[1]] == "Jun" ~ "June 2025",
  TRUE ~ file_path[[1]] # Keep others (e.g., "2024", "2025") for now
)

# --- Remove invalid dates (e.g., "2024", "2025") ---
file_path <- file_path %>% filter(Incident_Dates %in% c("July 2024", "Aug 2024", "Sept 2024", 
                                                        "Oct 2024", "Nov 2024", "Dec 2024",
                                                        "Jan 2025", "Feb 2025", "Mar 2025", 
                                                        "Apr 2025", "May 2025", "June 2025"))

# --- Debug: Check standardized dates ---
cat("Standardized Incident_Dates:\n")
print(unique(file_path$Incident_Dates))

# --- Lists ---
Other_Priority_PHEs <- c("Rabies", "Measles", "Food_poisoning", "Landslide",
                         "Influenza_H1N1", "Cholera", "Meningitis",
                         "Anthrax", "Floods")
VHF_Type <- c("CCCHF", "Ebola_SUVD", "ZIKV", "YFV", "RVF")

# --- Transpose ---
df_transposed <- as.data.frame(t(file_path[, -1]))
colnames(df_transposed) <- file_path$Incident_Dates
df_transposed$Disease <- rownames(df_transposed)
df_transposed$Disease <- as.character(df_transposed$Disease)

# --- Melt data ---
df_long <- melt(df_transposed, id.vars = "Disease", variable.name = "Date", value.name = "Confirmed_Cases") %>%
  mutate(Confirmed_Cases = as.numeric(as.character(Confirmed_Cases))) %>%
  filter(!is.na(Confirmed_Cases))

# --- Debug: Check dates and data ---
cat("Unique dates in df_long$Date:\n")
print(unique(df_long$Date))
cat("Rows in df_long:\n")
print(nrow(df_long))

# --- Set month order ---
month_order <- c("July 2024", "Aug 2024", "Sept 2024", "Oct 2024", "Nov 2024", "Dec 2024",
                 "Jan 2025", "Feb 2025", "Mar 2025", "Apr 2025", "May 2025", "June 2025")
df_long$Date <- factor(df_long$Date, levels = month_order, ordered = TRUE)

# --- Debug: Check for NA in Date ---
cat("Number of NA dates in df_long$Date:\n")
print(sum(is.na(df_long$Date)))

# --- Filter VHF and rename ---
df_VHF <- df_long %>%
  filter(Disease %in% VHF_Type) %>%
  mutate(Disease = recode(Disease,
                          "ZIKV" = "Zika Virus",
                          "YFV" = "Yellow Fever Virus",
                          "RVF" = "Rift Valley Fever",
                          "Ebola_SUVD" = "Ebola Sudan Virus",
                          "CCCHF" = "CCHF"),
         Category = "Viral Haemorrhagic Fevers")

# --- Filter Other PHEs with renaming + custom order ---
df_Other <- df_long %>%
  filter(Disease %in% Other_Priority_PHEs) %>%
  mutate(
    Disease = recode(Disease,
                     "Rabies" = "Rabies",
                     "Measles" = "Measles",
                     "Landslide" = "Landslides",
                     "Influenza_H1N1" = "Influenza-H1N1",
                     "Food_poisoning" = "Food Poisoning",
                     "Cholera" = "Cholera",
                     "Meningitis" = "Bacterial Meningitis",
                     "Anthrax" = "Anthrax",
                     "Floods" = "Floods"),
    Category = "Other Priority Public Health Emergencies"
  )

# --- Set custom disease order for plotting ---
disease_order <- c("Measles", "Influenza-H1N1", "Cholera", "Bacterial Meningitis", 
                   "Anthrax", "Rabies", "Food Poisoning", 
                   "Floods", "Landslides")

df_Other$Disease <- factor(df_Other$Disease, levels = disease_order, ordered = TRUE)



# --- Debug: Check filtered data ---
cat("Rows in df_VHF:\n")
print(nrow(df_VHF))
cat("Rows in df_Other:\n")
print(nrow(df_Other))

# --- Combine datasets ---
df_combined <- bind_rows(df_VHF, df_Other)
df_combined$Date <- factor(df_combined$Date, levels = month_order, ordered = TRUE)

# --- Debug: Final check for dates ---
cat("Unique dates in df_combined$Date:\n")
print(unique(df_combined$Date))
cat("Number of NA dates in df_combined$Date:\n")
print(sum(is.na(df_combined$Date)))

# --- Set Category order: VHF upper, Other PHEs lower ---
df_combined$Category <- factor(df_combined$Category,
                               levels = c("Viral Haemorrhagic Fevers", 
                                          "Other Priority Public Health Emergencies"),
                               ordered = TRUE)

# --- Faceted heatmap with controlled order ---
p <- ggplot(df_combined, aes(x = Date, y = Disease, fill = Confirmed_Cases)) +
  geom_tile(color = "white", size = 0.9) +
  geom_text(aes(label = Confirmed_Cases), color = "#000000", size = 5, face="bold") +
  scale_fill_gradientn(
    colors = c("#DEE", "pink", "#FF3333"),   # low → mid → high
    values = scales::rescale(c(0, 10, max(df_combined$Confirmed_Cases, na.rm = TRUE))),
    labels = scales::number_format(accuracy = 2)
  ) +
  theme_light(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 15),
    axis.text.y = element_text(face = "bold", size = 15),
    strip.text.x = element_text(face = "bold", size = 22),
    strip.text.y = element_text(face = "bold", size = 22),
    axis.title.x = element_text(size = 18),
    legend.position = "right"
  ) +
  labs(
    title = "",
    x = "Incident Month during 2024/25 FY",
    y = "",
    fill = "Key"
  ) +
  facet_wrap(~ Category, scales = "free_y", ncol = 1)

# --- Print and save plot --- 
 print(p) 

 ggsave("phe_heatmap.png", p, width = 14, height = 10)

