rm(list = ls())
library(readxl)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

# Set file path (ensure correct escape slashes)
file_path <- "D:\\Ssendi\\Week14\\Priority_diseases_Wk14.xls"

# Load Excel data
df <- read_excel(file_path)

# Transpose: Drop 'District' column, use it as colnames, transpose
df_transposed <- as.data.frame(t(df[,-1]))
colnames(df_transposed) <- df$District
df_transposed$Disease <- rownames(df_transposed)

# Reshape to long format
df_long <- melt(df_transposed, id.vars = "Disease", variable.name = "District", value.name = "Count")

# Draw the heatmap
ggplot(df_long, aes(x = District, y = Disease, fill = Count)) +
  geom_tile(color = "white", size = 0.9) +
  geom_text(aes(label = Count), color = "black", size = 6) +
  scale_fill_gradient(low = "#deebf7", high = "#08519e") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 18),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 25, face = "bold"),
    plot.caption = element_text(face = "italic", size = 12),
    plot.margin = margin(10, 10, 40, 10)
  ) +
  labs(
    title = "Epidemic Prone Diseases Reported in Wk14",
    x = "", y = "", caption = ""
  )

