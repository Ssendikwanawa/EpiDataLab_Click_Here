# Only run once
install.packages("extrafont")
library(extrafont)

# Import all system fonts
font_import(prompt = FALSE)

# Register fonts for Windows devices
loadfonts(device = "win")
