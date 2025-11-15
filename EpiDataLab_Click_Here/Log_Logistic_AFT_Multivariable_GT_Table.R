# Clear the workspace
rm(list = ls())

# Load required libraries
library(dplyr)
library(gt)
library(readxl)

# -----------------------------
# Data: Multivariable Log-Logistic AFT
# -----------------------------
aft_multi_data <- data.frame(
  Variable = c(
    "Sex",
    "  Female", "  Male",
    "Source of death alert",
    "  Community", "  Health facility",
    "Cause of death",
    "  Documented", "  Not documented"
  ),
  `aTR  (95% CI)` = c(
    "",
    "1", "1.0  (1.0 1.1)",
    "",
    "1", "1.0  (1.0 1.0)",
    "",
    "1", "1.05  (1.0 1.1)"
  ),
  `P-value` = c(
    "",
    "1", "0.148",
    "",
    "1", "0.894",
    "",
    "1", "0.046**"
  ),
  check.names = FALSE
)

# -----------------------------
# GT Table
# -----------------------------
aft_multi_table <- aft_multi_data %>%
  gt() %>%
  
  # Title
  tab_header(
    title = md("**Table 3: Multivariable -       Predictors of Delay in Postmortem Sample Collection (Log-Logistic AFT Model)**")
  ) %>%
  
  # Column labels
  cols_label(
    Variable = md("**Variable**"),
    `aTR  (95% CI)` = md("**aTR (95% CI)**"),
    `P-value` = md("**P-value**")
  ) %>%
  
  # Spanner over time-ratio columns (to mirror your layout note)
  tab_spanner(
    label = md("**Time Ratios (in days)**"),
    columns = c(`aTR  (95% CI)`, `P-value`)
  ) %>%
  
  # Alignment
  cols_align(
    align = "center",
    columns = c(`aTR  (95% CI)`, `P-value`)
  ) %>%
  
  # Bold main variable rows (no leading space)
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = !grepl("^\\s", Variable)
    )
  ) %>%
  
  # Indent subcategory rows (start with a space)
  tab_style(
    style = cell_text(indent = px(29)),
    locations = cells_body(
      columns = Variable,
      rows = grepl("^\\s", Variable)
    )
  ) %>%
  
  # Column-label styling
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  
  # Row striping & compactness
  tab_options(
    row.striping.include_table_body = TRUE,
    row.striping.background = "#f8f9fa",
    table.font.size = "100%",
    data_row.padding = px(4.6)
  ) %>%
  
  # Column widths
  cols_width(
    Variable ~ px(200),
    `aTR  (95% CI)` ~ px(200),
    `P-value` ~ px(170)
  ) %>%
  
  # Source note / footnote
  tab_source_note(
    source_note = md(" _CI = Confidence Interval; AFT = Accelerated Failure Time; **aTR** = Adjusted Time Ratio (reference category has aTR = 1)_")
  ) %>%
  
  # Optional: Roboto font (keep if available in your environment)
  opt_table_font(
    font = google_font("Roboto")
  )

# Render table
aft_multi_table

