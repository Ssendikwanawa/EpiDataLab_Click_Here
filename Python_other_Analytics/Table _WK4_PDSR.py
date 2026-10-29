import matplotlib.pyplot as plt
import pandas as pd

# Data for the table
data = {
    "District": [
        "Alebtong", "Amolatar", "Apac", "Dokolo", "Kole",
        "Kwania", "Lira City", "Lira", "Otuke", "Oyam"
    ],
    "Macerated\nStill Births": [0, 0, 0, 0, 0, 0, 2, 0, 0, 0],  # Wrapped text
    "Fresh\nStill Births": [0, 0, 1, 0, 0, 0, 1, 0, 1, 0],  # Wrapped text
    "Early\nNeonatal Deaths": [0, 0, 2, 1, 0, 0, 4, 0, 0, 1],  # Wrapped text
}

# Convert data to a DataFrame
df = pd.DataFrame(data)

# Create the figure and axis
fig, ax = plt.subplots(figsize=(6, 3))  # Compact table size

# Hide the axis (we only want the table)
ax.axis("tight")
ax.axis("off")

# Add the table
table = ax.table(
    cellText=df.values,  # Table values (data)
    colLabels=df.columns,  # Column headers with wrapped text
    cellLoc="center",  # Center-align all text
    loc="center",  # Center the table on the figure
)

# Styling the table
table.auto_set_font_size(False)
table.set_fontsize(9)  # Smaller font size for data cells
table.auto_set_column_width(col=list(range(len(df.columns))))  # Adjust column widths dynamically

# Make the column headers bigger and distinct
for (row, col), cell in table.get_celld().items():
    if row == 0:  # Header row
        cell.set_fontsize(12)  # Larger font size for headers
        cell.set_text_props(weight="bold")  # Bold text
        cell.set_facecolor("#4A7EBB")  # Blue background for headers
        cell.set_text_props(color="white")  # White font for better contrast
    elif row % 2 == 1:  # Odd rows (data rows)
        cell.set_facecolor("#d9e2f3")  # Light blue background
    else:  # Even rows (data rows)
        cell.set_facecolor("#f2f7fc")  # Very light blue background

# Add a bold title to the table
ax.set_title(
    "Perinatal Deaths during Wk4",
    fontsize=11,
    weight="bold",
    pad=-1,
    color="navy",
)

# Adjust layout to fit nicely
plt.tight_layout()

# Show the table
plt.show()
