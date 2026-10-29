import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

# Sample predicted alert data
data = {
    "Early_Alerts": [5.6, 5.2, 4.7, 4.3, 3.8, 3.3, 2.9, 2.4, 1.9, 1.5, 1.0],
    "Ring_Alerts": [1.4, 1.9, 2.5, 3.0, 3.5, 4.1, 4.6, 5.1, 5.7, 6.2, 6.8],
    "ACS_Alerts": [2.4, 2.3, 2.3, 2.2, 2.1, 2.0, 1.9, 1.8, 1.8, 1.7, 1.6]
}

# Reshape data to long format
df = pd.DataFrame(data)
df_long = df.melt(var_name="Intervention", value_name="Alerts")

# Define custom palette
palette = {
    "Early_Alerts": "#1d3557",  # Catchy Blue
    "Ring_Alerts": "#2a9d8f",   # Catchy Green
    "ACS_Alerts": "#e63946"     # Catchy Red
}

# Create the violin plot
plt.figure(figsize=(10, 6))

# Violin plot with matching outline and fill color
sns.violinplot(
    data=df_long,
    x="Intervention",
    y="Alerts",
    inner=None,
    linewidth=0,
    palette=palette
)

# Strip plot to overlay all dots
sns.stripplot(
    data=df_long,
    x="Intervention",
    y="Alerts",
    color="black",
    palette=palette,
    size=6,
    jitter=0.3,
    dodge=False,
    alpha=0.9,
    linewidth=0
)

# Clean up legend
plt.legend([], [], frameon=False)

# Styling
plt.title("Violin Plot Showing Distribution and Spread of Predicted Alerts by Intervention Type", fontsize=14)
plt.ylabel("Number of Alerts")
plt.xlabel("")
plt.grid(axis='y', linestyle='--', alpha=0.6)
plt.tight_layout()
plt.show()

