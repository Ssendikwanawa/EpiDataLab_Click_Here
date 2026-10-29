import pandas as pd
import seaborn as sns
import matplotlib.pyplot as ppa
import matplotlib.pyplot as plt

# Import file, bring file path
file_path = "D:\\Ssendi\\WK15\\Prioritydiseases_WK11.xls"
#
# Read the Excel file into a Pandas DataFrame
df = pd.read_excel(file_path)

# Transpose the DataFrame to swap rows and columns
df = df.set_index("District").transpose()

# Set default Seaborn style with a white background
sns.set_style("whitegrid")

# Create the heatmap with improvements
plt.figure(figsize=(16, 8))  # Increased width
ax = sns.heatmap(
    df,
    annot=True,
    fmt="d",
    cmap="Blues",
    cbar=False,
    linewidths=1.9,
    annot_kws={"size": 14, "color": "#073642"},
)

# Customize the title for readability
plt.title("Epidemic Prone Diseases, Wk11.", fontsize=26, weight="bold", color="black", loc="left")

# Set x-axis and y-axis labels (keep empty but styled)
plt.xlabel("", fontsize=8, weight="bold", color="black")
plt.ylabel("", fontsize=8, weight="bold", color="black")

# Adjust tick colors for better readability
plt.tick_params(axis="x", labelsize=18, colors="#333533")  # X-axis tick labels
plt.tick_params(axis="y", labelsize=22, colors="#333533")  # Y-axis tick labels

# Rotate the X-axis labels for better alignment
plt.xticks(rotation=20, ha="right")  # Rotate and align labels

# Ensure Y-axis labels are horizontal
plt.yticks(rotation=0, color="#333533")  # Set Y-axis labels to horizontal

# Adjust layout to prevent overlapping
plt.tight_layout()

# Show the plot
plt.show()
