import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
# Import file, bring file path
file_path= "D:\\Ssendi\\Week14\\Priority_diseases_Wk14.xls"
#"D:\Ssendi\Week14\Priority_diseases_Wk14.xls"
# Read the Excel file into a Pandas DataFrame
df = pd.read_excel(file_path)
#
# Transpose the DataFrame to swap rows and columns
# This operation sets the first column (Districts) as the index for better visualization
df = df.set_index("District").transpose()

# Create the heatmap using Seaborn
plt.figure(figsize=(12, 8))  # Set figure size
sns.heatmap(df, annot=True, fmt="d", cmap="Blues",
            cbar=False, linewidths=0.9, annot_kws={"size":18,"color": "black"}) # Diverging color scales such as
# `"coolwarm"`, `"seismic"`, `"RdBu" (Red-Blue)`,
# `"PiYG" (Pink-Green)`, `"PuOr" (Purple-Orange)`, and `"bwr" (Blue-White-Red)` are ideal for visualizing data that has both positive and negative values or a clear midpoint,
# while sequential color scales like `"Blues"`, `"Reds"`, `"Greens"`, `"Purples"`, `"Oranges"`, `"Greys"`, `"YlOrRd" (Yellow-Orange-Red)`, `"YlGn" (Yellow-Green)`, and `"BuPu" (Blue-Purple)`
# are best suited for data with a clear progression from smaller to larger values; additionally, perceptually uniform scales such as `"viridis" (Dark blue to yellow)`, `"plasma" (Dark purple to yellow)`,
# `"inferno" (Black to orange/yellow)`, `"magma" (Black to white/orange)`, and `"cividis" (Blue to yellow - colorblind friendly)` are designed for improved perceptual uniformity.

# Add title and labels
plt.title("Epidemic Prone Diseases Reported in Wk14", fontsize=25, weight="bold")
plt.xlabel("", fontsize=12, weight="bold")  # Larger X-axis label font
plt.ylabel("", fontsize=12, weight="bold")  # Larger Y-axis label font

# Adjust tick font sizes (critical for X and Y label clarity)
plt.tick_params(axis='x', labelsize=23)  # Adjust X tick labels' font size
plt.tick_params(axis='y', labelsize=23)  # Adjust Y tick labels' font size

# Rotate the X-axis labels
plt.xticks(rotation=30, ha='right')  # Rotate with alignment

# Adjust the layout to free up space for the caption
plt.subplots_adjust(bottom=0.3)  # Adjust bottom margin to make room for the caption

# Add italic caption below the plot
plt.figtext(0.5, 0.05, "",
            ha="center", fontstyle="italic", fontsize=12, weight="light")
# Show the heatmap
plt.tight_layout()
plt.show()

