import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Import file, bring file path
file_path = "D:\\Ssendi\\Week4\\PrioritydsesWK4.xls"

# Read the Excel file into a Pandas DataFrame
df = pd.read_excel(file_path)

# Transpose the DataFrame to swap rows and columns
df = df.set_index("District").transpose()

# Create the heatmap using Seaborn
plt.figure(figsize=(12, 8))  # Set figure size

# Set black background for the entire plot
plt.gcf().set_facecolor("black")  # Set global figure background color to black

# Create the heatmap with adjustments for text and grid visibility
sns.heatmap(df,
            annot=True,
            fmt="d",
            cmap="RdBu",
            cbar=False,
            linewidths=0.9,
            annot_kws={"size": 18, "color": "white"})  # White text for annotations

# Add title and labels with white text
plt.title("Epidemic Prone Diseases Reported in Wk4", fontsize=29, weight="bold", color="white")
plt.xlabel("", fontsize=12, weight="bold", color="white")  # Leave empty for X-axis label, white font
plt.ylabel("", fontsize=12, weight="bold", color="white")  # Leave empty for Y-axis, white font

# Adjust tick font sizes and colors for clarity on black background
plt.tick_params(axis='x', labelsize=14, labelcolor="white")  # X tick labels in white
plt.tick_params(axis='y', labelsize=14, labelcolor="white")  # Y tick labels in white

# Rotate the X-axis labels
plt.xticks(rotation=91, ha='right')  # Rotate with alignment
plt.yticks(rotation=0)  # Keep y-axis labels horizontal

# Adjust the layout for better spacing
plt.subplots_adjust(bottom=0.3)  # Adjust bottom margin to leave space for captions

# Black background for the axes (panel)
ax = plt.gca()  # Get current axes
ax.set_facecolor("black")  # Set panel background to black

# Add italic caption below the plot
plt.figtext(0.5, 0.05, "",  # Add a caption (adjust the text accordingly)
            ha="center", fontstyle="italic", fontsize=12, weight="light", color="white")

# Ensure the layout is tight and display the heatmap
plt.tight_layout()
plt.show()
