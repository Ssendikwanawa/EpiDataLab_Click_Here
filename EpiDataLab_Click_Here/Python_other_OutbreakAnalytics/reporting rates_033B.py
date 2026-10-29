import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Load the data (adjust file path accordingly)
file_path = "D://Ssendi//Week14//ReportingratesWk14.xlsx"
data = pd.read_excel(file_path)

# Reshape data
data_long = data.melt(id_vars=["District"], var_name="Metric", value_name="Percentage")

# Define colors
colors = {"Completeness": "#3E2F68", "Timeliness": "#20a4d8"} ## #124452, ##20a4d8, #B2E2E2, neon yellow: #F7FF00, #3C3C3C DARK GREY

# Create the plot
plt.figure(figsize=(12, 6))
sns.barplot(data=data_long, x="District", y="Percentage", hue="Metric", palette=colors)

# Add threshold line
plt.axhline(y=80, linestyle="dashed", color="lightgreen", linewidth=1.5)
plt.text(len(data["District"]) -11, 80, "MOH Target", color="green", fontsize=9, fontweight='bold')

# Customize axes and labels
plt.title("Reporting Rates", fontsize=26, color="black", fontweight='bold', loc="left")
plt.xlabel("", fontsize=22, color="black", fontweight='bold')
plt.ylabel("Rates in percentage", fontsize=12, color="black", fontweight='bold')

# Ensure y-axis starts above zero
plt.ylim(bottom=1)

# Rotate x-axis labels 90 degrees and set font size to 22
plt.xticks(rotation=-12, ha='center', fontsize=15, color="black")

# Display data labels
for p in plt.gca().patches:
    if p.get_height() > 0:  # Only annotate non-zero bars
        plt.gca().annotate(f'{p.get_height():.0f}',
                           (p.get_x() + p.get_width() / 2., p.get_height()),
                           ha='center', va='bottom', color='#333533', fontsize=10, fontweight='bold')

# Show the plot
plt.show()


