import matplotlib.pyplot as plt

# Data for the pie chart
labels = ["HIV Negative: Baby (2 months)", "HIV Negative: Adult (30 years)"]
sizes = [50, 50]  # Each contributes 50% because there are 2 deaths
colors = ["#FF9999", "#FF4B5C"]  # Soft and striking red tones for emphasis

# Create a pie chart
fig, ax = plt.subplots(figsize=(8, 8))
ax.pie(
    sizes,
    labels=labels,
    colors=colors,
    autopct="%1.1f%%",  # Show percentage
    textprops={"fontsize": 14},  # Font size for better readability
    startangle=90,  # Start angle for better orientation
    wedgeprops={"edgecolor": "black", "linewidth": 1}  # Add edge to pie slices
)

# Add title to pie chart
ax.set_title("Mpox Deaths by HIV Co-morbidity", fontsize=16, fontweight="bold", color="#333333")

# Equal aspect ratio ensures the pie is drawn as a circle
ax.axis("equal")

# Show the chart
plt.tight_layout()
plt.show()
