import matplotlib.pyplot as plt

# Data
categories = ["HIV Negative: Baby (2 months)", "HIV Negative: Adult (30 years)"]
values = [1, 1]  # death counts
colors = ["#FF9999", "#FF4B5C"]  # Softer and stronger reds

# Create horizontal bar chart
fig, ax = plt.subplots(figsize=(8, 5))
ax.barh(categories, values, color=colors, edgecolor="black")

# Add labels and title
for i, value in enumerate(values):
    ax.text(value + 0.02, i, f"{value} Death", va="center", fontsize=12, color="#333")  # Add text to bars

ax.set_title("Mpox Deaths by HIV Co-morbidity", fontsize=16, fontweight="bold", color="#333333")
ax.set_xlabel("Count of Deaths", fontsize=12)
ax.set_xlim(0, 1.5)  # Adjust x-axis for spacing
ax.set_yticks(range(len(categories)))
ax.set_yticklabels(categories, fontsize=12)
plt.tight_layout()
plt.show()
