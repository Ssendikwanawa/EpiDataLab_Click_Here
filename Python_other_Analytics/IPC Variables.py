import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator

# Define the variable and corresponding scores
df = pd.DataFrame({
    "Variable": ["IPC Focal\nPerson", "Screening\nstation", "Functional\nthermoflash",
                 "Holding area\neasily identified"],
    "Scores": [85.5, 47.3, 58.2, 49.1]
})

# Create the plot
fig, ax = plt.subplots(figsize=(10, 6))  # Adjusted figure size

bars = ax.bar(
    df["Variable"],
    df["Scores"],
    color="#199dba",
    width=0.5,  # Adjusted width for better spacing
    edgecolor="#199dba",
    alpha=0.85,
    zorder=9
)

# Customize the plot appearance
ax.set_facecolor("white")
fig.patch.set_facecolor("white")
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.spines["left"].set_linewidth(0.8)
ax.spines["bottom"].set_linewidth(0.8)
ax.grid(axis="y", color="lightgrey", linestyle="--", linewidth=0.5, zorder=1)

# Add title and labels
ax.set_title("IPC Scores", fontsize=18, color="#333333", fontweight="bold", loc="left", pad=12)
ax.set_ylabel("Score", fontsize=14, color="Darkgrey", labelpad=10)
ax.set_xlabel("")

# Display values on bars
for bar in bars:
    ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 1,
            f"{bar.get_height()}", ha='center', fontsize=12, fontweight="bold")

# Format x-axis and y-axis ticks
plt.xticks(rotation=-5, fontsize=12, fontweight="bold")
plt.yticks(fontsize=12, fontweight="bold")
ax.yaxis.set_major_locator(MaxNLocator(integer=True))

# Layout adjustments
plt.tight_layout()

# Show the plot
plt.show()
