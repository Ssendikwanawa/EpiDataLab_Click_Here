import matplotlib.pyplot as plt
from matplotlib.patheffects import withStroke

# Data for the timeline
dates = ["2025-01-25", "2025-01-25", "2025-01-26", "2025-01-27", "2025-01-28"]
locations = [
    "Traveled From Kampala\nto Mbale",
    "Reached Mbale & Visited a\nTraditional Healer",
    "Sought Treatment at\nMt. Elgon Hospital",
    "Admitted to Mbale RRH\nat 1:00AM EAT",
    "Referred To Mulago\nNational Referral Hospital"
]

# Create a figure and axis
fig, ax = plt.subplots(figsize=(16, 7))

# Draw the horizontal timeline line with an arrow at the end
ax.plot([0, len(dates) - 0.5], [0, 0], color="deepskyblue", linewidth=14, zorder=16)
ax.annotate(
    '', xy=(len(dates) + 0.5, 0), xytext=(0, 0),
    arrowprops=dict(color="deepskyblue", lw=14, mutation_scale=3, zorder=16)
)
# Add events branching off the timeline
for i, (date, location) in enumerate(zip(dates, locations)):
    if i % 2 == 0:  # Labels above the timeline
        # Arrow pointer above the timeline (reduced space)
        ax.annotate(
            '', xy=(i, 0.15), xytext=(i, 0),
            arrowprops=dict(arrowstyle='->', color="gray", lw=3)
        )
        # Text with box styling above (increased font size)
        text = ax.text(
            i, 0.22, f"**{date}**\n{location}", ha="center", va="bottom",
            fontsize=15, color="black",  # Increased text size
            bbox=dict(boxstyle="round,pad=0.5", facecolor="lightblue", edgecolor="royalblue", alpha=0.95)
        )

    else:  # Labels below the timeline
        # Arrow pointer below the timeline (reduced space)
        ax.annotate(
            '', xy=(i, -0.15), xytext=(i, 0),
            arrowprops=dict(arrowstyle='->', color="gray", lw=3)
        )
        # Text with box styling below (increased font size)
        text = ax.text(
            i, -0.22, f"**{date}**\n{location}", ha="center", va="top",
            fontsize=13, color="black",  # Increased text size
            bbox=dict(boxstyle="round,pad=0.5", facecolor="lightcoral", edgecolor="firebrick", alpha=0.95)
        )

# Customize appearance
ax.set_xlim(-0.2, len(dates) + 0.5)  # Extend x-axis for better spacing
ax.set_ylim(-0.6, 0.6)  # Adjust y-axis limits for reduced space
ax.axis("off")  # Remove axes for a cleaner look

# Add chart title
plt.title("Ebola Patient's Movement Timeline", fontsize=20, color="darkblue", fontweight="bold")

# Optimize layout and show plot
plt.tight_layout()
plt.show()
