import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator

# Define the dates and corresponding number of alerts
data2 = {
    "Date": [
         "2025-02-07", "2025-02-08",
        "2025-02-09", "2025-02-10", "2025-02-11", "2025-02-12", "2025-02-13",
        "2025-02-14", "2025-02-15", "2025-02-16", "2025-02-17", "2025-02-18"
    ],
    "Alerts": [13, 17,
               12, 24, 32, 33, 60,
               80, 50, 122, 139, 62]
}

# Create DataFrame and convert Date column to datetime format
df = pd.DataFrame(data2)
df["Date"] = pd.to_datetime(df["Date"])

# Format x-axis labels
df["DateFormatted"] = df["Date"].dt.strftime("%d %b").str.upper()

# Get the first and last dates for proper limits
first_alert_date = df["DateFormatted"].iloc[0]  # Ensure first date is included
last_alert_date = df["DateFormatted"].iloc[-1]  # Ensure last date is included

# Create the plot
fig, ax = plt.subplots(figsize=(12, 6))

# Plot the bar chart for alerts
bars = ax.bar(
    df["DateFormatted"], df["Alerts"],
    color="#1d9bbf", edgecolor="#20a4d8", width=0.8, label="Daily Alerts"
)

# Add data labels
for bar in bars:
    height = bar.get_height()
    if height > 0:  # Only label non-zero bars
        ax.text(
            bar.get_x() + bar.get_width() / 2,  # Center horizontally
            height - 0.1,  # Slightly below the top of the bar
            f"{int(height)}", fontsize=14, zorder=20,# Convert to integer
            #ha="center", va="top", fontsize=12, color="gold", fontweight="bold"
        )

# Adjust x-axis limits to fully include first and last bars
ax.set_xlim(-0.5, len(df["DateFormatted"]) - 0.5)  # Expands space to avoid cutoff

# Find the index of the date "11 FEB"
annotation_x_pos = df[df["DateFormatted"] == "11 FEB"].index[0]  # Get the index of "11 FEB"

# Annotate a key event BELOW the x-axis
ax.annotate(
    "Ring Sweep Strategy\nDeployed, (11th to 17th Feb)",
    xy=(annotation_x_pos, 1),  # Point on the bar (or x-axis) for "11 FEB"
    xytext=(annotation_x_pos, 120),  # Place the annotation text above
    arrowprops=dict(arrowstyle="->", color="#333333", lw=2),  # Customize the arrow
    fontsize=14, fontweight="bold", color="#333333",
    ha="center", va="top"
)

# Customize the plot appearance
ax.set_facecolor("white")
fig.patch.set_facecolor("white")
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.spines["left"].set_linewidth(0.8)
ax.spines["bottom"].set_linewidth(0.8)
ax.grid(axis="y", linestyle="-", linewidth=0.5, color="lightgrey")

# Title, labels, and legend
ax.set_title("Trend of Alerts Received", fontsize=25, fontweight="bold")
ax.set_xlabel("", fontsize=15)
ax.set_ylabel("Number of Alerts", fontsize=15)

# Format x-axis and y-axis
plt.xticks(rotation=90, fontsize=15)
plt.yticks(fontsize=15)
ax.yaxis.set_major_locator(MaxNLocator(integer=True))

plt.tight_layout()
plt.show()
