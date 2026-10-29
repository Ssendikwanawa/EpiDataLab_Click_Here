import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator

# Define the dates and corresponding number of alerts
data2 = {
    "Date": [
         "2025-02-07", "2025-02-08",
        "2025-02-09", "2025-02-10", "2025-02-11", "2025-02-12", "2025-02-13",
        "2025-02-14", "2025-02-15", "2025-02-16", "2025-02-17", "2025-02-18", "2025-02-19", "2025-02-20",
        "2025-02-21", "2025-02-22", "2025-02-23", "2025-02-24", "2025-02-25", "2025-02-26", "2025-02-27",
        "2025-02-28", "2025-03-01", "2025-03-02", "2025-03-03", "2025-03-04", "2025-03-05", "2025-03-06", "2025-03-07"
    ],
    "Alerts": [13, 17,
               12, 24, 32, 33, 60,
               80, 50, 122, 139, 62, 32, 22,
               56, 14, 8, 7, 16, 63, 86,
               150, 82, 23, 49, 64, 11, 15, 8]
}

# Create DataFrame and convert Date column to datetime format
df = pd.DataFrame(data2)
df["Date"] = pd.to_datetime(df["Date"])

# Compute the cumulative sum of alerts
df["CumulativeAlerts"] = df["Alerts"].cumsum()

# Format x-axis labels
df["DateFormatted"] = df["Date"].dt.strftime("%d %b").str.upper()

# Get first and last dates for proper limits
first_alert_date = df["DateFormatted"].iloc[0]  # Ensure first date is included
last_alert_date = df["DateFormatted"].iloc[-1]  # Ensure last date is included

# Create the plot
fig, ax = plt.subplots(figsize=(12, 6))

# Plot the bar chart for daily alerts
bars = ax.bar(
    df["DateFormatted"], df["Alerts"],
    color="#1d9bbf", edgecolor="#20a4d8", width=0.8, label="Daily Alerts"
)

# Plot cumulative alerts as a line with markers
ax.plot(
    df["DateFormatted"], df["CumulativeAlerts"],
    color="#E63946", linewidth=1, zorder=20, marker=".", markersize=10, label="Cumulative Alerts"
)

# Add data labels inside bars (for daily alerts)
for bar in bars:
    height = bar.get_height()
    if height > 0:  # Only label non-zero bars
        ax.text(
            bar.get_x() + bar.get_width() / 2,  # Center horizontally
            height - 0.1,   # Slightly below the top of the bar
            f"{int(height)}", zorder=20, # Convert to integer
            #removedthisline, ha="center", va="top", fontsize=12, color="#333333", fontweight="bold"
        )

# Add data labels above markers (for cumulative alerts)
for i, txt in enumerate(df["CumulativeAlerts"]):
    if txt > 947: # Only label texts >197
       ax.text(
        df["DateFormatted"][i], txt + 5, f"{txt}",
        ha="center", va="bottom", fontsize=9, color="#E63946", fontweight="normal"
    )

# Adjust x-axis limits to fully include first and last bars
ax.set_xlim(-0.5, len(df["DateFormatted"]) - 0.5)  # Expands space to avoid cutoff

# Find the index of the date "11 FEB"
annotation_x_pos = df[df["DateFormatted"] == "11 FEB"].index[0]  # Get the index of "11 FEB"

# Annotate a key event BELOW the x-axis
ax.annotate(
    "Ring Sweep Strategy\nDeployed, (11th to 17th Feb)",
    xy=(annotation_x_pos, 1),  # Point on the bar (or x-axis) for "11 FEB"
    xytext=(annotation_x_pos, 900),  # Place the annotation text above
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
#supported values are '-', '--', '-.', ':', 'None', ' ', '', 'solid', 'dashed', 'dashdot'

# Title, labels, and legend
ax.set_title("Trend of Alerts vs cumulative numbers",
             fontsize=20, fontweight="bold")
ax.set_xlabel("Date", fontsize=17)
ax.set_ylabel("Number of Alerts", fontsize=17)
ax.legend(fontsize=14, loc="upper left")

# Format x-axis and y-axis
plt.xticks(rotation=90, fontsize=14)
plt.yticks(fontsize=14)
ax.yaxis.set_major_locator(MaxNLocator(integer=True))

plt.tight_layout()
plt.show()
