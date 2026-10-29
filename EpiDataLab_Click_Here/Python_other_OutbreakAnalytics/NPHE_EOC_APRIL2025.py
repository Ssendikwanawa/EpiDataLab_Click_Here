import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator  # To ensure integer-only y-axis ticks

# Define the dates and the corresponding number of cases
data2 = {
    "Date": [
        "2025-03-17 to 23", "2025-03-24 to 30", "2025-03-31 to 2025-04-06", "2025-04-07 to 13", "2025-04-14 to 20"
    ],
    "Reported Cases": [1, 7, 14, 21, 31],
    "ICU Admision": [0, 0, 0, 4, 6],
    "Reported Deaths": [0, 0, 1, 2, 4]
}

# Create a DataFrame
df = pd.DataFrame(data2)

# Keep the original "Date" as a display column without year
df["DateFormatted"] = df["Date"].apply(lambda x: x[5:].replace("2025-", ""))  # Remove the year

# Plot (Epi curve)
fig, ax = plt.subplots(figsize=(14, 8))

# Plot bars for each dataset
ax.bar(
    df["DateFormatted"],
    df["Reported Cases"],
    color="#354267",  # Gray for cases ##d22b2b
    width=0.7,
    label="Reported Cases"
)
ax.bar(
    df["DateFormatted"],
    df["ICU Admision"],
    color="#199dba",  # Blue for ICU admissions
    width=0.7,
    label="ICU Admissions"
)
ax.bar(
    df["DateFormatted"],
    df["Reported Deaths"],
    color="#d22b2b",  # Red for deaths
    width=0.7,
    label="Reported Deaths"
)

# Customize the plot
ax.set_facecolor("white")
fig.patch.set_facecolor("white")
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.spines["left"].set_linewidth(0.8)
ax.spines["bottom"].set_linewidth(0.8)
ax.grid(axis="y", color="lightgrey", linestyle="--", linewidth=0.5)

# Add title, labels, and legend
ax.set_title("Epi Curve as of 22 April 2025, Country X", fontsize=20, color="#333333", fontweight="bold", loc="left",
             pad=12)
ax.set_xlabel("Weekly Date Range", fontsize=16, color="Grey", labelpad=9)
ax.set_ylabel("Count", fontsize=16, labelpad=9)
ax.legend(fontsize=14, loc="upper left", frameon=False)

# Format x-axis and y-axis ticks
plt.xticks(rotation=45, fontsize=12, fontweight="bold")
plt.yticks(fontsize=12, fontweight="bold")
ax.yaxis.set_major_locator(MaxNLocator(integer=True))

# Adjust layout and show the plot
plt.tight_layout()
plt.show()
