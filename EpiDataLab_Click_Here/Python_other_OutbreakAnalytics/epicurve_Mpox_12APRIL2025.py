import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator  # To ensure integer-only y-axis ticks

# Define the dates and the corresponding number of cases and deaths
data2 = {
    "Date": [
        "2024-10-11", "2024-10-29", "2024-11-09", "2024-11-12", "2024-11-13",
        "2024-11-15", "2024-12-02", "2024-12-08", "2024-12-19", "2024-12-29",
        "2025-01-04", "2025-01-07", "2025-01-17", "2025-01-18", "2025-01-29",
        "2025-02-19", "2025-02-20", "2025-02-26", "2025-02-28", "2025-03-08",
        "2025-03-13", "2025-03-19", "2025-03-23", "2025-03-26", "2025-04-01", "2025-04-02", "2025-04-04",
        "2025-04-10","2025-04-18","2025-04-24", "2025-04-26","2025-05-02",
        "2025-05-06"
    ],
    "Cases": [1, 1, 1, 1, 1, #5
              1, 1, 1, 1, 4, #8
              2, 1, 2, 1, 2, #8
              2, 1, 1, 2, 3, #8
              2, 2, 0, 1, 0, 1, 3, #9
              3, 2, 1, 3, 1, #11
              0],
    "Deaths": [0] * 22 + [1] + [0] + [1] + [0] * 8  # Deaths occur only on 2025-03-23 and 2025-04-01. #on adding No of Deaths, #It begins with 22 zeros (no deaths for the first 22 days).Followed by:
    # 1 for 2025-03-23 (indicating one death on this day).
    # 0 for the next day with no deaths.
    # 1 for 2025-04-01 (indicating one death on this day).
    # [0, 0, 0]for the final 3 days with no deaths.
}

# Create a DataFrame and ensure dates are in datetime format
df = pd.DataFrame(data2)
df["Date"] = pd.to_datetime(df["Date"])

# Add missing dates to ensure the last day is inclusive
final_date = pd.to_datetime("2025-05-06")
all_dates = pd.date_range(start=df["Date"].min(), end=final_date)
df = df.set_index("Date").reindex(all_dates, fill_value=0).reset_index()
df.rename(columns={"index": "Date"}, inplace=True)

# Add a 7-day moving average column
df["MovingAvg"] = df["Cases"].rolling(window=7, min_periods=1).mean()

# Plot x-axis labels as formatted strings for all dates
df["DateFormatted"] = df["Date"].dt.strftime("%d %b").str.upper()

# Create the plot (Epi curve)
fig, ax = plt.subplots(figsize=(15, 9))  # Adjust figure size as needed

# Plot blue bars for Cases
ax.bar(
    df["DateFormatted"],
    df["Cases"],
    color="#124452",  # Striking blue #199dba, #124452
    width=10.5,
    edgecolor="#4FC",  # Darker shade for contrast #115f74 #20a4d8
    zorder=3,
    label="Cases (N)",
)

# Plot red bars for Deaths
ax.bar(
    df["DateFormatted"],
    df["Deaths"],
    color="#FF4B5C",  # Striking red for deaths,  replaced #E63946
    width=10,
    edgecolor="#E63946",  # Darker shade for contrast #a11b34
    zorder=3,
    label="Deaths (n)",
)

# Plot the 7-day moving average as a single continuous blue line
ax.plot(
    df["DateFormatted"],
    df["MovingAvg"],
    color="grey",  # Lighter blue line for the average #20a4d8
    linewidth=3.5,
    label="7-Day Moving Avg",
    zorder=20,
)

# Adjust x-axis ticks for improved readability
step = 7  # Display every 7th day
ax.set_xticks(df["DateFormatted"][::step])
ax.set_xlim(df["DateFormatted"].iloc[0], df["DateFormatted"].iloc[-1])

# Customize the plot appearance
ax.set_facecolor("white")  # Set the plot background to white
fig.patch.set_facecolor("white")  # Set the overall figure background to white
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.spines["left"].set_linewidth(0.8)  # Add thin left and bottom spines
ax.spines["bottom"].set_linewidth(0.8)
ax.grid(axis="y", color="lightgrey", linestyle="--", linewidth=0.28, zorder=0)

# Add title, labels, and legend
ax.set_title("Mpox Epi Curve, 06MAY2025.                             | N=49,  n=02 |.",
             fontsize=22, color="#333333", fontweight="bold", loc="left", pad=12)
ax.set_xlabel("Date Confirmed ", fontsize=14, color="#333333", fontweight="bold", labelpad=9)
ax.set_ylabel("Case Load & Deaths counts", fontsize=14, labelpad=9)
ax.legend(fontsize=16, loc="upper left")

# Format x-axis and y-axis ticks
plt.xticks(rotation=-77, fontsize=18, fontweight="bold")  # Rotate x-axis labels for better readability
plt.yticks(fontsize=18, fontweight="bold")
ax.yaxis.set_major_locator(MaxNLocator(integer=True))  # Ensure y-axis displays integers only

# Layout adjustments
plt.tight_layout()

# Show the plot
plt.show()

