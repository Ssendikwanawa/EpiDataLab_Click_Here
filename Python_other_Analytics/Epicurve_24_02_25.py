import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator  # To ensure integer-only y-axis ticks

# Define the dates and the corresponding number of cases
data2 = {
    "Date": [
        "2024-10-11", "2024-10-29", "2024-11-09", "2024-11-12", "2024-11-13",
        "2024-11-15", "2024-12-02", "2024-12-08", "2024-12-19", "2024-12-29",
        "2025-01-04", "2025-01-07", "2025-01-17", "2025-01-18", "2025-01-29",
        "2025-02-19", "2025-02-20", "2025-02-26", "2025-02-28", "2025-03-08", #(2dokolo, 1LC, 1LC, 1LC,2Kole
        "2025-03-13", "2025-03-19", "2025-03-26", "2025-04-02", "2025-04-04", #(1LC, 1kole aHCW,1 Lira dist, 1 Lira District,  lira dist
        "2025-03-21" ],
    "Cases": [1, 1, 1, 1, 1,
              1, 1, 1, 1, 4,
              2, 1, 2, 1, 2,
              2, 1, 1, 2, 2,
              2, 2, 1, 1, 2,
              0]
}

# Create a DataFrame and ensure dates are in datetime format
df = pd.DataFrame(data2)
df["Date"] = pd.to_datetime(df["Date"])

# Add missing dates to ensure the last day is
final_date = pd.to_datetime("2025-03-25")
all_dates = pd.date_range(start=df["Date"].min(), end=final_date)
df = df.set_index("Date").reindex(all_dates, fill_value=0).reset_index()
df.rename(columns={"index": "Date"}, inplace=True)

# Add a 7-day moving average column
df["MovingAvg"] = df["Cases"].rolling(window=7, min_periods=1).mean()

# Plot x-axis labels as formatted strings for all dates
df["DateFormatted"] = df["Date"].dt.strftime("%d %b").str.upper()

# Create the plot (Epi curve)
fig, ax = plt.subplots(figsize=(14, 8))  # Adjust figure size as needed

# Plot the bar chart for cases,
# Other colors to choose dodgerblue, cornflowerblue, royalblue, mediumblue,
# navy, darkblue, cadetblue, mediumturquoise, lightseagreen,
#     "#333533", #073642, #333333, #586e754,
# 199dba,   best of the best blue colors
# 1d9bbf,
# 20a4d8,
############### Recommended Red Colors:Deep Red**: Strong and attention-catching
 #### #E63946`
### Bright and Vibrant Red**: Bold and stands out
### #FF4B5C`
####Muted/Soft Red**: Softer for a professional look
### #DC5C5C`
##### Rich Burgundy Red**: Elegant and deep
#### #A11B34`
#### #Crimson Red**: Balanced intensity
#### #D72638`
ax.bar(
    df["DateFormatted"],
    df["Cases"],
    color="#e04343",#replaced with #199dba a blue catchy one.  #e04343 is another red, #d22b2b
    width=10,
    edgecolor="#d22b2b",
    zorder=0.05,
    label="Cases"
)

# Plot the 7-day moving average as a single continuous red line
ax.plot(
    df["DateFormatted"],
    df["MovingAvg"],
    color="#20a4d8",##DC5C5C
    linewidth=3,
    label="7-Day Moving Avg",
    zorder=20
)
# Adjust x-axis ticks to improve readability
step = 7  # Display every 6th day
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
ax.set_title("Epi Curve.   [n=33]", fontsize=33,color="#333333", fontweight="bold", loc="left", pad=12)
ax.set_xlabel("", fontsize=35, color="Darkgrey", labelpad=9)
ax.set_ylabel("Case load", fontsize=18, labelpad=9)
ax.legend(fontsize=14, loc="upper right")

# Format x-axis and y-axis ticks
plt.xticks(rotation=90, fontsize=18 , fontweight="bold")  # Rotate x-axis labels for better readability
plt.yticks(fontsize=18 , fontweight="bold")
ax.yaxis.set_major_locator(MaxNLocator(integer=True))  # Ensure y-axis displays integers only

# Layout adjustments
plt.tight_layout()

# Show the plot
plt.show()
