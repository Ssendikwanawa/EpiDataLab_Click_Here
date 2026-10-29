import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator  # To ensure integer-only y-axis ticks

# Define the dates and the corresponding number of cases
data2 = {
    "Date": [
        "2024-10-11", "2024-10-29", "2024-11-09", "2024-11-12", "2024-11-13",
        "2024-11-15", "2024-12-02", "2024-12-08", "2024-12-19", "2024-12-29",
        "2025-01-04", "2025-01-07", "2025-01-17", "2025-01-18", "2025-01-30",
        "2025-02-15" ],
    "Cases": [1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 2, 1, 2, 1, 2, 0]
}
# Create a DataFrame and ensure dates are in datetime format
df = pd.DataFrame(data2)
df["Date"] = pd.to_datetime(df["Date"])

# Add missing dates to ensure the last day is
final_date = pd.to_datetime("2025-02-15")
all_dates = pd.date_range(start=df["Date"].min(), end=final_date)
df = df.set_index("Date").reindex(all_dates, fill_value=0).reset_index()
df.rename(columns={"index": "Date"}, inplace=True)

# Add a 7-day moving average column
df["MovingAvg"] = df["Cases"].rolling(window=7, min_periods=1).mean()

# Plot x-axis labels as formatted strings for all dates
df["DateFormatted"] = df["Date"].dt.strftime("%d %b").str.upper()

# Create the plot (Epi curve)
fig, ax = plt.subplots(figsize=(12, 7))  # Adjust figure size as needed

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
    color="#1d9bbf",
    width=6,
    edgecolor="#20a4d8",
    zorder=0.5,
    label="Cases"
)

# Plot the 7-day moving average as a single continuous red line
ax.plot(
    df["DateFormatted"],
    df["MovingAvg"],
    color="#E63946",
    linewidth=5,
    label="7-Day Moving Avg",
    zorder=6
)
# Add annotation for the specific date (24 FEB)
annotation_date = pd.to_datetime("2025-01-24")  # You can adjust this to match 24 FEB
annotation_point = df.loc[df["Date"] == annotation_date, "Cases"].values[0]
annotation_index = df.loc[df["Date"] == annotation_date, "DateFormatted"].values[0]

# Adjust x-axis ticks to improve readability
step = 7  # Display every 6th day
ax.set_xticks(df["DateFormatted"][::step])
ax.set_xlim(df["DateFormatted"].iloc[0], df["DateFormatted"].iloc[-1])

# Customize the plot appearance
ax.set_facecolor("black")  # Set the plot background to white
fig.patch.set_facecolor("black")  # Set the overall figure background to white
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.spines["left"].set_linewidth(0.8)  # Add thin left and bottom spines
ax.spines["bottom"].set_linewidth(0.8)
ax.spines["left"].set_color("white") # set left spine color white for visibility
ax.spines["bottom"].set_color("white")
ax.tick_params(axis="x", colors="white", labelsize=14, labelrotation=90)
ax.tick_params(axis="y", colors="white", labelsize=14)
ax.grid(axis="y", color="lightgrey", linestyle="--", linewidth=1, zorder=0)

# Add title, labels, and legend
ax.set_title("Mpox Epi Curve, Lango Sub-region. | (n=21) |", fontsize=33, fontweight="bold", pad=12,
             color="skyblue", loc="left")
ax.set_xlabel("", fontsize=38, labelpad=9, color="white")
ax.set_ylabel("Case load", fontsize=20, labelpad=9, color="white")
ax.legend(fontsize=14, loc="upper right", facecolor="white", edgecolor="white", framealpha=1, frameon=True,)

# Format x-axis and y-axis ticks
plt.xticks(rotation=90, fontsize=14, color="white")  # Rotate x-axis labels for better readability
plt.yticks(fontsize=19, color="white")
ax.yaxis.set_major_locator(MaxNLocator(integer=True))  # Ensure y-axis displays integers only

# Layout adjustments
plt.tight_layout()

# Show the plot
plt.show()


