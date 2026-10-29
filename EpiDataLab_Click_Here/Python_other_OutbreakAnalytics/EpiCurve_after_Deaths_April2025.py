import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator  # For integer y-axis ticks

# --- Define the data ---
dates = [
    "2024-10-11", "2024-10-29", "2024-11-09", "2024-11-12", "2024-11-13",
    "2024-11-15", "2024-12-02", "2024-12-08", "2024-12-19", "2024-12-29",
    "2025-01-04", "2025-01-07", "2025-01-17", "2025-01-18", "2025-01-29",
    "2025-02-19", "2025-02-20", "2025-02-26", "2025-02-28", "2025-03-08",
    "2025-03-13", "2025-03-19", "2025-03-23", "2025-03-26", "2025-04-01",
    "2025-04-02", "2025-04-04", "2025-04-06", "2025-04-07", "2025-04-11",
    "2025-04-17", "2025-04-19", "2025-04-26", "2025-04-28", "2025-04-29",
    "2025-04-30", "2025-05-01", "2025-05-05", "2025-05-09", "2025-05-12",
    "2025-05-13", "2025-05-16", "2025-05-24", "2025-06-16", "2025-06-28"
]

cases = [
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 4,
    2, 1, 2, 1, 2,
    2, 1, 1, 2, 2,
    2, 2, 1, 1, 1,
    1, 3, 1, 3, 1,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 0
]

# Assign deaths only on 2025-04-01 and 2025-04-23
death_dates = {"2025-03-23": 1, "2025-04-01": 1}
deaths = [death_dates.get(d, 0) for d in dates]

# --- Build DataFrame ---
df = pd.DataFrame({"Date": pd.to_datetime(dates), "Cases": cases, "Deaths": deaths})

# Fill in any missing dates
final_date = pd.to_datetime("2025-06-28")
all_dates = pd.date_range(start=df["Date"].min(), end=final_date)
df = df.set_index("Date").reindex(all_dates, fill_value=0).reset_index()
df.rename(columns={"index": "Date"}, inplace=True)

# Calculate 7-day moving average for cases
df["MovingAvg"] = df["Cases"].rolling(window=7, min_periods=1).mean()

# Format date labels for x-axis
df["DateFormatted"] = df["Date"].dt.strftime("%d %b").str.upper()

# --- Plot ---
fig, ax = plt.subplots(figsize=(14, 8))

# Plot cases
ax.bar(df["DateFormatted"], df["Cases"],
       color="#2A677A", edgecolor="#7EC8C7", width=10, zorder=2,    #20a4d8 ##2A677A Darkcadetblue, #7EC8C7 Bright cadetBlue
       label="Cases (N)")

# Plot deaths on top of cases
ax.bar(df["DateFormatted"], df["Deaths"],
       color="#E63946", edgecolor="#FF4B5C", width=10, zorder=30,
       label="Deaths (n)")

# Plot 7-day moving average
ax.plot(df["DateFormatted"], df["MovingAvg"],
        color="#696969", linewidth=3.5, zorder=50,
        label="7-Day Moving Avg")

# Customize x-axis
step = 7
ax.set_xticks(df["DateFormatted"][::step])
ax.set_xlim(df["DateFormatted"].iloc[0], df["DateFormatted"].iloc[-1])
plt.xticks(rotation=77, fontsize=18, fontweight="bold")
plt.yticks(fontsize=18, fontweight="bold")
ax.yaxis.set_major_locator(MaxNLocator(integer=True))

# Styling
ax.set_facecolor("white")
fig.patch.set_facecolor("white")
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.spines["left"].set_linewidth(0.8)
ax.spines["bottom"].set_linewidth(0.8)
ax.grid(axis="y", color="lightgrey", linestyle="--", linewidth=0.28, zorder=0)

# Titles and labels
ax.set_title("Mpox Epi Curve, 27Jun2025.                             | N=62,  n=02 |.",
             fontsize=22, color="#333333", fontweight="bold", loc="left", pad=12)
ax.set_xlabel("Date Confirmed ", fontsize=14, color="#333333", fontweight="bold", labelpad=9)
ax.set_ylabel("Case Load & Deaths counts", fontsize=14, labelpad=9)
ax.legend(fontsize=16, loc="upper left")

# Layout
plt.tight_layout()
plt.show()
