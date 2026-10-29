import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator

# === Data Setup ===
data = {
    "Date": [
        "31-Jan-2025", "07-Feb-2025", "09-Feb-2025", "10-Feb-2025", "11-Feb-2025",
        "15-Feb-2025", "17-Feb-2025", "19-Feb-2025", "20-Feb-2025", "21-Feb-2025",
        "22-Feb-2025", "25-Feb-2025", "27-Feb-2025", "01-Mar-2025", "03-Mar-2025",
        "04-Mar-2025", "05-Mar-2025", "06-Mar-2025", "08-Mar-2025", "09-Mar-2025",
        "10-Mar-2025", "12-Mar-2025", "14-Mar-2025", "31-Mar-2025",
        "22-Apr-2025", "25-Apr-2025",
    ],
    "Cases": [2, 1, 1, 3, 1, 1, 1, 1, 4, 5, 1, 1, 4, 3, 2, 2, 1, 1, 1,2,1,3,1,1,
              3, 2]
}
df = pd.DataFrame(data)
df["Date"] = pd.to_datetime(df["Date"], format="%d-%b-%Y")

# Fill full date range
all_dates = pd.date_range("2025-01-15", "2025-05-06")
df = df.set_index("Date").reindex(all_dates, fill_value=0).reset_index()
df.columns = ["Date", "Cases"]

# === Plotting ===
fig, ax = plt.subplots(figsize=(14, 8))
ax.bar(df["Date"], df["Cases"], color="#199dba", edgecolor="#199", width=4, label="Cases")

# X-axis formatting
step = 5
ax.set_xticks(df["Date"][::step])
ax.set_xticklabels([d.strftime("%d %b") for d in df["Date"][::step]], rotation=45, fontsize=16)

# === Annotations ===
annotations = {
    "2025-01-31": "Rumor\nverification",
    "2025-03-01": "Outbreak\nConfirmed in Lira District",
    "2025-04-25": "Investigation\nKwania District",
    "2025-05-02": "Outbreak\nConfirmed-Kwania District"
}
for date_str, text in annotations.items():
    date = pd.to_datetime(date_str)
    if date in df["Date"].values:
        cases = df.loc[df["Date"] == date, "Cases"].values[0]

        # Coordinates
        box_y = cases + 1.44  # vertical position for the text box
        arrow_end_y = box_y - 0.25  # just touching the box bottom
        arrow_start_y = cases + 0.2  # top of the bar

        # Annotation text box
        ax.text(
            date, box_y, text,
            ha="center", va="bottom",
            fontsize=10, color="black",
            bbox=dict(boxstyle="round,pad=0.4", edgecolor="black", facecolor="lightyellow")
        )

        # Arrow just touching the box (not deep inside)
        ax.annotate(
            "",
            xy=(date, arrow_start_y),
            xytext=(date, arrow_end_y),
            arrowprops=dict(arrowstyle="->", color="black", lw=1.5)
        )

# === Titles and Layout ===
ax.set_title("Epi Curve for Measles , n=44", fontsize=22, fontweight="bold", color="#333333")
ax.set_xlabel("Date of Symptom Onset", fontsize=16, fontweight="bold")
ax.set_ylabel("Number of Cases", fontsize=16, fontweight="bold")
ax.legend(loc="upper left", fontsize=15)
ax.yaxis.set_major_locator(MaxNLocator(integer=True))
ax.grid(axis="y", color="lightgrey", linestyle="--", zorder=1)
ax.set_facecolor("white")
fig.patch.set_facecolor("white")
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
fig.tight_layout()
plt.show()
