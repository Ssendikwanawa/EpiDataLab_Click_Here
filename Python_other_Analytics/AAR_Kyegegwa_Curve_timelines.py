import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MaxNLocator

# === Data Setup ===
data = {
    "Date": [
        "05/03/2025", "06/03/2025", "07/03/2025", "08/03/2025", "08/03/2025",
        "08/03/2025", "10/03/2025", "13/03/2025", "15/03/2025", "17/03/2025",
        "18/03/2025", "19/03/2025", "20/03/2025", "21/03/2025", "24/03/2025",
        "25/03/2025", "27/03/2025", "02/04/2025", "26/04/2025"
    ],
    "Intensity of Activities": [1, 1, 1, 1, 1,
                 1, 1, 1, 2, 1,
                 1, 1, 4, 1, 1,
                 1, 1, 1, 0]
}
df = pd.DataFrame(data)
df["Date"] = pd.to_datetime(df["Date"], format="%d/%m/%Y")
df = df.groupby("Date", as_index=False).sum()

# === Full Date Range ===
all_dates = pd.date_range("2025-03-01", "2025-04-26")
df = df.set_index("Date").reindex(all_dates, fill_value=0).reset_index()
df.columns = ["Date", "Intensity of Activities"]

# === Separate confirmed SUVD logic ===
df["Confirmed SUVD"] = 0
df["IsConfirmedOnly"] = False

confirmed_date = pd.to_datetime("2025-03-05")
df.loc[df["Date"] == confirmed_date, "Confirmed SUVD"] = 1
df.loc[df["Date"] == confirmed_date, "Intensity of Activities"] = 0
df.loc[df["Date"] == confirmed_date, "IsConfirmedOnly"] = True

# === Plotting ===
fig, ax = plt.subplots(figsize=(16, 9))

# Plot regular suspects
ax.bar(df.loc[~df["IsConfirmedOnly"], "Date"], df.loc[~df["IsConfirmedOnly"], "Intensity of Activities"],
       color="#199dba", edgecolor="#199", width=4, label="Intensity of Activities", zorder=1)

# Plot confirmed SUVD case in red
ax.bar(df.loc[df["IsConfirmedOnly"], "Date"], df.loc[df["IsConfirmedOnly"], "Confirmed SUVD"],
       color="red", edgecolor="red", width=4, label="Confirmed SUVD", zorder=2)

# X-axis formatting
step = 4
ax.set_xticks(df["Date"][::step])
ax.set_xticklabels([d.strftime("%d %b") for d in df["Date"][::step]], rotation=10, fontsize=14)

# === Annotations ===
annotations = {
    "2025-03-05": {"text": "Outbreak\ndetected", "style": "-L", "level": 0.5, "below": False},
    "2025-03-06": {"text": "DTF meeting", "style": "straight", "level": 0.35, "below": True},
    "2025-03-07": {"text": "Response pillars\nactivated", "style": "straight", "level": 1.2, "below": False},
    "2025-03-08": {"text": "Radio talk shows\nStarted", "style": "straight", "level": 0.15, "below": False},
    "2025-03-10": {"text": "Disinfection of premises\nfor the confirmed case", "style": "straight", "level": 0.6, "below": True},
    "2025-03-13": {"text": "UNICEF backs\nRCCE strategy update", "style": "straight", "level": 1, "below": False},
    "2025-03-15": {"text": "MOH team\ndeployed", "style": "straight", "level": 0.25, "below": False},
    "2025-03-16": {"text": "GISO & Lab teams\n-Mortality Surveillance", "style": "straight", "level": 0.75, "below": True},
    "2025-03-16": {"text": "VHT Orientation on Active Case Search", "style": "L", "level": 0.74,"below": True},
    "2025-03-17": {"text": "CCHF detected: Cow samples &Ticks picked", "style": "L", "level": 0.6, "below": True},
    "2025-03-19": {"text": "DHT Coordination with MOH team", "style": "L", "level": 1.3, "below": False},
    "2025-03-20": {"text": "Distributed hand washing facilities", "style": "L", "level": 0.5, "below": True},
    "2025-03-21": {"text": "83 VHTs Oriented in RCCE", "style": "L", "level": 0.65, "below": False},
    "2025-03-23": {"text": "5days Orientation of HCWs on\nAlert Mgt by Africa CDC", "style": "L", "level": 1.3, "below": False},
    "2025-03-24": {"text": "IPC Assessments", "style": "L", "level": 0.55, "below": False},
    "2025-03-25": {"text": "School EVD sensitization", "style": "L", "level": 0.35, "below": True},
    "2025-03-27": {"text": "Started Radio spot messages", "style": "L", "level": 0.25, "below": False},
    "2025-04-02": {"text": "Engagement with opinion leaders", "style": "L", "level": 0.3, "below": True},
    "2025-04-26": {"text": "End of EVD outbreak\nUganda Declared Ebola Free\nby MOH", "style": "straight", "level": 0.14, "below": False}
}

x_buffer = pd.Timedelta(days=2)
offset_unit = 2.8

for date_str, ann in annotations.items():
    date = pd.to_datetime(date_str)
    if date not in df["Date"].values:
        continue
    y_val = df.loc[df["Date"] == date, ["Intensity of Activities", "Confirmed SUVD"]].sum(axis=1).values[0]
    below = ann["below"]
    level = ann["level"]
    style = ann["style"]
    anchor_y = 0 if below else y_val
    offset_y = (-1 if below else 1) * offset_unit * level
    target_y = anchor_y + offset_y
    text_x = date + (x_buffer if style == "L" else pd.Timedelta(0))
    conn_style = "angle,angleA=0,angleB=90,rad=2" if style == "L" else "arc3"
    ax.annotate(
        ann["text"],
        xy=(date, anchor_y),
        xytext=(text_x, target_y),
        fontsize=9,
        ha="left" if style == "L" else "center",
        zorder=5,
        arrowprops=dict(
            arrowstyle="-|>",
            connectionstyle=conn_style,
            color="black",
            lw=0.8,
            zorder=5
        )
    )

# === Final Formatting ===
ax.set_title("", fontsize=22, fontweight="bold", color="#222222")
ax.set_xlabel("", fontsize=16, fontweight="bold")
ax.set_ylabel("", fontsize=16, fontweight="bold")
ax.legend(fontsize=16, loc="upper right")
ax.yaxis.set_major_locator(MaxNLocator(integer=True))
ax.grid(axis="y", linestyle="--", color="lightgrey", zorder=0)
ax.set_facecolor("white")
fig.patch.set_facecolor("white")
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
fig.tight_layout()
plt.show()
