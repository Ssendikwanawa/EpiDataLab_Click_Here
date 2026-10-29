import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Prepare the dataset
data = {
    "Date": ["03/02/2025", "05/02/2025", "06/02/2025", "07/02/2025", "08/02/2025", "09/02/2025", "10/02/2025",
             "11/02/2025", "12/02/2025", "13/02/2025", "14/02/2025", "15/02/2025", "16/02/2025", "17/02/2025",
             "18/02/2025", "19/02/2025", "20/02/2025", "21/02/2025", "22/02/2025", "23/02/2025", "24/02/2025",
             "25/02/2025"],
    "Number_of_Alerts": [6, 5, 3, 3, 5, 3, 2,
                         2, 4, 9, 15, 8, 9, 4,
                         2, 0, 0, 2, 1, 1, 1, 0],
    "intervention": ["Early Response Activities"] * 7 +
                    ["Ring Sweep Strategy"] * 7 +
                    ["Active Case Search"] * 8,
    "Time": list(range(22))
}

df = pd.DataFrame(data)
df["Date"] = pd.to_datetime(df["Date"], dayfirst=True)

# Set plot style
sns.set(style="whitegrid")
plt.figure(figsize=(16, 8))

# Define the custom color palette
custom_palette = {
    "Early Response Activities": "#0073e6",  # Blue for Early Response Activities
    "Ring Sweep Strategy": "#00cc44",  # Green for Ring Sweep Strategy
    "Active Case Search": "#ff3333"  # Red for Active Case Search
}

# Line plot
sns.lineplot(data=df, x="Date",
             y="Number_of_Alerts",
             hue="intervention",
             marker="o", markersize=10, linestyle="-", linewidth=3,
             palette=custom_palette)

## Enhance plot readability
plt.title("Alerts Received Over Time by Intervention Type During EVD Outbreak\nResponse in Mbale, Eastern-Uganda",
          fontsize=22, fontweight="bold")
plt.ylabel("Number of Alerts", fontsize=16)
plt.xlabel("")
plt.xticks(rotation=-10, fontsize=19, fontweight="bold")

# Legend w
plt.legend(title="Intervention Type", title_fontsize=21,
           prop={'weight': 'bold'}, fontsize=19, labelspacing=0.5, loc="upper left")

plt.tight_layout()


plt.show()
