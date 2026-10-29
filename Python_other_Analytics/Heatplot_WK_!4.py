# `"coolwarm"`, `"seismic"`, `"RdBu" (Red-Blue)`,
# `"PiYG" (Pink-Green)`, `"PuOr" (Purple-Orange)`, and `"bwr" (Blue-White-Red)` are ideal for visualizing data that has both positive and negative values or a clear midpoint,
# while sequential color scales like `"Blues"`, `"Reds"`, `"Greens"`, `"Purples"`, `"Oranges"`, `"Greys"`, `"YlOrRd" (Yellow-Orange-Red)`, `"YlGn" (Yellow-Green)`, and `"BuPu" (Blue-Purple)`
# are best suited for data with a clear progression from smaller to larger values; additionally, perceptually uniform scales such as `"viridis" (Dark blue to yellow)`, `"plasma" (Dark purple to yellow)`,
# `"inferno" (Black to orange/yellow)`, `"magma" (Black to white/orange)`, and `"cividis" (Blue to yellow - colorblind friendly)` are designed for improved perceptual uniformity.
import platform

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Load Excel file
file_path = "D:\\Ssendi\\Week14\\Priority_diseases_Wk14.xls"
df = pd.read_excel(file_path)

# Transpose and index
df = df.set_index("District").transpose()

# Create figure and axis
fig, ax = plt.subplots(figsize=(12, 8))

# Heatmap
sns.heatmap(
    df,
    annot=True,
    fmt="d",
    cmap="Blues",
    cbar=False,
    linewidths=0.9,
    annot_kws={"size": 18, "color": "black"},
    ax=ax
)

# Title and axis labels
ax.set_title("Epidemic Prone Diseases Reported in Wk17", fontsize=25, weight="bold", pad=20)
ax.set_xlabel("")
ax.set_ylabel("")
ax.tick_params(axis='x', labelsize=23, rotation=30)
ax.tick_params(axis='y', labelsize=23)

# Add caption (outside the axis)
caption = (
    "Bacterial Meningitis and Plague suspected in Amolatar at Amolatar HC IV and Etam HC III respectively. "
    "DSFP was reached and is yet to provide feedback. The region is currently responding to Measles in Lira District; "
    "cases reported have been line-listed accordingly."
)

# Add caption using `fig.text` instead of `plt.figtext`, more consistent within `subplots`
fig.text(
    2, 0.1, caption,
    ha='center',
    fontsize=11,
    style='italic',
    wrap=True
)

# Don't use tight_layout as it messes with `fig.text`
plt.subplots_adjust(bottom=0.2)  # Reserve space at bottom
plt.show()

Event Based Surveillance :

No Signals in the entire regions were sent through the SMS Alert based platform.

We urge Implementing Partners to support functionality of this very important platfrom for
    Early Warning.
