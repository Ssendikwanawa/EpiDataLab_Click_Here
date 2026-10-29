import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np

# Data for the chart
Perinatal_Death = ["Macereated\nStillBirths", "Fresh\nStillBirths", "Early\nNeonatalDeaths"]
Propotion = [15, 23, 62]

# Use a colormap to assign colors to each bar
colors = cm.coolwarm(np.linspace(0, 1,
                              len(Propotion)))  # 'viridis' is a visually appealing cmap
#Other Colors: - Sequential: `plasma`, `magma`, `inferno`, `coolwarm`, `cividis`
# Diverging: `RdYlGn`, `Spectral`Categorical: `tab20c`, `Paired`

# Create the timeline (horizontal bar) c
fig, ax = plt.subplots(figsize=(4, 1))

bars = plt.barh(Perinatal_Death, Propotion, color=colors,
         edgecolor=colors,
         linewidth=0.000001)

# Adding chart details
plt.xlabel("Propotions", fontsize=12)
plt.ylabel("", fontsize=12)
plt.title("Forms of Perinatal Deaths, Wk4",
          fontsize=18, weight="bold")
plt.grid(axis="x", linestyle="--",
         alpha=0.17, zorder=5)

# Customize the spines (box around the plot) to red
for spine in ax.spines.values():
    spine.set_edgecolor("white")  # Set the color of the spines to red
    spine.set_linewidth(1)  # Make the spines thicker (optional)

# Explicitly increase Y-axis tick label font size and color
plt.gca().yaxis.set_tick_params(labelsize=18,
                                labelcolor="black")  # Larger y-ticks font size
# Annotate values on bars
for index, value in enumerate(Propotion):
    plt.text(value -1.82, index, f"{value}%", color="Cadetblue",
             zorder=100, ha='center',
             va='center',
             fontsize=14, weight="bold") #the value places the percentage labels inside or outside the par position

# Show the chart
plt.tight_layout()
plt.show()
