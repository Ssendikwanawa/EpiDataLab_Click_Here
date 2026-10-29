import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import contextily as ctx
import matplotlib.colors as mcolors
import matplotlib.patches as mpatches  # For creating a custom legend

# Load District Shapefile
districts = gpd.read_file("C:\\Users\\Administrator\\Desktop\\uganda_districts.shp")

# Load Attack Rates CSV Data
attack_rates = pd.read_csv("D:\\Ssendi\\Week4\\attackrateswk42025.csv")

# Define districts of the Lango region
lango_districts = [
    "ALEBTONG", "AMOLATAR", "APAC", "DOKOLO", "KOLE", "KWANIA", "LIRA",
    "LIRA CITY", "OTUKE", "OYAM"
]

# Filter shapefile for the 10 Lango districts
lango_geo = districts[districts["District"].isin(lango_districts)]

# Merge shapefile data with attack rates CSV data
lango_geo = lango_geo.merge(attack_rates, on="District", how="left")

# Ensure Attack_Rates are numeric
lango_geo["Attack_Rates"] = pd.to_numeric(lango_geo["Attack_Rates"], errors="coerce")

# Define Custom Categorized Attack Rate Ranges
bins = [-1, 0, 1, 2, 3]  # Separate 1, 2, and 3 into distinct bins
labels = ["0", "1", "2", "3"]  # Label each bin
lango_geo["Rate_Category"] = pd.cut(
    lango_geo["Attack_Rates"], bins=bins, labels=labels, include_lowest=True
)

# Define Custom Colormap
colors = ["#FFFFFF", "#FFE0E0", "#FFB2B2", "#E63946"]  # Light to deep red
cmap = mcolors.ListedColormap(colors)  # Define the colormap

# Plot the map
fig, ax = plt.subplots(1, 1, figsize=(14, 10))

# Set the figure and axes background to black
fig.patch.set_facecolor("black")  # Black figure background
ax.set_facecolor("black")  # Black axis background

# Plot Lango districts with attack rate categories
lango_geo.plot(
    column="Rate_Category",  # Color map based on Attack Rate categories
    cmap=cmap,  # Use the defined colormap
    linewidth=1,
    edgecolor="grey",  # Grey borders for districts
    ax=ax
)

# Add District Labels in grey
for x, y, label in zip(
        lango_geo.geometry.centroid.x,  # X-coordinates for centroids
        lango_geo.geometry.centroid.y,  # Y-coordinates for centroids
        lango_geo["District"],  # District names
):
    ax.text(
        x, y, label,
        fontsize=10,  # Font size for labels
        color="grey",  # District labels in grey
        ha="center", va="center"  # Center alignment
    )

# Add Title in red
ax.set_title(
    "Attack Rates per 100,000 Persons",
    fontsize=28,
    color="red"  # Title in red
)
ax.axis("off")  # Remove axis boundaries for a cleaner look

# Create Custom Legend (Replace built-in legend)
categories = lango_geo["Rate_Category"].unique()  # Unique categories from Rate_Category
legend_handles = [
    mpatches.Patch(
        facecolor=colors[i], edgecolor="black",
        linewidth=0.5, label=cat
    )
    for i, cat in enumerate(labels)
]

# Add custom legend to the plot
legend = ax.legend(
    handles=legend_handles,
    title="AR/100,000",  # Legend title
    title_fontsize=14,  # Title font size
    fontsize=12,  # Labels font size
    loc="lower right",  # Place the legend in the bottom-right corner
    frameon=False  # Remove borders around the legend
)

# Change legend text and title to white
for text in legend.get_texts():
    text.set_color("white")  # Label colors set to white
legend.get_title().set_color("white")  # Title color set to white

# Add a black-background basemap
ctx.add_basemap(
    ax,
    crs=lango_geo.crs.to_string(),
    source=ctx.providers.CartoDB.DarkMatter  # Basemap with black background
)

# Show the map
plt.tight_layout()
plt.show()
