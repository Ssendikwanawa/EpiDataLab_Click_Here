import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import contextily as ctx
import matplotlib.colors as mcolors
import matplotlib.patches as mpatches  # For creating custom legend boxes

# Load District Shapefile (All districts)
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

# Merge filtered shapefile data with attack rates CSV data
lango_geo = lango_geo.merge(attack_rates, on="District", how="left")

# Ensure Attack_Rates are numeric
lango_geo["Attack_Rates"] = pd.to_numeric(lango_geo["Attack_Rates"], errors="coerce")

# Define Custom Categorized Attack Rate Ranges
bins = [-1, 0, 1, 2, 3, 16]  # Separate 1, 2, and 3 into distinct bins
labels = ["0", "1", "2", "3","16"]  # Assign a label for each bin
lango_geo["Rate_Category"] = pd.cut(
    lango_geo["Attack_Rates"], bins=bins, labels=labels, include_lowest=True
)

# Define Custom Colormap
colors = ["#FFFFFF", "#FFD6D6", "#FF7F7F", "#E63946", "#990000"]  # Custom light-to-deep reds"#FFFFFF", "#FFE0E0", "#FFB2B2", "#E63946", "#E63946",
cmap = mcolors.ListedColormap(colors)  # Define the colormap

# Plot the map
fig, ax = plt.subplots(1, 1, figsize=(14, 10))

lango_geo.plot(
    column="Rate_Category",  # Color map based on the categorized attack rates
    cmap=cmap,  # Use the defined cmap
    linewidth=0.3,
    edgecolor="grey",
    ax=ax
)

# Add district names as labels
for x, y, label in zip(
        lango_geo.geometry.centroid.x,  # X-coordinates for centroids
        lango_geo.geometry.centroid.y,  # Y-coordinates for centroids
        lango_geo["District"],  # District names
):
    ax.text(x- 0.1, #shifts left west
            y + 6, #shifte up(North)
            label,
            fontsize=13, color="#000000",
            va="center"
    )

# Add Title and Formatting
ax.set_title("Attack Rates/100,000p by District, 27/6/25", fontsize=24,
             color="#000000")
ax.axis("off")  # Remove axes for cleaner visuals people

# Create Custom Legend (Replace built-in legend)
categories = lango_geo["Rate_Category"].unique()  # Get unique categories from 'Rate_Category'
legend_handles = [
    mpatches.Patch(
        facecolor=colors[i], edgecolor="black",
        linewidth=0.3, label=cat
    )
    for i, cat in enumerate(labels)
]

# Add the custom Legend to the Plot
ax.legend(
    handles=legend_handles,
    title="AR/100K",  # Legend title
    title_fontsize=14,  # Title font size
    fontsize=12,  # Label font size
    loc="lower right",  # Place the legend near the lower right inside the map boundary
    frameon=False,  # Remove box around the legend
)

# Optional: Add a basemap
ctx.add_basemap(ax, crs=lango_geo.crs.to_string(),
                source=ctx.providers.CartoDB.Positron)

# Show the map
plt.tight_layout()
plt.show()
