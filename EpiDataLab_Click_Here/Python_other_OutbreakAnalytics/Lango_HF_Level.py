import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.lines import Line2D
import contextily as ctx  # For basemap integration
import numpy as np
from adjustText import adjust_text

# Load district shapefile
shapefile_path = "C:\\Users\\Administrator\\Desktop\\uganda_districts.shp"
districts = gpd.read_file(shapefile_path)
if districts.crs != "EPSG:4326":
    districts = districts.to_crs("EPSG:4326")

# Define Lango region districts
lango_districts = ["LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM",
                   "KWANIA", "LIRA", "AMOLATAR", "KOLE", "ALEBTONG"]
lango_map = districts[districts["District"].str.strip().str.upper().isin(lango_districts)]

# Transform to EPSG:3857 for basemap compatibility
lango_map = lango_map.to_crs(epsg=3857)

# Load health facility level data
csv_path = "D:\\Ssendi\\Lango_HF_level.csv"
HFLevel_data = pd.read_csv(csv_path)
HFLevel_gdf = gpd.GeoDataFrame(
    HFLevel_data,
    geometry=gpd.points_from_xy(HFLevel_data.LONG, HFLevel_data.LAT),
    crs="EPSG:4326"
)
HFLevel_gdf = HFLevel_gdf.to_crs(epsg=3857)

# Plot
fig, ax = plt.subplots(figsize=(12, 10))

# Adjust the plotting area to match data extent
ax.set_xlim(lango_map.total_bounds[[0, 2]])  # Set X limits
ax.set_ylim(lango_map.total_bounds[[1, 3]])  # Set Y limits

# Add basemap
ctx.add_basemap(ax, source=ctx.providers.Esri.WorldImagery, zoom=12,
                attribution=False)

# Plot district boundaries
lango_map.plot(ax=ax, edgecolor="#2F4F4F", facecolor="none",
               linewidth=1.9, label="Districts")

# Plot district labels (centroids)
for _, row in lango_map.iterrows():
    centroid_x, centroid_y = (
    row.geometry.centroid.x, row.geometry.centroid.y)
    ax.annotate(
        text=row["District"].title(),  # Capitalize district names
        xy=(centroid_x, centroid_y),  # Position labels at centroids
        xytext=(6, 6),  # Offset slightly to avoid overlap
        textcoords="offset points",
        fontsize=7,  # Font size of labels
        color="black",  # Label color
        weight="bold",
        bbox=dict(facecolor='#F5F5F5', edgecolor='none',
                  boxstyle='round,pad=0.01', alpha=1)  # White background highlight
    )

# Jitter scale for displacement
jitter_scale = 0.0005

# Collect all text labels for adjustment
texts = []

# Plot health facility levels with jitter and z-order
colors = {
    "Clinic": "#DC143C", #removed  #228B22
    "HC II": "#32CD32", ##..#008080
    "HC III": "#DA70D6",
    "HC IV": "#FFD700",
    "Special Clinic": "#4B0082",
    "General Hospital": "#00FFFF", ##7B68EE
    "RRH": "#00FF00"
}
markers = {
    "Clinic": "o",  # Small circle
    "HC II": ".",  # Smallest dot
    "HC III": "^",  # Upward triangle
    "HC IV": "s",  # Square
    "Special Clinic": "p",  # Pentagon
    "General Hospital": "P",
    "RRH": "D",  # Diamond
}

# Plotting each health facility level
for level in HFLevel_gdf["HFLevel"].unique():
    subset = HFLevel_gdf[HFLevel_gdf["HFLevel"] == level]

    # Add jitter to prevent point overlap
    jitter_x = np.random.normal(scale=jitter_scale, size=subset.geometry.x.size)
    jitter_y = np.random.normal(scale=jitter_scale, size=subset.geometry.y.size)

    # Scatter plot with jitter
    ax.scatter(
        subset.geometry.x + jitter_x,  # Jittered x-coordinates
        subset.geometry.y + jitter_y,  # Jittered y-coordinates
        color=colors[level], marker=markers[level],
        s=50, edgecolor=colors[level], label=level, zorder=10
    )

# Adjust labels to avoid overlap
adjust_text(texts, arrowprops=dict(arrowstyle="->", color="gray", lw=0.5))

# Add custom legend for health facility levels
custom_handles = [
    Line2D([0], [0], color="#DC143C", marker="o", markersize=10, label="Clinic", lw=0),
    Line2D([0], [0], color="#32CD32", marker=".", markersize=10, label="HC II", lw=0),
    Line2D([0], [0], color="#DA70D6", marker="^", markersize=10, label="HC III", lw=0, zorder=10),
    Line2D([0], [0], color="#FFD700", marker="s", markersize=10, label="HC IV", lw=0, zorder=10),
    Line2D([0], [0], color="#4B0082", marker="p", markersize=10, label="Special Clinic", lw=0),
    Line2D([0], [0], color="#00FFFF", marker="P", markersize=10, label="General Hospital", lw=0, zorder=10),
    Line2D([0], [0], color="#00FF00", marker="D", markersize=10, label="RRH", lw=0, zorder=30)
]

# Add legend to the plot
ax.legend(handles=custom_handles,
          title="Health Facility Levels",
          loc="upper left", bbox_to_anchor=(1, 0.7))

# Configure title and labels
ax.set_title("Health Facility Levels in Lango Sub-Region, March 2025.", fontsize=16)
ax.axis("off")
plt.tight_layout()
plt.show()
