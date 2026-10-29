import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.lines import Line2D
import contextily as ctx  # For basemap integration
import numpy as np
from adjustText import adjust_text

# load district shape Files
shapefile_path = "C:\\Users\\Administrator\\Desktop\\uganda_districts.shp"
districts = gpd.read_file(shapefile_path)
if districts.crs != "EPSG:4326":
    districts = districts.to_crs("EPSG:4326")

#define Lango region districts
lango_districts = ["LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM",
                   "KWANIA", "LIRA", "AMOLATAR", "KOLE", "ALEBTONG"]
#Extract districts in the Lango region
lango_region = districts[districts['District'].str.strip().str.upper().isin(lango_districts)]


# Step 4: Transform to Web Mercator projection for compatibility with basemaps
lango_region = lango_region.to_crs(epsg=3857)

#Load HF data
healthfacility_coordinates = "D:\\Ssendi\\HF ownership_govt_NGO_shapefiles.csv"
health_facilities = pd.read_csv(healthfacility_coordinates)
health_facilities_gdf = gpd.GeoDataFrame(
    health_facilities,
    geometry=gpd.points_from_xy(health_facilities.LONG, health_facilities.LAT),
    crs="EPSG:4326"
)
health_facilities_gdf = health_facilities_gdf.to_crs(epsg=3857)

# Step 6: Plot the map
fig, ax = plt.subplots(figsize=(12, 10))

# Adjust the plotting area to match data extent
ax.set_xlim(lango_region.total_bounds[[0, 2]])  # Set X limits
ax.set_ylim(lango_region.total_bounds[[1, 3]])  # Set Y limits

# Add basemap for context
ctx.add_basemap(ax, source=ctx.providers.Esri.
                WorldImagery, zoom=12, attribution=False)

#ctx.add_basemap(ax, source=ctx.providers.Stamen.TonerLite,
 #zoom=12, attribution=False)

# Plot district boundaries
lango_region.plot(ax=ax, edgecolor="#2F4F4F",
                  facecolor="none", linewidth=1.9, label="Districts")

# Plot district labels (centroids)
for _, row in lango_region.iterrows():
    centroid_x, centroid_y = (row.geometry.centroid.x,
    row.geometry.centroid.y)
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

# Define colors for facility ownership types
colors = {
    "GOV": "#FFD700",  # Government-owned
    "PNFP": "#00FF00",  # Private Not-for-Profit
    "PFP": "#DC143C"  # Private for Profit
}

markers = {
    "GOV": "s",
    "PNFP": "D",
    "PFP": "o"
}

# Plotting each health facility
for ownership in health_facilities_gdf["ownership"].unique():
    subset = health_facilities_gdf[health_facilities_gdf["ownership"] == ownership]

    # Add jitter to prevent point overlap
    jitter_x = np.random.normal(scale=jitter_scale, size=subset.geometry.x.size)
    jitter_y = np.random.normal(scale=jitter_scale, size=subset.geometry.y.size)

    # Scatter plot with jitter
    ax.scatter(
        subset.geometry.x + jitter_x,  # Jittered x-coordinates
        subset.geometry.y + jitter_y,  # Jittered y-coordinates
        color=colors[ownership], marker=markers[ownership],
        s=50, edgecolor=colors[ownership], label=ownership, zorder=10
    )

# Adjust labels to avoid overlap
adjust_text(texts, arrowprops=dict(arrowstyle="->", color="gray", lw=0.5))

# Add custom legend for health facility ownership
custom_handles = [
    Line2D([0], [0], color="#FFD700", marker="s", markersize=10, label="GOV", lw=0),
    Line2D([0], [0], color="#00FF00", marker="D", markersize=10, label="PNFP", lw=0),
    Line2D([0], [0], color="#DC143C", marker="o", markersize=10, label="PFP", lw=0)
]

# Add legend to the plot
ax.legend(handles=custom_handles,
          title="HF Ownership Type", loc="upper left",
          bbox_to_anchor=(1, 0.7))

# Configure title and labels
ax.set_title("Health Facility Ownership in Lango Sub-Region, March 2025.", fontsize=16)
ax.axis("off")
plt.tight_layout()
plt.show()
