import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.lines import Line2D
import contextily as ctx  # For basemap integration

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

# Load health facility ownership data
csv_path = "D:\\Ssendi\\HF ownership_govt_NGO_shapefiles.csv"
ownership_data = pd.read_csv(csv_path)
ownership_gdf = gpd.GeoDataFrame(
    ownership_data,
    geometry=gpd.points_from_xy(ownership_data.LONG, ownership_data.LAT),
    crs="EPSG:4326"
)
ownership_gdf = ownership_gdf.to_crs(epsg=3857)

# Plot
fig, ax = plt.subplots(figsize=(12, 10))

# Adjust the plotting area to match data extent
ax.set_xlim(lango_map.total_bounds[[0, 2]])  # Set X limits
ax.set_ylim(lango_map.total_bounds[[1, 3]])  # Set Y limits

# Add basemap
ctx.add_basemap(ax, source=ctx.providers.Esri.WorldImagery, zoom=12, attribution=False)

# Plot district boundaries
lango_map.plot(ax=ax, edgecolor="black", facecolor="none", linewidth=1.8, label="Districts")

# Plot health facilities
colors = {"PFP": "red", "PNFP": "blue", "GOV": "gold"}  # Define colors for ownership types
markers = {"PFP": ".", "PNFP": "^", "GOV": "o"}  # Define marker styles for ownership types
for owner in ownership_gdf["ownership"].unique():
    subset = ownership_gdf[ownership_gdf["ownership"] == owner]
    ax.scatter(
        subset.geometry.x, subset.geometry.y,
        color=colors[owner], marker=markers[owner], s=50, edgecolor=colors[owner], label=owner
    )

# Plot district labels (centroids)
for _, row in lango_map.iterrows():
    centroid_x, centroid_y = row.geometry.centroid.x, row.geometry.centroid.y
    ax.annotate(
        text=row["District"].title(),  # Capitalize district names
        xy=(centroid_x, centroid_y),  # Position labels at centroids
        xytext=(3, 3),  # Offset slightly to avoid overlap
        textcoords="offset points",
        fontsize=12,  # Font size of labels
        color="black",  # Label color
        weight="bold"
    )

# Add custom legend
custom_handles = [
    Line2D([0], [0], color="red", marker=".", markersize=10, label="PFP", lw=0),
    Line2D([0], [0], color="blue", marker="^", markersize=10, label="PNFP", lw=0),
    Line2D([0], [0], color="gold", marker="o", markersize=10, label="GOV", lw=0),
]
ax.legend(handles=custom_handles, title="HF Ownership", loc="upper left", bbox_to_anchor=(1, 1))

# Configure title and labels
ax.set_title("Health Facility Ownership in Lango Region, March 2025.", fontsize=16)
ax.axis("off")
plt.tight_layout()
plt.show()
