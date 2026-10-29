import geopandas as gpd
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch

# Load the shapefile
shapefile_path = "C:\\Users\\Administrator\\Desktop\\uganda_districts.shp"  # Replace with the actual path
districts = gpd.read_file(shapefile_path)

# Ensure district names are standardized (uppercase, no extra spaces)
districts['District'] = districts['District'].str.upper().str.strip()

# Define the affected districts
affected_districts = [
    "KAMPALA", "GULU", "GULU CITY"
]

# Filter the GeoDataFrame for affected districts and others
affected_geo = districts[districts['District'].isin(affected_districts)]
other_geo = districts[~districts['District'].isin(affected_districts)]

# Plot the map
fig, ax = plt.subplots(figsize=(16, 12))

# Plot other districts in light grey with faint borders
other_geo.plot(ax=ax, color="lightgrey", edgecolor="lightgrey", linewidth=0.5, alpha=0.6)

# Plot affected districts in striking red with bold borders
affected_geo.plot(ax=ax, color="red", edgecolor="red", linewidth=1, alpha=0.49)

# Extract coordinates for centroids of all districts
districts["centroid"] = districts.geometry.centroid
centroids = {district: (geo.x, geo.y) for district, geo in zip(districts["District"], districts["centroid"])}


# Function to draw arrows
def draw_arrow(ax, start, end, color="black"):
    """Draws an arrow from start to end on the map."""
    arrow = FancyArrowPatch(
        start, end, color=color, arrowstyle="->", mutation_scale=15, linewidth=1.8, alpha=0.8
    )
    ax.add_patch(arrow)


# Add arrows to indicate movement from KAMPALA to GULU CITY and GULU
draw_arrow(ax, centroids["KAMPALA"], centroids["GULU CITY"])  # Movement 1
draw_arrow(ax, centroids["KAMPALA"], centroids["GULU"])  # Movement 2

# Add labels for only GULU and KAMPALA
label_positions = {
    "KAMPALA": (centroids["KAMPALA"][0] + 30000, centroids["KAMPALA"][1] - 30000),  # Offset label outside district
    "GULU": (centroids["GULU"][0] - 30000, centroids["GULU"][1] + 30000)  # Offset label outside district
}

for district, (x_label, y_label) in label_positions.items():
    # Plot label text in striking blue outside district
    ax.text(x_label, y_label, district, fontsize=12, color="blue", fontweight="bold", ha="center")
    # Draw arrow pointing to district from label
    x_centroid, y_centroid = centroids[district]
    draw_arrow(ax, (x_label, y_label), (x_centroid, y_centroid), color="none")

# Add the map title
ax.set_title("Spread of Disease X in Uganda", fontsize=16, fontweight="bold")

# Disable axes for clean visualization
ax.set_axis_off()

# Show the plot
plt.show()
