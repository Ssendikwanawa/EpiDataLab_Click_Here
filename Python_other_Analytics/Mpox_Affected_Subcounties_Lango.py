import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patheffects as pe
from adjustText import adjust_text

# Load Shapefiles
districts = gpd.read_file("C:/Users/Administrator/Desktop/uganda_districts.shp")
subcounty_shapefile = gpd.read_file("C:/Users/Administrator/Desktop/Uganda-Subcounties-2021.shp")

# Filter Subcounties for the Lango Region and remove empty geometries
lango_subcounties = subcounty_shapefile[
    subcounty_shapefile["District"].isin(["APAC", "DOKOLO", "LIRA CITY", "OYAM", "AMOLATAR",
                                          "KWANIA", "LIRA", "KOLE", "ALEBTONG", "OTUKE"])
].copy()
lango_subcounties = lango_subcounties[~lango_subcounties.geometry.is_empty]

# Ensure consistent CRS
lango_subcounties = lango_subcounties.to_crs(districts.crs)

# Create Subcounty-Level Case Data
case_data_subcounties = pd.DataFrame({
    "Subcounty": ["CHEGERE", "DOKOLO TOWN COUNCIL", "KANGAI", "ADOK",
                  "LIRA EAST DIVISION", "LIRA WEST DIVISION", "MYENE", "KAMDINI",
                  "OYAM TOWN COUNCIL", "OTUKE TOWN COUNCIL"],
    "Cases": [1, 1, 1, 1, 1, 6, 2, 1, 1, 4],  # Number of cases in each subcounty
    "District": ["APAC", "DOKOLO", "DOKOLO", "DOKOLO",
                 "LIRA CITY", "LIRA CITY", "OYAM", "OYAM", "OYAM", "OTUKE"]
})

# Merge Case Data with Subcounty Spatial Data
merged_data = lango_subcounties.merge(case_data_subcounties, on=["Subcounty", "District"], how="left")
merged_data["Cases"].fillna(0, inplace=True)

# Create Labels with Subcounty Names and Case Counts
merged_data["Label"] = merged_data["Subcounty"] + " (" + merged_data["Cases"].astype(str) + ")"

# Compute centroids for affected subcounties
merged_data["centroid"] = merged_data.geometry.centroid

# Compute external label positions (move labels outward)
angle_step = 90 / len(merged_data)  # Evenly distribute labels in a circular pattern
radius = 0.2  # Distance from centroid to avoid overlap

label_positions = []
for i, (idx, row) in enumerate(merged_data.iterrows()):
    angle = angle_step * i  # Distribute labels in a circular pattern
    x_offset = radius * (1 if angle < 90 else -1)  # Move left or right
    y_offset = radius * (1 if 45 < angle < 180 else -1)  # Move up or down
    label_positions.append((row.centroid.x + x_offset, row.centroid.y + y_offset))

merged_data["label_x"], merged_data["label_y"] = zip(*label_positions)

# Plot the Map
fig, ax = plt.subplots(figsize=(12, 12))

# Plot affected subcounties
merged_data.plot(column="Cases", cmap="Blues", edgecolor="black", linewidth=1, ax=ax, legend=True)

# Plot subcounty boundaries in red
lango_subcounties.boundary.plot(ax=ax, color="red", linewidth=1)

# Add text labels OUTSIDE the map with arrows pointing to correct locations
texts = []
for idx, row in merged_data.iterrows():
    if row["Cases"] > 0:  # Only label affected subcounties
        text = ax.text(row.label_x, row.label_y, row["Label"], fontsize=10, color="blue", weight="bold",
                       path_effects=[pe.withStroke(linewidth=3, foreground="white")])
        texts.append(text)
        ax.annotate("", xy=(row.centroid.x, row.centroid.y), xytext=(row.label_x, row.label_y),
                    arrowprops=dict(arrowstyle="-", color="black", linewidth=1.5))

# Adjust text positions to avoid overlapping
adjust_text(texts, arrowprops=dict(arrowstyle="-", color="black"))

# Remove axes
ax.set_xticks([])
ax.set_yticks([])
ax.set_frame_on(False)

# Show the final map
plt.show()
