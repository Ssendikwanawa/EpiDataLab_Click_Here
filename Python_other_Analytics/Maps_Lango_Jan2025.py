import os
# Clear the terminal for different operating systems
def clear_terminal():
    if os.name == 'nt':
        os.system('cls')
# Call the function to clear the terminal
clear_terminal()

# Required Libraries
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from shapely.geometry import Point
import contextily as ctx  # For adding basemaps to the plot

# Load Shapefiles
districts = gpd.read_file(r"C:/Users/Administrator/Desktop/Districts_shapefiles/uganda_districts.shp")
subcounty_shapefile = gpd.read_file(r"C:/Users/Administrator/Desktop/Subcounty_shapefiles/Uganda-Subcounties-2021.shp")

# Filter Subcounties for the Lango Region and remove empty geometries
lango_subcounties = subcounty_shapefile[
    (subcounty_shapefile["District"].isin(["APAC", "DOKOLO", "LIRA CITY", "OYAM",
                                           "AMOLATAR", "KWANIA", "LIRA",
                                           "KOLE", "ALEBTONG", "OTUKE"])) &
    (~subcounty_shapefile.geometry.is_empty)
    ]

# Ensure Consistent CRS (Coordinate Reference Systems)
lango_subcounties = lango_subcounties.to_crs(districts.crs)

# Subcounty-Level Case Data
case_data_subcounties = pd.DataFrame({
    "Subcounty": ["CHEGERE", "DOKOLO TOWN COUNCIL", "KANGAI", "ADOK",
                  "LIRA EAST DIVISION", "LIRA WEST DIVISION", "MYENE", "KAMDINI",
                  "OYAM TOWN COUNCIL", "OTUKE TOWN COUNCIL"],
    "Cases": [1, 1, 1, 1, 1, 6, 2, 1, 1, 4],  # Number of cases
    "District": ["APAC", "DOKOLO", "DOKOLO", "DOKOLO",
                 "LIRA CITY", "LIRA CITY", "OYAM", "OYAM", "OYAM", "OTUKE"]
})

# Merge Case Data with Subcounty Geospatial Data
merged_data = lango_subcounties.merge(case_data_subcounties, on=["Subcounty", "District"], how="left")
merged_data["Cases"] = merged_data["Cases"].fillna(0)  # Replace NA with 0 for Subcounties with No Cases

# Create Labels with Subcounty Names and Case Counts
merged_data["Label"] = merged_data["Subcounty"] + " (" + merged_data["Cases"].astype(int).astype(str) + ")"

# Group by District, Merge Geometries, and Calculate Centroids for District-Level Labeling
district_centroids = (lango_subcounties
                      .groupby("District")
                      .geometry.apply(lambda g: g.unary_union)  # Merge geometries by district
                      .centroid.reset_index(name="geometry")  # Calculate centroids for labeling
                      )
district_centroids = gpd.GeoDataFrame(district_centroids, geometry="geometry", crs=lango_subcounties.crs)

# Plot the Map
fig, ax = plt.subplots(1, 1, figsize=(12, 8))

# Plot Subcounties and Fill Cases Data
merged_data.plot(column="Cases",
                 cmap="Blues",  # Color gradient
                 linewidth=0.8,
                 edgecolor="black",
                 ax=ax,
                 legend=True)

# Add District Boundaries with Red Color
lango_subcounties.boundary.plot(ax=ax, color="red", linewidth=1)

# Add Subcounty Labels (Names with Case Counts)
for _, row in merged_data.iterrows():
    if row.Cases > 0:  # Only label subcounties with cases
        ax.annotate(row["Label"],
                    xy=(row.geometry.centroid.x, row.geometry.centroid.y),
                    xytext=(3, 3),
                    textcoords="offset points",
                    color="blue",
                    fontsize=8)

# Add District Labels (Black Bold Text)
for _, row in district_centroids.iterrows():
    ax.annotate(row["District"],
                xy=(row.geometry.x, row.geometry.y),
                xytext=(7, 7),
                textcoords="offset points",
                color="black",
                fontsize=10,
                fontweight="bold")

# Add Title and Adjust Aesthetics
ax.set_title("Map of Affected Subcounties in Lango", fontsize=14, fontweight="bold")
ax.axis("off")  # Remove axes
plt.tight_layout()

# Optionally, Add Basemap (Optional: Requires Internet and contextily)
ctx.add_basemap(ax, crs=lango_subcounties.crs.to_string(), source=ctx.providers.Stamen.TerrainBackground)

# Show Plot
plt.show()
