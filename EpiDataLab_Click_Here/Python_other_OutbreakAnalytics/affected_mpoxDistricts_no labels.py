import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import contextily as ctx
from matplotlib.colors import LinearSegmentedColormap

# Load District Shapefile
districts = gpd.read_file("C:\\Users\\Administrator\\Desktop\\uganda_districts.shp")

# Define all districts in the Lango region
lango_districts = [
    "LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM", "KWANIA", "LIRA", "AMOLATAR", "KOLE", "ALEBTONG"
]

# Define Case Data for Affected Districts
case_data_districts = pd.DataFrame({
    "District": ["LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM", "KOLE", "LIRA"],  # Affected districts
    "Cases": [17, 3, 4, 5, 5, 3, 1]  # Corresponding case counts
})

# Assign 0 cases to non-affected districts
non_affected_districts = set(lango_districts) - set(case_data_districts["District"])
non_affected_data = pd.DataFrame({"District": list(non_affected_districts), "Cases": 0})

# Combine affected and non-affected district data
all_district_cases = pd.concat([case_data_districts, non_affected_data], ignore_index=True)

# Filter Lango districts from the shapefile
lango_geo = districts[districts["District"].isin(lango_districts)]

# Merge district-level case data with geospatial data
lango_geo = lango_geo.merge(all_district_cases, on="District", how="left")

# Define a custom colormap: Striking gray for low cases to red for high cases
colors = ["#D3D3D3", "#E63946"]  # Gray for low cases, red for high cases
custom_cmap = LinearSegmentedColormap.from_list("custom_red_gray", colors, N=256)

# Plot the Map
fig, ax = plt.subplots(1, 1, figsize=(14, 10))

# Plot districts with a color scale based on the number of cases
lango_geo.plot(
    column="Cases",
    cmap=custom_cmap,  # Custom gradient: gray to red
    linewidth=0.3,
    edgecolor="black",  # District boundaries
    legend=True,
    legend_kwds={
        "shrink": 0.5,  # Shrink legend size
        "label": "Case Load",  # Legend label
        "orientation": "vertical"  # Vertical legend
    },
    ax=ax
)

# Add a title to the map
ax.set_title("Mpox Caseload in Affected Districts", color="#333333", fontsize=20, fontweight="bold")

# Remove axes for a cleaner look
ax.axis("off")
plt.tight_layout()

# Add Basemap for geographic context (Optional)
ctx.add_basemap(ax, crs=lango_geo.crs.to_string(), source=ctx.providers.CartoDB.Positron)

# Show the Map
plt.show()
