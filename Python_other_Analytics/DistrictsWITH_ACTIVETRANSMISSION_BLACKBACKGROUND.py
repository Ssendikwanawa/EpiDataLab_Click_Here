import matplotlib.pyplot as plt
import pandas as pd
import geopandas as gpd
import contextily as ctx
import matplotlib.colors as mcolors
import matplotlib.patches as mpatches
from shapely.geometry import MultiPolygon
from shapely.ops import unary_union

# Load District Shapefile
districts = gpd.read_file("C:\\Users\\Administrator\\Desktop\\uganda_districts.shp")

# Define Lango Districts
lango_districts = [
    "LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM", "KWANIA", "LIRA", "AMOLATAR", "KOLE", "ALEBTONG"
]

# Create Case Data for Affected Districts
case_data_districts = pd.DataFrame({
    "District": ["LIRA CITY"],  # Affected districts
    "Cases": [34]
})

# Assign 0 cases to non-affected districts
non_affected_districts = set(lango_districts) - set(case_data_districts["District"])
non_affected_data = pd.DataFrame({"District": list(non_affected_districts), "Cases": 0})

# Combine case data
all_district_cases = pd.concat([case_data_districts, non_affected_data], ignore_index=True)

# Filter for Lango districts
lango_geo = districts[districts["District"].isin(lango_districts)]

# Merge the case data with geometries
lango_geo = lango_geo.merge(all_district_cases, on="District", how="left")

# Dissolve geometries by "District"
lango_geo = lango_geo.dissolve(by="District", as_index=False)

# Ensure multipolygons are unified into single polygons
lango_geo["geometry"] = lango_geo.geometry.apply(
    lambda geom: unary_union(geom) if isinstance(geom, MultiPolygon) else geom)

# Create Ranges for Cases
bins = [0, 4, float("inf")]  # 0-4: Low, 5+: High
labels = ["Other Districts", "Districts in Active Transmission"]
lango_geo["CaseCategory"] = pd.cut(lango_geo["Cases"], bins=bins, labels=labels, include_lowest=True)

# Define Custom Colormap
cmap = mcolors.ListedColormap(["#FFFFFF", "#FF0000"])
norm = mcolors.BoundaryNorm(bins, cmap.N)

# Plotting
fig, ax = plt.subplots(1, 1, figsize=(12, 8))

# Set black background
fig.patch.set_facecolor("black")
ax.set_facecolor("black")

# Plot Lango districts
lango_geo.plot(
    column="CaseCategory",
    cmap=cmap,
    linewidth=0.5,
    edgecolor="red",
    ax=ax
)

# Add Title
ax.set_title(
    "Districts with Active Transmission, 27/6/25.",
    fontsize=28,
    color="#FF0000"
)
ax.axis("off")
plt.tight_layout()

# Add Basemap
ctx.add_basemap(
    ax,
    crs=lango_geo.crs.to_string(),
    source=ctx.providers.CartoDB.DarkMatter
)

# Create Custom Legend
categories = lango_geo["CaseCategory"].unique()
colors = [cmap(i / len(categories)) for i in range(len(categories))]

legend_handles = [
    mpatches.Patch(
        facecolor=colors[i],
        edgecolor="red",
        linewidth=0.5,
        label=cat
    )
    for i, cat in enumerate(categories)
]

legend = ax.legend(
    handles=legend_handles,
    title="Legend",
    title_fontsize=18,
    fontsize=13,
    loc="lower right",
    frameon=False
)

# Change legend text color to white
for text in legend.get_texts():
    text.set_color("white")
legend.get_title().set_color("white")

# Show Plot
plt.show()
