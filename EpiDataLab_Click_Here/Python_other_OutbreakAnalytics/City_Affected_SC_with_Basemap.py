import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import contextily as ctx
import matplotlib.patheffects as PathEffects
from matplotlib.patches import Patch

# Load shapefiles
districts = gpd.read_file("C:/Users/Administrator/Desktop/uganda_districts.shp")
subcounties = gpd.read_file("C:/Users/Administrator/Desktop/Uganda-Subcounties-2021.shp")

# Filter LIRA CITY
lira_subs = subcounties[subcounties["District"] == "LIRA CITY"]

# Case data
case_data = pd.DataFrame({
    "Subcounty": ["LIRA WEST DIVISION", "LIRA EAST DIVISION"],
    "Cases": [18, 3],
    "District": ["LIRA CITY", "LIRA CITY"]
})

# Merge and clean
lira_merged = lira_subs.merge(case_data, how="left", on=["Subcounty", "District"])
lira_merged["Cases"] = lira_merged["Cases"].fillna(0)
lira_merged["Case_Label"] = lira_merged["Cases"].map({3: "3 Case", 18: "18 Cases"})

# Reproject to Web Mercator for basemap compatibility
lira_merged = lira_merged.to_crs(epsg=3857)

# Define color mapping
colors = {"3 Case": "tan", "18 Cases": "red"}

# Start plot
fig, ax = plt.subplots(figsize=(12, 12))

# Plot case categories
for label, group in lira_merged.groupby("Case_Label"):
    group.plot(ax=ax, color=colors[label], edgecolor="black")

# Label subcounties with cases
for idx, row in lira_merged.iterrows():
    if row["Cases"] > 0:
        centroid = row["geometry"].centroid
        ax.text(centroid.x, centroid.y, row["Subcounty"], fontsize=9, ha="center", va="center",
                bbox=dict(boxstyle="round,pad=0.2", fc="white", ec="black", lw=0.5),
                path_effects=[PathEffects.withStroke(linewidth=1, foreground="black")])

# Plot district border
lira_merged.boundary.plot(ax=ax, edgecolor="red", linewidth=2, alpha=0.7)

# ✅ Add the CartoDB.Positron base map (light, clean)
ctx.add_basemap(ax, crs=lira_merged.crs.to_string(), source=ctx.providers.CartoDB.Positron)

# Custom legend
legend_elements = [Patch(facecolor=colors[label], edgecolor='black', label=label) for label in colors]
ax.legend(handles=legend_elements, title="Key", title_fontsize=14, fontsize=12,
          loc='upper center', bbox_to_anchor=(0.5, -0.05), ncol=2)

# Final touches
ax.set_title("Mpox Case Load in Lira City, 23/04/25.", fontsize=16, fontweight="bold")
ax.set_axis_off()
plt.tight_layout()
plt.show()
