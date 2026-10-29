import matplotlib.pyplot as plt
import pandas as pd
import geopandas as gpd
from adjustText import adjust_text
import contextily as ctx
import matplotlib.colors as mcolors
import matplotlib.patches as mpatches  # For custom legend

# Load District Shapefile
districts = gpd.read_file("C:\\Users\\Administrator\\Desktop\\uganda_districts.shp")
print(districts.head())  # Check for correct geospatial data
print(districts.columns)  # Check for the "District" column

# Standardize district names in shapefile to match case data
districts["District"] = districts["District"].str.upper().str.strip()

# Define Lango Districts
lango_districts = [
    "LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM", "KWANIA",
    "LIRA", "AMOLATAR", "KOLE", "ALEBTONG"
]

# Define Case Data (Ensure District Names Match Exactly)
case_data_districts = pd.DataFrame({
    "District": ["LIRA CITY"],  # Affected districts
    "Cases": [38]  # Corresponding case counts
})

# Assign 0 cases to non-affected districts
non_affected_districts = set(lango_districts) - set(case_data_districts["District"])
non_affected_data = pd.DataFrame({"District": list(non_affected_districts), "Cases": 0})

# Combine affected and non-affected districts
all_district_cases = pd.concat([case_data_districts, non_affected_data], ignore_index=True)
print(all_district_cases)  # Debug: Ensure all districts are included with correct case counts

# Filter Lango districts from the shapefile
lango_geo = districts[districts["District"].isin(lango_districts)]

# Merge district-level case data with geospatial data
lango_geo = lango_geo.merge(all_district_cases, on="District", how="left")

# Debug: Check if all districts were merged correctly
print(lango_geo[["District", "Cases"]])  # Ensure cases for LIRA are present

# Add Labels for Districts
lango_geo["Label"] = lango_geo["District"] + " (" + lango_geo["Cases"].astype(str) + ")"

# Calculate Centroids for Label Placement
lango_geo["centroid"] = lango_geo.geometry.centroid

# Define Bins and Case Categories
bins = [0, 1, float("inf")]  # Categories: 0 cases = "Other Districts", 1+ cases = "Active Transmission"
labels = ["Other Districts", "Active Transmission"]  # Labels for case categories
lango_geo["CaseCategory"] = pd.cut(lango_geo["Cases"], bins=bins, labels=labels, include_lowest=True)

# Define a Custom Colormap for the Categories
cmap = mcolors.ListedColormap(["white", "#d22b2b"])  # White for no cases, dark red for active transmission
norm = mcolors.BoundaryNorm(bins, cmap.N)

# Plotting
fig, ax = plt.subplots(1, 1, figsize=(12, 8))

# Generate the Map Plot
lango_geo.plot(
    column="CaseCategory",  # Use case categories for shading
    cmap=cmap,  # Apply the custom colormap
    linewidth=0.5,
    edgecolor="black",  # Add black borderlines
    ax=ax
)

# Add District Labels
texts = []
for _, row in lango_geo.iterrows():
    x, y = row.geometry.centroid.x, row.geometry.centroid.y

    # Apply manual offsets to avoid overlap
    if row["District"] == "LIRA CITY":
        x += 0.09
        y += 0.05
    elif row["District"] == "KOLE":
        x -= 0.09
        y -= 0.04

    # Add labels only for districts with cases > 0
    if row["Cases"] > 0:
        text = ax.text(
            x=x, y=y,
            s=row["Label"],
            fontsize=10,
            color="black",
            fontweight="bold",
            ha="center", va="center"
        )
        texts.append(text)

# Adjust overlapping labels
adjust_text(texts, ax=ax, arrowprops=dict(arrowstyle="-", color="black", lw=0.8))

# Add Title
ax.set_title("Districts with a Case Reported in Last 21 Days",
             fontsize=18, fontweight="bold", color="#d22b2b")
ax.axis("off")  # Remove axes for a cleaner look

# Add Basemap for Context
ctx.add_basemap(ax, crs=lango_geo.crs,
                source=ctx.providers.CartoDB.Positron)

# Create Custom Legend
categories = lango_geo["CaseCategory"].cat.categories
legend_handles = [
    mpatches.Patch(facecolor=cmap(norm(i)), edgecolor="grey", label=cat)
    for i, cat in enumerate(categories)
]
ax.legend(
    handles=legend_handles,
    title="Case Status",
    title_fontsize=12,
    fontsize=10,
    loc="lower left",
    frameon=False
)

plt.tight_layout()
plt.show()
