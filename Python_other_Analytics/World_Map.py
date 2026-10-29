import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
from adjustText import adjust_text
from matplotlib.cm import plasma
from matplotlib.colors import Normalize
import matplotlib.patheffects as PathEffects

# Load shapefile
shapefile_path = "C:/Users/Administrator/Downloads/world-administrative-boundaries/world-administrative-boundaries.shp"
world = gpd.read_file(shapefile_path)

# Country data
target_countries = [
    "United Republic of Tanzania", "Iraq", "India", "Nigeria", "Ghana", "Cambodia",
    "Lebanon", "Indonesia", "South Africa", "Kenya", "Rwanda", "Uganda", "Burundi",
    "Cameroon", "Central Africa Republic", "Democratic Republic of the Congo", "Chad",
    "Malawi", "Botswana", "Comoros", "Lesotho", "Madagascar", "Mauritius",
    "Mozambique", "Namibia"
]
case_data = pd.DataFrame({
    "name": target_countries,
    "Number": [4, 1, 3, 7, 3, 1, 1, 1, 1, 3, 1, 2, 1, 2, 1, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1]
})

# Filter & merge
affected = world[world["name"].isin(target_countries)].copy()
affected = affected.merge(case_data, on="name", how="left")
affected["coords"] = affected.geometry.centroid

# Plot
fig, ax = plt.subplots(figsize=(200, 19))  # Bigger map

# Base layer
world.plot(ax=ax, color="gray", edgecolor="white", linewidth=0.6)

# Affected countries
norm = Normalize(vmin=case_data["Number"].min(), vmax=case_data["Number"].max())
affected.plot(
    ax=ax,
    column="Number",
    cmap="plasma_r",
    edgecolor="red",
    linewidth=0.4,
    legend=True,
    legend_kwds={
        'shrink': 0.3,
        'label': "Case Count",
        'orientation': "horizontal"
    }
)

# Titles
ax.set_title("Country-Level Case Distribution", fontsize=20, weight='bold')
ax.text(
    0.5, 1.02,
    "",
    transform=ax.transAxes,
    ha='center', fontsize=14, style='italic'
)
ax.text(
    0.5, -0.05,
    "", #can add caption
    transform=ax.transAxes,
    ha='center', fontsize=10, color="gray"
)

# Clean look
ax.axis("off")
plt.show()
