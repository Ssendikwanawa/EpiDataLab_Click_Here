import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import contextily as ctx
from adjustText import adjust_text  # For preventing label overlapping

# Load District Shapefile
districts = gpd.read_file("C:\\Users\\Administrator\\Desktop\\uganda_districts.shp")

# Define all districts in the Lango region
lango_districts = [
    "LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM", "KWANIA", "LIRA", "AMOLATAR", "KOLE", "ALEBTONG"
]

# Define Case Data for Affected Districts
case_data_districts = pd.DataFrame({
    "District": ["LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM"],  # Affected districts
    "Cases": [8, 1, 4, 3, 5]  # Corresponding case counts
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

# Add Labels for the Districts
lango_geo["Label"] = lango_geo["District"] + " (" + lango_geo["Cases"].astype(str) + ")"

# Calculate Centroids for District Label Placement
lango_geo["centroid"] = lango_geo.geometry.centroid

# Plot the Map
fig, ax = plt.subplots(1, 1, figsize=(14, 10))

# Set the figure and axes background colors to black
fig.patch.set_facecolor("black")  # Set figure background to black
ax.set_facecolor("black")  # Set axis background to black

# Plot Lango Districts with Blue Color Scale (White for Zero Cases)
lango_geo.plot(
    column="Cases",
    cmap="Blues",  # Use a valid colormap here
    linewidth=0.5,
    edgecolor="white",  # Edge color set to white for visibility
    legend=True,
    legend_kwds={
        "shrink": 0.59,
        "label": "Case load",  # Label for the legend
        "orientation": "horizontal"  # Legend orientation
    },
    ax=ax
)

# Customize the color bar frame (outline) color
colorbar = ax.get_figure().axes[-1]  # Access the color bar (added as a separate axis)
colorbar.spines["outline"].set_edgecolor("gray")  # Set the border color of the color bar to gray
colorbar.spines["outline"].set_linewidth(2)  # Set the thickness of the color bar border
colorbar.set_facecolor("black")  # Set the background of the color bar to black
colorbar.tick_params(color="white")  # Set the ticks on the color bar to white
colorbar.yaxis.label.set_color("white")  # Set label color to white

# Add District Labels (Name with Cases)
district_texts = []
for _, row in lango_geo.iterrows():
    district_texts.append(
        ax.text(
            x=row.centroid.x,
            y=row.centroid.y,
            s=row["Label"] if row["Cases"] > 0 else "",  # Only label districts with cases
            fontsize=17,
            color="grey",  # Set label text to white for black background readability
            fontweight="bold",
            ha="center",
            va="center"
        )
    )

# Adjust text positions to avoid overlaps
adjust_text(district_texts, arrowprops=dict(arrowstyle="-", color="gray", lw=0.5))

# Add Title and Adjust Aesthetics
ax.set_title("Case load in affected districts", color="Cadetblue", fontsize=27, fontweight="bold")  # Title in white

# Ensure areas outside the map (plot) are black
ax.axis("off")  # Remove axes for a cleaner look
plt.subplots_adjust(left=0, right=1, top=1, bottom=0)  # Remove extra white space around the plot

# Add Dark Basemap - Use CartoDB.DarkMatter for Black Background
ctx.add_basemap(ax, crs=lango_geo.crs.to_string(), source=ctx.providers.CartoDB.DarkMatter)

# Show the Map
plt.show()
