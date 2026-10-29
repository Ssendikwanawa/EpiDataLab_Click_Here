import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from matplotlib.offsetbox import TextArea, AnnotationBbox

# Load District Shapefile
districts = gpd.read_file("C:\\Users\\Administrator\\Desktop\\uganda_districts.shp")

# Define all districts in the Lango region
lango_districts = [
    "LIRA CITY", "APAC", "OTUKE", "DOKOLO",
    "OYAM", "KWANIA", "LIRA", "AMOLATAR",
    "KOLE", "ALEBTONG"
]

# Example GeoDataFrame (replace with your actual data)
lango_geo = gpd.GeoDataFrame({
    "District": ["LIRA CITY", "APAC", "OTUKE"],
    "Cases": [19, 3, 0],  # Replace with your case data
    "geometry": [
        gpd.points_from_xy([10], [20])[0],
        gpd.points_from_xy([30], [40])[0],
        gpd.points_from_xy([50], [60])[0]
    ]
})

# Add labels for districts
lango_geo["Label"] = lango_geo.apply(
    lambda row: f"{row['District']} ({row['Cases']})" if row['Cases'] > 0 else f"{row['District']}",
    axis=1
)

# Plot map
fig, ax = plt.subplots(1, 1, figsize=(10, 8))

# Plot the districts' geometries
lango_geo.plot(ax=ax, color="lightgrey", edgecolor="black", alpha=0.6)

# Add district labels with colors
for _, row in lango_geo.iterrows():
    if row["Cases"] > 0:  # Only show labels if Cases > 0
        district_name = row["District"]  # Blue text
        cases_text = f"({row['Cases']})"  # Red text

        # Create a Blue TextArea for district name
        text_name = TextArea(district_name, textprops=dict(color="blue", fontsize=12, weight="bold"))

        # Create a Red TextArea for cases
        text_cases = TextArea(cases_text, textprops=dict(color="red", fontsize=12, weight="bold"))

        # Combine and position both TextAreas using an AnnotationBbox
        annotation_box = AnnotationBbox(
            text_name,  # Add the blue district name
            (row.geometry.x, row.geometry.y),  # Use the centroid coordinates
            frameon=False,  # No bounding box for text
            xybox=(0, 20),  # Offset in display units
            xycoords="data",
            box_alignment=(0.5, 0)  # Center alignment for both
        )
        ax.add_artist(annotation_box)

        # Add red cases dynamically below or beside the district name
        annotation_box_cases = AnnotationBbox(
            text_cases,  # Add the red cases text
            (row.geometry.x, row.geometry.y),  # Same coordinates
            frameon=False,  # No bounding box for text
            xybox=(0, 5),  # Cases displayed slightly closer
            xycoords="data",
            box_alignment=(0.5, 0)  # Center alignment for cases
        )
        ax.add_artist(annotation_box_cases)

# Map styling
ax.set_title("District Labels with Cases (Name in Blue, Cases in Red)", fontsize=16)
ax.axis("off")

# Show the finalized map
plt.show()
