import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import contextily as ctx
from adjustText import adjust_text  # For preventing label overlapping
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.offsetbox import TextArea, AnnotationBbox

############### Recommended Red Colors:Deep Red**: Strong and attention-catching
 #### #E63946`
### Bright and Vibrant Red**: Bold and stands out
### #FF4B5C`
####Muted/Soft Red**: Softer for a professional look
### #DC5C5C`
##### Rich Burgundy Red**: Elegant and deep
#### #A11B34`
#### #Crimson Red**: Balanced intensity
#### #D72638`
# Load District Shapefile
districts = gpd.read_file("C:\\Users\\Administrator\\Desktop\\uganda_districts.shp")

# Define all districts in the Lango region
lango_districts = [
    "LIRA CITY", "APAC", "OTUKE", "DOKOLO",
    "OYAM", "KWANIA", "LIRA", "AMOLATAR",
    "KOLE", "ALEBTONG"
]

# Define Case Data for Affected Districts
case_data_districts = pd.DataFrame({
    "District": ["LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM", "KOLE", "LIRA"],  # Affected districts
    "Cases": [35, 4, 4, 6, 8, 4, 1]  # Corresponding case counts
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

# Define custom colormap: white -> #199dba, #d22b2b" IF RED
colors = ["#fff", "#FF4B5C"] #replaced this red #e04343 with #124452
custom_cmap = LinearSegmentedColormap.from_list("custom_blue", colors, N=256)


# Plot the Map
fig, ax = plt.subplots(1, 1, figsize=(14, 10))

# Plot Lango Districts with Blue Color Scale (White for Zero Cases)
lango_geo.plot(
    column="Cases",
    cmap=custom_cmap,  # Use a valid colormap here e.g alue for cmap; supported values are 'Accent', 'Accent_r',
    # 'Blues', 'Blues_r', 'BrBG', 'BrBG_r', 'BuGn', 'BuGn_r', 'BuPu', 'BuPu_r', 'CMRmap', 'CMRmap_r', 'Dark2',
    # 'Dark2_r', 'GnBu', 'GnBu_r', 'Grays', 'Grays_r', 'Greens', 'Greens_r', 'Greys', 'Greys_r', 'OrRd',
    # 'OrRd_r', 'Oranges', 'Oranges_r', 'PRGn', 'PRGn_r', 'Paired', 'Paired_r', 'Pastel1', 'Pastel1_r',
    # 'Pastel2', 'Pastel2_r', 'PiYG', 'PiYG_r', 'PuBu', 'PuBuGn', 'PuBuGn_r', 'PuBu_r', 'PuOr', 'PuOr_r', 'PuRd',
    # 'PuRd_r',  'Purples', 'Purples_r', 'RdBu', 'RdBu_r', 'RdGy', 'RdGy_r', 'RdPu', 'RdPu_r', 'RdYlBu', 'RdYlBu_r',
    # 'RdYlGn', 'RdYlGn_r', 'Reds', 'Reds_r', 'Set1', 'Set1_r', 'Set2', 'Set2_r', 'Set3', 'Set3_r', 'Spectral',
    # 'Spectral_r', 'Wistia', 'Wistia_r', 'YlGn', 'YlGnBu', 'YlGnBu_r', 'YlGn_r', 'YlOrBr', 'YlOrBr_r', 'YlOrRd',
    # 'YlOrRd_r', 'afmhot', 'afmhot_r', 'autumn', 'autumn_r', 'berlin', 'berlin_r', 'binary', 'binary_r', 'bone',
    # 'bone_r', 'brg', 'brg_r', 'bwr', 'bwr_r', 'cividis', 'cividis_r', 'cool', 'cool_r', 'coolwarm',
    # 'coolwarm_r', 'copper', 'copper_r', 'cubehelix', 'cubehelix_r', 'flag', 'flag_r', 'gist_earth',
    # 'gist_earth_r', 'gist_gray', 'gist_gray_r', 'gist_grey', 'gist_grey_r', 'gist_heat', 'gist_heat_r',
    # 'gist_ncar', 'gist_ncar_r', 'gist_rainbow', 'gist_rainbow_r', 'gist_stern', 'gist_stern_r', 'gist_yarg',
    # 'gist_yarg_r', 'gist_yerg', 'gist_yerg_r', 'gnuplot', 'gnuplot2', 'gnuplot2_r', 'gnuplot_r', 'gray',
    # 'gray_r', 'grey', 'grey_r', 'hot', 'hot_r', 'hsv', 'hsv_r', 'inferno', 'inferno_r', 'jet', 'jet_r', 'magma', 'magma_r', 'managua', 'managua_r', 'nipy_spectral', 'nipy_spectral_r', 'ocean', 'ocean_r', 'pink', 'pink_r', 'plasma', 'plasma_r', 'prism', 'prism_r', 'rainbow', 'rainbow_r', 'seismic', 'seismic_r', 'spring', 'spring_r', 'summer', 'summer_r', 'tab10', 'tab10_r', 'tab20', 'tab20_r', 'tab20b', 'tab20b_r', 'tab20c', 'tab20c_r', 'terrain', 'terrain_r', 'turbo', 'turbo_r', 'twilight', 'twilight_r', 'twilight_shifted', 'twilight_shifted_r', 'vanimo', 'vanimo_r', 'viridis', 'viridis_r', 'winter', 'winter_r'
    linewidth=0.3,
    edgecolor="#222222", #removed blue, #d22b2b
    legend=True,
    legend_kwds={
        "shrink": 0.6,
        "label": "Case load",  # Label for the legend
        "orientation": "vertical"  # Legend orientation
    },
    ax=ax
)

# Customize the color bar frame (outline) color to .......
colorbar = ax.get_figure().axes[-1]  # Access the color bar (added as a separate axis)
colorbar.spines["outline"].set_edgecolor("none")  # Set the border color of the color bar to red
colorbar.spines["outline"].set_linewidth(2)  # Set the thickness of the color bar border

# Add District Labels (Name with Cases)
district_texts = []
for _, row in lango_geo.iterrows():
    district_texts.append(
        ax.text(
            x=row.centroid.x,
            y=row.centroid.y,
            s=row["Label"] if row["Cases"] > 0 else "",  # Only label districts with cases
            fontsize=15,
            color="#222222", # #FF4B5C
            fontweight="bold",
            ha="center",
            va="center"
        )
    )

# Adjust text positions to avoid overlaps
adjust_text(district_texts, arrowprops=dict(arrowstyle="-", color="none", lw=0.8))

# Add Title and Adjust Aesthetics
ax.set_title("Mpox Caseload by District, 27/6/25.", color = "#333333", fontsize=25, fontweight="bold") #removed color #20a4d8
ax.axis("off")  # Remove axes for a cleaner look
plt.tight_layout()

# Add Basemap - Use CartoDB.Positron as Background
##ctx.add_basemap(ax, crs=lango_geo.crs.to_string(), source=ctx.providers.CartoDB.Positron)

# Show the Map
plt.show()
