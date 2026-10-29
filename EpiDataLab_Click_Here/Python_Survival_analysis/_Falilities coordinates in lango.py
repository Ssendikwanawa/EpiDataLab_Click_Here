import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt

# File paths
shapefile_path = "C:\\Users\\Administrator\\Desktop\\uganda_districts.shp"
healthfacility_coordinates ="D:\\Ssendi\\HF ownership_govt_NGO_shapefiles.csv"

# Step 1: Load shapefile data
districts = gpd.read_file(shapefile_path)

# Step 2: Define Lango region districts
lango_districts = [
    "LIRA CITY", "APAC", "OTUKE", "DOKOLO", "OYAM", "KWANIA",
    "LIRA", "AMOLATAR", "KOLE", "ALEBTONG"
]

# Step 3: Filter districts for Lango region
lango_region = districts[districts['District'].str.upper().isin([d.upper() for d in lango_districts])]

# Step 4: Load health facility coordinates data
health_facilities = pd.read_csv(healthfacility_coordinates)

# Convert health facility DataFrame into a GeoDataFrame
geometry = gpd.points_from_xy(health_facilities['LONG'], health_facilities['LAT'])
health_facilities_gdf = gpd.GeoDataFrame(health_facilities, geometry=geometry)

# Step 5: Plot the data
fig, ax = plt.subplots(figsize=(12, 10))

# Plot Lango region districts
lango_region.plot(ax=ax, color='white', edgecolor='black')

# Plot health facilities by ownership category
ownership_colors = {
    'GOV': 'blue',
    'PNFP': 'green',
    'PFP': 'red'
}

for ownership, color in ownership_colors.items():
    subset = health_facilities_gdf[health_facilities_gdf['ownership'] == ownership]
    subset.plot(ax=ax, color=color, markersize=50, label=f"{ownership} Ownership")

# Add legend and titles
plt.legend()
plt.title("Health Facilities Ownership in Lango Region", fontsize=16)
plt.xlabel("Longitude", fontsize=12)
plt.ylabel("Latitude", fontsize=12)
plt.grid(True)
plt.show()
