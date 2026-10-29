import matplotlib.pyplot as plt
import pandas as pd

# Data for the plot
data = {
    "District": ["Dokolo", "Other\nDistricts"],
    "Signals Received": [2, 0],  # Signals in each category
    "Signal Type": [
           "Mpox Signal", "No Signals Sent"

    ],
}
# Convert the data to a DataFrame
df = pd.DataFrame(data)

# Define colors to distinguish between different signal types
colors = {
    "Mpox Signal": "darkgreen",  # Red for human signals
    "No Signals Sent": "black",  # Grey for no signals
}

# Add a column for color based on signal type
df["Color"] = df["Signal Type"].map(colors)

# Create a bubble plot - Positioning bubbles with a slight horizontal offset for clarity
plt.figure(figsize=(18, 13))

# Set black backgrounds for the plot, x-axis, and y-axis
ax = plt.gca()  # Get the current axis
ax.set_facecolor("white")  # Set background of the plot to black
plt.gcf().set_facecolor("white")  # Set the overall figure background to black

# Set grid style
plt.grid(
    color="lightgray",  # Light gray grid lines for readability
    linestyle="--",  # Thin dashed lines
    linewidth=1.9,  # Gridline thickness
    alpha=0.8,  # Slight transparency
    zorder=3  # Render behind all other elements
)

# Dynamically calculate horizontal offsets for overlapping signals in the same district
unique_districts = df["District"].unique()
district_offsets = {}

for district in unique_districts:
    # Generate offsets dynamically based on number of signals for the district
    subset = df[df["District"] == district]
    num_signals = len(subset)
    if num_signals > 1:
        # Evenly space offsets from negative to positive around 0
        district_offsets[district] = [
            (i - (num_signals - 1) / 2) * 0.2 for i in range(num_signals)
        ]
    else:
        district_offsets[district] = [0]  # No offset needed for single signal

# Debugging: Print district_offsets to ensure correctness
print("District Offsets Debugging:", district_offsets)

# Plot the data
for district in unique_districts:
    subset = df[df["District"] == district]  # Filter data by district
    x_base_position = list(unique_districts).index(district)

    # Ensure the offset list matches the exact number of rows (just in case)
    offsets = district_offsets[district]
    if len(offsets) != len(subset):
        print(f"Error: Offsets for {district} do not match subset length.")
        print(f"Offsets: {offsets}, Subset Length: {len(subset)}")
        offsets = [0] * len(subset)  # Reset to default offsets to prevent errors

    # Create x_positions
    x_positions = [x_base_position + offsets[i] for i in range(len(subset))]

    # Debugging: Print x_positions for each district
    print(f"District: {district}, x_positions: {x_positions}")

    # Scatter bubbles; size scaled by signal count
    plt.scatter(
        x_positions,
        subset["Signals Received"],
        s=[value * 430 if value > 0 else 320 for value in subset["Signals Received"]],
        c=subset["Color"],  # Color based on signal type
        alpha=1,  # Brightness context of the dot
        edgecolors="None",  # Adding white edges for readability
        zorder=4  # Render above the grid and background
    )

    # Safely Add labels to bubbles: Ensure proper iteration
    for position, (index, row) in zip(x_positions, subset.iterrows()):
        plt.text(
            position,  # Slightly offset horizontally
            row["Signals Received"] + 0.3,  # Slightly above bubble to prevent overlap
            f"{row['Signals Received']}",  # Signals count
            ha="center",  # Align horizontally center
            fontsize=27, color="black", weight="bold",  # Set text color to white for visibility
            zorder=5  # Make labels appear on top
        )

# Customize the plot
plt.title(
    "Signals sent through Alert SMS 6767", fontsize=24, weight="bold", color="black"
)
plt.xlabel("", fontsize=12, color="black")
plt.ylabel("Count of Signals", fontsize=19, weight="bold", color="black")
plt.xticks(range(len(unique_districts)), unique_districts, fontsize=18, weight="bold", color="black")

# Set the Y-axis limits and show only whole numbers
y_min, y_max = -1, max(df["Signals Received"]) + 1
plt.ylim(y_min, y_max + 0.5)
plt.yticks(range(y_min, y_max), fontsize=19, weight="bold", color="black")  # Whole numbers only

# Add a legend for Signal Types at the bottom of the plot
for signal_type, color in colors.items():
    plt.scatter([], [], c=color, s=300, label=signal_type,
                alpha=1, edgecolors="None", zorder=2)

# Place the legend at the bottom
plt.legend(
    title="KEY",
    title_fontsize=16,
    fontsize=14,
    loc="upper right",  # Position legend at bottom-center of plot
    ncol=2,  # Organize into 2 columns for better layout
    facecolor="white",  # Black legend background
    edgecolor="black",  # White border for legend
    framealpha=1,  # Solid frame
    labelcolor="black"  # White text for legend labels
)

# Adjust layout with additional space for the legend
plt.tight_layout(rect=[0, 0, 1, 0.9])  # Adjust plot to leave extra space for bottom legend

# Display the plot
plt.show()
