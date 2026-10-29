import matplotlib.pyplot as plt

# Data for the bar chart
categories = ["Macerated\nStillbirths",
              "Early\nNeonatal\nDeaths",
              "Fresh\nStillbirths" ]
proportions = [14, 43, 43]

# Define a brown intriguing gradient color palette
colors = ["#72c6e9", "#20a4d8", "#1878a0"]

# Create the figure
plt.figure(figsize=(10, 4))

# Add the actual bars on top
plt.barh(
    categories,
    proportions,
    color=colors,
    height=0.8  # Normal bar height
)


# Adding title and labels
plt.title(
    "Forms of Perinatal Deaths, WK8",
    fontsize=23,  # Larger title font size
    weight="bold",
    color="#333333"  # Title color remains unchanged
)
plt.xlabel(
    "",
    fontsize=16,  # Larger x-axis label font size
    color="#5a3d2b"  # x-axis label color remains dark brown
)
plt.ylabel(
    "",
    fontsize=16,  # Larger y-axis label font size
    color="#5a3d2b"  # y-axis label color remains dark brown
)

# Explicitly increase Y-axis tick label font size and set tick label color to white for contrast
plt.gca().yaxis.set_tick_params(labelsize=14, labelcolor="black")  # White ticks on y-axis
# Explicitly increase X-axis tick label font size and set tick label color to white for contrast
plt.gca().xaxis.set_tick_params(labelsize=16, labelcolor="black")  # White ticks on x-axis

# Add percentage annotations next to each bar, slightly to the right
for i, value in enumerate(proportions):
    plt.text(
        value + -0.00007,  # Place annotations slightly to the right of the bar
        i,  # Bar position
        f"{value}%",
        va="center",
        fontsize=19,
        weight="bold",  # Larger font size for annotations
        color="black"  # Yellow annotation text color for contrast
    )

# Remove extra white space at the top and bottom of the plot
plt.ylim(-0.2, len(categories) - 0.2)  # Compress the vertical space around the bars

# Remove all spines (cleaner plot)
for spine in plt.gca().spines.values():
    spine.set_visible(False)

# Adjust layout to avoid clipping
plt.tight_layout()

# Display the plot
plt.show()
