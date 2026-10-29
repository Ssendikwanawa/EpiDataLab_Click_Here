import pandas as pd
import ace_tools as tools

# Create the data for the table
data = {
    "Group Comparison": [
        "ACS Alerts vs Early Alerts",
        "ACS Alerts vs Ring Sweep Alerts",
        "Early Alerts vs Ring Sweep Alerts"
    ],
    "Mean Difference": [1.33, 2.07, 0.744],
    "95% CI Lower": [0.051, 0.596, -1.05],
    "95% CI Upper": [2.61, 3.55, 2.54],
    "p-value": [0.042, 0.008, 0.555]
}

df = pd.DataFrame(data)

# Display the dataframe to the user
tools.display_dataframe_to_user(name="Games-Howell Post Hoc Comparison Table", dataframe=df)
