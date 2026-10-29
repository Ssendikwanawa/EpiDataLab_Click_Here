import pandas as pd
import plotly.graph_objects as go

# Data Initialization
data = {
    "District": [
        "Alebtong", "Amolatar", "Apac", "Dokolo", "Kole", "Kwania",
        "Lira City", "Lira", "Otuke", "Oyam", "Total"
    ],
    "Total OPD": [3290, 2328, 4548, 4639, 1452, 1736, 1442, 2976, 2246, 8035, 32692],
    "Screened TB in OPD": [2820, 1996, 4000, 4441, 1347, 1739, 1442, 2962, 2087, 7517, 30351],
    "% Screened": ["86%", "86%", "88%", "96%", "93%", "100%", "100%", "100%", "93%", "94%", "93%"],
    "Presumptive TB Identified": [5, 40, 91, 63, 114, 27, 3, 101, 27, 207, 673],
    "New TB Diagnosed": [2, 6, 4, 5, 24, 5, 1, 5, 5, 14, 74],
    "TB Incidence/100000P": ["-", 3, 2, 2, 8, 2, 0, 2, 3, 3, 3]
}

# Convert the data into a DataFrame
df = pd.DataFrame(data)

# Create the table using Plotly
fig = go.Figure(
    data=[
        go.Table(
            # Define the header
            header=dict(
                values=list(df.columns),  # Use column names from DataFrame
                fill_color="lightblue",
                align="center",
                font=dict(size=12, color="black"),
                height=30  # Header height
            ),
            # Define the table body (cells)
            cells=dict(
                values=[df[column] for column in df.columns],  # Table data
                fill_color="white",
                align="center",
                font=dict(size=11, color="black"),
                height=25  # Cell height
            )
        )
    ]
)

# Customize layout (optional)
fig.update_layout(
    title_text="GT Table - TB Screening Statistics by District",
    title_x=0.5,  # Center the title
    width=900,
    height=600
)

# Display the figure
fig.show()
