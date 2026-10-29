import pandas as pd

# Define the data as a dictionary
data = {
    "District": [
        "Alebtong", "Amolatar", "Apac", "Dokolo", "Kole", "Kwania",
        "Lira City", "Lira", "Otuke", "Oyam", "REGION"
    ],
    "Total OPD": [3187, 1449, 3062, 3998, 1196, 1897, 2325, 2655, 3243, 6945, 29957],
    "Screened_TB in OPD": [3187, 1436, 1735, 4089, 899, 1864, 2325, 2643, 2243, 6896, 27317],
    "% Screened": [100, 99, 57, 102, 75, 98, 100, 100, 69, 99, 91],
    "Presumptive TB": [21, 6, 11, 5, 13, 159, 45, 31, 73, 64, 428],
    "% Diagnosed": [10, 17, "No Data", "No Data", 15, 3, 10, 3, 10, 14, 8],
    "TB Diagnosed": [2, 1, "No Data", "No Data", 2, 5, 9, 1, 7, 9, 36],
    "% Started on Rx": [100, 100, "No Data", "No Data", 100, 100, 100, 100, 57, 100, 92],
    "TB Incidence": [1, 1, "No Data", "No Data", 1, 2, 4, 0, 4, 2, 1]
}

# Convert the dictionary data to a Pandas DataFrame
df = pd.DataFrame(data)

# Format numbers with commas for readability (where applicable)
df["Total OPD"] = df["Total OPD"].apply(lambda x: f"{x:,}")
df["Screened_TB in OPD"] = df["Screened_TB in OPD"].apply(lambda x: f"{x:,}")
df["Presumptive TB"] = df["Presumptive TB"].apply(lambda x: f"{x:,}" if isinstance(x, int) else x)
df["TB Diagnosed"] = df["TB Diagnosed"].apply(lambda x: f"{x:,}" if isinstance(x, int) else x)


# Define a function for row-based styling
def style_row(row_index):
    if row_index % 2 == 0:
        return "background-color: yellow; color: white;"
    else:
        return "background-color: blue; color: white;"


# Apply styles using Pandas Styler
styled_df = df.style.set_table_styles([
    {'selector': 'thead',
     'props': [('background-color', 'black'), ('color', 'white'), ('font-weight', 'bold'), ('text-align', 'center')]}
    # Header styles
]).set_properties(**{
    'background-color': 'black',  # Black background for all cells
    'color': 'white',  # White text for all cells
    'text-align': 'center',  # Center-align text in all cells
}).applymap(lambda _: style_row(df.index.get_loc(_)), subset=df.index).set_caption(
    'Table: TB Screening Summary by District (Black Background, Blended Colors)'
)

# Export to HTML (Optional for use outside Jupyter Notebook)
styled_df.to_html("styled_tb_screening_table.html", index=False)

# Display styled table in Jupyter Notebook (if applicable)
styled_df
