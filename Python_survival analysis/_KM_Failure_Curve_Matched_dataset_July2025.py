import pandas as pd
import matplotlib.pyplot as plt
from lifelines import KaplanMeierFitter

# Load  dataset
data = pd.read_excel("D:/EVD_Mbale/Mortality_Surveillance/Matched_Case_Control_MS_Data_July_2025.xlsx")

print(data.columns.tolist())

# Convert date columns to datetime
data['Date_of_Death'] = pd.to_datetime(data['Date_of_Death'], errors='coerce')
data['Date_of_sample_collection'] = pd.to_datetime(data['Date_of_sample_collection'], errors='coerce')


# Create time_to_event in days
data['time_to_event'] = (data['Date_of_sample_collection'] - data['Date_of_Death']).dt.days

# Drop rows with missing time or Sample_collected values
data = data.dropna(subset=['time_to_event', 'Sample_collected'])

# Ensure correct data types
data['Sample_collected'] = data['Sample_collected'].astype(int)
data['time_to_event'] = pd.to_numeric(data['time_to_event'], errors='coerce')

# Define groups for plotting
group_vars = {
    'District': 'District of Death',
    'Death_outside_HomeDistrict': 'Death happening Outside\nDistrict of Usual Residence',
    'Sampled_at_point_of_death': 'Sample Obtained\nat Point of Death'
}


# Line width setting — adjust this value to make lines thicker or thinner
line_width = 4.5

# Set up subplot grid
fig, axes = plt.subplots(1, 3, figsize=(21, 9), sharey=True)

# Plot failure curves
for ax, (col, title) in zip(axes, group_vars.items()):
    for name, grouped_df in data.groupby(col):
        kmf = KaplanMeierFitter()
        kmf.fit(grouped_df['time_to_event'], event_observed=grouped_df['Sample_collected'], label=str(name))
        kmf.plot_cumulative_density(ax=ax, ci_show=False, zorder=5, linewidth=line_width)

        # Add bold, larger legend
        legend = ax.legend(
            fontsize=12,
            loc='lower right',  #  fix legend to bottom-right
            frameon=True,
            facecolor='white',
            edgecolor='gray'
        )
        for text in legend.get_texts():
            text.set_fontweight('normal')

    ax.set_title(title, fontsize=17, fontweight='bold')
    ax.set_ylabel('Probability of Sample Collection', fontsize=17, fontweight='bold')
    ax.grid(True, linestyle='-', alpha=0.4)
    ax.set_xlabel('')  # remove individual x-axis labels

    ax.tick_params(axis='both', labelsize=12)  # 👈 increase number font sizes on both axes

# Shared x-axis label
fig.text(0.5, 0.04, 'Time (Days After Death)', ha='center', fontsize=17, fontweight='bold')

# Final plot title
plt.suptitle('Kaplan-Meier Failure Estimates for Other Exposure Variables on Postmortem Sample Collection', fontsize=19)
plt.tight_layout(rect=[0, 0.05, 1, 0.95])
plt.show()
