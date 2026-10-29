import pandas as pd
import matplotlib.pyplot as plt
from lifelines import KaplanMeierFitter

# Load your dataset
data = pd.read_csv("D:/EVD_Mbale/Mortality_Surveillance/excel_june012025.csv")
#"D:\EVD_Mbale\Mortality_Surveillance\nEW_DR.MS_DATA.xlsx"
# Prepare data
data = data.dropna(subset=['time_to_event', 'event'])
data['Sample_collected'] = data['event'].astype(int)
data['time_to_event'] = pd.to_numeric(data['time_to_event'], errors='coerce')

# Define variables for grouping and titles
group_vars = {
    'Source_of_death_alert_encoded': 'Source of Death Alert',
    'PlaceofdeathHF_LEVEL_encoded': 'Place of Death',
    'Sex_encoded': 'Sex'
}

# Set up plot
fig, axes = plt.subplots(1, 3, figsize=(20, 6), sharey=True)

# Plot cumulative failure curves
for ax, (col, title) in zip(axes, group_vars.items()):
    for name, grouped_df in data.groupby(col):
        kmf = KaplanMeierFitter()
        kmf.fit(grouped_df['time_to_event'], event_observed=grouped_df['Sample_collected'], label=str(name))
        kmf.plot_cumulative_density(ax=ax, ci_show=False, zorder=3)

    ax.set_title(title, fontsize=14, fontweight='bold')
    ax.set_xlabel('Time (Days After Death)', fontweight='bold')
    ax.set_ylabel('Probability of Sample Collection', fontweight='bold')
    ax.grid(True, linestyle='--', alpha=0.6)

plt.suptitle('Cumulative Failure Curves for Predictors of Delay in Postmortem Sample Collection', fontsize=16, fontweight='bold')
plt.tight_layout(rect=[0, 0, 1, 0.95])
plt.show()
