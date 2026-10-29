import pandas as pd
import matplotlib.pyplot as plt
from lifelines import KaplanMeierFitter

# Load your dataset
df = pd.read_csv("D:/EVD_Mbale/Mortality_Surveillance/excel_june012025.csv")

# Count values of the Sex variable
sex_counts = df['Sex_encoded'].value_counts()
# Display the counts
print(sex_counts)

# Recode event variable
df['Sample_collected'] = df['event']

# Clean and map labels
df = df[['time_to_event', 'Sample_collected', 'Source_of_death_alert_encoded',
         'PlaceofdeathHF_LEVEL_encoded', 'Sex_encoded']].dropna()

df['Source_of_death_alert_encoded'] = df['Source_of_death_alert_encoded'].map({
    0: 'Community Alert', 1: 'Health Facility Alert'
})
df['PlaceofdeathHF_LEVEL_encoded'] = df['PlaceofdeathHF_LEVEL_encoded'].map({
    'Community': 'Community', 'PFP': 'PFP', 'PNFP': 'PNFP', 'Public': 'Public'
})
df['Sex_encoded'] = df['Sex_encoded'].map({
    'Male': 'Male', 'Female': 'Female', 0: 'Male', 1: 'Female'
})

# Initialize plot
fig, axes = plt.subplots(1, 3, figsize=(20, 6), sharey=True)
fig.suptitle("Kaplan Meier Failure Curves for Predictors of Delay in the Adjusted AFT Model", fontsize=18.5, fontweight='bold')

kmf = KaplanMeierFitter()

# Function to plot failure curve for each group
def plot_failure(ax, group_col, title):
    for name, g in df.groupby(group_col):
        if g['Sample_collected'].sum() > 0:
            kmf.fit(g['time_to_event'], event_observed=g['Sample_collected'], label=name)
            kmf.plot_cumulative_density(ax=ax, linewidth=4.2, zorder=100, ci_show=False)
    ax.set_title(title, fontsize=18)
    ax.set_xlabel("")  # Remove anything below the X axis of each plots
    ax.set_ylabel("Cumulative Probability", fontsize=15, weight='bold')
    ax.grid(True, linestyle='--', linewidth=0.58)
    ax.legend(title="Key", fontsize=13, title_fontsize=15)

# Create all three subplots
plot_failure(axes[0], 'Source_of_death_alert_encoded', 'Source of Death Alert')
plot_failure(axes[1], 'PlaceofdeathHF_LEVEL_encoded', 'Place/Setting of Death')
plot_failure(axes[2], 'Sex_encoded', 'Sex of the Deceased')

# Add one shared X-axis label for all plots
fig.text(0.5, 0.01, 'Time (Days After Death)', ha='center', fontsize=15, weight='bold')

plt.tight_layout(rect=[0, 0.05, 1, 0.95])
plt.show()
