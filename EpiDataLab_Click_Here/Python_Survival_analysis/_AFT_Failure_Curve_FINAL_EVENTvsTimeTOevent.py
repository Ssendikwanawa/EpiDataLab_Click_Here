import pandas as pd
import matplotlib.pyplot as plt
from lifelines import KaplanMeierFitter
from torch.distributions.constraints import lower_cholesky

# Load your dataset
df = pd.read_csv("D:/EVD_Mbale/Mortality_Surveillance/excel_june012025.csv")
# Ensure 'time_to_event' and 'event' columns are present and drop missing values
df = df[['time_to_event', 'event']].dropna()


# Initialize the Kaplan-Meier fitter
kmf = KaplanMeierFitter()

# Fit the Kaplan-Meier estimator on the entire dataset
kmf.fit(df['time_to_event'], event_observed=df['event'])

# Plot the cumulative density function
plt.figure(figsize=(10, 6))
kmf.plot_cumulative_density(linewidth=4.2, zorder=100, ci_show=True, color='darkorange')
plt.title("Overall Failure Curve", fontsize=18.5, fontweight='bold')
plt.xlabel("Time (Days After Death)", fontsize=15, weight='bold')
plt.ylabel("Cumulative Probability", fontsize=15, weight='bold')
plt.grid(True, linestyle='--', linewidth=0.58)
plt.tick_params(axis='both', which='major', labelsize=13)
plt.show()
