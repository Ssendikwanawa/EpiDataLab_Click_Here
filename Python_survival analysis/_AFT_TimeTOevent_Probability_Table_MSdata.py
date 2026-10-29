import pandas as pd
from lifelines import KaplanMeierFitter

# Load your dataset
df = pd.read_csv("D:/EVD_Mbale/Mortality_Surveillance/excel_june012025.csv")

# Prepare data
df = df[['time_to_event', 'event']].dropna()

# Fit the Kaplan-Meier model
kmf = KaplanMeierFitter()
kmf.fit(durations=df['time_to_event'], event_observed=df['event'])

# Extract failure probabilities
failure_table = kmf.cumulative_density_.reset_index()
failure_table.columns = ['Time (Days After Death)', 'Cumulative Failure Probability']
print(failure_table)
