import pandas as pd
from lifelines import KaplanMeierFitter

# === Load and Clean Data ===
df = pd.read_csv(r"D:\EVD_Mbale\Mortality_Surveillance\Encoded_Mortality_Surveillance_Data.csv")

# Keep only relevant columns and drop missing
df = df[['time_to_event', 'Sample_collected', 'Sex_encoded']].dropna()
df = df[~df['Sex_encoded'].astype(str).str.strip().eq("")]

# Rename for consistency
df.rename(columns={
    'time_to_event': 'duration',
    'Sample_collected': 'event',
    'Sex_encoded': 'Sex'
}, inplace=True)

# Ensure Sex is string
df['Sex'] = df['Sex'].astype(str)

# === Median Survival Summary by Sex ===
summary = []
kmf = KaplanMeierFitter()

for sex_group in df['Sex'].unique():
    group_data = df[df['Sex'] == sex_group]
    kmf.fit(group_data["duration"], event_observed=group_data["event"], label=sex_group)

    median_ = kmf.median_survival_time_
    ci_lower, ci_upper = kmf.confidence_interval_survival_function_.iloc[:, 0].dropna().min(), kmf.confidence_interval_survival_function_.iloc[:, 0].dropna().max()

    summary.append({
        'Sex': sex_group,
        'Median Survival Time': median_,
        '95% CI Lower Bound': ci_lower,
        '95% CI Upper Bound': ci_upper
    })

# Convert to DataFrame and print
median_df = pd.DataFrame(summary)
print("\n=== Median Time to Sample Collection by Sex ===")
print(median_df)
