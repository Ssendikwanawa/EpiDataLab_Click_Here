import pandas as pd
from lifelines.statistics import logrank_test

# Load your dataset
df = pd.read_csv(r"D:\EVD_Mbale\Mortality_Surveillance\Encoded_Mortality_Surveillance_Data.csv")

# Keep only needed columns and clean
df = df[['time_to_event', 'Sample_collected', 'PlaceofdeathHF_LEVEL_encoded', 'Source_of_death_alert_encoded']].dropna()
df = df[~df['PlaceofdeathHF_LEVEL_encoded'].astype(str).str.strip().eq("")]
df = df[~df['Source_of_death_alert_encoded'].astype(str).str.strip().eq("")]

# Rename for clarity
df.rename(columns={
    'time_to_event': 'duration',
    'Sample_collected': 'event',
    'PlaceofdeathHF_LEVEL_encoded': 'Facility',
    'Source_of_death_alert_encoded': 'AlertSource'
}, inplace=True)

# Pairwise log-rank test function
def run_pairwise_logrank(data, group_col):
    groups = data[group_col].unique()
    results = []
    for i in range(len(groups)):
        for j in range(i + 1, len(groups)):
            g1, g2 = groups[i], groups[j]
            df1 = data[data[group_col] == g1]
            df2 = data[data[group_col] == g2]
            test = logrank_test(df1["duration"], df2["duration"],
                                event_observed_A=df1["event"],
                                event_observed_B=df2["event"])
            results.append({
                f"{group_col}_Group1": g1,
                f"{group_col}_Group2": g2,
                "Test Statistic": test.test_statistic,
                "P-value": test.p_value
            })
    return pd.DataFrame(results)

# Run tests
facility_results = run_pairwise_logrank(df, 'Facility')
alert_results = run_pairwise_logrank(df, 'AlertSource')

print("=== Log-Rank Test Results: Facility Type ===")
print(facility_results)

print("\n=== Log-Rank Test Results: Source of Death Alert ===")
print(alert_results)
