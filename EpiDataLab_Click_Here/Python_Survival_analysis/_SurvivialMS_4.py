# === Required Libraries ===
import pandas as pd
from lifelines import CoxPHFitter

# === Load Data ===
df = pd.read_csv(r"D:\EVD_Mbale\Mortality_Surveillance\Encoded_Mortality_Surveillance_Data.csv")

# === Data Cleaning ===
df = df[['time_to_event', 'Sample_collected', 'Age',
         'District_encoded', 'Source_of_death_alert_encoded',
         'PlaceofdeathHF_LEVEL_encoded', 'Sex_encoded',
         'Death_outside_HomeDistrict']].dropna()

df = df[~df['Sex_encoded'].astype(str).str.strip().eq("")]

# === Rename columns ===
df.rename(columns={
    'Sample_collected': 'event',
    'time_to_event': 'duration',
    'District_encoded': 'District',
    'Source_of_death_alert_encoded': 'Source_of_death_alert',
    'PlaceofdeathHF_LEVEL_encoded': 'PlaceofdeathHF_LEVEL',
    'Sex_encoded': 'Sex'
}, inplace=True)

# === Convert categorical variables to string ===
for col in ['District', 'Source_of_death_alert', 'PlaceofdeathHF_LEVEL']:
    df[col] = df[col].astype(str)

# === One-hot encode all EXCEPT 'Sex' ===
df_encoded = pd.get_dummies(df, columns=['District', 'Source_of_death_alert', 'PlaceofdeathHF_LEVEL'], drop_first=True)

# === Fit Cox Proportional Hazards Model Stratified by Sex ===
cph = CoxPHFitter()
cph.fit(df_encoded, duration_col='duration', event_col='event', strata=['Sex']) #We shall do this for the other variables that also violated PH assumptions

# === Output Summary ===
summary = cph.summary[['exp(coef)', 'se(coef)', 'z', 'p', 'exp(coef) lower 95%', 'exp(coef) upper 95%']].rename(columns={
    'exp(coef)': 'HR',
    'se(coef)': 'SE',
    'z': 'Z',
    'p': 'P-value',
    'exp(coef) lower 95%': 'CI Lower 95%',
    'exp(coef) upper 95%': 'CI Upper 95%'
})

print("\nStratified Cox Model Results (HR, SE, Z, P-value, 95% CI):")
pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
pd.set_option('display.width', 1000)

from lifelines.statistics import proportional_hazard_test

# Run PH assumption test on the stratified model
ph_test_results = proportional_hazard_test(cph, df_encoded, time_transform='rank')

# Display just the p-values
print("\nPH Assumption Test (P-values):")
print(ph_test_results.summary[['p']])


print(summary)
