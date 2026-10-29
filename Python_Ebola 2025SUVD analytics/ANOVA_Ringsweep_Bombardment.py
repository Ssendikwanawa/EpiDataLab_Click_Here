import scipy.stats as stats

# Define the group-wise alert predictions
acs_alerts = [2.416667, 2.333333, 2.25, 2.166667, 2.083333,
              2.0, 1.916667, 1.833333, 1.75, 1.666667, 1.583333]
early_alerts = [5.643443, 5.180328, 4.717213, 4.254098, 3.790984,
                3.327869, 2.864754, 2.401639, 1.938525, 1.47541, 1.012295]
ring_alerts = [1.392857, 1.928571, 2.464286, 3.0, 3.535714,
               4.071429, 4.607143, 5.142857, 5.678571, 6.214286, 6.75]

# Perform one-way ANOVA
anova_result = stats.f_oneway(acs_alerts, early_alerts, ring_alerts)

# Print the result
print(f"F-statistic: {anova_result.statistic:.4f}, p-value: {anova_result.pvalue:.4e}")
