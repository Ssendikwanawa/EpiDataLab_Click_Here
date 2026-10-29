import scipy.stats as stats
import pandas as pd
from statsmodels.stats.multicomp import pairwise_tukeyhsd
import matplotlib.pyplot as plt

# Define the group-wise alert predictions
acs_alerts = [2.416667, 2.333333, 2.25, 2.166667, 2.083333,
              2.0, 1.916667, 1.833333, 1.75, 1.666667, 1.583333]
early_alerts = [5.643443, 5.180328, 4.717213, 4.254098, 3.790984,
                3.327869, 2.864754, 2.401639, 1.938525, 1.47541, 1.012295]
ring_alerts = [1.392857, 1.928571, 2.464286, 3.0, 3.535714,
               4.071429, 4.607143, 5.142857, 5.678571, 6.214286, 6.75]

# Combine all data into a single DataFrame for Tukey's HSD
alerts = acs_alerts + early_alerts + ring_alerts
groups = (['ACS'] * len(acs_alerts)) + (['Early Response'] * len(early_alerts)) + (['Ring Sweep'] * len(ring_alerts))

df_tukey = pd.DataFrame({'Alerts': alerts, 'Group': groups})

# Perform Tukey's HSD test
tukey_result = pairwise_tukeyhsd(endog=df_tukey['Alerts'], groups=df_tukey['Group'], alpha=0.05)
print(tukey_result.summary())

# Plot Tukey's HSD test results
tukey_result.plot_simultaneous(comparison_name='ACS', alpha=0.05)
plt.title('Tukey HSD Test: Confidence Intervals for Group Differences')
plt.xlabel('Mean Difference')
plt.ylabel('Group')
plt.show()
