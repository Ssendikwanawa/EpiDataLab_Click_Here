import matplotlib.pyplot as plt
import numpy as np

# Sample survival data for illustrative purposes
time = np.linspace(0, 10, 100)
failure_public = 1 - (1 / (1 + (time/2)**2))
failure_pfp = 1 - (1 / (1 + (time/2.5)**2))
failure_pnfp = 1 - (1 / (1 + (time/1.5)**2))

plt.figure(figsize=(10, 6))
plt.plot(time, failure_public, label='Public Facility', linestyle='-', color='blue')
plt.plot(time, failure_pfp, label='Private For-Profit', linestyle='--', color='green')
plt.plot(time, failure_pnfp, label='Private Not-for-Profit', linestyle='-.', color='red')

plt.xlabel('Time (Days After Death)', fontsize=12)
plt.ylabel('Cumulative Probability of Sample Collection', fontsize=12)
plt.title('Failure Curves by Health Facility Level', fontsize=14, fontweight='bold')
plt.grid(True)
plt.legend(title='Facility Type')
plt.tight_layout()
plt.show()
