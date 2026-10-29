import os


# Clear the terminal for different operating systems
def clear_terminal():
    if os.name == 'nt':  # For Windows
        os.system('cls')
    else:  # For macOS and Linux
        os.system('clear')
# Call the function to clear the terminal
clear_terminal()




######### Now the Heat Plot
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# File path for your dataset (Update this to your actual file path)
file_path = r"C:\Users\Administrator\Desktop\EpidemicProneDses.csv"  # Update this to match your system

# Load the dataset
try:
    # Read the dataset from a CSV file
    df = pd.read_csv(file_path)
    # Ensure the "District" column exists and set it as the index
    if "District" in df.columns:
        df = df.set_index("District")
    else:
        raise KeyError("The file is missing the 'District' column. Please verify the dataset.")
    # For proper plotting, transpose the DataFrame: Diseases on the Y-axis, Districts on the X-axis
    df = df.transpose()
    # Create a heatmap
    plt.figure(figsize=(10, 6))  # Set figure size
    sns.heatmap(df, annot=True, fmt="d", cmap="YlGnBu", cbar=True, linewidths=0.5)
    # Add x-axis label rotation
    plt.xticks(rotation=80, ha='right')  # Rotates x-axis labels by 80 degrees
    # Add titles and axis labels
    plt.title("Disease Cases by District (Weekly Heatmap)", fontsize=28, weight="bold")
    plt.xlabel("", fontsize=30, weight="bold")
    plt.ylabel("", fontsize=30, weight="bold")
    # Adjust the plot for better layout
    plt.tight_layout()
    # Save the plot as an image to your desktop
    output_file_path = r"C:\Users\Administrator\Desktop\heatmap_plot.png"  # Update this if needed
    plt.savefig(output_file_path, dpi=300)  # Save the figure with high resolution
    # Show the heatmap
    plt.show()
    print(f"Heatmap successfully saved at: {output_file_path}")
except FileNotFoundError as e:
    logging.error(e)
    print(f"Error: {e}.")
except KeyError as e:
    logging.error(e)
    print(f"Error: {e}.")
except Exception as e:
    logging.error(f"An unexpected error occurred: {e}.")
    print(f"An unexpected error occurred: {e}. Please check the logs for more details.")
