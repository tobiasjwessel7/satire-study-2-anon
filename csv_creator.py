import pandas as pd

# File paths
INPUT_CSV = 'articles.csv'  # Source CSV with all articles
OUTPUT_CSV = 'selected_articles.csv'  # Destination CSV with selected articles

# Load the dataset
df = pd.read_csv(INPUT_CSV)

# Ensure there are enough articles to sample
if len(df) < 10:
    print("Not enough articles available in the dataset to select 10. Exiting.")
else:
    # Select 10 random articles without replacement
    selected_articles = df.sample(n=10, random_state=42)  # Random seed for consistency

    # Save to new CSV
    selected_articles.to_csv(OUTPUT_CSV, index=False)

    print(f"Successfully selected 10 random articles and saved to {OUTPUT_CSV}")
