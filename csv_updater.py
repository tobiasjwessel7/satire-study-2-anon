import pandas as pd

# Load the dataset
file_path = 'updated_articles_with_versions.csv'  # Replace with the path to your CSV file
articles_df = pd.read_csv(file_path)

# Add the new article
new_article = {
    'Category': 'climate',
    'Title': 'US Chamber, oil industry sue Vermont over law targeting climate disinformation',
    'Image URL': 'https://www.washingtonpost.com/wp-apps/imrs.php?src=https://www.exampleimage.com/climate-law.jpg',
    'Article URL': 'https://www.washingtonpost.com/business/2025/01/02/vermont-climate-disinformation-law/',
    'Content': (
        "The U.S. Chamber of Commerce and a top oil and gas trade group have filed a lawsuit against the state of Vermont, challenging a new law aimed at holding fossil fuel companies accountable for climate change misinformation. "
        "Vermont's recently passed legislation allows lawsuits against corporations that knowingly mislead the public about their role in global warming, marking one of the first such measures in the United States. Critics argue that the law violates free speech protections, while proponents say it is necessary to combat deceptive practices that hinder climate action. "
        "The lawsuit claims the statute is overly broad and could have a chilling effect on public discourse, potentially suppressing important debates about energy policy and climate solutions. ..."
    ),
    'Satirical Title': 'Big Oil Fights for Freedom to Fib—Sues Vermont for Ruining Climate Fairytales',
    'Satirical Content': (
        "In a bold stand for corporate creativity, the U.S. Chamber of Commerce and Big Oil’s VIP club have sued Vermont for daring to pass a law against climate disinformation. "
        "The law, which rudely insists companies tell the truth about global warming, has sent oil executives scrambling to protect their constitutional right to spin bedtime stories about clean coal and polar bear-friendly pipelines.\n\n"
        "Critics warn Vermont’s “anti-fibbing” crusade could silence crucial debates—like whether rising sea levels are just Earth’s way of experimenting with feng shui. "
        "Meanwhile, supporters claim it’s time to hold fossil fuel giants accountable for decades of hot air. As the case unfolds, observers eagerly await whether Vermont will douse Big Oil’s fire—or just add more fuel to it..."
    ),
    'Objective Title': 'US Chamber and Oil Industry Challenge Vermont\'s Climate Disinformation Law',
    'Objective Content': (
        "The U.S. Chamber of Commerce and a leading oil and gas trade association have filed a lawsuit against Vermont, contesting a newly enacted law that targets climate change misinformation. "
        "The legislation permits legal action against corporations accused of knowingly misrepresenting their contributions to global warming. "
        "Supporters of the law argue it addresses deceptive practices that obstruct climate progress, while opponents claim it infringes on free speech rights. "
        "The lawsuit contends the law’s broad language may discourage open discussions on energy and climate policy. This case highlights broader tensions between corporate accountability and protections for public debate as climate policies continue to evolve..."
    )
}

# Append the new article to the DataFrame using pd.concat
new_article_df = pd.DataFrame([new_article])
articles_df = pd.concat([articles_df, new_article_df], ignore_index=True)

# Save the updated dataset
updated_csv_path = 'updated_test_article.csv'
articles_df.to_csv(updated_csv_path, index=False)

print(f"Updated file saved as: {updated_csv_path}")
