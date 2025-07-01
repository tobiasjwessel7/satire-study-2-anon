import requests
import os
from dotenv import load_dotenv
import csv
import random
from datetime import datetime
import time

load_dotenv()

# NewsCatcher API settings
NEWS_API_KEY = os.getenv('NEWS_API_KEY')
API_URL = "https://v3-api.newscatcherapi.com/api/search"

# Categories and settings
categories = ['politics', 'sports', 'climate', 'technology', 'finance', 'conflicts', 'entertainment', 'lifestyle', 'weather']
articles_per_category = 10

# Fetch articles from NewsCatcher API
def fetch_news_articles(category):
    params = {
        "q": (
            # Weather: block Capital-Weather-Gang stubs
            'weather AND NOT "Capital Weather Gang" '
            'AND NOT "morning weather update" AND NOT podcast'
            if category == "weather"
            # Entertainment: block Fox-newsletter stubs
            else (
                'entertainment AND NOT "Fox News Entertainment Newsletter" '
                'AND NOT "entertainment newsletter" AND NOT newsletter'
                if category == "entertainment"
                # All other categories: plain keyword
                else category
            )
        ),
        'lang': 'en',
        'sources': 'nytimes.com, reuters.com, foxnews.com, cbsnews.com, washingtonpost.com, cnn.com',
        'sort_by': 'relevancy',
        'page_size': articles_per_category,  # Fetch 10 articles per category
        'include_nlp_data': True,
        'exclude_duplicates': True
    }

    headers = {
        'x-api-token': NEWS_API_KEY
    }

    response = requests.get(API_URL, headers=headers, params=params)

    if response.status_code == 200:
        return response.json().get('articles', [])
    else:
        print(f"Error fetching data for {category}: {response.status_code}")
        return []

# Write the articles to a CSV file
def write_articles_to_csv(articles, filename='articles.csv'):
    headers = ['Category', 'Title', 'Image URL', 'Article URL', 'Content', 'News Provider']

    with open(filename, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerow(headers)

        for article in articles:
            writer.writerow(article)

    print(f"Data has been written to {filename}")

# Extract one random article per category
def extract_random_articles(filename='articles.csv', output_file='selected_articles.csv'):
    category_articles = {}

    with open(filename, mode='r', encoding='utf-8') as file:
        reader = csv.DictReader(file)
        for row in reader:
            category = row['Category']
            if category not in category_articles:
                category_articles[category] = []
            category_articles[category].append(row)

    selected_articles = []
    for category in categories:
        if category in category_articles and category_articles[category]:
            selected_article = random.choice(category_articles[category])
            selected_articles.append(selected_article)

    # Write selected articles to new CSV
    with open(output_file, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=['Category', 'Title', 'Image URL', 'Article URL', 'Content', 'News Provider'])
        writer.writeheader()
        writer.writerows(selected_articles)

    print(f"Selected articles written to {output_file}")

# Main function
def main():
    all_articles = []

    for category in categories:
        articles = fetch_news_articles(category)
        time.sleep(1)  # Add delay to avoid rate limits
        for article in articles:
            title = article.get('title', 'N/A')
            image_url = article.get('media', 'N/A')
            article_url = article.get('link', 'N/A')
            content = article.get('content', 'No Content')
            provider = article.get('clean_url', 'N/A').lower()
            all_articles.append([category, title, image_url, article_url, content, provider])

    if all_articles:
        write_articles_to_csv(all_articles)
        extract_random_articles()
    else:
        print("No articles found.")

if __name__ == "__main__":
    main()


