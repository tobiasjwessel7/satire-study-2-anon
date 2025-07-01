import os
import csv
import openai
import re
import json
import time  # Added to allow a delay between retries
from dotenv import load_dotenv

load_dotenv()  # Loads environment variables from a .env file if available

OPENAI_API_KEY = os.environ.get("OPENAI_API_KEY")
openai.api_key = OPENAI_API_KEY

INPUT_CSV = 'articles.csv'
OUTPUT_CSV = 'manipulated_articles.csv'

def shorten_to_80_words(text: str) -> str:
    """
    Truncates content to approximately 80 words while trying to avoid splitting sentences.
    If a sentence would push the total well over 80 words, it stops before that sentence.
    If the first sentence is already longer than 80 words, it truncates that sentence.
    """
    words = text.split()
    if len(words) <= 80:
        return text

    # Split text into sentences using regex 
    sentences = re.split(r'(?<=[.!?])\s+', text)
    truncated_text = ""
    total_words = 0

    for sentence in sentences:
        sentence_words = sentence.split()
        if total_words + len(sentence_words) <= 80:
            truncated_text += sentence + " "
            total_words += len(sentence_words)
        else:
            if total_words == 0:
                truncated_text = " ".join(sentence_words[:80])
            break

    return truncated_text.strip()

def generate_satirical_title_and_content(original_title: str, original_content: str, max_retries: int = 3):
    """
    Generates a satirical title and content with the help of the OpenAI API.
    The prompt instructs the API to return valid JSON in the format:
      {
        "satirical_title": "<satirical title>",
        "satirical_content": "<satirical content>"
      }
    The function attempts to parse the output as JSON and retries if parsing fails.
    """
    truncated_content = shorten_to_80_words(original_content)
    truncated_length = len(truncated_content.split())

    # Determine a word constraint message based on content length.
    word_constraint = (
        f"Please ensure that the satirical content has approximately {truncated_length} words, where {truncated_length} is the word count of the truncated content. The output should be within ±10% of that number."
        if truncated_length > 40 #Are we sure about this one? Here we also get the problem that manipulations fail if the content is too short
        else f"Please produce satirical content that is roughly {truncated_length} words in length (matching the word count of the truncated content). While humor remains the main focus, aim to keep the output within about ±10% of that number."
    )
    # Using condition C ('Free satire')
    system_message = (
        "You are an expert satirical columnist writing for a modern humor-driven news platform. "
        "Rewrite this article in a free-flowing, sarcastic, and witty style while keeping every factual detail intact. "
        "Use sharp, observational humor to highlight the absurdity of the news, but avoid exaggeration that distorts the meaning. "
        "Make it feel engaging and shareable, as if it were written for a savvy digital audience. "
        "The satirical title should match the tone and content of the rewritten article. "
        f"{word_constraint}"
    )

    user_prompt = f"""
Original Title: {original_title}
Original Content ({truncated_length} words):
\"\"\"{truncated_content}\"\"\"

Requirements:
1) Produce a satirical version of the title.
2) Rewrite the content in a satirical tone.
3) If the original content is short, match its length but focus on humor.
4) Return your output as valid JSON in the following format exactly:

{{
  "satirical_title": "<satirical title>",
  "satirical_content": "<satirical content>"
}}
    """

    for attempt in range(1, max_retries + 1):
        try:
            response = openai.ChatCompletion.create(
                model="gpt-4",
                temperature=0.6,
                max_tokens=400,
                messages=[
                    {"role": "system", "content": system_message},
                    {"role": "user", "content": user_prompt}
                ]
            )
            full_text = response['choices'][0]['message']['content'].strip()
            try:
                json_output = json.loads(full_text)
                sat_title = json_output.get("satirical_title")
                sat_content = json_output.get("satirical_content")
            except Exception as e:
                print(f"Attempt {attempt}: Error parsing JSON: {e}. Full response: {full_text}")
                sat_title, sat_content = None, None

            if sat_title and sat_content:
                return sat_title, sat_content
            else:
                print(f"Attempt {attempt}: Incomplete JSON output. Full response: {full_text}")
        except Exception as e:
            print(f"Attempt {attempt}: Error generating satirical content: {e}")
        time.sleep(2)  # Delay before next attempt

    return "ERROR_SATIRICAL_TITLE", "ERROR_SATIRICAL_CONTENT"

def main():
    with open(INPUT_CSV, mode='r', encoding='utf-8') as infile:
        reader = csv.DictReader(infile)
        rows = list(reader)

    new_rows = []
    for i, row in enumerate(rows, start=1):
        original_title = row.get('Title', 'No Title')
        original_content = row.get('Content', '').strip()

        if not original_content:
            print(f"Warning: Empty content for article {i} - {original_title}")
            row['SatiricalTitle'] = "ERROR_SATIRICAL_TITLE"
            row['SatiricalContent'] = "ERROR_SATIRICAL_CONTENT"
            row['Article ID'] = i
            new_rows.append(row)
            continue

        satirical_title, satirical_content = generate_satirical_title_and_content(
            original_title,
            original_content
        )

        if satirical_content == "ERROR_SATIRICAL_CONTENT":
            print(f"Warning: Satirical content missing for article {i} - {original_title}")

        row['Article ID'] = i
        row['Content'] = shorten_to_80_words(original_content)
        row['SatiricalTitle'] = satirical_title
        row['SatiricalContent'] = satirical_content

        print(f"Processed article {i} of {len(rows)}: {original_title}")
        new_rows.append(row)

    fieldnames = [
        'Article ID',  # Unique identifier
        'Category', 
        'Title',          
        'SatiricalTitle',
        'Image URL', 
        'Article URL', 
        'Content',        
        'SatiricalContent'
    ]

    with open(OUTPUT_CSV, mode='w', newline='', encoding='utf-8') as outfile:
        writer = csv.DictWriter(outfile, fieldnames=fieldnames)
        writer.writeheader()
        for row in new_rows:
            filtered_row = {col: row.get(col, '') for col in fieldnames}
            writer.writerow(filtered_row)

    print(f"Manipulated articles with unique IDs written to: {OUTPUT_CSV}")

if __name__ == '__main__':
    main()
