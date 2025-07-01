import pandas as pd
from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from flask_migrate import Migrate
from datetime import datetime

# Set up the Flask app and database 
app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///mainst_database.db'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

db = SQLAlchemy(app)
migrate = Migrate(app, db)

# Define your Article model (must match your main app)
class Article(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    category = db.Column(db.String(100))
    title = db.Column(db.String(200))
    satirical_title = db.Column(db.String(200))
    image_url = db.Column(db.String(300))
    article_url = db.Column(db.String(300))
    content = db.Column(db.Text)
    satirical_content = db.Column(db.Text)
    review_count = db.Column(db.Integer, default=0)

def populate_articles():
    # Adjust the filename to the path of your CSV file.
    df = pd.read_csv('manipulated_articles.csv')
    
    with app.app_context():
        for _, row in df.iterrows():
            # Create an Article instance. Adjust the column names if necessary.
            article = Article(
                id=int(row['Article ID']),
                category=row['Category'],
                title=row['Title'],
                satirical_title=row['SatiricalTitle'],
                image_url=row['Image URL'],
                article_url=row.get('Article URL', ''),  # In case it's not in CSV.
                content=row['Content'],
                satirical_content=row['SatiricalContent'],
                review_count=0  # Default value.
            )
            # Add the article to the session.
            db.session.merge(article)
        # Commit all changes.
        db.session.commit()
        print("Articles table populated successfully.")

if __name__ == '__main__':
    populate_articles()
