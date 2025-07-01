from flask import Flask, render_template, request, redirect, url_for, session
import random
from datetime import datetime
from flask_sqlalchemy import SQLAlchemy
from flask_migrate import Migrate
from sqlalchemy import func

app = Flask(__name__)
app.secret_key = 'random_secret_key'
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///mainst_database.db'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

db = SQLAlchemy(app)
migrate = Migrate(app, db)

# ---------------------------
# Database Models
# ---------------------------

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

class Participant(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    state = db.Column(db.String(100))
    age = db.Column(db.Integer)
    gender = db.Column(db.String(50))
    occupation = db.Column(db.String(100))
    education = db.Column(db.String(100))
    political_party = db.Column(db.String(100))
    news_frequency = db.Column(db.String(100))
    news_reason = db.Column(db.String(100))
    no_news_reason = db.Column(db.String(100))
    transparency = db.Column(db.String(20))
    prolific_id = db.Column(db.String(100))

class CategoryPreference(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    participant_id = db.Column(db.Integer, db.ForeignKey('participant.id'))
    category = db.Column(db.String(50))
    score = db.Column(db.Integer)
    participant = db.relationship('Participant', backref='category_preferences')

class SelectedArticle(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    participant_id = db.Column(db.Integer, db.ForeignKey('participant.id'))
    article_id = db.Column(db.Integer, db.ForeignKey('article.id'))
    participant = db.relationship('Participant', backref='selected_articles')
    article = db.relationship('Article')

class ArticleResponse(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    participant_id = db.Column(db.Integer, db.ForeignKey('participant.id'))
    article_id = db.Column(db.Integer, db.ForeignKey('article.id'))
    preferred_version = db.Column(db.String(50))  # "original" or "satirical"
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)
    participant = db.relationship('Participant', backref='article_responses')
    article = db.relationship('Article')

class PostQuestionnaire(db.Model):
    id            = db.Column(db.Integer, primary_key=True)
    participant_id= db.Column(db.Integer,
                              db.ForeignKey('participant.id'),
                              nullable=False)
    article_id    = db.Column(db.Integer,
                              db.ForeignKey('article.id'),
                              nullable=True)
    feature       = db.Column(db.String(50),   
                              nullable=False)
    score         = db.Column(db.Integer,      
                              nullable=False)

    
    participant   = db.relationship('Participant',
                                    backref='post_q_answers')
    article       = db.relationship('Article')

# ---------------------------
# Categories and CSV Data
# ---------------------------

categories = [
    'Sports', 'Politics', 'Climate', 'Finance',
    'Entertainment', 'Lifestyle', 'Technology',
    'Weather', 'Conflicts'
]

# ---------------------------
# Routes
# ---------------------------

@app.route('/')
def home():
    if 'transparency' not in session:
        session['transparency'] = "transparent" if random.random() < 0.5 else "non-transparent"
    session['prolific_id'] = request.args.get('PROLIFIC_PID', None)
    return render_template('home.html', prolific_id=session['prolific_id'])

@app.route('/category-info', methods=['GET', 'POST'])
def category_info():
    if 'participant_id' not in session:
        return redirect(url_for('pre_questionnaire'))
    if request.method == 'POST':
        return redirect(url_for('category_preference'))
    return render_template('category_select_info.html')

@app.route('/article-info', methods=['GET', 'POST'])
def article_info():
    if 'participant_id' not in session or 'selected_articles' not in session:
        return redirect(url_for('pre_questionnaire'))
    if request.method == 'POST':
        return redirect(url_for('articles', page=0))
    return render_template('article_select_info.html')

@app.route('/pre-questionnaire', methods=['GET', 'POST'])
def pre_questionnaire():
    us_states = [ "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                  "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
                  "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
                  "Wisconsin", "Wyoming" ]
    
    if request.method == 'POST':
        pre_data = request.form.to_dict()
        new_participant = Participant(
            state=pre_data.get('state'),
            age=int(pre_data.get('age')),
            gender=pre_data.get('gender'),
            occupation=pre_data.get('occupation'),
            education=pre_data.get('education'),
            political_party=pre_data.get('political_party'),
            news_frequency=pre_data.get('news_frequency'),
            news_reason=pre_data.get('news_reason'),
            no_news_reason=pre_data.get('no_news_reason'),
            transparency=session.get('transparency'),
            prolific_id=session.get('prolific_id')
        )
        db.session.add(new_participant)
        db.session.commit()
        session['participant_id'] = new_participant.id
        session['pre_answers'] = pre_data
        return redirect(url_for('category_info'))
    return render_template('pre_questionnaire.html', us_states=us_states)

@app.route('/category-preference', methods=['GET', 'POST'])
def category_preference():
    if 'participant_id' not in session:
        return redirect(url_for('pre_questionnaire'))
    if request.method == 'POST':
        preferences = {cat: int(request.form.get(cat)) for cat in categories if request.form.get(cat)}
        session['category_preferences'] = preferences
        participant_id = session.get('participant_id')
        if participant_id:
            for cat, score in preferences.items():
                cp = CategoryPreference(participant_id=participant_id, category=cat, score=score)
                db.session.add(cp)
            db.session.commit()
        sorted_prefs = sorted(preferences.items(), key=lambda x: x[1], reverse=True)
        random.shuffle(sorted_prefs)
        sorted_prefs = sorted(sorted_prefs, key=lambda x: x[1], reverse=True)
        top_categories = sorted_prefs[:2]
        bottom_categories = sorted_prefs[-3:]
        selected_categories = [cat for cat, _ in top_categories + bottom_categories]
        session['selected_categories'] = selected_categories
        session['post_questionnaire_responses'] = {}
        # Set the attention test index for the post-questionnaire phase.
        session['attention_test_post_index'] = random.randint(0, len(selected_categories)-1)
        selected_ids = []
        for category, _ in top_categories + bottom_categories:
            # Using a case-insensitive match:
            articles = Article.query.filter(func.lower(Article.category) == category.strip().lower()).order_by(Article.review_count.asc()).all()
            if articles:
                min_review = min(article.review_count for article in articles)
                candidates = [article for article in articles if article.review_count == min_review]
                selected_article = random.choice(candidates)
                selected_article.review_count += 1
                db.session.add(selected_article)
                selected_ids.append(selected_article.id)
                if participant_id:
                    sa = SelectedArticle(participant_id=participant_id, article_id=selected_article.id)
                    db.session.add(sa)
            else:
                print(f"DEBUG: No article found for category: {category}")
        db.session.commit()
        random.shuffle(selected_ids)
        session['selected_articles'] = selected_ids
        session['responses'] = []
        session['articles_completed'] = []
        session['satirical_titles'] = {}
        return redirect(url_for('article_info'))
    return render_template('category_preference.html', categories=categories)

@app.route('/articles/<int:page>', methods=['GET', 'POST'])
def articles(page):
    if 'participant_id' not in session or 'selected_articles' not in session:
        return redirect(url_for('pre_questionnaire'))
    if 'responses' not in session:
        session['responses'] = []
    selected_ids = session.get('selected_articles', [])
    if not selected_ids:
        return redirect(url_for('category_preference'))
    if page < 0 or page >= len(selected_ids):
        return redirect(url_for('evaluation', idx=0))
    article_id = selected_ids[page]
    article = Article.query.get(article_id)
    if not article:
        return f"Error: Article ID {article_id} not found.", 404
    versions = [
        {'id': 'original', 'title': article.title, 'content': article.content, 'image_url': article.image_url},
        {'id': 'satirical', 'title': article.satirical_title, 'content': article.satirical_content, 'image_url': article.image_url}
    ]
    random.shuffle(versions)
    if request.method == 'POST':
        user_preference = request.form.get('preference')
        if not user_preference:
            return render_template('article_page.html',
                                   page=page,
                                   versions=versions,
                                   error="Please select a preference.",
                                   category=article.category)
        session['responses'].append({'article_id': article_id, 'preference': user_preference})
        participant_id = session.get('participant_id')
        if participant_id:
            ar = ArticleResponse(participant_id=participant_id, article_id=article_id, preferred_version=user_preference)
            db.session.add(ar)
            db.session.commit()
        completed = session.get('articles_completed', [])
        if article_id not in completed:
            completed.append(article_id)
        session['articles_completed'] = completed
        sat_titles = session.get('satirical_titles', {})
        cat_key = article.category.strip().lower()
        if cat_key not in sat_titles:
            sat_titles[cat_key] = article.satirical_title
        session['satirical_titles'] = sat_titles
        if page + 1 < len(selected_ids):
            return redirect(url_for('articles', page=page + 1))
        return redirect(url_for('post_questionnaire_info'))
    return render_template('article_page.html',
                           page=page,
                           versions=versions,
                           total_pages=len(selected_ids),
                           category=article.category)


@app.route('/post-questionnaire-info', methods=['GET', 'POST'])
def post_questionnaire_info():

    if 'participant_id' not in session or 'selected_articles' not in session:
        return redirect(url_for('pre_questionnaire'))
    # skip-ahead guard 
    selected  = session.get('selected_articles', [])
    completed = session.get('articles_completed', [])
    if selected and len(completed) < len(selected):
        return redirect(url_for('articles', page=len(completed)))
    
    if request.method == 'POST':
        session['evaluation_responses'] = []
        return redirect(url_for('evaluation', idx=0))
    return render_template('post_questionnaire_info.html')


# Post Questionnaire Evaluation
@app.route('/post-questionnaire/evaluation/<int:idx>', methods=['GET', 'POST'])
def evaluation(idx):
    if 'participant_id' not in session or 'selected_articles' not in session:
        return redirect(url_for('pre_questionnaire'))

    selected_articles = session['selected_articles']
    completed         = session.get('articles_completed', [])

    if selected_articles and len(completed) < len(selected_articles):
        return redirect(url_for('articles', page=len(completed)))

    if 'attention_idx' not in session:
        session['attention_idx'] = random.randrange(len(selected_articles))
    attention_idx = session['attention_idx']

    if idx < 0 or idx >= len(selected_articles):
        return redirect(url_for('thank_you'))

    evaluation_responses = session.get('evaluation_responses', [])
    if idx != len(evaluation_responses):
        return redirect(url_for('evaluation', idx=len(evaluation_responses)))

    article_id = selected_articles[idx]
    article    = Article.query.get(article_id)
    if not article:
        return f"Error: Article ID {article_id} not found.", 404

    versions = [
        {'id': 'original',  'title': article.title,           'content': article.content},
        {'id': 'satirical', 'title': article.satirical_title, 'content': article.satirical_content}
    ]

    chosen_version = next((r['preference'] for r in session.get('responses', [])
                           if r['article_id'] == article_id), "N/A")

    if request.method == 'POST':
        form_data      = request.form.to_dict()
        attention_pass = None
        if idx == attention_idx:
            attention_pass = 1 if form_data.pop('attention_check', None) == '3' else 0

        evaluation_responses.append({'article_id': article_id,
                                     'evaluation': form_data,
                                     'attention':  attention_pass})
        session['evaluation_responses'] = evaluation_responses

        pid = session['participant_id']
        for feat, score in form_data.items():
            db.session.add(PostQuestionnaire(participant_id=pid,
                                             article_id=article_id,
                                             feature=feat,
                                             score=int(score)))
        if attention_pass is not None:
            db.session.add(PostQuestionnaire(participant_id=pid,
                                             article_id=article_id,
                                             feature='attention_check',
                                             score=attention_pass))
        db.session.commit()

        if idx + 1 < len(selected_articles):
            return redirect(url_for('evaluation', idx=idx + 1))
        session['completed_study'] = True
        return redirect(url_for('post_question'))

    return render_template('post_questionnaire.html',
                           idx=idx,
                           page=idx,
                           total_pages=len(selected_articles),
                           article=article,
                           versions=versions,
                           chosen_version=chosen_version,
                           category=article.category,
                           attention_idx=attention_idx)

@app.route('/post-question', methods=['GET', 'POST'])
def post_question():
    # must have done pre, article-selection, responses & evaluations
    if 'participant_id' not in session or 'selected_articles' not in session:
        return redirect(url_for('pre_questionnaire'))

    selected   = session['selected_articles']
    completed  = session.get('articles_completed', [])
    if selected and len(completed) < len(selected):
        return redirect(url_for('articles', page=len(completed)))

    evals = session.get('evaluation_responses', [])
    if selected and len(evals) < len(selected):
        return redirect(url_for('evaluation', idx=len(evals)))

    if request.method == 'POST':
        post_data = request.form.to_dict()
        pid = session['participant_id']
        # save each item as a PostQuestionnaire
        for feature, score in post_data.items():
            db.session.add(PostQuestionnaire(
                participant_id=pid,
                article_id=None,           
                feature=feature,
                score=int(score)
            ))
        db.session.commit()
        session['completed_study'] = True
        return redirect(url_for('thank_you'))

    return render_template('post_question.html')


@app.route('/thank-you')
def thank_you():
    if not session.get('completed_study'):

        if 'participant_id' not in session or 'selected_articles' not in session:
            return redirect(url_for('pre_questionnaire'))
         
        selected   = session.get('selected_articles', [])
        completed  = session.get('articles_completed', [])
        if selected and len(completed) < len(selected):
            for idx, art_id in enumerate(selected):
                if art_id not in completed:
                    return redirect(url_for('articles', page=idx))

        eval_responses = session.get('evaluation_responses', [])
        if selected and len(eval_responses) < len(selected):
            return redirect(url_for('evaluation', idx=len(eval_responses)))

       
        session['completed_study'] = True

    return render_template('thank_you.html')

if __name__ == '__main__':
    app.run(debug=False, host='0.0.0.0', port=5001)
