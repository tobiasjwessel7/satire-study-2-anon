"""

Revision ID: 947c1d30f383
Revises: ace582ecadba
Create Date: 2025-04-24 15:36:09.547353

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import sqlite

# revision identifiers, used by Alembic.
revision = '947c1d30f383'
down_revision = 'ace582ecadba'
branch_labels = None
depends_on = None


def upgrade():
    op.drop_table('post_questionnaire')        # remove old JSON schema

    op.create_table(
        'post_questionnaire',
        sa.Column('id', sa.Integer, primary_key=True),
        sa.Column('participant_id', sa.Integer, nullable=False),
        sa.Column('article_id',    sa.Integer, nullable=False),
        sa.Column('feature',       sa.String(50), nullable=False),
        sa.Column('score',         sa.Integer,   nullable=False),
        sa.ForeignKeyConstraint(
            ['participant_id'], ['participant.id'],
            name='fk_postq_participant'
        ),
        sa.ForeignKeyConstraint(
            ['article_id'], ['article.id'],
            name='fk_postq_article'
        )
    )


def downgrade():
    op.drop_table('post_questionnaire')
