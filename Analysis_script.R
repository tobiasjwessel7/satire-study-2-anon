# R-script for the data analysis (Study 2)

library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(purrr)
library(ggplot2)
library(scales)
library(stats)
library(lme4)
library(ggeffects)
library(psych)
library(lavaan)


# Connect to your SQLite database (Add path to the location of study 2 DB to try)
db_path <- ""
con     <- dbConnect(SQLite(), db_path)

# Read tables into tibbles
participants        <- tbl(con, "participant")        %>% collect()
category_prefs      <- tbl(con, "category_preference")%>% collect()
selected_articles   <- tbl(con, "selected_article")   %>% collect()
article_responses   <- tbl(con, "article_response")   %>% collect()
post_questionnaire  <- tbl(con, "post_questionnaire") %>% collect()

# Quick checks
glimpse(participants)
glimpse(category_prefs)
glimpse(selected_articles)
glimpse(article_responses)
glimpse(post_questionnaire)


# Compute attention‐check pass/fail (1 = passed, 0 = failed)
attention_df <- post_questionnaire %>%
  filter(feature == "attention_check") %>%
  group_by(participant_id) %>%
  summarize(att_pass = max(score, na.rm = TRUE))  # should be 1 or 0

table(attention_df$att_pass)

# Keep only those who passed
pids_att <- attention_df %>% 
  filter(att_pass == 1) %>% 
  pull(participant_id)

participants2       <- participants        %>% filter(id   %in% pids_att)
category_prefs2     <- category_prefs      %>% filter(participant_id %in% pids_att)
selected_articles2  <- selected_articles   %>% filter(participant_id %in% pids_att)
article_responses2  <- article_responses   %>% filter(participant_id %in% pids_att)
post_questionnaire2 <- post_questionnaire  %>% filter(participant_id %in% pids_att)

# Define “complete” in terms of N articles and M post‐scores
N <- 5        # number of articles per participant
M <- 8        # likert items per article


# Deduplicate post_questionnaire rows

post_questionnaire_clean <- post_questionnaire2 %>% 
  arrange(id) %>%                                 # ascending PK ⇒ earliest
  group_by(participant_id, article_id, feature) %>% 
  slice(1) %>%                                    # KEEP first row in each cell
  ungroup()

# Diagnostic message
dup_pq <- post_questionnaire2 %>% 
  count(participant_id, article_id, feature, name = "n_rows") %>% 
  filter(n_rows > 1)

if (nrow(dup_pq) > 0) {
  message("Removed ", sum(dup_pq$n_rows - 1),
          " duplicate post-questionnaire rows from ",
          n_distinct(dup_pq$participant_id), " participants.")
} else {
  message("No duplicate post-questionnaire rows detected.")
}

# — selected_articles de-dupe 
dup_sa <- selected_articles2 %>% 
  count(participant_id, article_id, name = "n_rows") %>% 
  filter(n_rows > 1)

if (nrow(dup_sa) > 0) {
  message("Removed ", sum(dup_sa$n_rows - 1),
          " duplicate selected‐article rows from ",
          n_distinct(dup_sa$participant_id), " participants.")
  selected_articles2 <- selected_articles2 %>% 
    arrange(participant_id, article_id) %>% 
    group_by(participant_id, article_id) %>% 
    slice(1) %>% 
    ungroup()
} else {
  message("No duplicate selected‐article rows detected.")
}

# article_responses de-dupe
dup_ar <- article_responses2 %>% 
  count(participant_id, article_id, name = "n_rows") %>% 
  filter(n_rows > 1)

if (nrow(dup_ar) > 0) {
  message("Removed ", sum(dup_ar$n_rows - 1),
          " duplicate article‐response rows from ",
          n_distinct(dup_ar$participant_id), " participants.")
  article_responses2 <- article_responses2 %>% 
    arrange(participant_id, article_id) %>% 
    group_by(participant_id, article_id) %>% 
    slice(1) %>% 
    ungroup()
} else {
  message("No duplicate article‐response rows detected.")
}

# — category_prefs de-dupe -----------------------------------------------
dup_cp <- category_prefs2 %>% 
  count(participant_id, category, name = "n_rows") %>% 
  filter(n_rows > 1)

if (nrow(dup_cp) > 0) {
  message("Removed ", sum(dup_cp$n_rows - 1),
          " duplicate category‐preference rows from ",
          n_distinct(dup_cp$participant_id), " participants.")
  category_prefs2 <- category_prefs2 %>% 
    arrange(participant_id, category) %>% 
    group_by(participant_id, category) %>% 
    slice(1) %>% 
    ungroup()
} else {
  message("No duplicate category‐preference rows detected.")
}


att_df <- post_questionnaire %>%
  filter(feature == "attention_check") %>%
  group_by(participant_id) %>%
  summarize(att_pass = max(score, na.rm = TRUE))

ai_df <- post_questionnaire %>%
  filter(feature == "AI_trust") %>%
  distinct(participant_id) %>%
  mutate(ai_present = TRUE)

valid_pids <- att_df %>%
  inner_join(ai_df, by="participant_id") %>%
  filter(att_pass == 1) %>%
  pull(participant_id)

# Count article_responses per pid
resp_counts <- article_responses2 %>%
  count(participant_id, name = "n_resp")  

# Count post‐questionnaire rows per pid
pq_counts   <- post_questionnaire2 %>%
  count(participant_id, name = "n_post")

# Identify pids with full data
complete_pids <- resp_counts %>%
  inner_join(pq_counts, by = "participant_id") %>%
  filter(n_resp == N,
         n_post >= N * M + 1) %>%   # +1 for the single attention_check
  pull(participant_id)

# how many true completers
length(complete_pids)

# Subset all tables down to completers
participants_clean       <- participants2        %>% filter(id             %in% complete_pids)
category_prefs_clean     <- category_prefs2      %>% filter(participant_id %in% complete_pids)
selected_articles_clean  <- selected_articles2   %>% filter(participant_id %in% complete_pids)
article_responses_clean  <- article_responses2   %>% filter(participant_id %in% complete_pids)
post_questionnaire_clean <- post_questionnaire_clean  %>% filter(participant_id %in% complete_pids)


# extra security to remove duplicates from the category_prefs

category_prefs_clean <- category_prefs_clean %>%
  distinct(participant_id, category, .keep_all = TRUE)

# exact post-questionnaire row count
expected_rows <- N * M + 2        

dup_post <- post_questionnaire_clean %>%
  count(participant_id, name = "n_post_rows") %>%
  filter(n_post_rows != expected_rows)          # anything ≠ 42 is suspicious

if (nrow(dup_post) > 0) {
  message("Participants with unexpected post-questionnaire row counts:")
  print(dup_post, n = nrow(dup_post))
  
  
} else {
  message("All participants have exactly ", expected_rows, " post-questionnaire rows.")
}


# Which per-participant, non-article features are actually present?
single_feat_counts <- post_questionnaire_clean %>% 
  filter(feature %in% c("attention_check", "AI_trust")) %>% 
  count(feature)

print(single_feat_counts)


# Check
n_distinct(participants_clean$id)       # should equal length(complete_pids)
n_distinct(article_responses_clean$participant_id)
n_distinct(post_questionnaire_clean$participant_id)

cat("Total completers:", length(complete_pids), "\n")
table(att_df$att_pass)                           # Attn pass/fail counts
sum(ai_df$ai_present)  


# IDs of everyone at the start
all_ids <- participants$id

# Fail Gate 1
fail_att <- setdiff(all_ids, pids_att)

# Fail Gate 2 or 3
fail_comp <- setdiff(pids_att, complete_pids)

list(
  failed_attention = fail_att,
  incomplete_data  = fail_comp
)



# Quick descriptive summaries ----

# Extract only the article‐level features
likert_features <- post_questionnaire_clean %>%
  filter(feature %in% c("factual_accuracy",
                        "humor_appreciation",
                        "entertainment",
                        "comprehension",
                        "less_heavy",
                        "future_satire",
                        "interest_increase",
                        "mood_impact")) 

# Compute overall mean ± SD per feature
summary_table <- likert_features %>%
  group_by(feature) %>%
  summarize(
    mean_score = mean(score, na.rm = TRUE),
    sd_score   = sd(score, na.rm = TRUE),
    n_obs      = n()
  ) %>%
  arrange(feature)

print(summary_table)

# AI‐trust overall
ai_summary <- post_questionnaire_clean %>%
  filter(feature == "AI_trust") %>%
  summarize(
    mean_AI_trust = mean(score, na.rm = TRUE),
    sd_AI_trust   = sd(score, na.rm = TRUE),
    n_obs         = n()
  )

print(ai_summary)

# Descriptives by transparency arm 

# Join transparency flag into post_questionnaire_clean
post_flagged <- post_questionnaire_clean %>%
  left_join(
    participants_clean %>% select(id, transparency),
    by = c("participant_id" = "id")
  )

# Article‐level features by arm
summary_by_transparency <- post_flagged %>%
  filter(feature %in% c("factual_accuracy",
                        "humor_appreciation",
                        "entertainment",
                        "comprehension",
                        "less_heavy",
                        "future_satire",
                        "interest_increase",
                        "mood_impact")) %>%
  group_by(transparency, feature) %>%
  summarize(
    mean_score = mean(score, na.rm = TRUE),
    sd_score   = sd(score, na.rm = TRUE),
    n_obs      = n(),
    .groups    = "drop"
  ) %>%
  arrange(transparency, feature)

print(summary_by_transparency)

# AI‐trust by arm
ai_by_transparency <- post_flagged %>%
  filter(feature == "AI_trust") %>%
  group_by(transparency) %>%
  summarize(
    mean_AI_trust = mean(score, na.rm = TRUE),
    sd_AI_trust   = sd(score, na.rm = TRUE),
    n_obs         = n(),
    .groups       = "drop"
  )

print(ai_by_transparency)


# Satirical‐choice rates 

# Flag each trial as satirical choice (1) vs. original (0)
responses_flagged <- article_responses_clean %>%
  mutate(choice_satirical = if_else(preferred_version == "satirical", 1L, 0L))

# Overall satirical‐choice rate
overall_sat <- responses_flagged %>%
  summarize(
    total_trials = n(),
    n_sat        = sum(choice_satirical),
    prop_sat     = mean(choice_satirical)
  )

print(overall_sat)

# Satirical‐choice rate by transparency arm
# join in the participant‐level transparency flag
sat_by_transparency <- responses_flagged %>%
  left_join(
    participants_clean %>% select(id, transparency),
    by = c("participant_id" = "id")
  ) %>%
  group_by(transparency) %>%
  summarize(
    total_trials = n(),
    n_sat        = sum(choice_satirical),
    prop_sat     = mean(choice_satirical),
    .groups      = "drop"
  )

print(sat_by_transparency)


# Category‐level impact of satire on key post questions 

# Read in the article metadata (to get category)
articles <- tbl(con, "article") %>%
  select(id, category) %>%
  collect()

# Select only the four satire‐impact features
sat_features <- c("less_heavy", "future_satire", "interest_increase", "mood_impact")

# Join post‐questionnaire responses to article category
post_cat <- post_questionnaire_clean %>%
  filter(feature %in% sat_features) %>%
  left_join(
    articles,
    by = c("article_id" = "id")
  )

# Compute mean score by category & feature
cat_summary <- post_cat %>%
  group_by(category, feature) %>%
  summarize(
    mean_score = mean(score, na.rm = TRUE),
    sd_score   = sd(score, na.rm = TRUE),
    n_obs      = n(),
    .groups    = "drop"
  )

print(cat_summary)

# Pivot wider to see categories in columns for easy comparison
cat_summary_wide <- cat_summary %>%
  pivot_wider(
    names_from  = feature,
    values_from = mean_score
  )

# Rank categories by each feature (descending)
ranked <- cat_summary_wide %>%
  arrange(desc(less_heavy))       %>% mutate(rank_less_heavy = row_number()) %>%
  arrange(desc(future_satire))    %>% mutate(rank_future_satire = row_number()) %>%
  arrange(desc(interest_increase))%>% mutate(rank_interest_increase = row_number()) %>%
  arrange(desc(mood_impact))      %>% mutate(rank_mood_impact = row_number())

print(ranked)

# Top‐3 categories for each feature:
top3_each <- cat_summary %>%
  group_by(feature) %>%
  slice_max(order_by = mean_score, n = 3) %>%
  arrange(feature, desc(mean_score))

print(top3_each)


# Look at unique categories
unique(category_prefs_clean$category)

# Compute median (and IQR) of preference scores by category

median_prefs <- category_prefs_clean %>%
  group_by(category) %>%
  summarize(
    n_participants = n_distinct(participant_id),
    median_score   = median(score, na.rm = TRUE),
    IQR_score      = IQR(score, na.rm = TRUE)
  ) %>%
  arrange(desc(median_score))

print(median_prefs)


# Make sure you have pref_type on each trial:
trial_data <- trial_data %>%
  mutate(pref_type = case_when(
    is_preferred_cat == 1 ~ "preferred",
    is_avoided_cat   == 1 ~ "avoided",
    TRUE                  ~ "neutral"
  ),
  pref_type = factor(pref_type, levels = c("preferred","neutral","avoided")))

# Compute satirical‐choice rate by pref_type
sat_by_pref_type <- trial_data %>%
  group_by(pref_type) %>%
  summarize(
    n_trials    = n(),
    n_sat       = sum(choice_satirical),
    rate_sat    = mean(choice_satirical),
    .groups     = "drop"
  )

print(sat_by_pref_type)

# trial_df already has these two columns:
#   score            – 1‒7 preference for the article’s category
#   choice_satirical – 0 (picked original) / 1 (picked satirical)

glm_simple <- glm(choice_satirical ~ score,
                  data   = trial_df,
                  family = binomial)

summary(glm_simple)


# ------------------------------------------------------------------
#            Mminimal trial_df that contains
#    participant_id | article_id | choice_satirical | score
# ------------------------------------------------------------------

# A) normalise category strings once
clean_articles  <- articles %>%               # id, category
  mutate(category = str_to_lower(str_trim(category)))

clean_prefs <- category_prefs_clean %>%       # participant_id, category, score
  mutate(category = str_to_lower(str_trim(category))) %>%
  select(participant_id, category, score)

trial_df <- article_responses_clean %>%       # participant_id, article_id, preferred_version
  mutate(choice_satirical = as.integer(preferred_version == "satirical")) %>%
  # attach the category of that article
  left_join(clean_articles,  by = c("article_id" = "id")) %>%
  # attach *that participant’s* 1-7 score for that category
  left_join(clean_prefs,     by = c("participant_id", "category")) %>%
  filter(!is.na(score))                          # drop the very mismatch

# quick sanity check
nrow(trial_df)          
head(trial_df)



glm_simple <- glm(choice_satirical ~ score,
                  data   = trial_df,
                  family = binomial)

summary(glm_simple)

# attach transparency flag to each trial
trial_df <- trial_df %>% 
  left_join(
    participants_clean %>% select(participant_id = id, transparency),
    by = "participant_id"
  )

# sanity check
table(trial_df$transparency, useNA = "ifany")

# ------------------------------------------------------------
#     fit a glm in **each** transparency arm
# ------------------------------------------------------------

models_by_arm <- trial_df %>%
  group_by(transparency) %>%
  group_map(~ glm(choice_satirical ~ score,
                  family = binomial, data = .x),
            .keep = TRUE)        # returns list of models

names(models_by_arm) <- levels(factor(trial_df$transparency))

#  tidy summaries
model_summaries <- map_dfr(models_by_arm,
                           ~ tidy(.x, conf.int = TRUE),
                           .id = "transparency")

print(model_summaries)


# ------------------------------------------------------------
#    combined model with interaction
# ------------------------------------------------------------
trial_df <- trial_df %>% 
  mutate(transparency = factor(transparency))  # ensure factor

glm_interact <- glm(choice_satirical ~ score * transparency,
                    family = binomial, data = trial_df)

summary(glm_interact)


exp(coef(glm_interact))           # exponentiate to get ORs
exp(confint(glm_interact))        # 95 % CIs for those ORs

# ------------------------------------------------------------
#  trial-level data frame for the *transparent* arm
# ------------------------------------------------------------

#  participant-level AI-trust (single 1–7 rating)
ai_trust <- post_questionnaire_clean %>% 
  filter(feature == "AI_trust") %>% 
  select(participant_id, ai_trust = score)

#    baseline trial_df we already had (score = category-preference 1–7)
#    make sure category strings were normalized exactly as before
trial_transp <- trial_df %>%                        
  filter(transparency == "transparent") %>%         # keep only that arm
  left_join(ai_trust, by = "participant_id") %>%    # add AI-trust
  filter(!is.na(ai_trust))                         

# optional centring for interpretability
trial_transp <- trial_transp %>%
  mutate(
    c_score    = score    - mean(score,    na.rm = TRUE),
    c_ai_trust = ai_trust - mean(ai_trust, na.rm = TRUE)
  )

# quick sanity check
nrow(trial_transp)          

# ------------------------------------------------------------
#     Logistic model: does AI-trust matter?
#     Model 1: AI-trust alone
#     Model 2: add liking score as a covariate
# ------------------------------------------------------------
m_ai_only <- glm(choice_satirical ~ c_ai_trust,
                 family = binomial, data = trial_transp)

m_full <- glm(choice_satirical ~ c_ai_trust + c_score,
              family = binomial, data = trial_transp)

summary(m_ai_only)
summary(m_full)

# ------------------------------------------------------------
#        Odds-ratios for neat reporting
# ------------------------------------------------------------
exp(cbind(OR = coef(m_full), confint(m_full)))   # 95 % CI


# Checking if some of the feature variables differ significantly fromt the neutral midpoint (4)

# Pick the feature(s) you want to test -------------------------------
target_feats <- c("less_heavy",     # …add "future_satire", "mood_impact", etc.
                  "future_satire",
                  "entertainment")

# Loop over each feature and run the Wilcoxon signed-rank test
for (ft in target_feats) {
  
  vec <- post_questionnaire_clean %>%
    filter(feature == ft) %>%
    pull(score)           # vector of 1-7 responses
  
  cat("\n---", ft, "---------------------------------------\n")
  cat("n  =", length(vec),
      "median =", median(vec),
      "IQR =", IQR(vec), "\n")
  
  w <- wilcox.test(vec, mu = 4, exact = FALSE, conf.int = TRUE)
  print(w)
}

# ------------------------------------------------------------
# Frequency table 
# ------------------------------------------------------------

freq_tbl <- participants_clean %>%                     # 151 completers
  count(news_frequency) %>%                            # raw counts
  mutate(percent = 100 * n / sum(n)) %>%               # % of total
  arrange(desc(n))

print(freq_tbl, n = Inf)

ggplot(freq_tbl,
       aes(x = reorder(news_frequency, n),
           y = n)) +
  geom_col(fill = "#4f81bd") +
  geom_text(aes(label = n), vjust = -0.3) +
  labs(title = "News-consumption frequency in the sample",
       x = "Self-reported frequency",
       y = "Number of participants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Looking into what might happen if we split the participants into news-freq groups

# add a binary flag hi/lo to the participant table
participants_clean <- participants_clean %>% 
  mutate(
    news_group = case_when(
      news_frequency %in% c("several_times_day", "once_day") ~ "high",
      TRUE                                                   ~ "low"
    ),
    news_group = factor(news_group, levels = c("low","high"))
  )

table(participants_clean$news_group)
#   low high 
#    43  108


# Testing for the first four questionnaire items within the groups (Manipulation check)

## four “choice” items 

choice_items_v1 <- c("factual_accuracy",
                     "humor_appreciation",
                     "entertainment",
                     "comprehension")

# One row = participant × article × item 
choice_long_tbl <- post_questionnaire_clean %>% 
  filter(feature %in% choice_items_v1) %>% 
  select(participant_id, article_id, feature, score) %>% 
  # add the actual choice on that trial (satire = 1, original = 0)
  left_join(
    article_responses_clean %>% 
      transmute(participant_id,
                article_id,
                chosen_satire = as.integer(preferred_version == "satirical")),
    by = c("participant_id", "article_id")
  ) %>% 
  # attach Low / High news-frequency flag
  left_join(
    participants_clean %>% 
      select(participant_id = id, news_group),
    by = "participant_id"
  )                                   # → 4 × 715 ≈ 2 860 rows

# Whole-sample means by item × chosen version 
choice_summary_full <- choice_long_tbl %>% 
  mutate(choice_lab = factor(chosen_satire,
                             levels = c(0, 1),
                             labels  = c("Original chosen",
                                         "Satire chosen"))) %>% 
  group_by(feature, choice_lab) %>% 
  summarise(n    = n(),
            mean = mean(score, na.rm = TRUE),
            se   = sd(score,  na.rm = TRUE) / sqrt(n),
            .groups = "drop")

print(choice_summary_full)

# Means by item × chosen version × news-group 
choice_summary_ng <- choice_long_tbl %>% 
  mutate(choice_lab = factor(chosen_satire,
                             levels = c(0, 1),
                             labels  = c("Original chosen",
                                         "Satire chosen"))) %>% 
  group_by(feature, news_group, choice_lab) %>% 
  summarise(n    = n(),
            mean = mean(score, na.rm = TRUE),
            se   = sd(score,  na.rm = TRUE) / sqrt(n),
            .groups = "drop")

print(choice_summary_ng, n = Inf)


# Factual accuracy
wilcox.test(score ~ chosen_satire,
            data = filter(choice_long_tbl, feature == "factual_accuracy"),
            exact = FALSE)

# Humour appreciation
wilcox.test(score ~ chosen_satire,
            data = filter(choice_long_tbl, feature == "humor_appreciation"),
            exact = FALSE)



# ----------------------------------------------------------------

responses_flagged %>% 
  left_join(participants_clean %>% select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  group_by(news_group) %>% 
  summarise(prop_sat = mean(choice_satirical))

post_questionnaire_clean %>% 
  filter(feature == "less_heavy") %>% 
  left_join(participants_clean %>% select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  group_by(news_group) %>% 
  summarise(median = median(score), IQR = IQR(score))


# Looking into the significance of the above measures

# χ² for satirical-choice rate difference
tab_sat <- table(responses_flagged %>%
                   left_join(participants_clean %>% 
                               select(id, news_group),
                             by = c("participant_id" = "id")) %>%
                   select(news_group, choice_satirical))

chisq.test(tab_sat)

# Wilcoxon rank-sum to compare ‘less_heavy’ scores between groups
less_df <- post_questionnaire_clean %>%
  filter(feature == "less_heavy") %>%
  left_join(participants_clean %>% select(id, news_group),
            by = c("participant_id" = "id"))

print(less_df)

wilcox.test(score ~ news_group, data = less_df)


post_questionnaire_clean %>% 
  filter(feature == "future_satire") %>% 
  left_join(participants_clean %>% select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  group_by(news_group) %>% 
  summarise(median = median(score), IQR = IQR(score))

future_df <- post_questionnaire_clean %>% 
  filter(feature == "future_satire") %>% 
  left_join(participants_clean %>% select(id, news_group),
            by = c("participant_id" = "id"))

wilcox.test(score ~ news_group, data = future_df)

# ──────────────────────────────────────────────────────────────
#   Analysis data frame
# ──────────────────────────────────────────────────────────────
mood_df <- post_questionnaire_clean %>% 
  filter(feature == "mood_impact") %>%             
  left_join(participants_clean %>%                 # add low / high flag
              select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  filter(!is.na(news_group))                       

# ──────────────────────────────────────────────────────────────
#   Descriptives by group
# ──────────────────────────────────────────────────────────────
mood_summary <- mood_df %>% 
  group_by(news_group) %>% 
  summarise(
    n       = n(),
    mean    = mean(score,  na.rm = TRUE),
    median  = median(score, na.rm = TRUE),
    IQR     = IQR(score,    na.rm = TRUE),
    se      = sd(score, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

print(mood_summary)

# ──────────────────────────────────────────────────────────────
# Significance test 
# ──────────────────────────────────────────────────────────────
wilcox_res <- wilcox.test(score ~ news_group,
                          data = mood_df,
                          exact = FALSE,
                          conf.int = TRUE)

print(wilcox_res)

# ──────────────────────────────────────────────────────────────
# line-plot with 95 % CI error bars
# ──────────────────────────────────────────────────────────────
mood_plot <- ggplot(mood_summary,
                    aes(x = news_group, y = mean,
                        group = 1, colour = news_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean - 1.96*se,
                    ymax = mean + 1.96*se),
                width = .1, linewidth = .8) +
  scale_colour_manual(values = c(low = "#e41a1c", high = "#377eb8"),
                      guide = "none") +
  labs(title = "\"Satire had a positive impact on my mood\"",
       x = NULL, y = "Mean Likert score (1–7)") +
  theme_minimal(base_size = 13)

print(mood_plot)


ggsave("plot_mood_impact.png",
       mood_plot, width = 6, height = 4, dpi = 300)

# ──────────────────────────────────────────────────────────────
# line-plot for a single summary data frame
# ──────────────────────────────────────────────────────────────
line_plot <- function(df, ylab, title) {
  df <- df %>%
    mutate(x = as.numeric(factor(news_group, levels = c("low","high"))))
  
  ggplot(df, aes(x = x, y = mean)) +
    geom_line(colour = "grey40", linewidth = 1) +
    geom_point(aes(fill = news_group),          # colour by group
               size = 4, shape = 21, colour = "black") +
    geom_errorbar(aes(ymin = mean - 1.96*se,
                      ymax = mean + 1.96*se),
                  width = .05, linewidth = .8) +
    scale_x_continuous(breaks = c(1,2),
                       labels = c("Low (< daily)", "High (≥ daily)")) +
    scale_fill_manual(values = c("#e41a1c", "#377eb8"), guide = "none") +
    labs(title = title, x = NULL, y = ylab) +
    theme_minimal(base_size = 13)
}

# ──────────────────────────────────────────────────────────────
# Satirical-choice line plot
# ──────────────────────────────────────────────────────────────
sat_df <- responses_flagged %>% 
  left_join(participants_clean %>% select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  group_by(news_group) %>% 
  summarise(
    mean = mean(choice_satirical),
    n    = n(),
    se   = sqrt(mean * (1 - mean) / n),
    .groups = "drop")

plot_sat <- line_plot(
  sat_df,
  title = NULL,
  ylab  = "% of satirical version chosen"
) +
  theme(
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), 
                       units = "cm")           
  )
print(plot_sat)

# ──────────────────────────────────────────────────────────────
# “Less heavy” line plot
# ──────────────────────────────────────────────────────────────

less_df <- post_questionnaire_clean %>% 
  filter(feature == "less_heavy") %>% 
  left_join(participants_clean %>% select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  group_by(news_group) %>% 
  summarise(
    mean = mean(score, na.rm = TRUE),
    n    = n(),
    se   = sd(score, na.rm = TRUE)/sqrt(n),
    sd   = sd(score,  na.rm = TRUE),
    .groups = "drop")

print(less_df)

plot_less <- line_plot(
  less_df,
  ylab  = "Mean Likert score (1–7)",
  title = "\"Satire made the topic feel less heavy\""
)
print(plot_less)



 # Test for Mood impact

# Build the analysis data frame

mood_df <- post_questionnaire_clean %>% 
  filter(feature == "mood_impact") %>%             # keep only that Likert item
  left_join(participants_clean %>%                 # add low / high flag
              select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  filter(!is.na(news_group))                       # (should be none)

# ──────────────────────────────────────────────────────────────
#  Descriptives by group
# ──────────────────────────────────────────────────────────────
mood_summary <- mood_df %>% 
  group_by(news_group) %>% 
  summarise(
    n       = n(),
    mean    = mean(score,  na.rm = TRUE),
    median  = median(score, na.rm = TRUE),
    IQR     = IQR(score,    na.rm = TRUE),
    se      = sd(score, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

print(mood_summary)

# ──────────────────────────────────────────────────────────────
#  Significance test (non-parametric)
# ──────────────────────────────────────────────────────────────
wilcox_res <- wilcox.test(score ~ news_group,
                          data = mood_df,
                          exact = FALSE,
                          conf.int = TRUE)

print(wilcox_res)

# ──────────────────────────────────────────────────────────────
#  line-plot with 95 % CI error bars
# ──────────────────────────────────────────────────────────────

mood_plot <- ggplot(mood_summary,
                    aes(x = news_group, y = mean,
                        group = 1, colour = news_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean - 1.96*se,
                    ymax = mean + 1.96*se),
                width = .1, linewidth = .8) +
  scale_colour_manual(values = c(low = "#e41a1c", high = "#377eb8"),
                      guide = "none") +
  labs(title = "\"Satire had a positive impact on my mood\"",
       x = NULL, y = "Mean Likert score (1–7)") +
  theme_minimal(base_size = 13)

print(mood_plot)


ggsave("plot_mood_impact.png",
       mood_plot, width = 6, height = 4, dpi = 300)

# ──────────────────────────────────────────────────────────────
# Analysis data frame (Test for comprehension)
# ──────────────────────────────────────────────────────────────

comp_df <- post_questionnaire_clean %>% 
  filter(feature == "comprehension") %>%          # target item
  left_join(participants_clean %>% 
              select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  filter(!is.na(news_group))

# ──────────────────────────────────────────────────────────────
#  Descriptive statistics by group
# ──────────────────────────────────────────────────────────────
comp_summary <- comp_df %>% 
  group_by(news_group) %>% 
  summarise(
    n      = n(),
    mean   = mean(score,  na.rm = TRUE),
    median = median(score, na.rm = TRUE),
    IQR    = IQR(score,    na.rm = TRUE),
    se     = sd(score, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

print(comp_summary)

# ──────────────────────────────────────────────────────────────
# Significance test (Wilcoxon rank-sum)
# ──────────────────────────────────────────────────────────────
wilcox_comp <- wilcox.test(score ~ news_group,
                           data = comp_df,
                           exact = FALSE,
                           conf.int = TRUE)

print(wilcox_comp)

# ──────────────────────────────────────────────────────────────
#     Line plot with 95 % confidence error bars
# ──────────────────────────────────────────────────────────────
plot_comp <- line_plot(
  comp_summary,
  ylab  = "Mean Likert score (1–7)",
  title = "\"The summary was easier to understand\""
)
print(plot_comp)


ggsave("plot_comprehension.png",
       plot_comp, width = 6, height = 4, dpi = 300)


# ──────────────────────────────────────────────────────────────
# Analysis data frame: interest_increase item
# ──────────────────────────────────────────────────────────────
interest_df <- post_questionnaire_clean %>% 
  filter(feature == "interest_increase") %>%      
  left_join(participants_clean %>% 
              select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  filter(!is.na(news_group))

# ──────────────────────────────────────────────────────────────
# Descriptive statistics by news-group
# ──────────────────────────────────────────────────────────────
interest_summary <- interest_df %>% 
  group_by(news_group) %>% 
  summarise(
    n      = n(),
    mean   = mean(score,  na.rm = TRUE),
    median = median(score, na.rm = TRUE),
    IQR    = IQR(score,    na.rm = TRUE),
    se     = sd(score, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

print(interest_summary)

# ──────────────────────────────────────────────────────────────
#  Wilcoxon rank-sum test (low vs high consumers)
# ──────────────────────────────────────────────────────────────
wilcox_interest <- wilcox.test(score ~ news_group,
                               data = interest_df,
                               exact = FALSE,
                               conf.int = TRUE)

print(wilcox_interest)

# ──────────────────────────────────────────────────────────────
#  Line plot with 95 % error bars 
# ──────────────────────────────────────────────────────────────
plot_interest <- line_plot(
  interest_summary,
  ylab  = "Mean Likert score (1–7)",
  title = "\"Satire increased my interest in the topic\""
)
print(plot_interest)

ggsave("plot_interest_increase.png",
       plot_interest, width = 6, height = 4, dpi = 300)


# ──────────────────────────────────────────────────────────────
# “Future satire” line plot
# ──────────────────────────────────────────────────────────────
future_df <- post_questionnaire_clean %>% 
  filter(feature == "future_satire") %>% 
  left_join(participants_clean %>% select(id, news_group),
            by = c("participant_id" = "id")) %>% 
  group_by(news_group) %>% 
  summarise(
    mean = mean(score, na.rm = TRUE),
    n    = n(),
    se   = sd(score, na.rm = TRUE)/sqrt(n),
    .groups = "drop")

plot_future <- line_plot(
  future_df,
  ylab  = "Mean Likert score (1–7)",
  title = "\"I’d like to read this topic in satire again\""
)
print(plot_future)
  


ggsave("plot_satirical_choice.png",
       plot_sat,   width = 6, height = 4, dpi = 300)

ggsave("plot_less_heavy.png",
       plot_less,  width = 6, height = 4, dpi = 300)

ggsave("plot_future_satire.png",
       plot_future, width = 6, height = 4, dpi = 300)



# failsafe: See if any participants have ≠5 selected articles¨
selected_articles_clean %>% 
  count(participant_id, name="n_sel") %>% 
  filter(n_sel != 5) %>% 
  print()
selected_articles_clean <- selected_articles_clean %>% 
  distinct(participant_id, article_id, .keep_all = TRUE)

article_responses_clean <- article_responses_clean %>% 
  distinct(participant_id, article_id, .keep_all = TRUE)

category_prefs_clean <- category_prefs_clean %>% 
  distinct(participant_id, category, .keep_all = TRUE) 


# Regression analysis with robust “sampled‐five” flagging

# lower-case and trim spaces 
clean_cat <- function(x) str_trim(str_to_lower(x))

# Clean article categories                                              
articles_clean <- articles %>%
  mutate(category = clean_cat(category))

# Rebuild flags based only on each participant’s sampled five 

# Get each sampled (pid,category) + their original 1–7 score
sel_cat_scores <- selected_articles_clean %>%
  left_join(articles_clean %>% select(id, category),
            by = c("article_id" = "id")) %>%
  left_join(category_prefs_clean %>% mutate(category = clean_cat(category)),
            by = c("participant_id", "category")) %>%
  select(participant_id, category, score)

# Within each pid, rank those five and flag top‐2 vs bottom‐3
sel_flags <- sel_cat_scores %>%
  group_by(participant_id) %>%
  arrange(desc(score), .by_group = TRUE) %>%
  mutate(
    is_pref  = as.integer(row_number() <= 2),
    is_avoid = as.integer(row_number() >  2)   # the remaining 3
  ) %>%
  ungroup() %>%
  select(participant_id, category, is_pref, is_avoid)

# Rebuild responses_flagged (satire choice = 1)
responses_flagged <- article_responses_clean %>% 
  mutate(choice_satirical = as.integer(preferred_version == "satirical"))

# Assemble trial‐level data 
trial_df <- responses_flagged %>%
  left_join(articles_clean %>% select(id, category),
            by = c("article_id" = "id")) %>%
  left_join(sel_flags,
            by = c("participant_id", "category")) %>%
  filter(!is.na(is_pref), !is.na(is_avoid))

# quick sanity checks
stopifnot(nrow(trial_df)==nrow(selected_articles_clean))
stopifnot(nrow(trial_df %>% filter(is_pref==0 & is_avoid==0))==0)

# Simple logistic models

# H1: preferred vs choice
glm_pref  <- glm(choice_satirical ~ is_pref,
                 family = binomial, data = trial_df)

# H2: avoided vs choice
glm_avoid <- glm(choice_satirical ~ is_avoid,
                 family = binomial, data = trial_df)

print(summary(glm_pref))
print(summary(glm_avoid))

# regressions by news_group 

trial_df <- trial_df %>% 
  left_join(participants_clean %>% select(id, news_group),
            by = c("participant_id"="id"))

# separate GLMs
models_by_group <- trial_df %>% 
  group_by(news_group) %>% 
  group_map(~ glm(choice_satirical ~ is_pref,
                  family=binomial, data=.x), .keep=TRUE)
names(models_by_group) <- levels(trial_df$news_group)
map_dfr(models_by_group, ~ tidy(.x, conf.int=TRUE), .id="news_group")

# interactions
glm_pref_int  <- glm(choice_satirical ~ is_pref  * news_group,
                     family=binomial, data=trial_df)
glm_avoid_int <- glm(choice_satirical ~ is_avoid * news_group,
                     family=binomial, data=trial_df)
print(summary(glm_pref_int))
print(summary(glm_avoid_int))

# mixed‐effects with random intercepts for category & participant
glmer_pref_cat <- glmer(choice_satirical ~ is_pref * news_group +
                          (1|category)+(1|participant_id),
                        family=binomial, data=trial_df,
                        control=glmerControl(optimizer="bobyqa"))
print(summary(glmer_pref_cat))

# collapsing into one model (pref_type × news_group) 

trial_df <- trial_df %>% 
  mutate(
    pref_type = factor(
      case_when(is_pref==1 ~ "preferred",
                is_avoid==1 ~ "avoided"),
      levels=c("preferred","avoided"))
  )

glm_pref_all <- glm(choice_satirical ~ pref_type * news_group,
                    family=binomial, data=trial_df)
glm_pref_main <- glm(choice_satirical ~ pref_type + news_group,
                     family=binomial, data=trial_df)
print(summary(glm_pref_all))
print(anova(glm_pref_main, glm_pref_all, test="Chisq"))

glmer_pref_all <- glmer(choice_satirical ~ pref_type * news_group +
                          (1|participant_id)+(1|article_id),
                        family=binomial, data=trial_df,
                        control=glmerControl(optimizer="bobyqa"))
print(summary(glmer_pref_all))

# effect‐plot
eff_df <- ggeffect(glm_pref_all, terms=c("pref_type","news_group"))
ggplot(eff_df, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, colour=group)) +
  geom_pointrange(position=position_dodge(width=0.3)) +
  labs(x=NULL, y="Predicted P(choose satire)", colour="News group") +
  theme_minimal(base_size=13)

# Effect‐coded score (–1/+1) & extremes 

trial_df <- trial_df %>% 
  mutate(prefScore = if_else(is_pref==1, 1L, -1L))

glm_pref_only <- glm(choice_satirical ~ prefScore,
                     family=binomial, data=trial_df)
print(summary(glm_pref_only))
print(table(trial_df$prefScore))

# Extremes only
df_ext <- filter(trial_df, prefScore!=0)
glm_ext <- glm(choice_satirical ~ prefScore,
               family=binomial, data=df_ext)
print(summary(glm_ext))
print(exp(cbind(OR=coef(glm_ext), confint(glm_ext))))

glmer_ext <- glmer(choice_satirical ~ prefScore +
                     (1|participant_id)+(1|category),
                   family=binomial, data=df_ext,
                   control=glmerControl(optimizer="bobyqa"))
print(summary(glmer_ext))

# Mixed for both extremes & news_group
glmer_prefScore <- glmer(choice_satirical ~ prefScore * news_group +
                           (1|participant_id)+(1|category),
                         family=binomial, data=trial_df,
                         control=glmerControl(optimizer="bobyqa"))
print(summary(glmer_prefScore))



# Recode –1/0/+1 into a factor with three levels

trial_df <- trial_df %>%
  mutate(
    prefGroup = factor(
      prefScore,
      levels = c(-1, 0, 1),
      labels = c("avoided", "neutral", "preferred")
    )
  )

# Fit a one‐way ANOVA
anova_model <- aov(choice_satirical ~ prefGroup, data = trial_df)
summary(anova_model)

# Tukey’s HSD
tukey_results <- TukeyHSD(anova_model, "prefGroup")
print(tukey_results)


# test for a news_group × prefGroup interaction:
anova2 <- aov(choice_satirical ~ prefGroup * news_group, data = trial_df)
summary(anova2)
TukeyHSD(anova2, "prefGroup")  # or specify interaction terms


# Doubecheck T-test/Anova

trial_df <- trial_df %>%
  mutate(prefType = factor(prefScore,
                           levels = c(-1, +1),
                           labels = c("avoided","preferred")))

# Standard two‐sample t-test and two‐level ANOVA to confirm:
t.test(choice_satirical ~ prefType, data = trial_df)
# or
summary(aov(choice_satirical ~ prefType, data = trial_df))
anova_model <- aov(choice_satirical ~ prefType, data = trial_df)

tukey_out <- TukeyHSD(anova_model, "prefType")
print(tukey_out)

#──────────────────────────────────────────────────────────────
# AI-trust by transparency arm (Wilcoxon rank–sum)
#──────────────────────────────────────────────────────────────


ai_transp <- post_questionnaire_clean %>%
  filter(feature == "AI_trust") %>%
  left_join(participants_clean %>%               # add arm label
              select(id, transparency),
            by = c("participant_id" = "id"))

# Descriptives
ai_summary <- ai_transp %>%
  group_by(transparency) %>%
  summarise(n      = n(),
            mean   = mean(score),
            median = median(score),
            IQR    = IQR(score),
            .groups = "drop")
print(ai_summary)

# Wilcoxon test (non-parametric, two-sided)
w_ai <- wilcox.test(score ~ transparency,
                    data = ai_transp,
                    exact = FALSE,
                    conf.int = TRUE)
print(w_ai)

# ─────────────────────────────────────────────────────────────
#  participant-level table with all flags
# ─────────────────────────────────────────────────────────────

part_flags <- participants_clean %>%            
  mutate(
    news_group = case_when(
      news_frequency %in% c("several_times_day", "once_day") ~ "high",
      TRUE                                                   ~ "low"
    ),
    news_group   = factor(news_group,   levels = c("low", "high")),
    transparency = factor(transparency, levels = c("non-transparent",
                                                   "transparent"))
  ) %>% 
  distinct(id, news_group, transparency)        
# ─────────────────────────────────────────────────────────────
#  Collapse AI-trust to one value PER participant
# ─────────────────────────────────────────────────────────────

ai_tbl <- post_questionnaire_clean %>% 
  filter(feature == "AI_trust") %>% 
  group_by(participant_id) %>%                 
  summarise(ai = mean(score, na.rm = TRUE),    
            .groups = "drop")

# ─────────────────────────────────────────────────────────────
#  Build the trial-level data frame
# ─────────────────────────────────────────────────────────────


clean_cat <- function(x) str_trim(stringr::str_to_lower(x))

# ── 1.  one row per (participant, category) with the 1–7 score
pref_tbl <- category_prefs_clean %>% 
  mutate(category = clean_cat(category)) %>% 
  select(participant_id, category, score)

# ── 2.  build trial_df *with* that score
trial_df <- article_responses_clean %>%                      
  mutate(choice_satirical = as.integer(preferred_version == "satirical")) %>% 
  left_join(articles_clean %>% select(id, category),         # add category
            by = c("article_id" = "id")) %>% 
  left_join(pref_tbl, by = c("participant_id", "category"))  # add score

# (double-check)
stopifnot(!any(is.na(trial_df$score)))


trial_ai <- trial_df %>%                       
  left_join(ai_tbl,      by = "participant_id") %>% 
  left_join(part_flags %>% 
              rename(participant_id = id),     
            by = "participant_id") %>% 
  mutate(                                      # centre for readability
    c_ai   = ai    - mean(ai,    na.rm = TRUE),
    c_scr  = score - mean(score, na.rm = TRUE)
  ) %>% 
  filter(!is.na(c_ai))                         # should drop 0 rows

# ─────────────────────────────────────────────────────────────
#  Analyses
# ─────────────────────────────────────────────────────────────

# Do transparent vs non-transparent *differ in AI-trust?*
wilcox_ai <- wilcox.test(ai ~ transparency, data = trial_ai %>% 
                           distinct(participant_id, ai, transparency),
                         exact = FALSE, conf.int = TRUE)
print(wilcox_ai)

#  Does AI-trust predict picking satire, and does that depend on
#      transparency *and* news-frequency?
glm_3way <- glm(choice_satirical ~ c_ai * transparency * news_group,
                family = binomial, data = trial_ai)
summary(glm_3way) |> print()

# Slopes by news group 
map_dfr(split(trial_ai, trial_ai$news_group),  # two data frames: low / high
        ~ tidy(glm(choice_satirical ~ c_ai * transparency,
                   family = binomial, data = .x),
               conf.int = TRUE),
        .id = "news_group") %>% 
  filter(str_detect(term, "c_ai")) %>%         # only AI-slopes
  print()


# Seeing if low-ai trust makes the users less prone to select the satirical version of the article.

# ─────────────────────────────────────────────────────────────
# LOW- vs HIGH- AI-trust and satirical choice
# ─────────────────────────────────────────────────────────────

# participant-level AI-trust (already built as ai_tbl)
#     ai_tbl  :  participant_id | ai   (1–7)

median_ai <- median(ai_tbl$ai, na.rm = TRUE)

ai_tbl <- ai_tbl %>%
  mutate(low_ai = as.integer(ai <= median_ai))   # 1 = low-trust

# Merge low_ai flag into the trial data
trial_ai2 <- trial_df %>%                              # 755 article trials
  left_join(ai_tbl, by = "participant_id") %>% 
  filter(!is.na(ai))                                   

#  Descriptive satirical-choice rate by trust group
sat_by_ai <- trial_ai2 %>% 
  group_by(low_ai) %>% 
  summarise(
    n_trials = n(),
    n_sat    = sum(choice_satirical),
    prop_sat = mean(choice_satirical),
    .groups  = "drop"
  )

print(sat_by_ai)
# low_ai = 1 → low trust, 0 → high trust

## χ² test (2 × 2)
chisq_low <- with(trial_ai2,
                  chisq.test(table(low_ai, choice_satirical)))
print(chisq_low)

## Logistic regression with *continuous* AI-trust
glm_ai <- glm(choice_satirical ~ scale(ai),   
              family = binomial, data = trial_ai2)
summary(glm_ai)

# ──────────────────────────────────────────────────────────────
#  Visualise AI-trust -> satire-choice relationship
#  logistic regression curve with 95 % CI
# ──────────────────────────────────────────────────────────────

ggplot(trial_ai2,                       
       aes(x = ai, y = choice_satirical)) +
  geom_jitter(width  = 0.05, height = 0.02,
              alpha  = 0.20, colour = "grey40") +
  stat_smooth(method        = "glm",
              method.args   = list(family = binomial),
              se            = TRUE,
              colour        = "#377eb8",
              fill          = "#377eb8",
              linewidth     = 1,
              alpha         = 0.15) +
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1),
                     labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 1:7) +
  labs(title = "Higher trust in AI predicts a greater\nprobability of choosing the satirical version",
       x     = "AI-trust rating  (1 = very low … 7 = very high)",
       y     = "Probability of choosing satire") +
  theme_minimal(base_size = 13)


ggsave("plot_ai_trust_vs_satire.png",
       width = 6, height = 4, dpi = 300)



# Running factor analysis on all satirical question items

# Install psych the first time 
# (omit or comment out on machines where it is already present)
# if (!requireNamespace("psych", quietly = TRUE)) {
   #install.packages("psych")        

# ──────────────────────────────────────────────────────────────
#  Trial-level wide table for the four satire items
# ──────────────────────────────────────────────────────────────
sat_items <- c("less_heavy",
               "future_satire",
               "interest_increase",
               "mood_impact")

sat_wide <- post_questionnaire_clean %>% 
  filter(feature %in% sat_items) %>% 
  select(participant_id, article_id, feature, score) %>%          
  pivot_wider(names_from = feature, values_from = score) %>% 
  mutate(                                                          # composite per trial
    satire_factor = rowMeans(across(all_of(sat_items)), na.rm = TRUE)
  ) %>% 
  left_join(participants_clean %>% 
              select(participant_id = id, news_group),
            by = "participant_id") %>% 
  drop_na(all_of(c(sat_items, "satire_factor")))                   # keep complete rows

# ──────────────────────────────────────────────────────────────
#  Exploratory FA + Cronbach-α
# ──────────────────────────────────────────────────────────────
sat_mat <- as.matrix(sat_wide[sat_items])

fa.parallel(sat_mat, fm = "pa", fa = "fa")   # → one factor is enough

efa1 <- fa(sat_mat, nfactors = 1, fm = "pa", rotate = "none")
print(efa1$loadings, cutoff = .20)

alpha_res <- psych::alpha(sat_mat)
cat("\nCronbach α =", round(alpha_res$total$raw_alpha, 2), "\n")

# ──────────────────────────────────────────────────────────────
#  Group comparison of the composite
# ──────────────────────────────────────────────────────────────
summary_sat <- sat_wide %>% 
  group_by(news_group) %>% 
  summarise(n      = n(),
            mean   = mean(satire_factor),
            sd     = sd(satire_factor),
            se     = sd / sqrt(n),
            median = median(satire_factor),
            IQR    = IQR(satire_factor),
            .groups = "drop")
print(summary_sat)

wilcox_sat <- wilcox.test(satire_factor ~ news_group,
                          data = sat_wide,
                          exact = FALSE, conf.int = TRUE)
print(wilcox_sat)

#  Quick line-plot
ggplot(summary_sat,
       aes(x = news_group, y = mean,
           group = 1, colour = news_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean - 1.96 * se,
                    ymax = mean + 1.96 * se),
                width = .1, linewidth = .8) +
  scale_colour_manual(values = c(low = "#e41a1c", high = "#377eb8"),
                      guide = "none") +
  labs(title = "Composite Satire-based Engagement factor\n(low vs high news consumers)",
       x = NULL, y = "Mean of 4-item factor (1–7)") +
  theme_minimal(base_size = 13)

ggsave("plot_satire_factor.png", width = 6, height = 4, dpi = 300)


# -----------------------------------------------
# Satire-impact factor plot 
# -----------------------------------------------
factor_plot <- line_plot(
  summary_sat,                                     
  ylab  = "Mean of Satire-based Engagement factor",
  title = NULL
) +
  theme(
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")  # t, r, b, l
  )   # <- closes theme()

print(factor_plot)

ggsave(
  "plot_satire_factor2.png",
  factor_plot,
  width  = 6,
  height = 4,
  dpi    = 300
)

# ─────────────────────────────────────────────────────────────
#  Confirmatory factor analysis 
# ─────────────────────────────────────────────────────────────
onefac_model <- '
  Sat =~ less_heavy + future_satire +
          interest_increase + mood_impact
'

# CFA on all trials
fit_cfa <- cfa(onefac_model, data = sat_wide[, sat_items], std.lv = TRUE)
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

# Invariance by news-frequency group
fit_conf   <- cfa(onefac_model, data = sat_wide,
                  group = "news_group", std.lv = TRUE)           # configural
fit_metric <- cfa(onefac_model, data = sat_wide,
                  group = "news_group", std.lv = TRUE,
                  group.equal = "loadings")                      # metric

lavTestLRT(fit_conf, fit_metric)   



# ------------------------------------------------------------
# Looking into the factor by category
# ------------------------------------------------------------

sat_cat <- sat_wide %>%                     
  left_join(
    articles %>%                            
      transmute(article_id = id,
                category    = str_to_title(category)),   # nicer caps
    by = "article_id"
  )

# Category-level means (separately for low / high)

cat_means <- sat_cat %>% 
  group_by(category, news_group) %>% 
  summarise(
    n    = n(),
    mean = mean(satire_factor),
    se   = sd(satire_factor) / sqrt(n),
    .groups = "drop"
  ) %>%                                     
  mutate(lwr = mean - 1.96 * se,
         upr = mean + 1.96 * se)

# two dots per category (low vs high) 
pd <- position_dodge(width = .45)           

factor_cat_plot <- ggplot(cat_means,
                          aes(x = mean,
                              y = reorder(category, mean),     # sort by grand mean
                              colour = news_group,
                              group  = news_group)) +
  
  # error bars
  geom_errorbarh(aes(xmin = lwr, xmax = upr),
                 height = .15, linewidth = .8,
                 position = pd, colour = "grey50") +
  
  # points
  geom_point(size = 3, position = pd) +
  
  
  scale_colour_manual(values = c(low  = "#e41a1c",
                                 high = "#377eb8"),
                      name   = "News Frequency",
                      labels = c("Low (< daily)", "High (≥ daily)")) +
  
  labs(title    = NULL,
       subtitle = NULL,
       x        = "Mean of 4-item factor (1-7)",
       y        = "Satire-based Engagement by category") +
  
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

print(factor_cat_plot)

ggsave("plot_satire_factor_by_category.png",
       factor_cat_plot, width = 6, height = 5, dpi = 300)


# Disconnect
dbDisconnect(con)

