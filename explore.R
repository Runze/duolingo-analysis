source('helper_functions.R')

# load data
duo = read_csv('settles.acl16.learning_traces.13m.csv')

# create boolean variable to indicate whether a word is recalled correctly without making any mistake
duo = duo %>%
  mutate(no_mistake = p_recall == 1)

mean(duo$no_mistake)  # 83.8%

# split into training and test sets and perform all exploratory analysis on the training set only
set.seed(123456)
ix = caret::createDataPartition(duo$no_mistake, p = .7, list = F)

duo_train = duo[ix, ]
duo_test = duo[-ix, ]

write_feather(duo_train, 'duo_train.feather')
write_feather(duo_test, 'duo_test.feather')

# analyze each column in turn
## `p_recall`, `session_seen`, `session_correct`
### verify that `p_recall` = `session_correct` / `session_seen`
stopifnot(round(duo_train$p_recall, 2) == round(duo_train$session_correct / duo_train$session_seen, 2))  # verified

### plot the distribution of `p_recall` and `session_seen`
duo_train_lng = duo_train %>%
  gather(variable, value, p_recall, session_seen)

eda_p_recall_session_seen_dist_plt =
  ggplot(duo_train_lng, aes(x = value, y = ..density..)) +
  geom_histogram(alpha = .5) +
  facet_wrap(~ variable, scales = 'free') +
  ggtitle('Distribution of p_recall and session_seen') +
  better_theme()

### plot the relationship between `p_recall` and `session_seen`
eda_p_recall_vs_session_seen_plt =
  ggplot(duo_train, aes(x = as.factor(session_seen), y = p_recall)) +
  geom_boxplot() +
  ggtitle('p_recall ~ session_seen') +
  better_theme()

png(file = 'plots/eda_p_recall_session_seen_plt.png', units = 'in', width = 12, height = 6, res = 400)
grid.arrange(eda_p_recall_session_seen_dist_plt, eda_p_recall_vs_session_seen_plt, nrow = 1)
dev.off()

## `no_mistake` and `session_seen`
duo_train_no_mistake_by_session_seen = duo_train %>%
  group_by(session_seen) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  group_by(session_seen) %>%
  nest() %>%
  mutate(binom_test = map(data, function(df) tidy(binom.test(df$no_mistake, df$n)))) %>%
  unnest()

eda_no_mistake_vs_session_seen_plt =
  ggplot(duo_train_no_mistake_by_session_seen, aes(x = session_seen, y = estimate)) +
  geom_col(alpha = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  ggtitle('Chances of making no mistake ~ session_seen') +
  better_theme()

ggsave(eda_no_mistake_vs_session_seen_plt, file = 'plots/eda_no_mistake_vs_session_seen_plt.png', width = 8, height = 6, dpi = 400)

## `timestamp`
duo_train = duo_train %>%
  mutate(timestamp = as.POSIXct(timestamp, origin = '1970-01-01', tz = 'UTC'),
         date = as.Date(timestamp),
         dow = wday(date),
         hour = hour(timestamp))

duo_train_by_time = duo_train %>%
  group_by(dow, hour) %>%
  summarise(n = n(),
            prc_at_least_one_mistake = 1 - mean(no_mistake)) %>%
  ungroup() %>%
  mutate(prc_exercises = n / sum(n)) %>%
  gather(variable, value, prc_exercises, prc_at_least_one_mistake, factor_key = T)

eda_time_plt =
  ggplot(duo_train_by_time, aes(x = hour, y = value)) +
  geom_line() +
  facet_grid(variable ~ dow, scales = 'free') +
  expand_limits(ymin = 0) +
  ggtitle('Distribution of time and its relationship with the error rate') +
  better_theme()

ggsave(eda_time_plt, file = 'plots/eda_time_plt.png', width = 12, height = 7, dpi = 400)

## `delta`
duo_train = duo_train %>%
  mutate(delta_days = floor(delta / (3600 * 24)))

duo_by_delta = duo_train %>%
  group_by(delta_days) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  group_by(delta_days) %>%
  nest() %>%
  mutate(binom_test = map(data, function(df) tidy(binom.test(df$no_mistake, df$n)))) %>%
  unnest()

eda_delta_plt =
  ggplot(duo_by_delta, aes(x = delta_days, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .4) +
  coord_cartesian(xlim = c(0, 30)) +
  geom_vline(xintercept = 1, alpha = .5, linetype = 'longdash') +
  ggtitle('Chances of making no mistake ~ days since last time the word was seen') +
  better_theme()

ggsave(eda_delta_plt, file = 'plots/eda_delta_plt.png', width = 10, height = 7, dpi = 400)

## `user_id`
### count the number of exercises a user has done before the current session
duo_train_exerciese_per_user_session = duo_train %>%
  group_by(learning_language, user_id, timestamp) %>%
  summarise(exercises = n(),
            no_mistake = sum(no_mistake)) %>%
  arrange(learning_language, user_id, timestamp) %>%
  mutate(prior_exercises = lag(cumsum(exercises)),
         prior_no_mistake = lag(cumsum(no_mistake) / cumsum(exercises)))

### for users that are observed for the first time, impute success rate with global avearge per language
duo_train_exerciese_per_user_session = duo_train_exerciese_per_user_session %>%
  group_by(learning_language) %>%
  mutate(prc_no_mistake_global = sum(no_mistake) / sum(exercises),
         prior_exercises = ifelse(is.na(prior_exercises), 0, prior_exercises),
         prior_no_mistake = ifelse(is.na(prior_no_mistake), prc_no_mistake_global, prior_no_mistake))

### compare current success rate with the prior history
duo_train = duo_train %>%
  inner_join(duo_train_exerciese_per_user_session %>% select(learning_language, user_id, timestamp, prior_exercises, prior_no_mistake))

duo_train_lng = duo_train %>%
  mutate(prior_exercises_grp = cut2(prior_exercises, g = 10),
         prior_no_mistake_grp = cut2(prior_no_mistake, cuts = seq(0, 1, .1))) %>%
  gather(variable, value, prior_exercises_grp, prior_no_mistake_grp, factor_key = T) %>%
  group_by(variable, value) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  group_by(variable, value) %>%
  nest() %>%
  mutate(binom_test = map(data, function(df) tidy(binom.test(df$no_mistake, df$n)))) %>%
  unnest()

levels(duo_train_lng$variable) = c('Number of prior exercises (in deciles)', 'Prior chances of making no mistake')

eda_prior_per_user_plt =
  ggplot(duo_train_lng, aes(x = value, y = estimate)) +
  geom_col(alpha = .5) +
  facet_grid(~ variable, scales = 'free') +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  ggtitle('Chances of making no mistake ~ number of prior exercises and prior success rate per user') +
  better_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(eda_prior_per_user_plt, file = 'plots/eda_prior_per_user_plt.png', width = 12, height = 7, dpi = 400)

## `learning_language`
### `learning_language` ~ users
duo_train_exercises_per_user_lang = duo_train %>%
  group_by(learning_language, user_id) %>%
  summarise(n = )

duo_train_users_per_lang = duo_train_exercises_per_user_lang %>%
  summarise(users = n())

eda_users_per_lang_plt =
  ggplot(duo_train_users_per_lang, aes(x = learning_language, y = users)) +
  geom_col(alpha = .5) +
  ggtitle('Number of users per learning language') +
  better_theme()

eda_exercises_per_user_lang_plt =
  ggplot(duo_train_exercises_per_user_lang, aes(x = learning_language, y = n)) +
  geom_boxplot() +
  geom_quasirandom(data = sample_n(duo_train_exercises_per_user_lang, 500), alpha = .4, colour = 'skyblue') +
  coord_cartesian(ylim = c(0, 100)) +
  ggtitle('Distribution of exercises per user per learning language') +
  better_theme()

png(file = 'plots/eda_lang_vs_users_plt.png', units = 'in', width = 12, height = 6, res = 400)
grid.arrange(eda_users_per_lang_plt, eda_exercises_per_user_lang_plt, nrow = 1)
dev.off()

### `no_mistake` ~ `learning_language`
duo_train_by_lang = duo_train %>%
  group_by(learning_language) %>%
  summarise(prc_no_mistake = mean(no_mistake))

eda_lang_plt =
  ggplot(duo_train_by_lang, aes(x = learning_language, y = prc_no_mistake)) +
  geom_col(alpha = .5) +
  ggtitle('Chances of making no mistake ~ learning language') +
  better_theme()

ggsave(eda_lang_plt, file = 'plots/eda_lang_plt.png', width = 8, height = 6, dpi = 400)

## `lexeme_string`
### parse out `surface_form` and `lemma`
duo_train = duo_train %>%
  mutate(surface_form_lemma = sub('^(.*?/.*?)<.+$', '\\1', lexeme_string)) %>%
  separate(surface_form_lemma, into = c('surface_form', 'lemma'), sep = '/')

### for `surface_form` wildcards, assume it is the same as `lemma`
duo_train = duo_train %>%
  mutate(surface_form = ifelse(grepl('<.+>', surface_form), lemma, surface_form))

### compute the length of `surface_form` and `lemma`
duo_train = duo_train %>%
  mutate(surface_form_len = nchar(surface_form),
         lemma_len = nchar(lemma))

### compute similarity between `surface_form` and `lemma`
duo_train_surface_form_lemma = duo_train %>%
  select(starts_with('surface_form'), starts_with('lemma')) %>%
  rowwise() %>%
  mutate(similarity_w_lemma = as.numeric(1 - adist(surface_form, lemma) / max(surface_form_len, lemma_len))) %>%
  ungroup()

duo_train$similarity_w_lemma = duo_train_surface_form_lemma$similarity_w_lemma

### plot `no_mistake` with `similarity_w_lemma` and `surface_form_len`
duo_train_lng = duo_train %>%
  mutate(similarity_w_lemma_grp = cut2(similarity_w_lemma, cuts = seq(0, 1, .1)),
         surface_form_len_grp = cut2(surface_form_len, cuts = 1:10)) %>%
  gather(variable, value, similarity_w_lemma_grp, surface_form_len_grp, factor_key = T) %>%
  group_by(variable, value) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  group_by(variable, value) %>%
  nest() %>%
  mutate(binom_test = map(data, function(df) tidy(binom.test(df$no_mistake, df$n)))) %>%
  unnest()

# order cuts
duo_train_lng = duo_train_lng %>%
  group_by(variable) %>%
  arrange(parse_number(value))

duo_train_lng$value = factor(duo_train_lng$value, levels = duo_train_lng$value)

eda_by_similarity_w_lemma_and_surface_len_plt =
  ggplot(duo_train_lng, aes(x = value, y = estimate)) +
  geom_col(alpha = .5) +
  facet_grid(~ variable, scales = 'free') +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  ggtitle('Chances of making no mistake ~ similarity between the surface form and the lemma and the length of the surface form') +
  better_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(eda_by_similarity_w_lemma_and_surface_len_plt, file = 'plots/eda_by_similarity_w_lemma_and_surface_len_plt.png', width = 12, height = 7, dpi = 400)

write_feather(duo_train, 'duo_train.feather')
