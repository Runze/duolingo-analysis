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
### count the number of exercises a user has done and the success rate before the current session
duo_train_per_user_per_session = duo_train %>%
  group_by(learning_language, user_id, timestamp) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  arrange(learning_language, user_id, timestamp) %>%
  mutate(prior_exercises_per_user = lag(cumsum(n)),
         prior_no_mistake_per_user = lag(cumsum(no_mistake)),
         prior_exercises_per_user = ifelse(is.na(prior_exercises_per_user), 0, prior_exercises_per_user),
         prior_no_mistake_per_user = ifelse(is.na(prior_no_mistake_per_user), 0, prior_no_mistake_per_user))

### nest by language and fit a beta prior per language
duo_train_per_user_per_session_ns_by_lang = duo_train_per_user_per_session %>%
  group_by(learning_language) %>%
  nest() %>%
  mutate(prior = map(data, function(df) fit_beta_prior(df, x = 'prior_no_mistake_per_user', n = 'prior_exercises_per_user')))

### extract the fit plots
duo_train_per_user_per_session_ns_by_lang = duo_train_per_user_per_session_ns_by_lang %>%
  mutate(prior_fit_plt = map2(prior, learning_language, function(prior, lang) {
    prior$fit_plt +
      xlab('Prior no mistake rate per user per session\n(where number of observations >= 50)') +
      ggtitle(paste('Learning language:', lang))
  }))

png(file = 'plots/eda_prior_no_mistake_per_user_per_session_prior_fit_plt.png', units = 'in', width = 12, height = 8, res = 400)
do.call('grid.arrange', c(duo_train_per_user_per_session_ns_by_lang$prior_fit_plt, nrow = 2))
dev.off()

### extract the alpha and beta estimates and save for future use
prior_no_mistake_beta_params = duo_train_per_user_per_session_ns_by_lang %>%
  mutate(beta_params = map(prior, function(df) df$prior$parameters)) %>%
  unnest(beta_params) %>%
  select(learning_language, alpha, beta) %>%
  rename(prior_no_mistake_alpha = alpha,
         prior_no_mistake_beta = beta)

write_csv(prior_no_mistake_beta_params, 'prior_no_mistake_beta_params.csv')

### compute posterior estimates of prior no mistake rate
duo_train_per_user_per_session = duo_train_per_user_per_session %>%
  inner_join(prior_no_mistake_beta_params) %>%
  mutate(prior_no_mistake_rate_per_user = (prior_no_mistake_per_user + prior_no_mistake_alpha) / (prior_exercises_per_user + prior_no_mistake_alpha + prior_no_mistake_beta))

### compare the current success rate with the prior history
duo_train = duo_train %>%
  inner_join(duo_train_per_user_per_session %>% select(learning_language, user_id, timestamp, prior_exercises_per_user, prior_no_mistake_rate_per_user))

duo_train_lng = duo_train %>%
  mutate(prior_exercises_per_user_grp = cut2(prior_exercises_per_user, g = 10),
         prior_no_mistake_rate_per_user_grp = cut2(prior_no_mistake_rate_per_user, g = 10)) %>%
  gather(variable, value, prior_exercises_per_user_grp, prior_no_mistake_rate_per_user_grp, factor_key = T) %>%
  group_by(variable, value) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  group_by(variable, value) %>%
  nest() %>%
  mutate(binom_test = map(data, function(df) tidy(binom.test(df$no_mistake, df$n)))) %>%
  unnest()

levels(duo_train_lng$variable) = c('Number of prior exercises per user\n(in deciles)', 'Prior chances of making no mistake per user\n(adjusted by Empirical Bayes; in deciles)')

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
  summarise(n = n())

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

### `learning_language` ~ `ui_language`
duo_train_learning_ui_lang = duo_train %>%
  group_by(learning_language, ui_language) %>%
  summarise(users = n_distinct(user_id))

eda_leanring_ui_lang_plt =
  ggplot(duo_train_learning_ui_lang, aes(x = learning_language, y = ui_language, fill = users)) +
  geom_tile() +
  scale_fill_distiller(type = 'seq', palette = 'Blues', direction = 1) +
  coord_fixed() +
  ggtitle('Learning language vs. UI language') +
  better_theme() %+replace%
  # theme(legend.key.width = unit(1, 'cm'))
  theme(legend.text = element_text(size = 10))

ggsave(eda_leanring_ui_lang_plt, file = 'plots/eda_leanring_ui_lang_plt.png', width = 8, height = 6, dpi = 400)

### `no_mistake` in learning English ~ `ui_language`
duo_train_learning_en = duo_train %>%
  filter(learning_language == 'en') %>%
  group_by(ui_language) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  group_by(ui_language) %>%
  nest() %>%
  mutate(binom_test = map(data, function(df) tidy(binom.test(df$no_mistake, df$n)))) %>%
  unnest()

eda_learning_en_plt =
  ggplot(duo_train_learning_en, aes(x = ui_language, y = estimate)) +
  geom_col(alpha = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  ggtitle('Chances of making no mistake in learning English') +
  better_theme()

### `no_mistake` ~ `learning_language` by English speakers
duo_train_ui_en = duo_train %>%
  filter(ui_language == 'en') %>%
  group_by(learning_language) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  group_by(learning_language) %>%
  nest() %>%
  mutate(binom_test = map(data, function(df) tidy(binom.test(df$no_mistake, df$n)))) %>%
  unnest()

eda_ui_en_plt =
  ggplot(duo_train_ui_en, aes(x = learning_language, y = estimate)) +
  geom_col(alpha = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  ggtitle('Chances of making no mistake by English speakers') +
  better_theme()

png(file = 'plots/eda_learning_en_and_ui_en_plt.png', units = 'in', width = 12, height = 6, res = 400)
grid.arrange(eda_learning_en_plt, eda_ui_en_plt, nrow = 1)
dev.off()

## `lexeme_string`
### parse out `surface_form` and `lemma`
duo_train = duo_train %>%
  mutate(surface_form_lemma_pos = sub('^(.*?/.*?<.*?)>.*$', '\\1', lexeme_string),
         surface_form_lemma_pos = gsub('<([^*]*)$', ';\\1', surface_form_lemma_pos)) %>%
  separate(surface_form_lemma_pos, into = c('surface_form', 'lemma', 'pos'), sep = '/|;', remove = F) %>%
  mutate(pos = ifelse(grepl('@', pos), 'missing', pos))

### for `surface_form` wildcards, assume it is the same as `lemma`
duo_train = duo_train %>%
  mutate(surface_form = ifelse(grepl('<.+>', surface_form), lemma, surface_form))

### compute no mistake rate per word
surface_forms = duo_train %>%
  group_by(learning_language, surface_form_lemma_pos, surface_form, lemma, pos) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  mutate(prc_no_mistake = no_mistake / n)

### nest by language and fit a beta prior per language
surface_forms_ns_by_lang = surface_forms %>%
  group_by(learning_language) %>%
  nest() %>%
  mutate(prior = map(data, fit_beta_prior))

### extract the fit plots
surface_forms_ns_by_lang = surface_forms_ns_by_lang %>%
  mutate(prior_fit_plt = map2(prior, learning_language, function(prior, lang) {
    prior$fit_plt +
      xlab('Prior no mistake rate per word\n(where number of observations >= 50)') +
      ggtitle(paste('Learning language:', lang))
  }))

png(file = 'plots/eda_no_mistake_per_word_prior_fit_plt.png', units = 'in', width = 12, height = 8, res = 400)
do.call('grid.arrange', c(surface_forms_ns_by_lang$prior_fit_plt, nrow = 2))
dev.off()

### apply the prior to the data to estimate the posterior no mistake rate
surface_forms_ns_by_lang = surface_forms_ns_by_lang %>%
  mutate(data = map2(data, prior, function(df, prior) augment(prior$prior, df)))

### find the easiest and hardest words per language
surface_forms = surface_forms_ns_by_lang %>%
  unnest(data)

surface_forms_top = surface_forms %>%
  group_by(learning_language) %>%
  mutate(rk_asc = row_number(.fitted),
         rk_desc = row_number(desc(.fitted))) %>%
  filter(rk_asc <= 10 | rk_desc <= 10) %>%
  mutate(difficulty = ifelse(rk_asc <= 10, 'Top 10 hardest', 'Top 10 easiest')) %>%
  arrange(learning_language, rk_asc)

surface_forms_top$surface_form_lemma_pos = factor(surface_forms_top$surface_form_lemma_pos, levels = surface_forms_top$surface_form_lemma_pos)

eda_easiest_hardest_words_plt =
  ggplot(surface_forms_top, aes(x = .fitted, y = surface_form_lemma_pos, colour = difficulty)) +
  geom_point() +
  facet_wrap(~ learning_language, scales = 'free_y') +
  geom_errorbarh(aes(xmin = .low, xmax = .high)) +
  xlab('Estimated no mistake rate') +
  ggtitle('Top 10 easiest and hardest words per language') +
  better_theme()

ggsave(eda_easiest_hardest_words_plt, file = 'plots/eda_easiest_hardest_words_plt.png', width = 14, height = 12, dpi = 400)

### analyze what correlates with a word's no mistake rate
#### the length of `surface_form`
surface_forms = surface_forms %>%
  group_by(learning_language) %>%
  mutate(surface_form_len = nchar(surface_form),
         surface_form_len_grp = cut2(surface_form_len, g = 10))

surface_forms = order_factors(surface_forms, var = 'surface_form_len_grp')

eda_by_surface_form_len_plt =
  ggplot(surface_forms, aes(x = surface_form_len_grp, y = .fitted)) +
  geom_boxplot() +
  facet_grid(~ learning_language, scales = 'free_x') +
  ylab('no mistake rate adjusted by empirical Bayes') +
  ggtitle('Chances of making no mistake ~ length of the surface form') +
  better_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(eda_by_surface_form_len_plt, file = 'plots/eda_by_surface_form_len_plt.png', width = 12, height = 6, dpi = 400)

#### similarity between `surface_form` and `lemma`
surface_forms = surface_forms %>%
  mutate(surface_form_eq_lemma = surface_form == lemma)

mean(surface_forms$surface_form_eq_lemma)  # 74%

eda_surface_form_eq_lemma_plt =
  ggplot(surface_forms, aes(x = surface_form_eq_lemma, y = .fitted)) +
  geom_boxplot() +
  facet_grid(~ learning_language, scales = 'free_x') +
  labs(x = '`surface_form` is the same as `lemma`', y = 'no mistake rate adjusted by empirical Bayes') +
  ggtitle('Chances of making no mistake ~ similarity between `surface_form` and `lemma`') +
  better_theme()

ggsave(eda_surface_form_eq_lemma_plt, file = 'plots/eda_surface_form_eq_lemma_plt.png', width = 12, height = 6, dpi = 400)

#### POS
eda_by_pos_plt =
  ggplot(surface_forms, aes(x = pos, y = .fitted)) +
  geom_boxplot(position = 'identity') +
  facet_grid(~ learning_language) +
  coord_flip() +
  ylab('no mistake rate adjusted by empirical Bayes') +
  ggtitle('Chances of making no mistake ~ part-of-speech tagging') +
  better_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(eda_by_pos_plt, file = 'plots/eda_by_pos_plt.png', width = 12, height = 6, dpi = 400)

## compute the same metrics on training data
duo_train = duo_train %>%
  mutate(surface_form_len = nchar(surface_form),
         surface_form_eq_lemma = surface_form == lemma)

## `history_seen` and `history_correct`
### nest by language and fit a beta prior per language
duo_train_history_ns_by_lang = duo_train %>%
  group_by(learning_language) %>%
  nest() %>%
  mutate(prior = map(data, function(df) fit_beta_prior(df, x = 'history_correct', n = 'history_seen')))

### extract the fit plots
duo_train_history_ns_by_lang = duo_train_history_ns_by_lang %>%
  mutate(prior_fit_plt = map2(prior, learning_language, function(prior, lang) {
    prior$fit_plt +
      xlab('Historical correct rate per word\n(where number of observations >= 50)') +
      ggtitle(paste('Learning language:', lang))
  }))

png(file = 'plots/eda_history_correct_rate_prior_fit_plt.png', units = 'in', width = 12, height = 8, res = 400)
do.call('grid.arrange', c(duo_train_history_ns_by_lang$prior_fit_plt, nrow = 2))
dev.off()

### extract the alpha and beta estimates and save for future use
history_correct_rate_beta_params = duo_train_history_ns_by_lang %>%
  mutate(beta_params = map(prior, function(df) df$prior$parameters)) %>%
  unnest(beta_params) %>%
  select(learning_language, alpha, beta) %>%
  rename(history_correct_rate_alpha = alpha,
         history_correct_rate_beta = beta)

write_csv(history_correct_rate_beta_params, 'history_correct_rate_beta_params.csv')

### compute posterior estimates of historical correct rate
duo_train = duo_train %>%
  inner_join(history_correct_rate_beta_params) %>%
  mutate(history_correct_rate = (history_correct + history_correct_rate_alpha) / (history_seen + history_correct_rate_alpha + history_correct_rate_beta))

### compare the current success rate with the historical one
duo_train_lng = duo_train %>%
  mutate(history_seen_grp = cut2(history_seen, g = 10),
         history_correct_rate_grp = cut2(history_correct_rate, g = 10)) %>%
  gather(variable, value, history_seen_grp, history_correct_rate_grp, factor_key = T) %>%
  group_by(variable, value) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  group_by(variable, value) %>%
  nest() %>%
  mutate(binom_test = map(data, function(df) tidy(binom.test(df$no_mistake, df$n)))) %>%
  unnest()

duo_train_lng = order_factors(duo_train_lng, var = 'value')
levels(duo_train_lng$variable) = c('Number of times the user has seen the word\n(in deciles)', 'Historical correct rate\n(adjusted by Empirical Bayes; in deciles)')

eda_history_correct_plt =
  ggplot(duo_train_lng, aes(x = value, y = estimate)) +
  geom_col(alpha = .5) +
  facet_grid(~ variable, scales = 'free') +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  ggtitle('Chances of making no mistake ~ number of times the user has seen the word and the historical correct rate') +
  better_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(eda_history_correct_plt, file = 'plots/eda_history_correct_plt.png', width = 12, height = 7, dpi = 400)

write_feather(duo_train, 'duo_train.feather')
