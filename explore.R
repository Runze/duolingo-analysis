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

# for users that are observed for the first time, impute success rate with global avearge per language
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
