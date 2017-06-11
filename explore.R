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

png(file = 'plots/eda_p_recall_session_seen.png', units = 'in', width = 12, height = 6, res = 400)
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
  ggtitle('no_mistake ~ session_seen') +
  better_theme()

ggsave(eda_no_mistake_vs_session_seen_plt, file = 'plots/eda_no_mistake_vs_session_seen_plt.png', width = 8, height = 6, dpi = 400)
