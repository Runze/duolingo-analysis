source('helper_functions.R')

# load training data
duo_train = read_feather('duo_train.feather')

# define training features
## transform variables if necessary
duo_train = duo_train %>%
  mutate(log_delta_days = log(delta_days + 1),
         log_prior_exercises_per_user = log(prior_exercises_per_user + 1),
         log_history_seen = log(history_seen),
         ui_learning_language = paste(ui_language, learning_language, sep = '_'))

## group minor POS together
duo_train_pos = duo_train %>%
  group_by(pos) %>%
  summarise(cnt = n(),
            langs = n_distinct(learning_language)) %>%
  mutate(prc = cnt / sum(cnt)) %>%
  arrange(-cnt)

duo_train = duo_train %>%
  mutate(pos_binned = ifelse(pos %in% duo_train_pos$pos[1:10], pos, 'other'))

## define features
features = c(
  'log_delta_days',
  'log_prior_exercises_per_user',
  'prior_no_mistake_rate_per_user',
  'ui_learning_language',
  'surface_form_len',
  'surface_form_eq_lemma',
  'pos_binned',
  'log_history_seen',
  'history_correct_rate'
)

# downsample training data (because it's too big for my laptop to handle...)
set.seed(123456)
duo_train_ds = duo_train %>%
  group_by(no_mistake) %>%
  sample_frac(size = .2, replace = F)

# create model matrix
duo_train_X = model.matrix(no_mistake ~ ., data = duo_train_ds[, c('no_mistake', features)])[, -1]
duo_train_Y = make.names(duo_train_ds$no_mistake)

# check for highly correlated variables
high_corr = findCorrelation(cor(duo_train_X))
# none found

# train models
## 5-fold cv
ctrl = trainControl(method = 'cv', number = 5, classProbs = T,
                    summaryFunction = twoClassSummary)

## logistic regression
duo_train_logit = train(x = duo_train_X, y = duo_train_Y,
                        method = 'glm', family = 'binomial',
                        metric = 'ROC', trControl = ctrl)

## ROC: 0.64
save(duo_train_logit, file = 'duo_train_logit.rda')

## gradient boost trees
duo_train_gbm = train(x = duo_train_X, y = duo_train_Y,
                      method = 'gbm', metric = 'ROC',
                      trControl = ctrl, verbose = F)

## ROC: 0.65

# GBM provided minimal improvement, so stick with logistic regression

# summarize model coefficients
duo_train_logit_coefs = tidy(duo_train_logit$finalModel)

duo_train_logit_coefs = duo_train_logit_coefs %>%
  mutate(conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error) %>%
  arrange(abs(statistic))

# clean up names for dummy variables
duo_train_logit_coefs$term = gsub('language', 'language_', duo_train_logit_coefs$term)
duo_train_logit_coefs$term = gsub('binned', 'binned_', duo_train_logit_coefs$term)

duo_train_logit_coefs$term = factor(duo_train_logit_coefs$term, levels = duo_train_logit_coefs$term)

train_logit_coefs_plt =
  ggplot(duo_train_logit_coefs, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = 'longdash', alpha = .5) +
  ggtitle('Summary of coefficient estimates per logistic regression', 'Dependent variable: making no mistake on a given word in the current session\nIndependent variables ranked by their statistical significance') +
  better_theme()

ggsave(train_logit_coefs_plt, file = 'plots/train_logit_coefs_plt.png', width = 10, height = 9, dpi = 400)
