source('helper_functions.R')

# load test data
duo_test = read_feather('duo_test.feather')

# generate all features
## `log_delta_days`
duo_test = duo_test %>%
  mutate(log_delta_days = log(floor(delta / (3600 * 24)) + 1))

## `log_prior_exercises_per_user` and `prior_no_mistake_rate_per_user`
### compute prior exercises and successes per user
duo_test_per_user_per_session = duo_test %>%
  group_by(learning_language, user_id, timestamp) %>%
  summarise(n = n(),
            no_mistake = sum(no_mistake)) %>%
  arrange(learning_language, user_id, timestamp) %>%
  mutate(prior_exercises_per_user = lag(cumsum(n)),
         prior_no_mistake_per_user = lag(cumsum(no_mistake)),
         prior_exercises_per_user = ifelse(is.na(prior_exercises_per_user), 0, prior_exercises_per_user),
         prior_no_mistake_per_user = ifelse(is.na(prior_no_mistake_per_user), 0, prior_no_mistake_per_user))

### apply the estimated beta parameters
prior_no_mistake_beta_params = read_csv('prior_no_mistake_beta_params.csv')

duo_test_per_user_per_session = duo_test_per_user_per_session %>%
  inner_join(prior_no_mistake_beta_params) %>%
  mutate(prior_no_mistake_rate_per_user = (prior_no_mistake_per_user + prior_no_mistake_alpha) / (prior_exercises_per_user + prior_no_mistake_alpha + prior_no_mistake_beta))

### add to test data
duo_test = duo_test %>%
  inner_join(duo_test_per_user_per_session %>% select(learning_language, user_id, timestamp, prior_exercises_per_user, prior_no_mistake_rate_per_user))

### log-transform
duo_test = duo_test %>%
  mutate(log_prior_exercises_per_user = log(prior_exercises_per_user + 1))

## `ui_learning_language`
duo_test = duo_test %>%
  mutate(ui_learning_language = paste(ui_language, learning_language, sep = '_'))

## `surface_form_len` and `surface_form_eq_lemma`
### parse out `surface_form` and `lemma`
duo_test = duo_test %>%
  mutate(surface_form_lemma_pos = sub('^(.*?/.*?<.*?)>.*$', '\\1', lexeme_string),
         surface_form_lemma_pos = gsub('<([^*]*)$', ';\\1', surface_form_lemma_pos)) %>%
  separate(surface_form_lemma_pos, into = c('surface_form', 'lemma', 'pos'), sep = '/|;', remove = F) %>%
  mutate(pos = ifelse(grepl('@', pos), 'missing', pos)) %>%
  mutate(surface_form = ifelse(grepl('<.+>', surface_form), lemma, surface_form))

### compute the length of `surface_form` and the similarity between `surface_form` and `lemma`
duo_test = duo_test %>%
  mutate(surface_form_len = nchar(surface_form),
         surface_form_eq_lemma = surface_form == lemma)

## `pos_binned`
duo_test = duo_test %>%
  mutate(pos_binned = ifelse(pos %in% duo_train_pos$pos[1:10], pos, 'other'))

## `log_history_seen` and `history_correct_rate`
### apply the estimated beta parameters
history_correct_rate_beta_params = read_csv('history_correct_rate_beta_params.csv')

duo_test = duo_test %>%
  inner_join(history_correct_rate_beta_params) %>%
  mutate(history_correct_rate = (history_correct + history_correct_rate_alpha) / (history_seen + history_correct_rate_alpha + history_correct_rate_beta))

### log-transform
duo_test = duo_test %>%
  mutate(log_history_seen = log(history_seen))

# convert to model matrix
duo_test_X = model.matrix(no_mistake ~ ., data = duo_test[, c('no_mistake', features)])[, -1]

# apply the trained model
duo_test_no_mistake_pred = predict(duo_train_logit, newdata = duo_test_X, type = 'prob')
duo_test$no_mistake_pred = duo_test_no_mistake_pred$TRUE.

# plot the distribution of predicted probabilities
duo_test_pred_prob_plt =
  ggplot(duo_test, aes(x = no_mistake_pred, y = ..density..)) +
  geom_histogram(fill = 'skyblue', alpha = .8) +
  xlab('Predicted probability of correctly recalling a given word') +
  ggtitle('Distribution of predicted probability') +
  better_theme()

# compute AUC under ROC and PR, as well as F1 score per threshold
roc = roc.curve(scores.class0 = duo_test$no_mistake_pred[duo_test$no_mistake], scores.class1 = duo_test$no_mistake_pred[!duo_test$no_mistake])$auc  # 0.64
pr = pr.curve(scores.class0 = duo_test$no_mistake_pred[duo_test$no_mistake], scores.class1 = duo_test$no_mistake_pred[!duo_test$no_mistake])$auc.integral  # 0.89

thresholds = seq(0, 1, .01)
duo_test_no_mistake_actual = duo_test$no_mistake

duo_test_model_metrics = lapply(thresholds, function(t) {
  duo_test_no_mistake_pred_label = duo_test$no_mistake_pred >= t
  
  # sensitivity / recall
  sensitivity = mean(duo_test_no_mistake_pred_label[duo_test_no_mistake_actual])
  
  # specificity
  specificity = mean(!duo_test_no_mistake_pred_label[!duo_test_no_mistake_actual])
  
  # precision
  precision = mean(duo_test_no_mistake_actual[duo_test_no_mistake_pred_label])
  
  # F1 score
  f1 = 2 * precision * sensitivity / (precision + sensitivity)
  
  data_frame(
    threshold = t,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    f1 = f1
  )
}) %>%
  bind_rows()

## plot AUC under ROC and PR
duo_test_model_metrics_roc_plt =
  ggplot(duo_test_model_metrics, aes(x = 1 - specificity, y = sensitivity)) +
  geom_point() +
  geom_abline(slope = 1, linetype = 'longdash', alpha = .5) +
  ggtitle(paste('AUC under ROC:', round(roc, 2))) +
  expand_limits(xmin = 0, ymin = 0) +
  coord_fixed() +
  better_theme()

duo_test_model_metrics_pr_plt =
  ggplot(duo_test_model_metrics, aes(x = sensitivity, y = precision)) +
  geom_point() +
  ggtitle(paste('AUC under Precision-Recall:', round(pr, 2))) +
  coord_fixed() +
  expand_limits(xmin = 0, ymin = 0) +
  better_theme()

png(file = 'plots/test_logit_auc_plt.png', units = 'in', width = 11, height = 6, res = 400)
grid.arrange(duo_test_model_metrics_roc_plt, duo_test_model_metrics_pr_plt, nrow = 1)
dev.off()

## plot F1 score per threshold
### reshape to long
duo_test_model_metrics_lng = duo_test_model_metrics %>%
  select(threshold, sensitivity, precision, f1) %>%
  gather(variable, value, -threshold, factor_key = T)

duo_test_model_metrics_f1_plt =
  ggplot(duo_test_model_metrics_lng, aes(x = threshold, y = value, colour = variable)) +
  geom_point() +
  ggtitle('Precision and recall per threshold') +
  better_theme() %+replace%
  theme(legend.position = c(.03, .03), legend.justification = c(0, 0))

### illustrate alongside the distribution of predicted probabilities
png(file = 'plots/test_logit_f1_plt.png', units = 'in', width = 12, height = 6, res = 400)
grid.arrange(duo_test_model_metrics_f1_plt, duo_test_pred_prob_plt, nrow = 1)
dev.off()

write_feather(duo_test, 'duo_test.feather')
