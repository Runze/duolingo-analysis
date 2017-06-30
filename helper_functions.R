library(Hmisc)
library(stringr)
library(feather)
library(gridExtra)
library(broom)
library(lubridate)
library(ggbeeswarm)
library(ebbr)
library(caret)
library(tidyverse)
options(stringsAsFactors = F)

# customize ggplot theme
better_theme = function() {
  theme_bw() %+replace%
    theme(
      legend.position = 'bottom',
      strip.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 14),
      panel.grid.minor = element_line(linetype = 'dotted', colour = 'grey'),
      strip.background = element_rect(colour = 'grey')
    )
}

### fit a beta prior to each language's rate of no mistake
fit_beta_prior = function(df, x = 'no_mistake', n = 'n') {
  # filter to subjects (e.g., users, words) with at least 50 occurrences
  # (based on EDA, this is enough to create a smooth distribution)
  df = df %>%
    mutate_('no_mistake' = x,
            'n' = n) %>%
    filter(n >= 50) %>%
    mutate(prc_no_mistake = no_mistake / n)
  
  return_values = list()
  
  # fit prior
  return_values$prior = df %>%
    ebb_fit_prior(no_mistake, n)
  
  # extract prior mean
  return_values$prior_mean = return_values$prior$parameters$alpha / (return_values$prior$parameters$alpha + return_values$prior$parameters$beta)
  
  # visualize the fit
  label = paste0(
    'alpha: ', round(return_values$prior$parameters$alpha, 2),
    '\nbeta: ', round(return_values$prior$parameters$beta, 2),
    '\nprior mean: ', round(return_values$prior_mean, 2),
    '\n'
  )
  
  return_values$fit_plt =
    ggplot(df) +
    geom_histogram(aes(x = prc_no_mistake, y = ..density..), alpha = .5) +
    stat_function(fun = function(x) dbeta(x, return_values$prior$parameters$alpha, return_values$prior$parameters$beta), colour = 'red', size = 1) +
    annotate(geom = 'text', x = 0, y = 0, label = label, hjust = 0, vjust = 0) +
    better_theme()
  
  return_values
}

#### order factor levels
order_factors = function(df, var) {
  df = df %>%
    mutate_('order_var' = var) %>%
    arrange(parse_number(order_var))
  
  df[[var]] = factor(df[[var]], levels = unique(df$order_var))
  
  df %>%
    select(-order_var)
}
