library(Hmisc)
library(stringr)
library(feather)
library(gridExtra)
library(broom)
library(tidyverse)
options(stringsAsFactors = F)

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
