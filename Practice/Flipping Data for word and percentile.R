# ==============================
# Libraries
# ==============================
library(dplyr)
library(psych)
library(car)
library(tidyr)

packageVersion("tidyr")

# Course functions
source("/Users/abbyhultquist/Documents/Stats100/gradstats_funcs.R")

# ==============================
# Load data
# ==============================
df <- read.csv(
  '/Users/abbyhultquist/Documents/First Year Project/CDI_cleaned.csv',
  header = TRUE,
  na.strings = ".")

library(dplyr)
library(tidyr)

df_wide <- df %>%
  select(child_id, session_num, words_spoken, percentile) %>%
  pivot_wider(
    names_from  = session_num,
    values_from = c(words_spoken, percentile),
    names_glue  = "session_{session_num}_{.value}"
  )


df_wide <- df %>%
  dplyr::select(child_id, session_num, words_spoken, percentile) %>%
  tidyr:::pivot_wider(
    names_from  = session_num,
    values_from = c(words_spoken, percentile),
    names_glue  = "session_{session_num}_{.value}"
  )

df %>%
  dplyr::summarise(n = dplyr::n(), .by = c(child_id, session_num)) %>%
  dplyr::filter(n > 1)

df %>%
  filter(child_id == 4319, session_num == 8) %>%
  select(words_spoken, percentile)


df_clean <- df %>%
  group_by(child_id, session_num) %>%
  summarise(
    words_spoken = first(words_spoken),
    percentile   = first(percentile),
    .groups = "drop"
  )

df_wide <- df_clean %>%
  tidyr::pivot_wider(
    names_from  = session_num,
    values_from = c(words_spoken, percentile),
    names_glue  = "session_{session_num}_{.value}"
  )

colnames(df_wide)


write.csv(
  df_wide,
  file = "/Users/abbyhultquist/Documents/First Year Project/session_by_child.csv",
  row.names = FALSE
)
