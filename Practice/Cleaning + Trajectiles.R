library(tidyverse)
library(dplyr)
df <- read.csv('/Users/abbyhultquist/Desktop/First Year Project/CDI results.csv', header=TRUE, na.strings=".")
#load in data
df_raw <- read.csv("/Users/abbyhultquist/Downloads/cdi_results (3).csv", header = TRUE, check.names = FALSE)



# Transpose so variables become columns and children become rows
df_t <- as.data.frame(t(df_raw))
colnames(df_t) <- df_t[1, ]
df_t <- df_t[-1, ]
df_t$child_column_id <- rownames(df_t)
rownames(df_t) <- NULL
df <- df_t
#changing to numeric
df$'session num' <- as.numeric(df$'session num')
df$percentile   <- as.numeric(df$percentile)
df$words_spoken <- as.numeric(df$words_spoken)
#renaming DF
df <- df %>%
  rename(
    session_num = `session num`,
    session_date = `session date`,
    child_id = `child id`,
    words_spoken = `words spoken`
  )

#defining a late talker
df <- df %>%
  arrange(child_id, session_num) %>%          
  group_by(child_id) %>%
  mutate(
    first_session_num = min(session_num, na.rm = TRUE),
    late_talker = percentile[session_num == first_session_num] < 20, 
    typical_talker =  percentile[session_num == first_session_num] >= 20
  ) %>%
  ungroup()


library(ggplot2)
library(dplyr)

df <- df %>%
  mutate(group = ifelse(late_talker, "Late Talker", "Typical Talker"))

ggplot(df, aes(x = session_num, y = percentile, group = child_id, color = group)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 1, alpha = 0.6) +
  scale_color_manual(values = c("Late Talker" = "#e31a1c",   # red
                                "Typical Talker" = "#1f78b4")) + # blue
  labs(
    title = "Percentile Trajectories (Late vs. Typical Talkers)",
    x = "Session Number",
    y = "Percentile",
    color = "Group"
  ) +
  theme_minimal(base_size = 14)


setwd("~/Desktop")

write.csv(df, "df_cleaned.csv", row.names = FALSE)
getwd()







