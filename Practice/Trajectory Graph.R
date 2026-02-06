df <- read.csv('/Users/abbyhultquist/Documents/First Year Project/CDI_cleaned.csv', header=TRUE, na.strings=".")

library(dplyr)

# Count each group
group_counts <- df %>%
  count(group)
# Count unique child IDs
num_children <- n_distinct(df$child_id)
print(num_children)

df %>%
  summarise(
    late_bloomers = n_distinct(child_id[late_bloomer == 1]),
    plts = n_distinct(child_id[plt == 1]),
    late_talkers = n_distinct(child_id[late_talker == 1]),
    typical_talkers = n_distinct(child_id[typical_talker == 1]),
    total_children = n_distinct(child_id)
  )

# double checking overlap 
df %>%
  distinct(child_id, late_bloomer, plt, late_talker) %>%
  count(late_bloomer, plt, late_talker)





#------ graph work -----------

library(dplyr)

df2 <- df %>%
  mutate(
    group = case_when(
      plt == TRUE ~ "PLT",
      late_bloomer == TRUE ~ "Late Bloomer",
      TRUE ~ "Typical Talker"
    )
  )

library(ggplot2)

ggplot(df2, aes(x = session_num, y = words_spoken, 
                group = child_id, color = group)) +
  geom_line(alpha = 0.4, linewidth = 0.6) +
  scale_color_manual(values = c(
    "PLT" = "red",
    "Late Bloomer" = "orange",
    "Typical Talker" = "blue"
  )) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Individual Words Spoken Trajectories",
    x = "Session Number",
    y = "Words Spoken",
    color = "Group"
  )


#------------------------vector------------------------------
df

word_cols <- names(df)[ !(names(df) %in% c(
  "child_id", "study.id", "study", "gender", "age", "birthday",
  "session_date", "session_num", "total.num.sessions", "words_spoken",
  "items.excluded", "percentile", "extra.categories", "revision",
  "languages", "num.languages", "cdi.type", "hard.of.hearing",
  "deleted", "class", "typical_talker", "group", 
  "child_column_id", "first_session_num", "late_talker",
  "last_session_num", "late_bloomer", "plt"
)) ]

# For each row, store which word columns equal 1
df$known_words <- apply(df[word_cols], 1, function(x) names(x)[x == 1])

#known words stores another column for each child with the words they know at that month as a vector
# Check
df$known_words[[1]]
df$known_words[[4]]

df[1,]

library(igraph)

make_random_network <- function(words, p = 0.5) {
  n <- length(words)
  
  if (n <= 1) {
    return(NULL)
  }
  
  g <- sample_gnp(n = n, p = p, directed = FALSE)
  V(g)$name <- words
  
  return(g)
}

df$random_network <- lapply(df$known_words, make_random_network)


df$known_words[[1]]
df$random_network[[1]]
plot(df$random_network[[1]])


#----------------------child 1----------------------------
library(igraph)

# Filter child 1 and order sessions
child1_df <- df[df$child_id == 4139, ]
child1_df <- child1_df[order(child1_df$session_num), ]

# Step 1: Create a single random network structure with all words the child ever knows
all_words <- unique(unlist(child1_df$known_words))
n <- length(all_words)

# Fixed random network for all sessions
set.seed(123)  # for reproducibility
full_network <- sample_gnp(n = n, p = 0.1, directed = FALSE)
V(full_network)$name <- all_words

# Step 2: Generate session-specific subnetworks
child1_networks <- lapply(child1_df$known_words, function(words) {
  # Induce subgraph of only the words known at this session
  induced_subgraph(full_network, vids = words)
})

# Step 3: Plot networks in a 3x4 grid
par(mfrow = c(3,4), mar = c(1,1,2,1))
for (i in 1:length(child1_networks)) {
  g <- child1_networks[[i]]
  if (!is.null(g)) {
    plot(g,
         vertex.size = 15,
         vertex.label.cex = 0.7,
         vertex.label.color = "black",
         main = paste("Session", i))
  } else {
    plot.new()
    title(main = paste("Session", i))
  }
}
par(mfrow = c(1,1))

#-----------------------------plt graph ------------------------------------
session12_df <- df[df$session_num == 12, ]

# Count number of known words for each child
session12_df$known_count <- rowSums(session12_df[word_cols] == 1)

# Find the minimum number of known words
min_words <- min(session12_df$known_count, na.rm = TRUE)

# Find child(ren) with that minimum
child_fewest_words <- session12_df$child_id[session12_df$known_count == min_words]

child_fewest_words


# Filter child 
child1_df <- df[df$child_id == 4214, ]
child1_df <- child1_df[order(child1_df$session_num), ]

all_words <- unique(unlist(child1_df$known_words))
n <- length(all_words)

#  random network for all sessions - how do i make this a semantic network 
set.seed(123)  
full_network <- sample_gnp(n = n, p = 0.1, directed = FALSE)
V(full_network)$name <- all_words

# Step 2: Generate session-specific subnetworks
child1_networks <- lapply(child1_df$known_words, function(words) {
  # Induce subgraph of only the words known at this session
  induced_subgraph(full_network, vids = words)
})


par(mfrow = c(3,4), mar = c(1,1,2,1))
for (i in 1:length(child1_networks)) {
  g <- child1_networks[[i]]
  if (!is.null(g)) {
    plot(g,
         vertex.size = 15,
         vertex.label.cex = 0.7,
         vertex.label.color = "black",
         main = paste("Session", i))
  } else {
    plot.new()
    title(main = paste("Session", i))
  }
}
par(mfrow = c(1,1))



#-----------------calculate degree---------------------
# Compute degree for all networks
df$degree <- lapply(df$random_network, function(g) {
  if (is.null(g)) return(NA)
  degree(g)
})

# Filter for child 4214
child1_df <- df[df$child_id == 4214, ]

# Compute summary statistics for each session
child1_df$degree_mean <- sapply(child1_df$degree, function(d) if (is.na(d[1])) NA else mean(d))
child1_df$degree_max  <- sapply(child1_df$degree, function(d) if (is.na(d[1])) NA else max(d))
child1_df$degree_sd   <- sapply(child1_df$degree, function(d) if (is.na(d[1])) NA else sd(d))

child1_df$degree_mean
child1_df$degree_max
child1_df$degree_sd



