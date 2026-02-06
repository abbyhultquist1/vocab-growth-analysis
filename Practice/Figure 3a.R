library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# ---------------------------
# SEMANTIC DATA
# ---------------------------
df_semantic <- data.frame(
  Group = c("LB", "PLT"),
  
  # Observed values
  clustering_obs = c(.29, .21),
  indegree_obs = c(3.1, 1.9),
  distance_obs = c(1.0, 2.6),
  
  # RAN baselines
  clustering_ran = c(.27, .27),
  indegree_ran = c(2.5, 2.5),
  distance_ran = c(1.5, 1.5),
  
  # ER baselines
  clustering_er = c(.20, .20),
  indegree_er = c(2.0, 2.0),
  distance_er = c(2.5, 2.5),
  
  # Standard errors
  clustering_se = c(.05, .04),
  indegree_se = c(.3, .2),
  distance_se = c(.1, .3)
)

# ---------------------------
# PHONOLOGIC DATA
# Based on your anticipated results:
# LB: not different from random (small deviations)
# PLT: significant small-world properties (higher clustering, higher in-degree, smaller distance)
# ---------------------------
df_phonologic <- data.frame(
  Group = c("LB", "PLT"),
  
  # Observed values - LB similar to random, PLT showing structure
  clustering_obs = c(.21, .31),
  indegree_obs = c(2.6, 3.3),
  distance_obs = c(2.0, 1.1),
  
  # RAN baselines
  clustering_ran = c(.27, .27),
  indegree_ran = c(2.5, 2.5),
  distance_ran = c(1.8, 1.8),
  
  # ER baselines
  clustering_er = c(.20, .20),
  indegree_er = c(2.0, 2.0),
  distance_er = c(2.5, 2.5),
  
  # Standard errors
  clustering_se = c(.05, .04),
  indegree_se = c(.3, .2),
  distance_se = c(.1, .3)
)

# ---------------------------
# Function to process data
# ---------------------------
process_network_data <- function(df) {
  df_long <- df %>%
    pivot_longer(
      cols = c(clustering_ran, clustering_er, indegree_ran, indegree_er, distance_ran, distance_er),
      names_to = c("metric", "baseline"),
      names_pattern = "(.*)_(.*)"
    ) %>%
    pivot_wider(names_from = metric, values_from = value)
  
  df_long <- df_long %>%
    mutate(baseline = factor(baseline, levels = c("ran", "er")))
  
  df_long <- df_long %>%
    mutate(
      clustering_dev = clustering_obs - clustering,
      indegree_dev   = indegree_obs - indegree,
      distance_dev   = distance_obs - distance 
    )
  
  df_long <- df_long %>%
    mutate(
      clustering_z = clustering_dev / clustering_se,
      indegree_z = indegree_dev / indegree_se,
      distance_z = distance_dev / distance_se,
      
      clustering_sig = case_when(
        abs(clustering_z) > 2.576 ~ "***",
        abs(clustering_z) > 1.96 ~ "**",
        abs(clustering_z) > 1.645 ~ "*",
        TRUE ~ ""
      ),
      indegree_sig = case_when(
        abs(indegree_z) > 2.576 ~ "***",
        abs(indegree_z) > 1.96 ~ "**",
        abs(indegree_z) > 1.645 ~ "*",
        TRUE ~ ""
      ),
      distance_sig = case_when(
        abs(distance_z) > 2.576 ~ "***",
        abs(distance_z) > 1.96 ~ "**",
        abs(distance_z) > 1.645 ~ "*",
        TRUE ~ ""
      )
    )
  
  return(df_long)
}

# Process both datasets
df_semantic_long <- process_network_data(df_semantic)
df_phonologic_long <- process_network_data(df_phonologic)

# ---------------------------
# Helper plotting function
# ---------------------------
apa_dev_plot_baselines <- function(df, metric_dev, se_metric, sig_col, ylab, title) {
  ggplot(df, aes(x = Group, y = .data[[metric_dev]], fill = baseline)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), 
             color = "black", width = 0.6) +
    geom_errorbar(aes(ymin = .data[[metric_dev]] - .data[[se_metric]],
                      ymax = .data[[metric_dev]] + .data[[se_metric]]),
                  position = position_dodge(width = 0.7), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_text(aes(label = paste0(round(.data[[metric_dev]], 2), .data[[sig_col]]),
                  y = .data[[metric_dev]] + sign(.data[[metric_dev]]) * max(.data[[se_metric]]) * 1.5),
              position = position_dodge(width = 0.7), size = 4) +
    scale_fill_manual(values = c("ran" = "#6B9AC4", "er" = "#E57373"), 
                      labels = c("ran" = "RAN", "er" = "ER"),
                      name = "Rand. Network Type") +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.title.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray85", linewidth = 0.3),
      legend.position = "right"
    ) +
    labs(y = ylab, title = title)
}

# ---------------------------
# SEMANTIC PLOTS
# ---------------------------
p1_sem <- apa_dev_plot_baselines(df_semantic_long, "clustering_dev", "clustering_se", "clustering_sig",
                                 "Deviation from Random", "Semantic: Clustering")
p2_sem <- apa_dev_plot_baselines(df_semantic_long, "indegree_dev", "indegree_se", "indegree_sig",
                                 "Deviation from Random", "Semantic: In-degree")
p3_sem <- apa_dev_plot_baselines(df_semantic_long, "distance_dev", "distance_se", "distance_sig",
                                 "Deviation from Random", "Semantic: Geodesic Distance")

# ---------------------------
# PHONOLOGIC PLOTS
# ---------------------------
p1_phon <- apa_dev_plot_baselines(df_phonologic_long, "clustering_dev", "clustering_se", "clustering_sig",
                                  "Deviation from Random", "Phonologic: Clustering")
p2_phon <- apa_dev_plot_baselines(df_phonologic_long, "indegree_dev", "indegree_se", "indegree_sig",
                                  "Deviation from Random", "Phonologic: In-degree")
p3_phon <- apa_dev_plot_baselines(df_phonologic_long, "distance_dev", "distance_se", "distance_sig",
                                  "Deviation from Random", "Phonologic: Geodesic Distance")

# ---------------------------
# Combine into comprehensive figure
# ---------------------------
# Option 1: All in one row (6 panels)
Figure_Combined <- (p1_sem | p2_sem | p3_sem) / (p1_phon | p2_phon | p3_phon) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')

Figure_Combined

# Option 2: Side by side with metrics grouped
# Figure_Alt <- (p1_sem | p1_phon) / (p2_sem | p2_phon) / (p3_sem | p3_phon) + 
#   plot_layout(guides = "collect") +
#   plot_annotation(tag_levels = 'A')
# 
# Figure_Alt

