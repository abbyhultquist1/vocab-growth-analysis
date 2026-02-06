library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# ---------------------------
# Restructured data with separate baselines for each group
# ---------------------------
df <- data.frame(
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
# Reshape data to long format
# ---------------------------
df_long <- df %>%
  pivot_longer(
    cols = c(clustering_ran, clustering_er, indegree_ran, indegree_er, distance_ran, distance_er),
    names_to = c("metric", "baseline"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  pivot_wider(names_from = metric, values_from = value)

# ---------------------------
# SET FACTOR ORDER HERE - RAN first, then ER
# ---------------------------
df_long <- df_long %>%
  mutate(baseline = factor(baseline, levels = c("ran", "er")))

# ---------------------------
# Compute deviation from baseline
# ---------------------------
df_long <- df_long %>%
  mutate(
    clustering_dev = clustering_obs - clustering,
    indegree_dev   = indegree_obs - indegree,
    distance_dev   = distance_obs - distance 
  )

# ---------------------------
# Add significance indicators for ALL comparisons
# ---------------------------
df_long <- df_long %>%
  mutate(
    # Calculate z-scores for each metric
    clustering_z = clustering_dev / clustering_se,
    indegree_z = indegree_dev / indegree_se,
    distance_z = distance_dev / distance_se,
    
    # Determine significance stars (assuming two-tailed test)
    clustering_sig = case_when(
      abs(clustering_z) > 2.576 ~ "***",  # p < .01
      abs(clustering_z) > 1.96 ~ "**",    # p < .05
      abs(clustering_z) > 1.645 ~ "*",    # p < .10
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
# Create plots for each metric
# ---------------------------
p1 <- apa_dev_plot_baselines(df_long, "clustering_dev", "clustering_se", "clustering_sig",
                             "Mean Clustering above Random", "Clustering")
p2 <- apa_dev_plot_baselines(df_long, "indegree_dev", "indegree_se", "indegree_sig",
                             "Mean In-Degree above Random", "In-degree")
p3 <- apa_dev_plot_baselines(df_long, "distance_dev", "distance_se", "distance_sig",
                             "Mean Geodesic Dist above Random", "Geodesic Distance")

# ---------------------------
# Combine into Figure 1 A–C
# ---------------------------
Figure1 <- p1 + p2 + p3 + plot_layout(ncol = 3, guides = "collect")
Figure1

