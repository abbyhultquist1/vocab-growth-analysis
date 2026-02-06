# ---------------------------
# Load libraries
# ---------------------------
library(ggplot2)
library(dplyr)
library(patchwork)

# ---------------------------
# Example data (replace with your values)
# ---------------------------
df <- data.frame(
  group = c("LB", "PLT", "RAN", "ER"),
  clustering = c(.52, .22, .39, .23),
  clustering_se = c(.05, .04, .03, .03),
  indegree = c(4.2, 1.7, 2.6, 1.8),
  indegree_se = c(.3, .2, .25, .2),
  distance = c(1.1, 3.8, 2.6, 4.1),
  distance_se = c(.1, .3, .2, .2)
)

# Ensure factor levels are ordered
df$group <- factor(df$group, levels = c("LB", "PLT", "RAN", "ER"))

# ---------------------------
# APA-style plotting function with error bars
# ---------------------------
apa_plot <- function(df, metric, se_metric, ylab, title) {
  ggplot(df, aes(x = group, y = .data[[metric]])) +
    geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.7) +
    geom_errorbar(aes(ymin = .data[[metric]] - .data[[se_metric]],
                      ymax = .data[[metric]] + .data[[se_metric]]),
                  width = 0.2, color = "black") +
    # Move labels above error bars
    geom_text(aes(label = round(.data[[metric]], 2),
                  y = .data[[metric]] + .data[[se_metric]] * 1.8),
              size = 4) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      axis.title.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray85", linewidth = 0.3)
    ) +
    labs(y = ylab, title = title) +
    ylim(0, max(df[[metric]] + df[[se_metric]]) * 1.35)
}

# ---------------------------
# Create the three panels
# ---------------------------
p1 <- apa_plot(df, "clustering", "clustering_se",
               ylab = "Clustering Coefficient",
               title = "Lexical Network Clustering")

p2 <- apa_plot(df, "indegree", "indegree_se",
               ylab = "Mean In-Degree",
               title = "Network Connectivity")

p3 <- apa_plot(df, "distance", "distance_se",
               ylab = "Average Geodesic Distance",
               title = "Average Path Length")

# ---------------------------
# Combine into Figure 1 A–C horizontally
# ---------------------------
Figure1 <- p1 + p2 + p3 + plot_layout(ncol = 3)

# Display the figure
Figure1

