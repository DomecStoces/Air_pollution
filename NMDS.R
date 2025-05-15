#NMDS based on community matrix of PERMANOVA

library(vegan)
library(ggplot2)
library(dplyr)

# Ensure sample names match between matrix and metadata
all(rownames(sp_df) %in% env_data$SampleID)  # Should return TRUE

# Set seed for reproducibility
set.seed(123)

# Run NMDS with Bray-Curtis distance
nmds_result <- metaMDS(sp_df, distance = "bray", k = 2, trymax = 5, autotransform = FALSE)

# Check stress
nmds_result$stress  # Ideally < 0.2

# Get site scores (coordinates in 2D space)
site_scores <- as.data.frame(scores(nmds_result, display = "sites"))

# Add SampleID as a column
site_scores$SampleID <- rownames(site_scores)

# Join with environmental metadata
nmds_plot_data <- left_join(site_scores, env_data, by = "SampleID")

ef <- envfit(nmds_result, env_data[, c("Immission", "T", "Precipitation", "Wind")], permutations = 999)

#####
#Environmental factors fitted in NMDS
#####
# Extract scores (vectors)
ef_scores <- as.data.frame(scores(ef, "vectors"))
ef_scores$Variable <- rownames(ef_scores)

# Optional: filter only significant variables (p < 0.05)
ef_pvals <- as.data.frame(ef$vectors$pvals)
ef_scores$pval <- ef_pvals[, 1]

# Keep only significant (e.g., p < 0.05)
ef_sig <- ef_scores %>% filter(pval < 0.05)
#####

#Plotting NMDS Ordination (Bray-Curtis) of set PolicyPeriod
ggplot(nmds_plot_data, aes(x = NMDS1, y = NMDS2, color = PolicyPeriod)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(level = 0.95, type = "t") +
  geom_segment(data = ef_sig,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")), color = "black", inherit.aes = FALSE) +
  geom_text(data = ef_sig,
            aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = Variable),
            color = "black", size = 5, inherit.aes = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    subtitle = paste("Stress =", round(nmds_result$stress, 3)),
    x = "NMDS1", y = "NMDS2"
  ) +
  scale_color_brewer(palette = "Dark2")