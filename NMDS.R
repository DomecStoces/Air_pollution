#NMDS based on community matrix of PERMANOVA

library(vegan)
library(ggplot2)
library(dplyr)
library(labdsv)

### Step 1: Run NMDS ###
set.seed(123)
nmds_result <- metaMDS(sp_df, distance = "bray", k = 2, trymax = 5, autotransform = FALSE)

# Check stress
nmds_result$stress

### Step 2: Site Scores and Metadata ###
site_scores <- as.data.frame(scores(nmds_result, display = "sites"))
site_scores$SampleID <- rownames(site_scores)
nmds_plot_data <- left_join(site_scores, env_data, by = "SampleID")

### Step 3: Fit Environmental Variables ###
ef <- envfit(nmds_result, env_data[, c("Immission", "T", "Precipitation", "Wind")], permutations = 999)

# Extract significant vectors
ef_scores <- as.data.frame(scores(ef, "vectors"))
ef_scores$Variable <- rownames(ef_scores)
ef_scores$pval <- ef$vectors$pvals
ef_sig <- ef_scores %>% filter(pval < 0.05)

### Step 4: Indicator Species Analysis ###
indval_result <- indval(sp_df, env_data$PolicyPeriod)
indval_sig <- indval_result$indcls[indval_result$pval <= 0.05, ]
signif_species <- rownames(indval_sig)

# Get NMDS coordinates for species and filter
species_scores <- as.data.frame(scores(nmds_result, display = "species"))
species_scores$Species <- rownames(species_scores)
species_scores_sig <- species_scores %>% filter(Species %in% signif_species)

### Step 5: Plot NMDS Ordination with Environmental Vectors and Indicator Species###
ggplot(nmds_plot_data, aes(x = NMDS1, y = NMDS2, color = PolicyPeriod)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(level = 0.95, type = "t") +
  
  # Environmental vectors
  geom_segment(data = ef_sig,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")), color = "black", inherit.aes = FALSE) +
  geom_text(data = ef_sig,
            aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = Variable),
            color = "black", size = 5, inherit.aes = FALSE) +
  
  # Indicator species labels
  geom_point(data = species_scores_sig, aes(x = NMDS1, y = NMDS2),
             shape = 21, fill = "black", size = 2, inherit.aes = FALSE) +
  geom_text(data = species_scores_sig,
            aes(x = NMDS1, y = NMDS2, label = Species),
            color = "black", size = 4, fontface = "italic", inherit.aes = FALSE, hjust = -0.1) +
  
  theme_minimal(base_size = 14) +
  labs(subtitle = paste("Stress =", round(nmds_result$stress, 3)),
    x = "NMDS1", y = "NMDS2") +
  scale_color_brewer(palette = "Dark2")