#PERMANOVA: to test global effects
#How community composition (species assemblages) varies with SO₂ and year?

library(vegan)
library(dplyr)

#Create a unique sample ID for grouping variable
format1 <- format1 %>%
  mutate(SampleID = paste(Date, Locality, Woody.species, sep = "_"))

#Create species matrix: rows = samples, columns = species, values = total abundance
sp_matrix <- format1 %>%
  group_by(SampleID, Species) %>%
  summarise(Abundance = sum(Number), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = Abundance, values_fill = 0)
#####
#Convert to matrix
#####
#Save row names
rownames(sp_matrix) <- sp_matrix$SampleID
sp_matrix <- sp_matrix[, -1]  # remove SampleID column

# Reuse the same SampleID structure
env_data <- format1 %>%
  distinct(SampleID, .keep_all = TRUE) %>%
  select(SampleID, PolicyPeriod, Immission, T, Precipitation)

# Ensure matching row order with species matrix
env_data <- env_data[match(rownames(sp_matrix), env_data$SampleID), ]
rownames(env_data) <- env_data$SampleID
env_data <- env_data[, -1]  # remove SampleID column
#####
#PERMANOVA
#####
# Bray-Curtis dissimilarity
bray_dist <- vegdist(sp_matrix, method = "bray")

# PERMANOVA test
adonis_result <- adonis2(bray_dist ~ PolicyPeriod + Immission + T + Precipitation,
                         data = env_data,
                         permutations = 999,
                         method = "bray")

# View result
print(adonis_result)