#PERMANOVA: to test global effects
#How community composition (species assemblages divided into life-history traits) varies with SO₂ and year?
#Are the species identities and abundances different between time periods?
#I noticed that I cannot do PERMANOVA for this dataset, because there are no true replicates, only for Birch = Woody.species

library(vegan)
library(dplyr)
library(tidyr)
library(readxl)

#Birch filtrating
birch_data <- format1 %>% filter(Woody.species == "Birch")

#Create a unique sample ID for grouping variable
birch_data <- birch_data %>%
  mutate(SampleID = paste(Time.period, Locality, Woody.species, sep = "_"))

#Create species matrix: rows = samples, columns = species, values = total abundance
sp_matrix <- birch_data %>%
  group_by(SampleID, Species) %>%
  summarise(Abundance = sum(Number), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = Abundance, values_fill = 0)
#####
#Convert to matrix
#####

# Convert to base data frame
sp_df <- as.data.frame(sp_matrix)
rownames(sp_df) <- sp_df$SampleID
sp_df$SampleID <- NULL

# Set row names from SampleID
rownames(sp_df) <- sp_df$SampleID

# Remove SampleID column now that it's row names
sp_df$SampleID <- NULL

# Summarize environmental metadata per SampleID
env_data <- birch_data %>%
  group_by(SampleID) %>%
  summarise(
    PolicyPeriod = first(PolicyPeriod),
    Immission = mean(Immission, na.rm = TRUE),
    T = mean(T, na.rm = TRUE),
    Precipitation = mean(Precipitation, na.rm = TRUE),
    Wind = mean(Wind, na.rm = TRUE),
    .groups = "drop"
  )
env_data <- as.data.frame(env_data)

#####
#PERMANOVA
#####
# Bray-Curtis dissimilarity
bray_dist <- vegdist(sp_df, method = "bray")

# PERMANOVA test
adonis_result <- adonis2(bray_dist ~ PolicyPeriod * Immission + T + Precipitation + Wind,
                         data = env_data,
                         permutations = 999,
                         method = "bray",
                         by = NULL)

print(adonis_result)

