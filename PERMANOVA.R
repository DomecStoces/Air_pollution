#PERMANOVA: to test global effects
#How community composition (species assemblages divided into life-history traits) varies with SO₂ and year?
#Are the species identities and abundances different between time periods?
#I noticed that I cannot do PERMANOVA for this dataset, because there are no true replicates, only for Birch = Woody.species

library(vegan)
library(dplyr)
library(tidyr)
library(readxl)

#Create a unique sample ID for grouping variable
format1 <- format1 %>%
  mutate(SampleID = paste(Time.period, Locality, Woody.species, sep = "_"))

#Create species matrix: rows = samples, columns = species, values = total abundance
sp_matrix <- format1 %>% group_by(Woody.species, Species)
summarise(Abundance = sum(Number)) %>%
  pivot_wider(names_from = Species, values_from = Abundance, values_fill = 0)
#####
#Convert to matrix
#####

# Convert to base data frame
sp_df <- as.data.frame(sp_matrix)

# Set row names from SampleID
rownames(sp_df) <- sp_df$SampleID

# Remove SampleID column now that it's row names
sp_df$SampleID <- NULL

# Summarize environmental metadata per SampleID
env_data <- format1 %>%
  group_by(SampleID) %>%
  summarise(
    PolicyPeriod = first(PolicyPeriod),
    Immission = mean(Immission, na.rm = TRUE),
    T = mean(T, na.rm = TRUE),
    Precipitation = mean(Precipitation, na.rm = TRUE), Wind=mean(Wind,na.rm=TRUE)
  )

#####
#PERMANOVA
#####
# Bray-Curtis dissimilarity
bray_dist <- vegdist(sp_df, method = "bray")

# PERMANOVA test
adonis_result <- adonis2(bray_dist ~ PolicyPeriod*Immission + T + Precipitation + Wind,
                         data = env_data,
                         permutations = 999,
                         method = "bray")

# View result
print(adonis_result3)

adonis_result2<-adonis2(bray_dist ~ PolicyPeriod*Immission, data = env_data, permutations = 1, method = "bray",by=NULL)

adonis_result3<-adonis2(bray_dist ~ PolicyPeriod+Immission, data = env_data, permutations = 1)


