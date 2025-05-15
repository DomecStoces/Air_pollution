#PERMANOVA: to test global effects
#How community composition (species assemblages divided into life-history traits) varies with SO₂ and year?
#Are the species identities and abundances different between time periods?

library(vegan)
library(dplyr)
library(tidyr)
library(readxl)

format1 <- format1 %>%
  mutate(Date = as.Date(Date))
format1 <- format1 %>%
  mutate(
    PolicyPeriod = case_when(
      Date < as.Date("1991-10-04") ~ "Pre1991",
      Date >= as.Date("1991-10-04") & Date < as.Date("2002-06-01") ~ "1991_2002",
      Date >= as.Date("2002-06-01") & Date < as.Date("2012-09-01") ~ "2002_2012",
      Date >= as.Date("2012-09-01") ~ "Post2012"
    )
  )

#Create a unique sample ID for grouping variable
format1 <- format1 %>%
  mutate(SampleID = paste(Time.period, Locality, Woody.species, sep = "_"))

#Create species matrix: rows = samples, columns = species, values = total abundance
sp_matrix <- format1 %>%
  group_by(SampleID, Species) %>%
  summarise(Abundance = sum(Number), .groups = "drop") %>%
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
