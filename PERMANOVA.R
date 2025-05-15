#PERMANOVA: to test global effects
#How community composition (species assemblages divided into life-history traits) varies with SO₂ and year?
#Are the species identities and abundances different between time periods?

library(vegan)
library(dplyr)
library(tidyr)
library(readxl)

#Set formating of format1 dataset
#####
format1 <- read_excel("format1.xlsx")
format1 <- format1 %>%
  mutate(Date = as.Date(Date))

format1 <- format1 %>%
  mutate(
    PolicyPeriod = case_when(
      Date < as.Date("1991-10-04") ~ "Pre1991",
      Date >= as.Date("1991-10-04") & Date < as.Date("2002-06-01") ~ "1991_2002",
      Date >= as.Date("2002-06-01") & Date < as.Date("2012-09-01") ~ "2002_2012",
      Date >= as.Date("2012-09-01") ~ "Post2012"
    ),
    PolicyPeriod = factor(PolicyPeriod)
  )

format1$PolicyPeriod <- as.factor(format1$PolicyPeriod)
format1$Date <- as.factor(format1$Date)
format1$Woody.species <- as.factor(format1$Woody.species)

str(format1$Date)
#####
#Create a unique sample ID for grouping variable
format1 <- format1 %>%
  mutate(SampleID = paste(Time.period, Woody.species, sep = "_"))

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
    PolicyPeriod = first(PolicyPeriod), Time.period = first(Time.period),
    Woody.species = first(Woody.species),
    Immission = mean(Immission, na.rm = TRUE),
    T = mean(T, na.rm = TRUE),
    Precipitation = mean(Precipitation, na.rm = TRUE),
    Wind = mean(Wind, na.rm = TRUE)
  )

env_data$Woody.species <- as.factor(env_data$Woody.species)
#####
#PERMANOVA
#####
# Bray-Curtis dissimilarity calculation
bray_dist <- vegdist(sp_df, method = "bray")
#PERMDISP
dispersion <- betadisper(bray_dist, env_data$PolicyPeriod)
permutest(dispersion)
plot(dispersion)
boxplot(dispersion)
#differences in group dispersion exist?
pairwise.perm.test <- TukeyHSD(dispersion)
print(pairwise.perm.test)
#A test for homogeneity of multivariate dispersion (PERMDISP) was significant (p = 0.001), indicating slight differences in group dispersion. Visual inspection, however, showed only moderate variation, suggesting group differences in the PERMANOVA are likely driven by both location and spread effects.
#Although PERMDISP was significant (p = 0.001), visual inspection indicated only moderate variation in dispersion. Therefore, differences in community composition among PolicyPeriods are likely influenced by both location (centroid shifts) and group spread.
# PERMANOVA test
adonis_result2<-adonis2(bray_dist ~ Immission + PolicyPeriod, data = env_data, permutations = 999, method = "bray",by="margin",strata=env_data$Woody.species)
#PolicyPeriod and Immission together explain XXX% of the total variation in community structure.
adonis_result1<-adonis2(bray_dist ~ Immission + PolicyPeriod + Time.period + T + Precipitation + Wind, data = env_data, permutations = 999, method = "bray",by="margin",strata=env_data$Woody.species)
#After accounting for climatic and temporal variation, PolicyPeriod and Immission still significantly explain variation in community composition (R² = ...).
print(adonis_result2)
#Calculation for adjusted R² for vegan::adonis2() 
adjusted_R2 <- function(R2, n, m) {
  1 - ((1 - R2) * (n - 1) / (n - m - 1))
}

adjusted_R2(R2 = 0.00454, n = 2608, m = 2)

Permutation test for adonis under reduced model
Marginal effects of terms
Blocks:  strata 
Permutation: free
Number of permutations: 999

adonis2(formula = bray_dist ~ Immission + PolicyPeriod + Time.period + T + Precipitation + Wind, data = env_data, permutations = 999, method = "bray", by = "margin", strata = env_data$Woody.species)
                  Df SumOfSqs R2       F     Pr(>F)    
Immission        1     1.56 0.00174  4.9285  0.001 ***
PolicyPeriod     3     8.64 0.00961  9.0841  0.001 ***
Time.period      1     2.40 0.00267  7.5763  0.001 ***
T                1    21.18 0.02356 66.7903  0.001 ***
Precipitation    1     3.92 0.00436 12.3604  0.001 ***
Wind             1     3.02 0.00336  9.5345  0.001 ***
Residual      2599   824.00 0.91677                   
Total         2607   898.82 1.00000

adonis2(formula = bray_dist ~ Immission + PolicyPeriod, data = env_data, permutations = 999, method = "bray", by = "margin", strata = env_data$Woody.species)
                Df SumOfSqs R2      F       Pr(>F)    
Immission       1     3.59 0.00400 10.739  0.001 ***
PolicyPeriod    3    17.36 0.01932 17.301  0.001 ***
Residual     2603   870.75 0.96877                  
Total        2607   898.82 1.00000 
