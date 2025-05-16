library(FD)          
library(lme4)        
library(ggplot2)
library(dplyr)
library(lmerTest)
library(broom.mixed)

### Step 1: CWM Calculation ###
cwm_result <- functcomp(traits_df, sp_df, CWM.type = "all")
cwm_df <- as.data.frame(cwm_result)
cwm_df$SampleID <- rownames(sp_df)

### Step 2: Merge with environmental data ###
env_merged <- left_join(cwm_df, env_data, by = "SampleID")

### Step 3: Fit LMER Models for each trait ###
mod1 <- lmer(CWM ~ Immission + T + Precipitation + (1 | Woody.species), data = env_merged)
summary(mod1)
# Loop method
traits <- colnames(cwm_result)  # or select manually
model_list <- list()
summary_list <- list()

for (trait in traits) {
  formula <- as.formula(paste(trait, "~ T + Precipitation + (1|Woody.species)"))
  model <- lmer(formula, data = env_merged)
  model_list[[trait]] <- model
  summary_list[[trait]] <- summary(model)
}
### Step 4: Create Table of Results ###
results_table <- purrr::map_df(model_list, ~tidy(.x), .id = "Trait")
results_table <- results_table %>%
  filter(effect == "fixed") %>%
  select(Trait, term, estimate, std.error, statistic, p.value)

### Step 5: Graphing Predicted CWM ~ Environment ###
ggplot(env_merged, aes(x = T, y = Trait1)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Temperature", y = "CWM Trait1", title = "CWM ~ Temperature")

# Multiple traits
env_merged_long <- env_merged %>%
  pivot_longer(cols = all_of(traits), names_to = "Trait", values_to = "CWM")

ggplot(env_merged_long, aes(x = T, y = CWM)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Trait, scales = "free_y") +
  theme_bw()

