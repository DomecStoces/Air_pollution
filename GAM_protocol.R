#We have dropped Temperature from data stations at each sampled stands; 
#Wind, T, Precipitation, interaction of Time.period and Immission is used in final model for better fit of AIC and LKT
#Wind has a non-linear and slightly positive effect, with the strongest increase in abundance at higher wind speeds. This suggests wind may facilitate either beetle dispersal or trap performance under certain conditions.

library(mgcv) #GAM model
library(gratia) #ggplot like visualization of estimated smooths
library(DHARMa)
library(mgcViz)
library(ggplot2)
library(dplyr)
library(patchwork)

#Set formating of format1 dataset
#####
format1$Woody.species <- as.factor(format1$Woody.species)
#Model hint
#####
#Always check concurvity (mgcv::concurvity()), and plot interactions using vis.gam() or gratia::draw()

#####
#Check for better model fit of family()
#####
# Poisson model
m_pois <- gam(Number ~ s(Time.period) + s(Immission) + ti(Time.period, Immission) + 
                s(Woody.species, bs = "re"),
              data = format1, family = poisson(), method = "REML")

# Negative Binomial model
m_nb <- gam(Number ~ s(Time.period) + s(Immission) + ti(Time.period, Immission) + 
              s(Woody.species, bs = "re"),
            data = format1, family = nb(), method = "REML")

AIC(m_pois, m_nb)

# Pearson residuals and dispersion for Poisson
disp_pois <- sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)

# Pearson residuals and dispersion for NB
disp_nb <- sum(residuals(m_nb, type = "pearson")^2) / df.residual(m_nb)

disp_pois
disp_nb
#####
#Modelling
#Final model fit
#####
#Smooth interaction using tensor product smooth, but not to overpredict abundance (i.e., overfitting or extrapolating)
fit1<-gam(Number ~ s(Time.period, k = 10) + s(Wind,k=12)+ s(T, k = 8) + s(Precipitation, k = 8) + ti(Time.period, Immission, k = c(10, 8)) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "REML")
# 4.3% are explained by the model with n=16 213
#Check concurvity
concurvity(fit1)
#Residual diagnosis k-index >= 1 (setting for model); basis dimensions were checked and tuned.
gam.check(fit7)

#multicollinearity => The alignment of low SO₂ and higher precipitation around that time is real, not statistical redundancy.
cor(format1[, c("Immission", "T", "Precipitation", "Wind")], use = "complete.obs")
#Visualization
draw(fit1, select = "ti(Time.period,Immission)", 
     contour = TRUE) +
  ggtitle("Tensor product smooth: Time × Immission") +
  xlab("Time.period") + ylab("Immission")

vis.gam(fit1,
        view = c("Time.period", "Immission"),
        plot.type = "contour",
        color = "heat",
        too.far = 0.05,
        main = "Time period",
        xlab = "Time period",
        ylab = "Immission")

vis.gam(fit1,
        view = c("Time.period", "Immission"),
        plot.type = "persp",
        color = "topo",
        theta = 40,
        phi = 30,
        ticktype = "detailed",
        main = "Interaction: Time × SO₂ Immission")



#Fit Time.period as linear predictor
#####
fit_linear_time <- gam(Number ~ Time.period + 
                         s(T) + 
                         s(Precipitation) + 
                         ti(Time.period, Immission) + 
                         s(Woody.species, bs = "re"),
                       data = format1, family = nb(), method = "ML")
#####
#Different model options of 'Immission' term:
#####
fit2<-gam(Number ~ s(Time.period) + s(T) + s(Precipitation) + s(Immission)+ ti(Time.period, Immission) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "ML")

fit3<-gam(Number ~ s(Time.period) + s(T) + s(Precipitation) + ti(Time.period, Immission) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "ML")

fit4<-gam(Number ~ s(Time.period) + ti(Time.period, Immission) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "ML")

fit5<-gam(Number ~ s(T) + s(Precipitation) + s(Immission) + ti(Time.period, Immission) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "ML")
AIC(fit2,fit7)
anova(fit2, fit7, test = "Chisq")
#it is better to stick with 'Immission' as interaction term only
#it is better to use model with the Temperature and Precipitation; and Immission in interaction term with Time period
#there is an independent temporal trend in data (e.g., seasonal or long-term effects)
#that trend is further modulated by Immission, captured by the interaction.
#Time.period does not behave as linear predictor


#Visualization of model in ggplot2
#####
format1$Predicted <- predict(fit1, type = "response")
format1$Residual <- format1$Number - format1$Predicted
format1 <- format1 %>%
  mutate(Date = as.Date("1989-04-15") + Time.period - 1)

# Policy change dates
#903
policy1 <- as.numeric(as.Date("1991-10-04") - as.Date("1989-04-15")) + 1
#4785
policy2 <- as.numeric(as.Date("2002-06-01") - as.Date("1989-04-15")) + 1
policy3 <- as.numeric(as.Date("2012-09-01") - as.Date("1989-04-15")) + 1

policy1 <- as.Date("1991-10-04")
policy2 <- as.Date("2002-06-01")
policy3 <- as.Date("2012-09-01")

#only residuals ≥ 5 are shown in points
#the GAM smooth still uses the full dataset (format1) for context
fig1 <- ggplot(format1 %>% filter(abs(Residual) >= 5), aes(x = Date, y = Immission)) +
  geom_point(aes(size = abs(Residual)),
             shape = 21, fill = "gray70", colour = "black", alpha = 0.7) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              colour = "black", fill = "gray50", alpha = 0.4,
              linewidth = 1, se = TRUE) +
  scale_size_continuous(
    range = c(0.1, 6),
    name = "Abs(Residual)",
    breaks = c(5, 10, 25, 50, 75)
  ) +
  geom_vline(xintercept = policy1, linetype = "dashed", color = "darkred", linewidth = 0.8) +
  geom_vline(xintercept = policy2, linetype = "dashed", color = "darkred", linewidth = 0.8) +
  geom_vline(xintercept = policy3, linetype = "dashed", color = "darkred", linewidth = 0.8) +
  annotate("text", x = policy1, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Start of air policy (04/10/1991)", angle = 90, vjust = -0.5, size = 4, color = "darkred") +
  annotate("text", x = policy2, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Regulation update (01/06/2002)", angle = 90, vjust = -0.5, size = 4, color = "darkred") +
  annotate("text", x = policy3, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Air protection act (01/09/2012)", angle = 90, vjust = -0.5, size = 4, color = "darkred") +
  scale_x_date(
    date_breaks = "4 years",
    date_labels = "%Y",
    limits = as.Date(c("1989-04-15", "2015-12-31"))
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(5, "pt")
  ) +
  labs(
    title = "Residuals of GAM: observed - predicted with pollution trend and policy changes",
    x = "Year",
    y = "SO₂ pollution [μg·m⁻³]"
  )

fig1

tiff("SO2_pollution.tiff", 
     width = 25, height = 10,     
     units = "in",                  
     res = 500,                     
     compression = "lzw")           
fig1
dev.off()

#What the graph shows:
The vertical axis (y-axis) shows the level of SO₂ pollution over time.

The horizontal axis (x-axis) shows the progression of time starting from April 15, 1989.

Each point represents a measurement. The bigger the point, the larger the difference between what the model predicted and what was actually measured (these differences are called residuals).

The black curve shows the overall trend in SO₂ pollution estimated by your model, with a light gray area around it showing the model’s uncertainty.

The three vertical red lines mark important policy changes:
  
In 1991: a policy change began.

In 2002: pollution regulations were updated.

In 2012: a new air protection act came into force.

🟡 What it means:
  SO₂ pollution clearly decreased over the 26-year period.

The drop was strongest in the early years, especially before 2002.

After 2002, pollution levels leveled off and stayed low.

The largest modeling errors (biggest points) happened before 2000, suggesting:
  
  Either SO₂ levels were harder to predict in the early years,

Or that the model fits the later years much better.

After the 2002 and 2012 policy changes, both pollution and residuals stayed low, which supports the idea that the regulations were effective.

🟠 In summary:
  Your model shows a clear improvement in air quality over time.

The biggest deviations between predicted and observed values happened early in the timeline.

The pattern matches well with the timing of policy interventions, suggesting they likely had a positive impact.
