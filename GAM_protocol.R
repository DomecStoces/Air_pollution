#We have dropped Temperature from data stations at each sampled stands; 
#Wind, T, Precipitation, interaction of Time.period and Immission is used in final model for better fit of AIC and LKT
#Wind has a non-linear and slightly positive effect, with the strongest increase in abundance at higher wind speeds. This suggests wind may facilitate either beetle dispersal or trap performance under certain conditions.
#Does policy changes affected the total number of carabids?

library(mgcv)
library(gratia)
library(DHARMa)
library(mgcViz)
library(ggplot2)
library(dplyr)
library(patchwork)
library(segmented)
library(broom)
library(readxl)
library(tidyr)
#Set formating of format1 dataset
format1 <- read_excel("format1.xlsx")
#####
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
str(format1$PolicyPeriod)
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
#Group-varying smooth model; interaction using tensor product smooth, but not to overpredict abundance (i.e., overfitting or extrapolating)
fit1 <- gam(Number ~ s(Time.period, by = PolicyPeriod,bs="tp",k=10) + 
              s(Wind, k = 12) + s(T, k = 8) + s(Precipitation, k = 8) + 
              ti(Time.period, Immission, k = c(10, 8)) + 
              s(Woody.species, bs = "re"), 
            data = format1, family = nb(), method = "REML")

fit1 <- gam(Number ~ s(Time.period, by = PolicyPeriod) + 
              s(Wind) + s(T) + s(Precipitation) + 
              ti(Time.period, Immission) + 
              s(Woody.species, bs = "re"), 
            data = format1, family = nb(), method = "REML")
# 4.3% are explained by the model with n=16 213
summary(fit1)

fig2<-draw(fit1)

tiff("Plot_smooths.tiff", 
     width = 15, height = 10,     
     units = "in",                  
     res = 600,                     
     compression = "lzw")           
fig2
dev.off()


tidy(fit1, parametric = TRUE)   # for parametric terms
tidy(fit1, parametric = FALSE)  # for smooth terms
#Check concurvity
concurvity(fit1)
#Residual diagnosis k-index >= 1 (setting for model); basis dimensions were checked and tuned.
gam.check(fit1)
#Check AIC for complexity and penalty for lower parsimony
AIC(fit1)
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

#####
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

#Plotting the residuals
#####
#only residuals ≥ 5 are shown in points
#the GAM smooth still uses the full dataset (format1) for context
fig1 <- ggplot(format1 %>% filter(abs(Residual) >= 5), aes(x = Date, y = Immission)) +
  geom_point(aes(size = abs(Residual)), shape = 21, fill = "gray70", colour = "black", alpha = 0.7) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              colour = "black", fill = "gray50", alpha = 0.4,
              linewidth = 1, se = TRUE) +
  scale_size_continuous(
    range = c(0.1, 6),
    name = "Abs(Residual)",
    breaks = c(5, 10, 25, 50, 75)
  ) +
  geom_vline(xintercept = policy1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy2, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy3, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = policy1, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Start of air policy (04/10/1991)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy2, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Regulation update (01/06/2002)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy3, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Air protection act (01/09/2012)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  scale_y_continuous(
    limits = c(0, 150),
    breaks = seq(0, 150, by = 20),
    expand = expansion(add = c(5, 0))
  ) +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    expand = expansion(add = c(250, 0))
  ) +
  theme_minimal(base_size = 15, "Calibri") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(5, "pt")) +
  labs(
    x = "Year",
    y = "SO₂ pollution [μg·m⁻³]")
#####
#Plotting the predicted values with threshold >2
#####
format1$Fitted <- fitted(fit1, type = "response")
threshold <- 2

filtered_data <- format1 %>% filter(Fitted >= threshold)

fig1 <- ggplot(filtered_data, aes(x = Date, y = Immission)) +
  geom_point(aes(size = Fitted), shape = 21, fill = "gray70", colour = "black", alpha = 0.7) +
  geom_smooth(data = format1, aes(x = Date, y = Immission),
              method = "gam", formula = y ~ s(x, bs = "cs"),
              colour = "black", fill = "gray50", alpha = 0.4,
              linewidth = 1, se = TRUE) +
  scale_size_continuous(
    range = c(0.1, 5),
    name = "Predicted abundance",
    breaks = c(3,4,5)
  ) +
  geom_vline(xintercept = policy1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy2, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = policy3, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = policy1, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Start of air policy (04/10/1991)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy2, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Regulation update (01/06/2002)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  annotate("text", x = policy3, hjust = 1, y = max(format1$Immission, na.rm = TRUE) - 5,
           label = "Air protection act (01/09/2012)", angle = 90, vjust = -0.5, size = 4, color = "black") +
  scale_y_continuous(
    limits = c(0, 150),
    breaks = seq(0, 150, by = 20),
    expand = expansion(add = c(5, 0))
  ) +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    expand = expansion(add = c(250, 0))
  ) +
  theme_minimal(base_size = 15, base_family = "Calibri") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(5, "pt")) +
  labs(
    x = "Year",
    y = "SO₂ pollution [μg·m⁻³]")
fig1

breaks = pretty(range(filtered_data$Fitted, na.rm = TRUE), n = 5
#####
# Save the plot as a TIFF file
#####
tiff("SO2_pollution_predictions.tiff", 
     width = 15, height = 10,     
     units = "in",                  
     res = 600,                     
     compression = "lzw")           
fig1
dev.off()

#What the graph shows:
Multiscale ecological inference towards SO2 polluttion over 26 years: view of carabids
We used a generalized additive model (GAM) with a negative binomial error distribution to model changes in carabid abundance over time. To test for temporal patterns specific to air quality policy periods, we included a varying smooth for Time.period by PolicyPeriod using s(Time.period, by = PolicyPeriod). Additional smooth terms accounted for nonlinear effects of temperature, precipitation, and wind, along with a tensor interaction between Time.period and SO₂ immission levels. Random variation among woody species was controlled with a random effect term (s(Woody.species, bs = "re")). Model fitting was performed using restricted maximum likelihood (REML).
How does carabid activity change under declining SO₂ levels across policy periods?
#####
Tests how individual carabid abundance (response) is influenced by:
  
  Time trends per policy period (i.e. segmented temporal effects)

SO₂ × time interaction

Microclimatic variables (Wind, Temp, Precip)

Random effects of tree species

> summary(fit1)

Family: Negative Binomial(1.609) 
Link function: log 

Formula:
  Number ~ s(Time.period, by = PolicyPeriod, k = 10) + s(Wind, 
                                                         k = 12) + s(T, k = 8) + s(Precipitation, k = 8) + ti(Time.period, 
                                                                                                              Immission, k = c(10, 8)) + s(Woody.species, bs = "re")

Parametric coefficients:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)   0.6333     0.6489   0.976    0.329

Approximate significance of smooth terms:
  edf Ref.df  Chi.sq  p-value    
s(Time.period):PolicyPeriod1991_2002  4.205  4.673  15.848  0.00319 ** 
  s(Time.period):PolicyPeriod2002_2012  6.007  6.395  84.731  < 2e-16 ***
  s(Time.period):PolicyPeriodPost2012   1.928  2.067   5.178  0.07344 .  
s(Time.period):PolicyPeriodPre1991    1.773  1.885   3.983  0.09790 .  
s(Wind)                               9.824 10.494 114.475  < 2e-16 ***
  s(T)                                  6.019  6.629 378.776  < 2e-16 ***
  s(Precipitation)                      6.164  6.729  60.512  < 2e-16 ***
  ti(Time.period,Immission)            27.524 33.559 103.993  < 2e-16 ***
  s(Woody.species)                      2.539  3.000  22.057 9.94e-06 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.0438   Deviance explained = 9.42%
-REML =  37111  Scale est. = 1         n = 16213

