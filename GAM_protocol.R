library(mgcv) #GAM model
library(gratia) #ggplot like visualization of estimated smooths
library(DHARMa)
library(mgcViz)

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
#Smooth interaction using tensor product smooth
fit1<-gam(Number ~ s(Time.period, k = 12) + s(T, k = 13) + s(Precipitation, k = 15) + ti(Time.period, Immission, k = c(20, 15)) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "REML")
#Check concurvity
concurvity(fit1)
#Residual diagnosis k-index >= 1 (setting for model); basis dimensions were checked and tuned.
gam.check(fit1)
#Visualization
plot(fit1, select = 4, scheme = 2, 
     shade = TRUE, 
     main = "Interaction: Time.period × Immission", 
     xlab = "Time.period", 
     ylab = "Immission")

draw(fit1, select = "ti(Time.period,Immission)", 
     contour = TRUE) +
  ggtitle("Tensor product smooth: Time × Immission") +
  xlab("Time.period") + ylab("Immission")

vis.gam(fit1,
        view = c("Time.period", "Immission"),
        plot.type = "contour",
        color = "terrain",
        too.far = 0.05,
        main = "Interaction: Time × SO₂ Immission",
        xlab = "Time period",
        ylab = "SO₂ immission")

vis.gam(fit1,
        view = c("Time.period", "Immission"),
        plot.type = "persp",
        color = "topo",
        theta = 40,
        phi = 30,
        ticktype = "detailed",
        main = "Interaction: Time × SO₂ Immission")
#####
fit_linear_time <- gam(Number ~ Time.period + 
                         s(T) + 
                         s(Precipitation) + 
                         ti(Time.period, Immission) + 
                         s(Woody.species, bs = "re"),
                       data = format1, family = nb(), method = "ML")
#Different model options of 'Immission' term:
#####
fit2<-gam(Number ~ s(Time.period) + s(T) + s(Precipitation) + s(Immission)+ ti(Time.period, Immission) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "ML")

fit3<-gam(Number ~ s(Time.period) + s(T) + s(Precipitation) + ti(Time.period, Immission) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "ML")

fit4<-gam(Number ~ s(Time.period) + ti(Time.period, Immission) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "ML")

fit5<-gam(Number ~ s(T) + s(Precipitation) + s(Immission) + ti(Time.period, Immission) + s(Woody.species, bs = "re"), data = format1, family = nb(), method = "ML")
AIC(fit2,fit5)
anova(fit2, fit5, test = "Chisq")
#it is better to stick with 'Immission' as interaction term only
#it is better to use model with the Temperature and Precipitation; and Immission in interaction term with Time period
#there is an independent temporal trend in data (e.g., seasonal or long-term effects)
#that trend is further modulated by Immission, captured by the interaction.
