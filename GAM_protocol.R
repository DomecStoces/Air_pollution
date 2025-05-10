library(mgcv) #GAM model
library(gratia) #ggplot like visualization of estimated smooths
library(DHARMa)
library(mgcViz)

#Set formating of format1 dataset
#####
format1$Woody.species <- as.factor(format1$Woody.species)
format1_clean <- na.omit(format1[, c("Number", "Time.period", "Immission", "Woody.species")])
#Model hint
#####
#Always check concurvity (mgcv::concurvity()), and plot interactions using vis.gam() or gratia::draw()

#Smooth interaction using tensor product smooth
fit1<-gam(Number ~ s(Time.period) + s(Immission) + ti(Time.period, Immission) + s(Woody.species, bs = "re"), 
    data = format1, family = nb(), method = "REML")

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

#Modelling
#####
draw(fit1, select = "ti(Time.period,Immission)")

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
