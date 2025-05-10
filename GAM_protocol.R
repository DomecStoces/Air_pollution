library(mgcv) #GAM model
library(gratia) #ggplot like visualization of estimated smooths

#Always check concurvity (mgcv::concurvity()), and plot interactions using vis.gam() or gratia::draw()

gam(Abundance ~ s(year) + s(SO2) + ti(year, SO2) + s(WoodySpecies, bs = "re"), 
    data = df, family = nb(), method = "REML")

# Poisson
m_pois <- gam(Abundance ~ s(year) + s(SO2) + ti(year, SO2), 
              data = format1, family = poisson(), method = "REML")

# Negative Binomial (theta estimated automatically)
m_nb <- gam(Abundance ~ s(year) + s(SO2) + ti(year, SO2), 
            data = format1, family = nb(), method = "REML")
