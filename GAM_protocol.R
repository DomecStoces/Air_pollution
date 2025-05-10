library(mgcv) #GAM model
library(gratia) #ggplot like visualization of estimated smooths

#Always check concurvity (mgcv::concurvity()), and plot interactions using vis.gam() or gratia::draw()

gam(Abundance ~ s(year) + s(SO2) + ti(year, SO2) + s(WoodySpecies, bs = "re"), 
    data = df, family = nb(), method = "REML")
