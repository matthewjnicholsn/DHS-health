# R
# 1) aggregate kriged predictions to polygons -> df with id, mort_1990, mort_2018, wealth_1990, wealth_2018, covariates
# 2) compute deltas and neighbors, fit spatial error

library(sf); library(spdep); library(spatialreg); library(dplyr)

# assume `polys` (sf) has columns wealth_1990, wealth_2018, mort_1990, mort_2018, cov1,...
dat <- polys |> 
  mutate(delta_wealth = wealth_2018 - wealth_1990,
         delta_mort   = mort_2018   - mort_1990) |> 
  filter(!is.na(delta_wealth), !is.na(delta_mort))

# neighbors / weights
nb <- poly2nb(dat, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# OLS baseline
ols <- lm(delta_mort ~ delta_wealth + cov1 + cov2, data = dat)

# test residual autocorrelation
spdep::moran.test(residuals(ols), lw, zero.policy = TRUE)

# spatial error model (accounts for correlated errors)
sem <- spatialreg::errorsarlm(delta_mort ~ delta_wealth + cov1 + cov2, data = dat, listw = lw, zero.policy = TRUE)

# spatial lag model (if spillovers plausible)
slag <- spatialreg::lagsarlm(delta_mort ~ delta_wealth + cov1 + cov2, data = dat, listw = lw, zero.policy = TRUE)

# Inspect
summary(ols)
summary(sem)
summary(slag)