#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# Fit and explore different spatial and spatio-temporal glmms with 
# sdmtmb: https://github.com/pbs-assess/sdmTMB
# 
# A. Load libraries
# 
# B. Read data
# 
# C. Fit simple GLM
# 
# D. Fit spatial + spatiotemporal predictive process GLMMs
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# The code and structure follows that of the spatial trends
# (vignette)[https://github.com/pbs-assess/sdmTMB/blob/master/vignettes/spatial-trend-models.Rmd]
# in (sdmTMB)[https://github.com/pbs-assess/sdmTMB]

# In this script I will fit spatiotemporal models of allometric weight-length using 
# (sdmTMB)[https://github.com/pbs-assess/sdmTMB]
# relationships (log(w) ~ alpha + b_1*log(w) + b_x*x) and find an appropriate model 
# structure for evaluating how alpha, log condition factor, (or any other weight) for a 
# given length varies over space and time.

# Next I will see evaluate the parsimony of additional predictor variables besides length
# for predicting weight. I currently consider: 
#- Density of cod (Svedäng & Hornborg, 2014). CPUE available for each haul (added in data)
#- Density of flounder (Orio et al, 2019). CPUE available for each haul (added in data)
#- Oxygen concentration (e.g. Casini et al, 2016). Possible to link data to nearest haul (not yet added)
#- Sprat CPUE (e.g. Gårdmark et al, 2015).**
#- Herring CPUE (e.g. Gårdmark et al, 2015).**
# **Not yet added. Semi coarse predictions exist (by ICES rectangle). I can either do 
# a join operation to get the estimated CPUE by rectangle linked to all hauls in that 
# rectangle, or attempt to fit a new model and predict CPUE for the locations where I
# have haul conditions. Going for the former I think...


# A. LOAD LIBRARIES ================================================================
# Load libraries, install if needed
library(tidyverse)
library(tidylog)
library(viridis)
library(sdmTMB)


# B. READ DATA =====================================================================
d <- read.csv("data/mdat_cond.csv") 

# Calculate some variables in data
d <- d %>%
  dplyr::select(-X) %>% 
  rename("Y" = "lat", "X" = "lon") %>% 
  mutate(ln_weight_g = log(weight_g),
         ln_length_cm = log(length_cm),
         Fulton_K = weight_g/(0.01*length_cm^3)) %>% # Just an approximation for now
  dplyr::select(year, Y, X, sex, length_cm, weight_g, Quarter, CPUE_cod, CPUE_fle,
                ln_length_cm, ln_weight_g, Fulton_K)

# Read in prediction grid
pred_grid <- read.csv("data/pred_grid.csv")

pred_grid <- pred_grid %>%
  dplyr::select(-X.1) %>% 
  mutate(ln_length_cm = log(1)) %>% # For now we'll predict changes in the intercept (condition factor)
  filter(year %in% c(unique(d$year)))

# Plot "Fulton K" in space and time 
d %>%
  ggplot(., aes(X, Y, color = Fulton_K)) + 
  geom_point(size = 1.2, alpha = 0.8) + 
  facet_wrap(~ year, ncol = 5) +
  scale_color_gradient2(midpoint = mean(d$Fulton_K)) +
  theme_classic(base_size = 8) + 
  NULL

# Check sample size by year
d %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(year, n)) + 
  geom_bar(stat = "identity") +
  theme_classic(base_size = 12) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25)

## Ok, a few things: There's a clear temporal development in condition and the spatial
## coverage of data varies by year (less initially).


# C. FIT SIMPLE GLM  ===============================================================
# Next we can fit a simple intercept-only GLM without any spatial or temporal structure
m_glm <- glm(Fulton_K ~ 1, data = d, family = gaussian(link = "log"))

# Extract residuals
d$m_glm_residuals <- residuals(m_glm)

# Plot residuals over map
ggplot(d, aes(X, Y, colour = m_glm_residuals)) +
  scale_color_gradient2() +
  geom_point(size = 2) + 
  theme_classic() +
  NULL

# Plot residuals by year
ggplot(d, aes(year, m_glm_residuals, group = year)) +
  geom_boxplot() + 
  theme_classic() +
  NULL

## Residuals appear to vary both spatially and are somewhat clustered within years.

## Next we fit spatial/spatiotemporal models to account for this.
## Given that we do not have samples in all spatial locations for all years, 
## and we believe that there are "hotspots" of body condition, we model a 
## temporal correlation for the spatiotemporal variation (Thorson, 2019),
## specifically, this is done by estimating the spatiotemporal fields as an AR1 process.
## Hence, we set ar1_fields = TRUE.


# D. FIT SPATIOTEMPORAL MODELS OF WEIGHT~LENGTH ====================================
# I will start with 100 knots and alter I will fit and compare models with more knots
spde <- make_spde(d$X, d$Y, n_knots = 75)
plot_spde(spde)

# First we want to see which distribution seems appropriate

# Compare Gaussian and student t models with a spatiotemporal AR1 process
m0 <- sdmTMB(formula = ln_weight_g ~ ln_length_cm, data = d, time = "year", spde = spde,
             family = gaussian(link = "identity"), ar1_fields = TRUE,
             include_spatial = TRUE,  spatial_trend = FALSE, spatial_only = FALSE) 

m1 <- sdmTMB(formula = ln_weight_g ~ ln_length_cm, data = d, time = "year", spde = spde,
             family = student(link = "identity"), ar1_fields = TRUE,
             include_spatial = TRUE, spatial_trend = FALSE, spatial_only = FALSE) 

# Inspect fitted model
print(m0)
print(m1)

# Look at the residuals:
df <- d

df$residuals_m0 <- residuals(m0)
df$residuals_m1 <- residuals(m1)

qqnorm(df$residuals_m0); abline(a = 0, b = 1)
qqnorm(df$residuals_m1); abline(a = 0, b = 1)

# Gaussian looks bad. Student looks a lot better but could perhaps be improved further

# Check the residuals for the student t model on a map
ggplot(df, aes(X, Y, colour = residuals_m1)) +
  geom_point(size = 1) +
  facet_wrap(~year, ncol = 5) +
  scale_color_gradient2() +
  theme_classic() +
  coord_fixed()

# Seems to be some clustering still...

# We can also look at the AR1 parameter to ensure it is warranted
sd1 <- as.data.frame(summary(TMB::sdreport(m1$tmb_obj)))
sd1$Estimate[row.names(sd1) == "ar1_phi"]
# [1] 1.019191
sd1$Estimate[row.names(sd1) == "ar1_phi"] +
  c(-2, 2) * sd1$`Std. Error`[row.names(sd1) == "ar1_phi"]
# [1] 0.7932714 1.2451101

# Strong support for it, will not run model without AR1 process in the spatiotemporal
# field for now.

# Predict and plot estimates using all fixed and random effects on pre-made grid
# This grid is made by doing an expand grid over survey ranges, then filtering out
# areas that are actually in the ocean using ICES shapefiles. Lastly some areas are 
# too deep for sampling (-135 m). So I've added a depth column so that I can make
# those predictions NA so it's clear they are different from e.g. land and islands
p <- predict(m1, newdata = pred_grid)

# Replace too-deep predictions with NA
p <- p %>% mutate(est2 = ifelse(depth < -130, NA, est))

ggplot(p, aes(X, Y, fill = est2)) +
  geom_raster() +
  facet_wrap(~year, ncol = 5) +
  scale_fill_viridis(option = "magma", 
                     name = "log(condition factor)") + 
  theme_bw() +
  coord_cartesian(expand = 0) + 
  ggtitle("Prediction (random + fixed")

# Plot the spatiotemporal random effects
ggplot(p, aes(X, Y, fill = est_rf)) +
  geom_raster() +
  facet_wrap(~year, ncol = 5) +
  scale_fill_viridis(option = "magma", 
                     name = "log(condition factor)") + 
  theme_bw() +
  coord_cartesian(expand = 0) + 
  ggtitle("Spatiotemporal random effects")

# Plot the spatial random effects
ggplot(filter(p, year == 2005), aes(X, Y, fill = omega_s)) +
  geom_raster() +
  scale_fill_viridis(option = "magma", 
                     name = "log(condition factor)") + 
  theme_bw() +
  coord_cartesian(expand = 0) + 
  geom_point(data = d, aes(X, Y), color = "green", inherit.aes = FALSE, size = 0.2) +
  ggtitle("Spatial random effects + data")

# The spatial random effects seem to follow depth pretty well.
baltic_sea <- getNOAA.bathy(lon1 = min(d$X), lon2 = max(d$X),
                            lat1 = min(d$Y), lat2 = max(d$Y), resolution = 15)

autoplot(baltic_sea, geom = c("r", "c")) +
  scale_fill_gradient2(low = "darkblue", high = "gray", midpoint = 0)


# D. FIT SPATIOTEMPORAL MODELS OF WEIGHT~LENGTH WITH ADDITIONAL COVARIATES =========
# Now we want to refit the same model with the additional fixed effects outlined above
# First we include the density of cod (measured as #caught per trawling hour)
# There's quite a spread in the CPUE variables. Highest cod CPUE is ~3 cod per second!
# Will need to check the data again, maybe select certain sizes or hauls with a
# minimum towing time. For now I'll just set a roof on the CPUE to help models converge...

# I will next standardize the covariates to have a mean of 0 and variance of 1 to 
# facilitate comparison between different ones

d <- d %>% 
  mutate(CPUE_cod_st = CPUE_cod,
         CPUE_fle_st = CPUE_fle) %>% 
  mutate_at(c("CPUE_cod_st", "CPUE_fle_st"), ~(scale(.) %>% as.vector))

# Fit model with cod cpue as covariate
mcod <- sdmTMB(formula = ln_weight_g ~ ln_length_cm + CPUE_cod_st, data = d, time = "year",
               spde = spde, family = student(link = "identity"), 
               ar1_fields = TRUE,
               include_spatial = TRUE, 
               spatial_trend = FALSE, 
               spatial_only = FALSE) 

#... And with flounder 
mfle <- sdmTMB(formula = ln_weight_g ~ ln_length_cm + CPUE_fle_st, data = d, time = "year",
               spde = spde, family = student(link = "identity"), 
               ar1_fields = TRUE,
               include_spatial = TRUE, 
               spatial_trend = FALSE, 
               spatial_only = FALSE) 

# Run extra optimization steps to help convergence:
mfle2 <- run_extra_optimization(mfle, nlminb_loops = 1, newton_steps = 1)

# Warning message:
#   The model may not have converged. Maximum final gradient: 0.168784611827846.
# I can't seem to get passed this

# Check the models
print(mcod)
print(mfle2)

# Look at the new parameter (cod)
sdmcod <- as.data.frame(summary(TMB::sdreport(mcod$tmb_obj)))

sdmcod$Estimate[row.names(sdmcod) == "b_j.2"] # The second term, aka cod
# [1] 0.001785035

sdmcod$Estimate[row.names(sdmcod) == "b_j.2"] +
  c(-2, 2) * sdmcod$`Std. Error`[row.names(sdmcod) == "b_j.2"]
# [1] 0.0006023947 0.0029676758

#... And the same for flounder
sdmfle <- as.data.frame(summary(TMB::sdreport(mfle2$tmb_obj)))

sdmfle$Estimate[row.names(sdmfle) == "b_j.2"] # The second term, aka flounder
# [1] 0.0009320629

sdmfle$Estimate[row.names(sdmfle) == "b_j.2"] +
  c(-2, 2) * sdmfle$`Std. Error`[row.names(sdmfle) == "b_j.2"]
# [1] -0.0008480145  0.0027121403

# Compare the models with AIC (unsure actually if this is correct for a sdmTMB model...)
aic_m1 <- extractAIC(m1)
aic_mcod <- extractAIC(mcod)
aic_mfle <- extractAIC(mfle2)

aic_m1
aic_mcod
aic_mfle

# Interesting. The model with flounder has a smaller AIC, even though the 95% confidence
# interval crosses 0.

# Now let's look more closely at the our estimates. If I understand Thorson (2015) correctly:
# "This implies that γX (the covariate times its coefficient) has a standard deviation of γ
# such that coefficients can be interpreted via comparison with the standard deviation
# of spatial, temporal and spatio-temporal variation, as well as that of residual variation."

# I can now compare the coefficient of flounder with the standard deviation of the 
# spatial and spatiotemporal effects, i.e. sigma_E and sigma_A in Thorson (2015)
# (Eqns. 6b-7), which are the marginal variances of the random fields.

# In sdmTMB I would have thought these sigma_E and sigma_A correspond to 
# model$tmb_params$ln_tau_O and model$tmb_params$ln_tau_E for the spatial and 
# spatiotemporal trend after looking at equations following E.2 in Anderson et al (2019) DFO
# but I think now that's wrong!

# However, I think I can find these parameters using print(), so I will continue with that

# Compare the model without covariates with the flounder model
print(m1)
# Spatial SD (sigma_O): 0.08
# Spatiotemporal SD (sigma_E): 0.08

print(mfle2)
# Spatial SD (sigma_O): 0.08
# Spatiotemporal SD (sigma_E): 0.08

# Now look at the flounder coefficient again:
sdmfle$Estimate[row.names(sdmfle) == "b_j.2"] # The second term, aka flounder
# [1] 0.0009320629

# Ok, so I interpret this as that the the flounder coefficient is small relative to 
# other sources of temporally constant variation across space (omega) and factors 
# varying in space from year to year. Further, I don't think inclusion of the flounder
# covariate leads to less variation explained by the spatial and spatiotemporal effects,
# (although it could be because I got them wrong or because print() is rounded) 

# Either way, for the sake of comparison, I can also produce a map to look at the
# differences there

# Add in a fixed covariate here
pred_grid_fle <- pred_grid
pred_grid_fle$CPUE_fle_st <- 0 # Mean since standardized

pfle <- predict(mfle2, newdata = pred_grid_fle)

# Replace too-deep predictions with NA
pfle <- pfle %>% mutate(est2 = ifelse(depth < -130, NA, est))

ggplot(pfle, aes(X, Y, fill = est2)) +
  geom_raster() +
  facet_wrap(~year, ncol = 5) +
  scale_fill_viridis(option = "magma", 
                     name = "log(condition factor)") + 
  theme_bw() +
  coord_cartesian(expand = 0) + 
  ggtitle("Prediction (random + fixed")


# To do: Fix any misunderstandings (the spatial_trend argument, how to compare predictors
# with each other and with the marginal variances of the random fields [and how to extract
# the latter]). Then, go through the covariance data, might need to do it in a length-resolved 
# way (check CPUE by size first)


