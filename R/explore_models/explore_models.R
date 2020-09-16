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

# In this script I will fit spatiotemporal models of allometric weight-length
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

# Plot "Fulton K" in space and time 
d %>%
  ggplot(., aes(X, Y, color = Fulton_K)) + 
  geom_sf(data = world, inherit.aes = F, size = 0.2) +
  coord_sf(xlim = c(min(d$X), max(d$X)),
           ylim = c(min(d$Y), max(d$Y))) +
  geom_point(size = 1.2, alpha = 0.8) + 
  facet_wrap(~ year, ncol = 5) +
  scale_color_gradient2(midpoint = mean(d$Fulton_K)) +
  theme_classic(base_size = 8) + 
  NULL

# Read in prediction grid
pred_grid <- read.csv("data/pred_grid.csv")

pred_grid <- pred_grid %>%
  dplyr::select(-X.1) %>% 
  mutate(ln_length_cm = log(1)) %>% # For now we'll predict changes in the intercept
  filter(year %in% c(unique(d$year)))

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
## Hence, we set ar1_fields = TRUE. Other sdmTMB settings are as follows:
## ar1_fields = TRUE,

## include_spatial: Should a separate spatial random field be estimated? If enabled
## then there will be a separate spatial field and spatiotemporal fields.
## - We set this to TRUE because we also want a spatiotemporal model

## spatial_trend: Should a separate spatial field be included in the trend?
## Requires spatiotemporal data.
## - We set this to TRUE because we also want a spatiotemporal model



# HOW does include_spatial differ from spatial_trend? IS IT ABOUT THE INTERCEPTS OF THE SPATIAL FIELDS???
# SEE THORSON AND THEIR REPORT AND THE VIGNETTE. BETTER CONVERGECE WITH THE CURRENT SETTINGS...
# MAYBE BECAUSE I CANT ESTIMATE THE SPACE PROPERLY FOR EACH YEAR... 
# THEY MUST BE DIFFERENT THINGS!!!

# THEN ADD SIMPLE COVARIATES FUNCTIONS; AND THEN I WANT TO PLOT THE CONDITIONAL EFFECTS OR SOMEHING LIKE THAT

# SO 2.5 QUESTIONS: PLOTTING EFFECTS OF VARAIBLES, DID I SET UP CORRECTLY, ANYTINH ELSE I CAN DO ABOUT CONVERGENGCE?



## spatial_only: Logical: should only a spatial model be fit (i.e. do not include
## spatiotemporal random effects)? By default a spatial-only model will be fit if there
## is only one unique value in the time column or the time argument is left at its default value of NULL.
## - We set this to FALSE because we also want a spatiotemporal model


# D. FIT SPATIOTEMPORAL MODELS OF LE CRENS' RELATIVE CONDITION =====================
# I will start with 100 knots and alter I will fit and compare models with more knots
spde <- make_spde(d$X, d$Y, n_knots = 75)
plot_spde(spde)

# First we want to see which distribution seems appropriate

# Compare Gaussian and student t models with a spatiotemporal AR1 process
m_gauss <- sdmTMB(formula = ln_weight_g ~ ln_length_cm, data = d, time = "year",
                  spde = spde, family = gaussian(link = "identity"), # DF parameter get's fixed to 3 here. ?student
                  ar1_fields = TRUE,
                  include_spatial = TRUE, 
                  spatial_trend = FALSE, 
                  spatial_only = FALSE) 

m_student <- sdmTMB(formula = ln_weight_g ~ ln_length_cm, data = d, time = "year",
                    spde = spde, family = student(link = "identity"), 
                    ar1_fields = TRUE,
                    include_spatial = TRUE, 
                    spatial_trend = FALSE, 
                    spatial_only = FALSE) 

saveRDS(m_gauss, "output/explore/m_gauss.rds")
#m_gauss <- readRDS("output/explore/m_gauss.rds")

saveRDS(m_student, "output/explore/m_student.rds")
#m_student <- readRDS("output/explore/m_student.rds")

# Inspect fitted model
print(m_gauss)
print(m_student)

# Look at the residuals:
df <- d
df$residuals_gauss <- residuals(m_gauss)
df$residuals_student <- residuals(m_student)
qqnorm(df$residuals_gauss); abline(a = 0, b = 1)
qqnorm(df$residuals_student); abline(a = 0, b = 1)

# Student looks a lot better but could perhaps be improved further

# Predict and plot model on pre-made grid
# This grid is made by doing an expand grid over survey ranges, then filtering out
# areas that are actually in the ocean using ICES shapefiles, and lastly areas that 
# are too deep for sampling (-135 m) are also removed after merging the data with a 
# bathymetry map

p <- predict(m_student, newdata = pred_grid)

ggplot(p, aes(X, Y, fill = est)) +
  geom_raster() +
  facet_wrap(~year, ncol = 5) +
  scale_fill_viridis(option = "magma", 
                     name = "log(condition factor)")


