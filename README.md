# Evaluating drivers of spatiotemporal individual condition of a bottom-associated marine fish

[Lindmark, M.](https://maxlindmark.github.io/), [Anderson, S. C.](https://seananderson.ca/), [Gogina, M.](https://www.io-warnemuende.de/mayya-gogina.html), and [Casini, M](https://www.slu.se/cv/michele-casini/). 2022, October 21. Evaluating drivers of spatiotemporal individual condition of a bottom-associated marine fish. bioRxiv. <https://www.biorxiv.org/content/10.1101/2022.04.19.488709v3> (Accessed 19 January 2023).

This repo contains R code for:

(1) Identifying which covariates are associated with observed variation in condition; (2) Developing a spatiotemporally standardized condition index for cod in the eastern Baltic Sea that takes into account the heterogeneous and temporally varying distribution of cod; (3) Exploring how the spatiotemporal distribution of cod impacts the environmental conditions experienced by cod and their implications on trends body condition.

We use Spatiotemporal linear mixed-effects models and a predictive-modeling framework within the R package [sdmTMB](https://github.com/pbs-assess/sdmTMB). This repository contains all data and code used for analyses and figures from Lindmark et al (2022).


## How to replicate our analyses and navigate this repo

`data`
Contains data from the following sources:

* Cod, flounder and condition data are downloaded from ICES databases [DATRAS](https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx)
* Herring and sprat abundance estimates are from the ICES WGBIFS database for the [BIAS](https://community.ices.dk/ExpertGroups/wgbifs/2018%20Meeting%20docs/06.%20Data/01_BIAS%20Database/) survey.
* Modelled oxygen and temperature data stem from the NEMO-Nordic-SCOBI model (Almroth-Rosell et al., 2011; Eilola et al., 2009; Kuznetsov et al., 2016), downloaded from [EU Copernicus](https://resources.marine.copernicus.eu/?option=com_csw&task=results) 
* Biomass density estimates of Saduria are from Gogina et al., (2020)

`R`
Contains code for analysis and data processing

`figures`
Contains figures of results

`output`
Contains .rds objects of model outputs due to long computation times

For reproducing models, run `density_model.Rmd` and `condition_model_cf.Rmd` (and `sensitivity_analysis.Rmd` if you want). For collating data from scratch, run scripts in this order: 

1. `R/clean_data/collate_cpue_data_exchange.Rmd` (clean cpue data)
2. `R/clean_data/cod_fle_density_models_as_covars.Rmd` (fit cpue models without covariates for prediction onto data and grid)
3. `R/clean_data/make_pred_grid_utm.Rmd` (to get large scale predictor variables [ices rectangle and sub-division])
4. `R/clean_data/collate_cond_data_exchange.Rmd` (to get condition data, haul-level and large scale covariates from the cpue model and the pred_grid)

### References

Almroth-Rosell, E., Eilola, K., Hordoir, R., Meier, H.E.M., Hall, P.O.J., 2011. Transport of fresh and resuspended particulate organic material in the Baltic Sea — a model study. *J.Mar.Sys*. 87, 1-12. Doi: 10.1016/j.jmarsys.2011.02.005

Eilola, K., Meier, M.H.E., Almroth, E., 2009. On the dynamics of oxygen, phosphorus and cyanobacteria in the Baltic Sea; A model study. *J.Mar.Sys*. 75, 163-184. Doi: doi:10.1016/j.jmarsys.2008.08.009.

Gogina M, Zettler ML, Wåhlström I, Andersson H, Radtke H, Kuznetsov I, MacKenzie BR. 2020. A combination of species distribution and ocean-biogeochemical models suggests that climate change overrides eutrophication as the driver of future distributions of a key benthic crustacean in the estuarine ecosystem of the Baltic Sea. ICES J Mar Sci 77:2089–2105. doi:10.1093/icesjms/fsaa107

Kuznetsov, I., Eilola, K., Dieterich, C., Hordoir, R., Axell, L., Höglund, A. and Schimanke, S., 2016. Model study on the variability of ecosystem parameters in the Skagerrak-Kattegat area, effect of load reduction in the North Sea and possible effect of BSAP on Skagerrak-Kattegat area. *SMHI Report* Oceanografi 119
