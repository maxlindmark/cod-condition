# Evaluating drivers of spatiotemporal changes in the condition of Eastern Baltic cod

This repo contains R code for analyzing individual-level condition of cod and to contrast the ability, or parsimony, of different variables to explain variation in cod condition, each linked to specific hypothesis proposed in the literature.

We use Spatiotemporal linear mixed-effects models and a predictive-modelling framework within the R package [sdmTMB](https://github.com/pbs-assess/sdmTMB). This repository contains all data and code used for analyses and figures from Lindmark et al (20XX).

**Authors:** [Max Lindmark](https://maxlindmark.netlify.app/), [Sean Anderson](https://seananderson.ca/) and [Michele Casini](https://www.slu.se/cv/michele-casini/)


## How to replicate our analyses and navigate this repo

`data`
Cod, flounder and condition data are downloaded from ICES databases [DATRAS](https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx), oxygen from CTD samples from  [OCEANOGRAPHY](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes). We extracted herring and sprat abundance estimates from the ICES WGBIFS database for the [BIAS](https://community.ices.dk/ExpertGroups/wgbifs/2018%20Meeting%20docs/06.%20Data/01_BIAS%20Database/) survey data. Modelled oxygen data stem from the NEMO-Nordic-SCOBI model*, downloaded from [EU Copernicus](https://resources.marine.copernicus.eu/?option=com_csw&task=results) 

* References for NEMO-Nordic-SCOBI
**Almroth-Rosell**, E., Eilola, K., Hordoir, R., Meier, H.E.M., Hall, P.O.J., 2011. Transport of fresh and resuspended particulate organic material in the Baltic Sea — a model study. J.Mar.Sys. 87, 1-12. Doi: 10.1016/j.jmarsys.2011.02.005
**Eilola, K.**, Meier, M.H.E., Almroth, E., 2009. On the dynamics of oxygen, phosphorus and cyanobacteria in the Baltic Sea; A model study. J.Mar.Sys. 75, 163-184. Doi: doi:10.1016/j.jmarsys.2008.08.009.
**Kuznetsov**, I., Eilola, K., Dieterich, C., Hordoir, R., Axell, L., Höglund, A. and Schimanke, S., 2016. Model study on the variability of ecosystem parameters in the Skagerrak-Kattegat area, effect of load reduction in the North Sea and possible effect of BSAP on Skagerrak-Kattegat area.

`R`
Contains code for analysis and data processing

`figures`
Contains figures of results

`output`
Contains .rds objects of model outputs due to long compuation times


