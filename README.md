# Long-term changes in the condition of Eastern Baltic cod: Increased competition with flounder for benthos or lack of pelagic food?

This repo contains R code for analyzing individual-level condition of cod from the BITS survey to characterize the spatio-temporal development of condition and to identify parsimonius predictor variables, including cod, flounder and sprat density as well as temperature and oxygen concentration.

We use Spatiotemporal linear mixed-effects models and a predictive-modelling framework within the R package [sdmTMB](https://github.com/pbs-assess/sdmTMB). This repository contains all data and code used for analyses and figures from Lindmark et al (20XX).

**Authors:** [Max Lindmark](https://maxlindmark.netlify.app/) and [Michele Casini](https://www.slu.se/cv/michele-casini/)


## How to replicate our analyses and navigate this repo

`data`
Rawr data are not uploaded here due to being large, only processed data for replicating the analysis. Raw data are available from ICES databases [DATRAS](https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx) and [OCEANOGRAPHY](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes). Can also be sent upon request to reproduce the data cleaning.

`R`
Contains code for analysis and data processing

`figures`
Contains figures of results

`output`
Contains .rds objects of model outputs due to long compuation times


