[![Github All Releases](https://img.shields.io/github/downloads/IMOS-AnimalTracking/remora/total.svg)]() 
[![Github All Releases](https://img.shields.io/github/forks/IMOS-AnimalTracking/remora)]()
[![Github All Releases](https://img.shields.io/github/issues/IMOS-AnimalTracking/remora)]()

<p align="center">
  <img src="vignettes/images/remora_hex_logo.png" width="200">
</p>
<h1 align="center"><span style="color:#BEBEBE">re</span><span style="color:#808080"><b>mo</b></span><span style="color:#BEBEBE">ra</span></h1>
<h4 align="center">A R package to facilitate the Rapid Extraction of Marine Observations for Roving Animals</h4>

<p align="center">
  <a href="#overview">Overview</a> •
  <a href="#installation">Installation</a> •
  <a href="#key-functionalities">Key functionalities</a> •
  <a href="#acknowledgements">Acknowledgements</a> •
  <a href="#data-accessibility">Data accessibility</a>
  <a href="#licence">Licence</a>
</p>

![](vignettes/images/env_extract/env_extract_plot6.png)

## Overview
**remora** is a R package enabling the integration of animal acoustic telemetry data with oceanographic observations collected by ocean observing programs. It includes functions for:  
- Performing robust quality-control of acoustic telemetry data as described in [Hoenner et al. 2018](https://doi.org/10.1038/sdata.2017.206) 
- Identifying available satellite-derived and sub-surface *in situ* oceanographic datasets coincident and collocated with the animal movement data, based on regional Ocean Observing Systems  
- Extracting and appending these environmental data to animal movement data  
- Interactively exploring animal movements in space and time  

Whilst the functions in **remora** were primarily developed to work with acoustic telemetry data, the oceanographic data extraction and integration functionalities will work with other spatio-temporal ecological datasets (eg. satellite telemetry, species sightings records, fisheries catch records).

## Installation
**remora** requires R version > 3.5

You will need the remotes package to install **remora**:

```r
install.packages("remotes")
library("remotes")     
```
The latest stable version of **remora** can be installed from GitHub:

```r
remotes::install_github('IMOS-AnimalTracking/remora')
```

## Key functionalities
A set of functions within the **remora** workflow allow users to run each of the below functionalities independently, as required. 

LINK TO WORKFLOW DIAGRAM HERE

Quick links to vignettes for each functionality can be found below:  

- [Quality control of acoustic telemetry data]()  
- [Integration with remotely-sensed or gridded environmental data]()  
- [Integration with *in situ* sub-surface oceanographic moooring data]()  
- [Interactive visualisation of data associated with an acoustic receiver array]()  
- [Interactive visualisation of data associated with an acoustic tagging project]()  

## Acknowledgements
ADD IMOS AND NSW GOV LOGOS HERE

This work was funded by Australia’s Integrated Marine Observing System (IMOS, www.imos.org.au) and a Research Attraction and Acceleration Program grant from the Office of the New South Wales Chief Scientist & Engineer awarded to Sydney Institute of Marine Science. IMOS is enabled by the National Collaborative Research Infrastructure Strategy (NCRIS). It is operated by a consortium of institutions as an unincorporated joint venture, with the University of Tasmania as Lead Agent. 

We thank Michelle Heupel, Colin Simpfendorfer and Paul Butcher for allowing us access to the following sample datasets to aid code development.

Heupel, M., Simpfendorfer, C. *et al.* (2021) Townsville Reefs (Available: Integrated Marine Observing System. Animal Tracking Database. https://animaltracking.aodn.org.au. Accessed: 2021-10-11).

Butcher, P. *et al.* (2021) NSW DPI (Whaler, White and Tiger Shark Program). NSW Department of Primary Industries. (Available: Integrated Marine Observing System. Animal Tracking Database. https://animaltracking.aodn.org.au. Accessed: 2021-10-11).

## Data accessibility
- Continental-scale acoustic telemetry data collated as part of Australia’s Integrated Marine Observing System (IMOS) are available through the [IMOS Australian Animal Acoustic Telemetry Database](https://animaltracking.aodn.org.au). 

- Oceanographic datasets are available from the [IMOS Australian Ocean Data Network](https://portal.aodn.org.au/).

## Licence
NEED TO CONFIRM - Creative Commons Attribution 4.0 International (CC BY 4.0)

**Citation:**  
Jaine *et al.* **remora**: A R package to facilitate Rapid Extraction of Marine Observations for Roving Animals. *In preparation* for publication in Methods in Ecology and Evolution.

**Contributors:**  
**Ian Jonsen** @ianjonsen (IMOS Animal Tracking Facility, Macquarie University)  
**Vinay Udyawer** @vinayudyawer (Australian Institute of Marine Science)  
**Ross Dwyer** @RossDwyer (University of the Sunshine Coast)  
**Kylie Scales** @KScales (University of the Sunshine Coast)  
**Francisca Maron** @fmaron (IMOS Animal Tracking Facility, Sydney Institute of Marine Science)  
**Xavier Hoenner** @xhoenner (Commonwealth Scientific and Industrial Research Organisation)  
**Charlie Huveneers** @huve0001 (Flinders University)  
**Fabrice Jain**e @fjaine (IMOS Animal Tracking Facility, Sydney Institute of Marine Science)  

