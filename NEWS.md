# remora 0.9 (31/07/2025)

* `extractBlue` and `extractEnv` updated with faster environmental data download via tidync
* TownsvilleReefQC example data and underlying raw data updated to reflect IMOS-ATF/AODN change in metadata variable from `tagging_project_name` to `tag_device_project_name` and `tag_deployment_project_name`
* minor fixes


# remora 0.8-0 (14/11/2023)

First major update to remora:

* addresses retirement of underlying R spatial packages - rgeos and rdgal
* fixes environmental data access issues when using `extractEnv()`
* adds `extractBlue()` function for accessing CSIRO Bluelink (BRAN) reanalysis environmental data with global, 3-D coverage from 0 - 4905 m depth
* adds an interactive `plotQC()` function for visualizing quality-controlled detection locations. This function replaces the original static `plotQC` function.


# remora 0.7.1 (07/12/2021)

* Initial release version of the remora R package
