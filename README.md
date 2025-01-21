# ClustSnow

**Version**: 4.0  
**Author**: Joschka Geissler  
**Last modified**: 25 January 2025  

## Background
This documentation introduces the required steps for applying the **ClustSnow** model. ClustSnow determines snow distribution patterns from spatially distributed, multitemporal snow depth maps.  

ClustSnow is implemented in **R (Version 4.1.0)** and requires the following libraries to be installed:  

### Required R-Packages
| Name     | Version | Literature                                       |
|----------|---------|-------------------------------------------------|
| raster   | 3.4-13  | Hijmans (2021)                                  |
| stringr  | 1.5.0   | Wickham (2009)                                  |
| caret    | 6.0-88  | Kuhn (2008)                                     |
| nixmass  | 1.0.2   | Winkler et al. (2021)                           |

---

The ClustSnow workflow, introduced by Geissler et al. (2023) and Geissler et al. (2024), derives daily **snow depth (HS)** and **snow water equivalent (SWE)** maps based on observations.  

Key steps include:
1. **Spatial clustering**: Derive spatial clusters (areas with similar snow dynamics) using the `getCluster()` function.
2. **Cluster ordering**: Order clusters based on their mean snow depth using the `orderCluster()` function.
3. **Synthetic HS time series**: Generate daily synthetic snow depth time series for each cluster with the `hs.synth()` function.
4. **SWE time series**: Convert synthetic HS time series to SWE time series using the `delta.swe` model.
5. **Mapping**: Extrapolate synthetic time series into space to create spatiotemporally continuous SWE and HS maps using the `createMap()` function.

**Important Notes**:
- SWE maps are only derived if time series are continuous and snow depth time series start with **0 m**.
- Ensure proper directory structure as required.


│─── ClustSnow_Functions_v4.R <br>
│─── ClustSnow_v4.R <br>
│─── Data_ClustSnow <br>
│    ├─── Input <br>
│    │       hs_raster_stack.tif <br>
│    │       hs_sensor_location.cpg <br>
│    │       hs_sensor_location.dbf <br>
│    │       hs_sensor_location.kml <br>
│    │       hs_sensor_location.prj <br>
│    │       hs_sensor_location.shp <br>
│    │       hs_sensor_location.shx <br>
│    │       hs_time_series.csv <br>
│    │ <br>
│    └─── Output <br>

---

## Required Input Data and Format

### 1. Time Series
- **Name**: `hs_time_series.csv`  
- **Separator**: `;`  
- **Decimal Delimiter**: `.`  
- **Additional Note**: Specify the date format in the R script.

### 2. Sensor Locations
- **Name**: `hs_sensor_location.shp`  
- **Description**: Shapefile containing all sensor locations. Indicate the attribute name containing sensor IDs in the R script. Sensor IDs must match the headers in the time series file.

### 3. HS Maps
- **Name**: `hs_raster_stack.tif`  
- **Description**: Raster stack containing spatial maps of snow depth (e.g., UAV-based LiDAR).

![grafik](https://github.com/user-attachments/assets/cea0f368-368f-42bd-913f-2db392cb0a45)

---

## Additional Tools

### CloudCompare_CoregisterLiDAR.R
- **Purpose**: Co-registration of LiDAR-derived point clouds using Cloud Compare's Command Line Mode.
- **Description**: Wrapper script for running the co-registration workflow in R.
- **Notes**:
  - Adapt directories, AOI, and filtering thresholds to individual datasets.
  - Refer to Geissler et al. (2023) for more information.

---

## References

1. Geissler, J., Mazzotti, G., Rathmann, L., Webster, C., & Weiler, M. (2024). **ClustSnow: Utilizing temporally persistent forest snow patterns under variable environmental conditions**. [DOI:10.22541/essoar.172222597.78203131/v1](https://doi.org/10.22541/essoar.172222597.78203131/v1)
2. Geissler, J., Rathmann, L., & Weiler, M. (2023). **Combining Daily Sensor Observations and Spatial LiDAR Data for Mapping Snow Water Equivalent in a Sub‐Alpine Forest**. *Water Resources Research, 59*(9), Article e2023WR034460. [DOI:10.1029/2023WR034460](https://doi.org/10.1029/2023WR034460)
3. Hijmans, R. J. (2021). **raster: Geographic Data Analysis and Modeling**. [CRAN](https://CRAN.R-project.org/package=raster)
4. Kuhn, M. (2008). **Building Predictive Models in R Using the caret Package**. *Journal of Statistical Software, 28*(5). [DOI:10.18637/jss.v028.i05](https://doi.org/10.18637/jss.v028.i05)
5. Wickham, H. (2009). **stringr: CRAN Contributed Packages**. [DOI:10.32614/CRAN.package.stringr](https://doi.org/10.32614/CRAN.package.stringr)
6. Winkler, M., Schellander, H., & Gruber, S. (2021). **Snow water equivalents exclusively from snow depths and their temporal changes: the Δsnow model**. *Hydrology and Earth System Sciences, 25*(3), 1165–1187. [DOI:10.5194/hess-25-1165-2021](https://doi.org/10.5194/hess-25-1165-2021)
