# Data Companion for:
## *Blueprints of REDD+ Additionality: Evaluating Project Features and Design in Voluntary Carbon Markets*

This repository serves as a data companion to my MPhil dissertation. It accompanies Table 8 ("Data sources") and contains the original datasets used to construct the variables included in the empirical analysis.

## Included datasets

The following datasets are included in this repository:

- **International Database on REDD+ Projects and Programs (IDRECCO)** – project-level REDD+ features
- **Tang et al. (2025) Supplementary Materials** – outcome variable (additionality)
- **Global Human Settlement Layer (GHS-SMOD R2023A)** – distance to nearest city and town
- **GLOPOP Regional Statistics** – wealth/income

## Datasets not included

Some datasets are not included because they were extracted directly from R packages or because of file size limitations. 

### OpenStreetMap

Road and waterway datasets were retrieved in April 2026 using the `osmdata` R package. These data are publicly available through OpenStreetMap.

### Global Data Lab (GDL)

The Global Data Lab (GDL) shapefiles used in this dissertation exceed GitHub's file size limits and are therefore not included.

They can be downloaded from:

https://globaldatalab.org/mygdl/downloads/shapefiles/

Required files:

- `GDL_Shapefiles_V6.6_large.shp`
- `GDL_Shapefiles_V6.6_large.dbf`
- `GDL_Shapefiles_V6.6_large.shx`
- `GDL_Shapefiles_V6.6_large.prj`
- `GDL_Shapefiles_V6.6_large.cpg`

## Dissertation

The dissertation documents the construction of all variables and the empirical strategy in detail. Table 8 provides a complete overview of the variables, their data sources, and the literature motivating their inclusion.
