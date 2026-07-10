# MPhil-Dissertation


This repository contains the data and code used for my MPhil dissertation at the University of Cambridge named "Blueprints of REDD+ Additionality: Evaluating Project Features and Design in Voluntary Carbon Markets"

## Repository Structure

```
code/
    Coding Y and X Variables.R
    Coding More X Variables.R
    GLOPOP Data.R
    Data_Cleaning.R
    Main_Code.R

data/
    original/
    intermediary/
    final/
```

## Code

The scripts are intended to be run in the following order:

1. **Coding Y and X Variables.R**
   - Creates and codes the main outcome and explanatory variables from the consolidated project database.
   - Merges several project-level datasets.

2. **Coding More X Variables.R**
   - Adds additional explanatory variables, including spatial variables such as distance to roads, water bodies, and urban areas.

3. **GLOPOP Data.R**
   - Assigns projects to Global Data Lab (GDL) regions using spatial joins and merges regional wealth and income indicators from GLOPOP.

4. **Data_Cleaning.R**
   - Performs final data preparation, including variable recoding, creation of derived variables (e.g. area-normalised deforestation), and produces the final analysis-ready dataset.

5. **Main_Code.R**
   - Contains the statistical analyses reported in the dissertation, including exploratory analyses, regression models, variable selection procedures, robustness checks, model diagnostics, interaction analyses, and exploratory multivariate analyses.

---

## Data

The `data` folder is organised into three stages.

### `original/`

Contains the original datasets used in the project where redistribution is permitted.

Some spatial datasets (e.g. the Global Data Lab shapefiles) exceed GitHub's file size limits and are therefore not included. README files within the relevant folders provide download links and documentation for these datasets.

### `intermediary/`

Contains intermediate datasets created during data preparation.

These files document the principal processing steps linking the original datasets to the final analysis dataset.

### `final/`

Contains the final analysis-ready dataset used in `Main_Code.R`.

---

## Workflow

The overall workflow is:

```
Original data
        ↓
Coding Y and X Variables.R
        ↓
Coding More X Variables.R
        ↓
GLOPOP Data.R
        ↓
Data_Cleaning.R
        ↓
Processed Data Master File.csv
        ↓
Main_Code.R
```

---

## Notes

This repository reflects the final workflow used to produce the analyses presented in the dissertation.

The research developed iteratively over time, and some intermediate datasets were updated or overwritten as coding decisions evolved. Rather than preserving every exploratory version, this repository contains the principal data-processing steps and the final analytical workflow used to generate the reported results.

The code has been organised to maximise clarity and reproducibility while remaining faithful to the workflow used during the research process.

---

## Software

The analyses were conducted in **R** using packages including:

- tidyverse
- sf
- glmnet
- car
- lmtest
- sandwich
- FactoMineR
- factoextra
- ggplot2
- dunn.test

---

## Contact

If you have any questions regarding the repository, data preparation, or analysis, please feel free to contact me.
