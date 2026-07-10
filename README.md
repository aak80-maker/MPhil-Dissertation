# MPhil-Dissertation


This repository contains the data and code used for my MPhil dissertation at the University of Cambridge named "Blueprints of REDD+ Additionality: Evaluating Project Features and Design in Voluntary Carbon Markets"

## Repository Structure

```
code/
    Coding Y and X Variables.R
    Coding More X Variables.R
    Extra_Data_Preparation.R
    GLOPOP Data.R
    Main_Analysis.R

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
   - Performs some more data preparation, including variable recoding, creation of derived variables (e.g. area-normalised deforestation), and produces the final analysis-ready dataset.

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

## Notes

This repository tries to reflect the workflow used to produce the analyses presented in the dissertation.

My research developed iteratively over time, and some intermediate datasets were updated or overwritten as coding decisions evolved. As a result, this repository focuses on the main data-processing and analysis steps used to generate the reported results. Importantly, it aims to provide transparency into the primary data used and how it was processed; to outline the code used to process the data and run the analysis; and importantly, to supplement the dissertation paper itself.

I have done my best to organise the code and data in a clear and reproducible way. If anything is unclear, or if you think something is missing, please don't hesitate to get in touch and I'd be happy to provide clarification or any additional files where possible!


## Contact

If you have any questions or require more information, please feel free to contact me!
