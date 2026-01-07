# IVC Injuries Analysis

This repository contains code and data used for the analysis presented in the submitted manuscript 'Venous Thromboembolism Burden in IVC Injuries: Is there an Optimal Strategy in Prevention?' by Himmler et al. 2025.

## Getting Started

To clone this repository to your local machine:
```bash
git clone https://github.com/jdpreston30/IVC-injuries-2025
cd IVC-injuries-2025
```

Make sure you have [Git](https://git-scm.com/) installed. After cloning, you can run the analysis using the automated pipeline (see Usage section below).

## Requirements
- R version ≥ 4.5.1
- All required packages will be automatically installed when running the analysis

## Usage
The analysis uses an automated pipeline that handles package installation and data processing:

```r
# Run the complete analysis pipeline
source("all_run/run.R")
```

This will automatically:
- Install any missing CRAN packages (from DESCRIPTION.txt)  
- Install TernTablesR from GitHub (jdpreston30/TernTablesR)
- Load all required packages
- Import data using dynamic path detection
- Execute all analysis scripts in sequence

## Repository Structure

### Main Analysis Pipeline
- `all_run/run.R`: Main pipeline script that orchestrates the entire analysis
- `all_run/config_dynamic.yaml`: Configuration file for dynamic data import settings

### Analysis Scripts
- `R/Scripts/00a_environment_setup.R`: Environment setup and package loading
- `R/Scripts/00b_setup_and_import.r`: Data import and initial processing
- `R/Scripts/01_table1.r`: Demographics table (Table 1)
- `R/Scripts/02_table2.r`: Mechanism/Survival Table (Table 2)
- `R/Scripts/03_table3.r`: VTE Details Table (Table 3)
- `R/Scripts/04_table4.r`: VTE Cohort Characteristics (Table 4)
- `R/Scripts/05_figures.r`: Figure generation and data for figures
- `R/Scripts/06_data_not_shown.r`: Supplementary analyses and data not visualized.
- `R/Scripts/07_sup_table1.r`: Supplementary table generation

### Utility Functions
- `R/Utilities/dynamic_import.R`: Dynamic data import functionality
- `R/Utilities/load_dynamic_config.R`: Configuration loading utilities
- `R/Utilities/my_render_cat.R`: Custom rendering function
- `R/Utilities/output_functions.R`: Output formatting and table generation functions

### Outputs and Figures
- `Outputs/`: Generated data files and tables from the analysis (CSV and Word formats)
- `Figures.prism`: GraphPad Prism file used to create manuscript figures

### Project Metadata
- `DESCRIPTION.txt`: Package dependencies and project metadata

## Accessing Raw Data
Raw data files are stored securely via OneDrive.
- Please request access by contacting me directly (joshua.preston@emory.edu)
- OR contact the corresponding and senior authors (AHimmler@dhs.lacounty.gov; jason.d.sciarretta@emory.edu) for permission