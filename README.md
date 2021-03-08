Please cite as:
Sangkaew et. al., 2020, Enhancing risk prediction of prgression to severe disease during the febrile phase of dengue: A systmeatic review and meta-analysis, The Lancet Infectious Diseases, https://doi.org/10.1016/S1473-3099(20)30601-0. 

# Repository structure

This repository contains
- the data included in our systematic review and meta-analysis 
- the code used to perform the meta-analyses  

## Data
The data folder contains the following input files 
- `All_included.csv` : characteristic and risk of bias assessment of the studies included in the systematic review and meta-analysis
- `Df_macon.csv`: age and weight data
- `Df_magen_ca.csv`: other categorical variables (sex, mixed comorbidity, diabetes mellitus, hypertension, cardiovascular disease, renal disease, presence of rash, vomiting, abdominal pain and tenderness, headache, bleeding, body effusion, tourniquet test outcome, and immune status)
- `Df_magen_co.csv`: other continuous variables (platelet count, leukocyte count, levels of haematocrit, aminotransferase, serum albumin)
- `Df_nutri.csv`: nutritional status 
- `Df_serotype.csv`: dengue virus serotypes

The results of the meta-analysis are given in the following output files
- `Summary_meta-analysis_ORs.csv` and `data_1.csv`: effect sizes as odds ratios
- `Summary_meta-analysis_SMD` and `data_2.csv`: effect sizes as standard mean differences

## Scripts
The script folder contains the scripts to perform the meta-analysis
- `dengue_meta_analysis.R`: this is the main script to perform the meta-analyses (i.e. both main and sub analyses)
- `function.R`: code adapted from Harrer, M, Cuijpers, P, Furukawa, TA, & Ebert, DD (2019) Doing Meta-Analysis in R: A Hands-on Guide. DOI: 10.5281/zenodo.2551803.

