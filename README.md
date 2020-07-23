Please cite as:
Sangkaew et. al., 2020, Enhancing risk prediction of prgression to severe disease during the febrile phase of dengue: A systmeatic review and meta-analysis, The Lancet Infectious Diseases, doi:****

# Repository structure

This repository contains
- the summarised data of studies included in our systematic review and meta-analysis and summarised data of 27 associated variables included in our meta-analysis
- the code used to perform meta-analyses for each associated factor 
- the code adapted from from Harrer, M, Cuijpers, P, Furukawa, TA, & Ebert, DD (2019) Doing Meta-Analysis in R: A Hands-on Guide. DOI: 10.5281/zenodo.2551803.

## Data
The data folder contains the following input files 
- `All_included.csv` : Characteristic and risk of bias assessment of the studies included in this systematic review and meta-analysis
- `Df_macon.csv`: Summarised data of age and weight variables 
- `Df_magen_ca.csv`: Summarised data of other variables reported as categorical variables (sex, mixed comorbidity, diabetes mellitus, hypertension, cardiovascular disease, renal disease, presence of rash, vomiting, abdominal pain and tenderness, headache, bleeding, body effusion, tourniquet test outcome, and immune status)
- `Df_magen_co.csv`: Summarised data of variables reported with as continuous variables (platelet count, leukocyte count, levels of haematocrit, aminotransferase, serum albumin)
- `Df_nutri.csv`: Summarised data of nutritional status 
- `Df_serotype.csv`: Summaried data of dengue virus serotypes

The results of the meta-analysis are given in the following output files
- `Summary_meta-analysis_ORs`: Results of meta-analyses for factors reported effect sizes as odds ratios
- `Summary_meta-analysis_SMD`: Results of meta-analyses for factors reported effect sizes as standard mean differences

## Scripts
The script folder contains the scripts to perform mate-analysis and function used in our meta-analysis
- Dengue_meta_analysis.R: the main script to perform meta-analyses (i.e. both main analysis and sub-analyses in this study) and the visualizations of  summary results 
- Function.R: Special function code mainly taken and adapted from Harrer, M, Cuijpers, P, Furukawa, TA, & Ebert, DD (2019) Doing Meta-Analysis in R: A Hands-on Guide. DOI: 10.5281/zenodo.2551803.


