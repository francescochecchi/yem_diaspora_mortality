## Adult mortality patterns in Yemen before and during armed conflict and the COVID-19 pandemic: Evidence from a web survey of the global diaspora
### Explanation of R scripts and input datasets

-----------------------

20 February 2023

Francesco Checchi (francesco.checchi@lshtm.ac.uk), Catherine R. McGowan

Funding: United Kingdom Foreign, Commonwealth and Development Office

Contributors: Mervat Alhaffar, Promise Ekoriko, Sawsan Al-Refai, Jamal Badr, Lucy Bell

### Background
The London School of Hygiene and Tropical Medicine (LSHTM) has been conducting a project to estimate crisis- and COVID-19-attributable mortality in Yemen. This repository contains datasets and R scripts required to replicate the statistical analysis for one of the studies featured under this project. Specifically, the study sought to collect data from members of the Yemeni diaspora in different countries on the vital status of their parents, siblings and nephews/nieces still living in Yemen, as well as the parents, siblings and nephews/nieces of their most recent spouse, if any. Data were collected through a webRDS (web respondent-driven sampling) ODK platform. This repository contains datasets and R scripts required to replicate the analysis.

### Description of input datasets
The repository contains the following input data files:

* several files named `YemenRDS_DEPLOY_[...].csv` , which contain fully anonymised survey participants' responses to different sections of the questionnaire (as will be obvious from the file names, these correspond to questions about different categories of relatives: as such, the datasets are structured as one row = one individual). The file simply named 'YemenRDS_DEPLOY_02_March_2022.csv' contains data on the respondent's household (one row = one respondent).
* `yem_diaspora_pop_gov_sep2021.xlsx` contains population estimates for each subdistrict in Yemen for the month of September 2021, as published in Checchi et al. (https://www.sciencedirect.com/science/article/pii/S2666623522000289).
* a 'mapping.zip' file, which containsUN OCHA shape files for administrative boundaries and should be unzipped to the same working directory in which all the other files are stored, such that a subfolder 'mapping' is created.

### Description of R scripts
The repository contains several R scripts, numbered in the order in which they should be run in order to replicate the analysis:
* `yem_diaspora_0_control_script.R` sets general parameters, loads or installs required packages, reads files and calls all other scripts;
* `yem_diaspora_1_prepare_data.R` performs cleaning and management for all input datasets;
* `yem_diaspora_2_describe_data.R` generates statistics and graphs for the sample of respondents and individual family members;
* `yem_diaspora_4_analyse_mortality.R` prepares datasets for mortality analysis, and estimates mortality among adult siblings as well as age- and birth cohort-adjusted survival among parents aged 50+ years. The script does not yet estimate child mortality indicators. This script also calls two further R scripts that prepare and perform adult sibling mortality rate estimation, both of which can be sourced from and are documented in this UN paper: https://desapublications.un.org/working-papers/r-scripts-computing-adult-and-maternal-mortality-dhs-sibling-survival-histories 
  * `FUNCTION.PrepForSiblingsDHS.R`
  * `FUNCTION.SiblingSurvival`
* `yem_diaspora_survey_sampsi.R` is an older and completely self-contained script that estimates sample size requirements to detect a hypothesised increase in mortality during the crisis period, compared to the pre-crisis period, in Yemen, based on the survey instrument used in this study. Users can change parameters within the script. Currently sample size is estimated based on simulation, with changes in child mortality as the basis for hypothesis testing.

As R scripts are run, they produce intermediate and final output datasets and graphs, saved onto the working directory.

### How to replicate the analysis
Analysts should make sure all files, scripts and subdirectories are saved to the same folder, where output files will also appear. The directory for reading files is set automatically when `yem_diaspora_0_control_script.R` is run. An updated version of R software should be installed (https://www.r-project.org/). It is also recommended to run the code from the open-source RStudio interface (https://www.rstudio.com/products/rstudio/download/). Both R and RStudio are free and open-source.
