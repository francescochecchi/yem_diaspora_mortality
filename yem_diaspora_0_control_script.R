#..........................................................................................
### ++++ RESPONDENT-DRIVEN SAMPLING MORTALITY SURVEY OF THE YEMENI DIASPORA (2022) ++++ ###
#..........................................................................................

#..........................................................................................
## ------------ R SCRIPT TO SET PARAMETERS, READ DATA AND CALL OTHER SCRIPTS ----------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Dec 2022)
                                          # francesco.checchi@lshtm.ac.uk 


#..........................................................................................
### Preparatory steps

  #...................................      
  ## Install or load required R packages
    
    # List of required packages
    x1 <- c("car", "ggplot2", "ggpubr", "lattice", "lubridate", "MASS", "readxl", "RColorBrewer",
      "scales", "sf", "survival", "survminer", "timereg", "tmap", "zoo")
    
    # Install any packages not yet installed
    x2 <- x1 %in% row.names(installed.packages())
    if (any(x2 == FALSE)) { install.packages(x1[! x2]) }
    
    # Load all packages    
    lapply(x1, library, character.only = TRUE)
    

  #...................................      
  ## Starting steps

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
  
    # Set font
    windowsFonts(Arial=windowsFont("Arial"))

    # Set working directory to where this file is stored
    current_path = rstudioapi::getActiveDocumentContext()$path 
    setwd(dirname(current_path ))
    print( getwd() )
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
    palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    

#.........................................................................................
### Specify parameters
    
    # Dates at which baseline period starts, crisis period starts / COVID-19 starts / analysis ends
    dates_analysis <- as.Date(c("1Jan2008", "1Jun2014", "1Apr2020", "15Aug2022"), "%d%b%Y")
      
      # dates at which period changes
      date_knots <- dates_analysis[c(2, 3)]
 
      # start and end of analysis period
      date_start <- dates_analysis[1]
      date_end <- dates_analysis[length(dates_analysis)]
      

#.........................................................................................
### Reading in required files

  #...................................      
  ## Read in OCHA/CSO shape file for governorates
  dir_maps <- paste(getwd( ), "/mapping", sep = "")
  ocha_shape_gov <- sf::st_read(paste(dir_maps, "/yem_admbnda_adm1_govyem_cso_20191002.shp", sep = ""))
      
  #...................................      
  ## Read in governorate population estimates
  pop <- read_excel("yem_diaspora_pop_gov_sep2021.xlsx")
    # remove tibble
    pop <- as.data.frame(pop)
  
  #...................................      
  ## Read in respondent dataset
  resp <- read.csv("YemenRDS_DEPLOY_02_March_2022.csv")

  #...................................      
  ## Read in sibling survival datasets

    # Brothers
    siblings1 <- read.csv("YemenRDS_DEPLOY_02_March_2022-brotheryour.csv")

    # Spouse's brothers
    siblings2 <- read.csv("YemenRDS_DEPLOY_02_March_2022-brotherspouse.csv")

    # Sisters
    siblings3 <- read.csv("YemenRDS_DEPLOY_02_March_2022-sisteryour.csv")

    # Spouse's sisters
    siblings4 <- read.csv("YemenRDS_DEPLOY_02_March_2022-sisterspouse.csv")


  #...................................      
  ## Read in children survival datasets

    # Brothers' children
    children1 <- read.csv("YemenRDS_DEPLOY_02_March_2022-brotheryour_kid_questions.csv")

    # Spouse's brothers' children
    children2 <- read.csv("YemenRDS_DEPLOY_02_March_2022-brotherspouse_kid_questions.csv")

    # Sisters' children
    children3 <- read.csv("YemenRDS_DEPLOY_02_March_2022-sisteryour_kid_questions.csv")

    # Spouse's sisters' children
    children4 <- read.csv("YemenRDS_DEPLOY_02_March_2022-sisterspouse_kid_questions.csv")


#.........................................................................................
### Preparing the dataset
     
source("yem_diaspora_1_prepare_data.R")
    
    
#.........................................................................................
### Describing the dataset
 
source("yem_diaspora_2_describe_data.R")
    
    
#.........................................................................................
### Analysing mortality patterns
    
source("yem_diaspora_3_analyse_mortality.R")    

    
#.........................................................................................
### ENDS



  