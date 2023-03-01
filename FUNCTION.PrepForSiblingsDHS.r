
#'
#' PrepForSiblingsDHS - Transform sibling survival microdata to a person-period file for mortality estimation 
#' 
#' @description This function reads DHS microdata in Stata format with female respondents' reports of the survival of their siblings (from the individual recode file) and transforms it to a long dataset with one record per sibling/exposure period.
#' 
#' @param ind.recode file path for the Stata format individual recode file
#' @param SurveyRefDate the middle reference date of the survey (halfway between start and end dates of field work) as a character string in "dd/mm/yyyy" format 
#' 
#' @details Quits and returns an error message if one or more of the required variables is not found on the ind.recode file
#' 
#' @return person-period data.frame. mmidx=0 identifies the respondent
#' 
#' @export 
#' 
#' @importFrom dplyr
#' @importFrom foreign read.dta
#' @importFrom tidyr gather spread
#' 

PrepForSiblingsDHS <- function(ind.recode,SurveyRefDate) {
#  
# Author: Sara Hertog
# Written: 20 February 2014
# Last revised: 28 August 2019 to simplify for processing one survey at a time (no batch), and streamline code with tidyvers and dplyr
# Example:
# df <- PrepForSiblingsDHS(ind.recode="ZZIR62FL.DTA",
#                          SurveyRefDate="01/07/2015")


# list of packages for session
.packages = c("foreign","dplyr","tidyr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# parse the survey reference date to day, month, year
RefDay<-as.numeric(unlist((strsplit(SurveyRefDate,"/")))[1])
RefMo<-as.numeric(unlist((strsplit(SurveyRefDate,"/")))[2])
RefYr<-as.numeric(unlist((strsplit(SurveyRefDate,"/")))[3])


# # read the DHS ir.recode file in Stata format and force variable names to lower case
# ir.data <- read.dta(ind.recode, convert.factors=F, convert.underscore=T) %>%
#            rename_all(tolower)

ir.data <- read.csv("ind_recode.csv")

test<-names(ir.data)[which(substr(names(ir.data),1,2)=="mm")]
# Stop the function and return an error message if any of the required variables are missing
  required.fields<-c("v001","v005","v008","v011","v015","v016","mmidx.01","mm1.01","mm2.01","mm3.01","mm4.01","mm6.01","mm7.01","mm8.01","mm9.01")
  check.missing.fields<-required.fields %in% names(ir.data)
  if(length(check.missing.fields[which(check.missing.fields==FALSE)])>0){
    stop("The individual recode file is missing the following required fields and cannot be processed: ",required.fields[which(check.missing.fields==FALSE)])
  }
  
# wrangle data
ir.data <- ir.data %>%
           filter(v015==1) %>%
           bind_cols(mother = c(1:nrow(.)), .) %>%
           mutate(v008 = v008 + v016/31, # add day of month to cmc interview date
                  v008 = ifelse(is.na(v008), ((RefYr-1900)*12) + RefMo + RefDay/31, v008), # if interview date is missing then use survey reference year
                  mm1.0 = c(rep(2,nrow(.))), # create a set of mm (sibling vars) with characteristics of the respondent herself
                  mm2.0 = c(rep(1,nrow(.))),
                  mm3.0 = floor((v008-v011)/12),
                  mm4.0 = v011,
                  mm6.0 = c(rep(NA,nrow(.))),
                  mm7.0 = c(rep(NA,nrow(.))),
                  mm8.0 = c(rep(NA,nrow(.))),
                  mm9.0 = c(rep(NA,nrow(.))),
                  mmidx.0 = c(rep(0,nrow(.)))) %>%
          select_if(grepl("mother|v001|v005|v008|mm1[[:punct:]]|mm2[[:punct:]]|mm3[[:punct:]]|mm4[[:punct:]]|mm6[[:punct:]]|mm7[[:punct:]]|mm8[[:punct:]]|mm9[[:punct:]]|mmidx",names(.)) & !(grepl("v008a",names(.)))) %>%
          gather("variable","value",-c(mother,v001,v005,v008)) %>%
          separate(variable,c("variable","sibling")) %>%
          mutate(sibling = as.numeric(sibling)) %>%
          spread(variable,value) %>%
          filter(!is.na(mmidx)) %>% # eliminate records for non-existent siblings
          mutate(randomday = (runif(nrow(.), min=0, max=30))/31, # assign random days to century-month coded dobs and dods
                 dob = mm4 + randomday,
                 dod = mm8 + randomday,
                 dod = ifelse(mm4==mm8, dob + (runif(nrow(.),min=randomday,max=30))/31, dod), # if sibling died in first month of life then assign date of death between dob and end of month
                 dod = ifelse(dod>v008, v008, dod), # if interview date was before date of death then set date of death equal to interview date
                 dob = ifelse(dob < v008 - (50*12) & mmidx==0, v008 - (50*12), dob),# if dob is greater than 50 years before interview date and record is for respondent then set dob equal to exactly 50 years before interview date
                 dob = ifelse(dob > v008 - (15*12) & mmidx==0, v008 - (15*12), dob), # if dob is less than 15 years before interview date and record is for respondent then set dob equal to exactly 15 years before the interview date
                 agelast = trunc((v008 - dob)/12),
                 agelast = ifelse(!is.na(dod), trunc((dod-dob)/12), agelast),
                 age5last = trunc(agelast/5),
                 num5yrperiods = age5last+1) %>%
  select(-c(mm4,mm8,randomday,sibling)) %>%
  arrange(mother)

# look for missing dates of birth and exclude these records if any
test<-length(ir.data$mother[which(is.na(ir.data$dob))])
if (test>0){
  message(paste0("The dates of birth of ",test, " respondents or siblings are missing. These records have been excluded from the output person-period data frame."))
}
ir.data <- ir.data %>%
  filter(!is.na(dob))

# expand to one record per sibling per 5-year period of life
sib.pd<-bind_cols(count = unlist(lapply(ir.data$num5yrperiods, function(i){1:i})),
                ir.data[rep(1:nrow(ir.data),times=ir.data$num5yrperiods),]) %>%
  mutate(age5 = count -1,
         age = factor(age5, 
                      levels = c(0:20), 
                      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "+100"))) %>%
           select(-c(count,mm2,mm3,mm6,mm7,agelast,age5last,num5yrperiods))

return(sib.pd)

}
