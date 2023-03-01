#' Estimate adult and maternal mortality indicators by 5-year age group and for the full age range 15-49
#'
#' @param PreppedSibData the dataframe output by the function PreppedforSiblingsDHS
#' @param include.Respondent A logical object indicating whether the respondent should be included among the siblings for estimation of mortality (default=FALSE)
#' @param impute.Sex A logical object indicating whether sex should be imputed for observations with missing sex information
#' @param PregRelCode A vector object of codes associated with pregnancy-related deaths (see freq table for mm9 codes)
#' @param recall.back An integer object indicating the number of years before the survey date to open the reference period for mortality estimation.
#' @param recall.end An integer object indicating the number of years before the survey date to close the reference period for mortality estimation
#' @param stderror A logical object indicating whether to calculate jackknife standard errors
#' @param age.standardize A logical object indicating whether to standardize 35m15 and pmdf for ages 15-49 according to the age distribution of the survey population
#' @param pr.recode A character object with the file path of the household member recode file (pr file) in Stata format to use for age standardization
#'
#' @return A data frame with male and female adult mortality rates by 5-year age group, q5s and 35q15s, 
#   maternal mortality rates, pmdf (proportion maternal of deaths to females) and log(pmdf).  
#   Jackknife standard errors are calculated by removing clusters from the
#   data one at a time and recalculating the estimates, then calculating the variance of the estimates observed
#   with one cluster removed.


SiblingSurvival <- function(PreppedSibData, 
                            include.Respondent=FALSE, 
                            impute.Sex=FALSE, 
                            PregRelCode=c(2,3,6),
                            recall.back=7, 
                            recall.end=0, 
                            stderror=FALSE, 
                            age.standardize=FALSE,
                            pr.recode=NULL) {
  
# 
# Author: Sara Hertog
# Written: 20 February 2014
# Last revised: 8 September 2019 to streamline code with tidyverse and dplyr
#
# Example:
# out <- SiblingSurvival (PreppedSibData = df, 
#                         include.Respondent = FALSE, 
#                         impute.Sex = FALSE, 
#                         PregRelCode = c(2,3,6),
#                         recall.back = 7, 
#                         recall.end = 0, 
#                         stderror = TRUE, 
#                         age.standardize = TRUE,
#                         pr.recode = "ZZPR62FL.DTA")

  # load foreign package to enable r to read Stata files
  # List of packages for session
  .packages = c("foreign","dplyr","tidyr")
  
  # Install CRAN packages (if not already installed)
  .inst <- .packages %in% installed.packages()
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)

df<-PreppedSibData

if (impute.Sex==TRUE){
  
  # DHS standard procedure is to impute a value for missing information on siblings'sex from
  # binomial random variate generation with 1 trial and 0.5 probability of success per trial
  n.imp.sex<-unique(df[is.na(df$mm1) | df$mm1>2, c("mother","mmidx","mm1")])
  if (length(n.imp.sex$mother)>0){
    print(paste0("WARNING: For ",length(n.imp.sex$mother)," siblings (",
                 round(length(n.imp.sex$mother)/nrow(unique(df[,c("mother","mmidx")]))*100,2),
                 "% of all siblings reported), missing sex has been imputed using binomial random variate generation with 1 trial and 0.5 probability of success per trial. To exclude these sibling from mortality estimation, select impute.Sex=FALSE."))
  }
  set.seed(100)
  df <- df %>%
    mutate(sex.imp = is.na(mm1) | mm1>2,
           mm1 = ifelse(is.na(mm1) | mm1>2, 1+(rbinom(nrow(.), 1, 0.5)), mm1))
}

# identify deaths and exposures that occur within the recall period defined by recall.back and recall.end
df <- df %>%
  filter(age5>=3 & age5<=9) %>% # keep only periods of exposure between ages 15 and 49
  mutate(sex=ifelse(mm1 %in% c(1,2), mm1, NA),
         refpd.begin = v008 - recall.back*12,
         refpd.end = v008 - recall.end*12,
         age5.lower = age5*5,
         age5.upper = age5.lower+5,
         enter.age5 = dob + age5.lower*12,
         exit.age5 = dob + age5.upper*12,
         death = ifelse(!is.na(dod) & dod>enter.age5 & dod>refpd.begin & dod<=exit.age5 & dod<=refpd.end, 1, 0),
         exp.start = pmax(enter.age5,refpd.begin),
         exp.end = ifelse(death==0, pmin(exit.age5,refpd.end), dod),
         exposure = (exp.end-exp.start)/12,
         death.mat = death,
         death.mat = ifelse(death>0 & !is.na(mm9) & mm9 %in% PregRelCode, 1, death.mat), # deaths with code indicating pregnancy-related
         death.mat = ifelse(death>0 & !is.na(mm9) & !(mm9 %in% PregRelCode), 0, death.mat), # deaths with code indicating pregnancy-related
         death.mat = ifelse(death>0 & (is.na(mm9) | mm9>90), NA, death.mat), # missing cause of death codes set to NA
         death.mat = ifelse(sex==1, NA, death.mat), # all males set to NA
         death = death*v005/1000000,
         exposure.unw = exposure,
         exposure = exposure*v005/1000000,
         death.mat = death.mat*v005/1000000,
         exposure.mat = ifelse(!is.na(death.mat), exposure, NA)) %>%
  filter(!is.na(sex) & exposure>0) %>%
  select(c("v001","mmidx","age5","sex","death","exposure","exposure.unw","death.mat","exposure.mat"))

# exclude index case for standard unadjusted estimate of exposure
if (include.Respondent==FALSE){
  df  <-  filter(df, mmidx > 0)
  } 


# and tabulate adult females by 5-year age group
if (age.standardize==TRUE){
  # get hh population distribution from hh member roster file
  pop.dist  <-  read.dta(pr.recode, convert.factors=F, convert.underscore=T) %>%
    select(c(hv005,hv104,hv105)) %>%
    filter((hv105>=15 & hv105<=49)) %>%
    mutate(Age = trunc(hv105/5)) %>%
    group_by(hv104,Age) %>%
    summarise(wts = sum(hv005)/1000000) %>%
    mutate(prop = prop.table(wts))
}

# this function sums all-cause and maternal deaths and exposures within the reference period by sex and 5-year age group
# and then calculates mortality rates, qx and pmdf for 5-year age group as well as the full age range 15-49
EstimateAdultMortality <- function(indata, age.standardize=FALSE, pop.dist=NULL){
  # summarise by five year age group
  out.data <- indata %>%
    group_by(sex,age5) %>%
    summarise(death = sum(death,na.rm=TRUE),
              exposure = sum(exposure,na.rm=TRUE),
              exposure.unw = sum(exposure.unw,na.rm=TRUE),
              death.mat = sum(death.mat,na.rm=TRUE),
              exposure.mat = sum(exposure.mat,na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(mx = death/exposure,
           qx = (10*mx)/(2+mx), # from Croft (2011) pg 27
           px = 1-qx,
           mx.mat = death.mat/exposure.mat,
           pmdf = mx.mat/mx*100,
           Age = rep(c(seq(15,45,5)),2),
           AgeInt = rep(5,14)) 
  
  # summarise for age 15-49
  df1549<-NULL
  df1549 <- out.data %>%
    group_by(sex) %>%
    summarise(death = sum(death),
              exposure = sum(exposure),
              exposure.unw = sum(exposure.unw),
              death.mat = sum(death.mat),
              exposure.mat = sum(exposure.mat),
              px = min(cumprod(px))) %>%
    ungroup() %>%
    mutate(mx = death/exposure,
           qx = 1-px,
           mx.mat = death.mat/exposure.mat,
           pmdf = mx.mat/mx*100,
           Age = c(15,15),
           AgeInt = c(35,35)) %>%
    select(c("Age","AgeInt","sex","death","exposure","exposure.unw","mx","qx","mx.mat","pmdf"))
  
  # adjust 35m15 and pmdf to age distribution of survey population
  if (age.standardize==TRUE){
    df1549$mx[which(df1549$sex==1)]<-sum(out.data$mx[which(out.data$sex==1)]*pop.dist$prop[which(pop.dist$hv104==1)])
    df1549$mx[which(df1549$sex==2)]<-sum(out.data$mx[which(out.data$sex==2)]*pop.dist$prop[which(pop.dist$hv104==2)])
    df1549$mx.mat[which(df1549$sex==2)]<-sum(out.data$mx.mat[which(out.data$sex==2)]*pop.dist$prop[which(pop.dist$hv104==2)])
    df1549$pmdf[which(df1549$sex==2)]<-(sum(out.data$mx.mat[which(out.data$sex==2)]*pop.dist$prop[which(pop.dist$hv104==2)])/
                                          sum(out.data$mx[which(out.data$sex==2)]*pop.dist$prop[which(pop.dist$hv104==2)]))*100
  }
  
  out.data <- out.data %>%
    select(c("sex","Age","AgeInt","death","exposure","exposure.unw","death.mat","exposure.mat","mx","qx","mx.mat","pmdf")) %>%
    bind_rows(df1549) %>%
    mutate(log.pmdf = log(pmdf)) %>%
    gather("Indicator","Estimate",4:13) %>%
    filter(!is.na(Estimate)) %>%
    arrange(Indicator,sex,AgeInt,Age)
  
  
  return(out.data)
}


# run EstimateAdultMortality function on full sample to get estimates
out.full<-EstimateAdultMortality(df,age.standardize=age.standardize,pop.dist=pop.dist)

# if jackknife standard errors are desired, loop through the dataset once for each cluster and re-do
# calculations removing one cluster at a time (without replacement)
if (stderror==TRUE){
  
  out.clusters<-NULL
  
  # summarise by cluster, age and sex
  dfc <- df %>%
    group_by(v001,age5,sex) %>%
    summarise(death = sum(death, na.rm=TRUE),
              exposure = sum(exposure, na.rm=TRUE),
              exposure.unw = sum(exposure.unw, na.rm=TRUE),
              death.mat = sum(death.mat, na.rm=TRUE),
              exposure.mat = sum(exposure.mat, na.rm=TRUE)) 
  
  # loop through clusters, removing one at a time and re-estimating adult mortality indicators
  clusters<-unique(dfc$v001)
  
  for (i in 1:length(clusters)){
    use.dfc <- filter(dfc, v001 != clusters[i])
    out.temp <- EstimateAdultMortality(use.dfc,age.standardize=age.standardize,pop.dist=pop.dist)
    out.temp$omitted.cluster<-clusters[i]
    out.clusters <- rbind(out.clusters,out.temp)
  }
  
  out.clusters<-out.clusters %>%
    rename(Jack=Estimate) %>%
    left_join(out.full,by=c("Indicator","sex","AgeInt","Age")) %>%
    mutate(forjack = (Jack - Estimate)^2) %>%
    group_by(Indicator,sex,AgeInt,Age) %>%
    summarise(jack=sum(forjack)) %>%
    mutate(SE = (((length(clusters)-1)/length(clusters))*(jack))^(1/2)) %>%
    select(-jack)
  
  out.full<-left_join(out.full,out.clusters,by=c("Indicator","sex","AgeInt","Age"))
  out.full$SE[which(out.full$Indicator %in% c("death","exposure","exposure.unw","death.mat","exposure.mat"))]<-NA
}

return(out.full)
}