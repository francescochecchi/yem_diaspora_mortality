#..........................................................................................
### +++++++++++ SAMPLE SIZE SIMULATION FOR YEMENI DIASPORA MORTALITY STUDY ++++++++++++ ###
#..........................................................................................

                                         # Written by Francesco Checchi, LSHTM (Dec 2020)
                                          # francesco.checchi@lshtm.ac.uk 


#..........................................................................................
### Preparatory steps
#.........................................................................................

  #...................................      
  ## Install or load required R packages

    # List of required packages
    x1 <- c("ggplot2", "ggpubr", "gtools", "lubridate", "NCmisc", "RColorBrewer", "readxl", "reshape2", "scales", "zoo")
    
    # Install any packages not yet installed
    x2 <- x1 %in% row.names(installed.packages())
    if (any(x2 == FALSE)) { install.packages(x1[! x2]) }

    # Load all packages    
    lapply(x1, library, character.only = TRUE)
    
  #...................................      
  ## Starting setup

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
    show_col(palette_cb)

#.........................................................................................
### Specifying parameters and uncertainty distributions
#.........................................................................................

  #...................................      
  ## RDS survey parameters
    
    # Number of respondents
    n_respondents <- seq(100, 200, by = 100)
    
    # Attrition (proportion of unusable responses)
    attrition <- 0.20
    
    # Number of siblings per respondent
    mean_siblings <- 6 # WPP: mean fertility per woman about 30 years ago
    sd_siblings <- 1
        
    # Proportion of siblings still in Yemen
    prop_yemen <- 0.50
    
  #...................................      
  ## Yemen demographic parameters
    
    # Proportion of population aged 15-49yo
    prop_15to49 <- 0.48 # World Population Prospects (WPP)
    
    # Crude birth rate per capita per month
    cbr <- 0.004 # World Population Prospects (WPP)
    
    # Neonatal mortality ratio per live birth (pre-crisis)
    nmr <- 0.0027 # Unicef
    
    # Infant mortality ratio per live birth (pre-crisis)
    imr <- 0.043 # WPP
      
    # Under 5yo mortality ratio per live birth (pre-crisis), or 5q0
    u5mr <- 0.055 # WPP
  
    # Factor by which mortality might be different in the sample, compared to the general population
    mortality_diff <- c(0.3333, 0.6667, 1.0000)
    
  #...................................      
  ## Sample size parameters
    
    # Duration of burn-in period (in months) - needed to reach equilibrium population
    t_burnin <- 120
    
    # Duration of pre-crisis period (in months)
    t_baseline <- 48
    
    # Duration of crisis period (in months)
    t_crisis <- 48 # eliminate COVID-19 year (2020)
    
    # Minimum relative increase in mortality (crisis vs baseline) that we wish to detect with given power and significance
    increase <- seq(0.10, 0.50, by = 0.10)
    
    # Significance level
    alpha <- 0.05
    
    # Desired power
    power_needed <- 0.80
    
    # Number of simulations per scenario
    n_sim <- 10

#.........................................................................................
### Bespoke functions
#.........................................................................................
    
  #...................................      
  ## Set up time series and age groups depending on length of period
  f_ts <- function(t_burnin, t_baseline, t_crisis)  {
    
    # Month
    ts <- data.frame ("month" = c(1:(t_burnin + t_baseline + t_crisis)))
    
    # Period
    ts[, "period"] <- c(rep("burnin", t_burnin), rep("baseline", t_baseline), rep("crisis", t_crisis))
    
    # Children alive in each age group (month of life)
    ts[, paste(c(0:59))] <- 0
    
    # Children born during each month
    ts[ "born"] <- NA
    
    # Children dead during each month
    ts[, "dead"] <- NA
    
    # Return
    return(ts)
  }
       
    
  #...................................      
  ## compute monthly mortality risk from NMR, IMR and U5MR
  f_xmr <- function(nmr, imr, u5mr) {
    
    # first 60 ages of life
    ts_xmr <- data.frame("age" = c(0:59), "mr" = NA)
    
    # NMR data point (probability of dying in first month)
    ts_xmr[ts_xmr$age == 0, "mr"] <- nmr

    # (IMR - NMR) data point (post-natal mortality probability per month)
    pnmr <- 1 - ((1 - imr) / (1 - nmr))^(1/11)
    ts_xmr[ts_xmr$age %in% c(1:11), "mr"] <- pnmr

    # (U5MR - IMR) data point (child mortality probability per month or 4q0)
    cmr <- 1 - ((1 - u5mr) / (1 - imr))^(1/48)
    ts_xmr[ts_xmr$age %in% c(12:59), "mr"] <- cmr

    # check that U5MR is as expected
    ts_xmr[, "surv"] <- 1 - ts_xmr$mr
    print("check that U5MR is as expected:")
    print(1- prod(ts_xmr$surv) )
    
    # output
    return(ts_xmr[, c("age", "mr")])
      
  }
    

  #...................................      
  ## Simulate population in all age groups through time series (a single run)
  f_sim <- function(n_respondents, attrition, mean_siblings, sd_siblings, prop_yemen, 
    cbr, prop_15to49, ts, ts_xmr, mortality_diff, increase, alpha, want_plot) {

    # Establish random number of siblings
    n_siblings <- as.integer(round(rnorm(as.integer(round(n_respondents * (1 - attrition), digits = 0)), 
      mean = mean_siblings, sd = sd_siblings) * prop_yemen, digits = 0))
    n_siblings <- sum(n_siblings)
    
    # Set birth rate and mortality risks
    xbr <- cbr / prop_15to49 # assumes all births occur in this age group
    xmr <- ts_xmr$mr * mortality_diff
    xmr_crisis <- xmr * (1 + increase)
    
    # For each month in the time series...
    for (i in 1:nrow(ts) ) {
      
      # establish which period we're in
      period <- ts[i, "period"]
      
      # apply mortality risks through binomial probabilities
      dead_now <- rbinom(n = length(xmr), size = unlist(ts[i, paste(c(0:59))]), 
        prob = ifelse(period == "crisis", xmr_crisis, xmr) )
      if (i < nrow(ts) ) {ts[i+1, paste(c(0:59))] <- ts[i, paste(c(0:59))] - dead_now}
      ts[i, "dead"] <- sum(dead_now, na.rm = TRUE)
      
      # age population by one month
      if (i < nrow(ts) ) {ts[i+1, paste(c(1:59))] <- ts[i, paste(c(0:58))] }
      
      # new births
      ts[i, "born"] <- sum(rpois(n_siblings, xbr))
      if (i < nrow(ts) ) {ts[i+1, "0"] <- ts[i, "born"] }
      
    }
    
    # Plot time series if desired
    if (want_plot == TRUE) {
      ts[, "alive"] <- rowSums(ts[, paste(c(0:59))], na.rm = TRUE)
      pl <- ggplot(ts) +
        theme_bw() +
        annotate("rect", xmin = min(ts[ts$period == "baseline", "month"]),
          xmax = max(ts[ts$period == "baseline", "month"]), ymin = 0, ymax = Inf, fill = "seagreen4", alpha = 0.2) +
        annotate("rect", xmin = max(ts[ts$period == "baseline", "month"]), 
          xmax = max(ts[ts$period == "crisis", "month"]), ymin = 0, ymax = Inf, fill = "red", alpha = 0.2) +
        geom_line(aes(x = month, y = alive), colour = palette_cb[4], size = 1, alpha = 0.5) +
        geom_line(aes(x = month, y = born), colour = palette_cb[6], size = 1, alpha = 0.5) +
        geom_line(aes(x = month, y = dead), colour = palette_cb[7], size = 1, alpha = 0.5) +
        geom_point(aes(x = month, y = alive), colour = palette_cb[4], size = 1) +   
        geom_point(aes(x = month, y = born), colour = palette_cb[6], size = 1) +        
        geom_point(aes(x = month, y = dead), colour = palette_cb[7], size = 1)
      print(pl)      
    }

    # Aggregate births and deaths by period and prepare matrix for test
    tab <- aggregate(ts[, c("dead", "born")], by = list("period" = ts[, "period"]), FUN = sum)
    tab <- subset(tab, period != "burnin")
    tab <- cbind(as.vector(tab[tab$period == "baseline", c("dead", "born")]),
      as.vector(tab[tab$period == "crisis", c("dead", "born")]) )
    tab <- matrix(as.integer(tab), nrow = 2)
    
    # Test significance of difference
    out <- fisher.test(tab, alternative = "less", conf.level = 1 - alpha)
    
    # Output test p-value
    return(out$p.value)
  }
  
      
#.........................................................................................
### Implement simulation
#.........................................................................................
    
  #...................................      
  ## Set up time series and mortality risks
  ts <- f_ts(t_burnin, t_baseline[1], t_crisis)      
  ts_xmr <- f_xmr(nmr, imr, u5mr)
  
  #...................................      
  ## Run one simulation (try)
  f_sim(n_respondents[1], attrition, mean_siblings, sd_siblings, prop_yemen, cbr, prop_15to49, 
    ts, ts_xmr, mortality_diff[1], increase[5], alpha, TRUE)

  #...................................      
  ## Generate all possible scenarios
  scenarios <- expand.grid(n_respondents, mortality_diff, t_baseline, increase)
  colnames(scenarios) <- c("n_respondents", "mortality_differential", "baseline_period", "increase_crisis")
  scenarios[, "power"] <- NA
  scenarios[, paste(1:n_sim)] <- NA
  
  #...................................      
  ## Do simulations for each scenario
  for (i in 1:nrow(scenarios)) {
    
    # Control statement
    print(paste("now working on scenario ", i, " of ", nrow(scenarios), sep = ""))

    # Set up scenario output
    out_sim <- rep(NA, n_sim)
    
    # Set up variable parameters
    n_respondents_sim <- scenarios[i, "n_respondents"]
    mortality_diff_sim <- scenarios[i, "mortality_differential"]
    increase_sim <- scenarios[i, "increase_crisis"]

    # Replicate simulations
    for (j in 1:n_sim) {
      loop.tracker(j, n_sim)  
      out_sim[j] <- f_sim(n_respondents_sim, attrition, mean_siblings, sd_siblings, prop_yemen, 
        cbr, prop_15to49, ts, ts_xmr, mortality_diff_sim, increase_sim, alpha, FALSE)
    }
    scenarios[i, paste(1:n_sim)] <- out_sim

    # Calculate power
    scenarios[i, "power"] <- length(out_sim[out_sim < alpha]) / length(out_sim)
    
  }

  
  #...................................      
  ## Graph output
  
    # Prepare data
    x1 <- scenarios[, c("n_respondents", "power", "mortality_differential", "increase_crisis")]
    x1[, "mortality_differential"] <- factor(x1[, "mortality_differential"])
    x1[, "increase_crisis"] <- factor(x1[, "increase_crisis"])
    x1[, "increase_crisis"] <- paste("u5mr increased by ", x1[, "increase_crisis"], sep = "")
    
    # Plot
    pl <- ggplot(x1, aes(x = n_respondents, y = power, colour = mortality_differential)) +
      geom_path(size = 1.5, alpha = 0.5) +
      scale_colour_manual("mortality differential (diaspora vs Yemen)", 
        values = palette_cb[2:(length(unique(x1$mortality_differential)) + 1)] ) +
      facet_wrap(increase_crisis ~ .) +
      theme_bw() +
      theme(legend.position = "bottom")
    print(pl)
    
    # Save plot
    ggsave("yemen_diaspora_u5mr_sampsi.png", width = 30, height = 20, units = "cm", dpi = "print")
      
  #...................................      
  ## Write output
  write.csv(scenarios[, c("n_respondents", "mortality_differential", "baseline_period", "increase_crisis", "power")],
    "yemen_diaspora_u5mr_sampsi.csv", row.names = FALSE)

        
#.........................................................................................
### ENDS
#.........................................................................................
   
    