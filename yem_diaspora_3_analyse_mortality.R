#..........................................................................................
### ++++ RESPONDENT-DRIVEN SAMPLING MORTALITY SURVEY OF THE YEMENI DIASPORA (2022) ++++ ###
#..........................................................................................

#..........................................................................................
## ---------------- R SCRIPT TO ANALYSE MORTALITY PATTERNS IN THE SAMPLE --------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Jan 2023)
                                          # francesco.checchi@lshtm.ac.uk 


    
#.........................................................................................      
### Estimating older age survival - exposure of interest: period (pre-war, war and pandemic)

  #...................................
  ## Read data if not already in environment
  if (! exists("resp")) {resp <- read.csv("out_respondent_data.csv") }
  if (! exists("all")) {all <- read.csv("out_individual_data.csv") }
    
  #...................................      
  ## Function to select and prepare data based on desired age cutoff and whether parents or all adults are included
  f_prep <- function(all, age_cutoff, types, date_knots) {
    # Any people in the sample who reached age age_cutoff and have known status
    adults <- subset(all, (aliveage >= age_cutoff | died_age >= age_cutoff) & status != "unknown" & type %in% types)
  
    # Generate dates of birth, entry (age cutoff birthday) and exit from the cohort
      # first date of exit
      adults[, "date_exit"] <- as.Date(ifelse(! is.na(adults$died_date), adults$died_date, adults$today))

      # then date of entry - if >= date of exit, correct so it is at midpoint between 1 Jan of that year and date of exit
      adults[, "date_entry"] <- adults$today - (adults$age - age_cutoff) * 365.25
      x1 <- which(adults$date_entry >= adults$date_exit)      
      adults[x1, "date_entry"] <- adults[x1, "date_exit"] - 
        (adults[x1, "date_exit"] - ymd(paste(year(adults[x1, "date_exit"]), "01", "01", sep = "-")) ) / 2
      
      # then date of birth
      adults[, "date_birth"] <- adults$date_entry - age_cutoff * 365.25

    # Generate 10-year birth cohort
      # range of birth dates
      x1 <- range(adults$date_birth)
  
      # breaks are based on range
      x2 <- ymd(c(x1[1], "1920-01-01", "1930-01-01", "1940-01-01", "1950-01-01", "1960-01-01", "1970-01-01", x1[2] + 1))
      x2 <- x2[x2 < (x1[2] - 10 * 365.25)] # eliminate breaks that are too close or above the maximum date of birth
      x2 <- x2[x2 > (x1[1] + 10 * 365.25)] # eliminate breaks that are too close or above the maximum date of birth
      x2 <- c(x1[1] - 1, x2, x1[2] + 1)
      
      # labels are defined accordingly
      x3 <- c()
      for (i in 1:(length(x2) - 1)) {x3[i] <- paste(year(x2[i]), "to", year(x2[i+1]))}
      
      adults[, "birth_cohort"] <- cut(adults$date_birth, breaks = x2, labels = x3, include.lowest = TRUE, right = TRUE)    
      table(adults$birth_cohort)

    # Generate a unique ID for each adult
    adults[, "adult_id"] <- paste(adults$resp_id, substr(adults$relative_of, 1, 1), substr(adults$gender, 1, 1),
      adults$sibling_id, sep = "_")

    # Eliminate unneeded columns
    x1 <- c("resp_id", "sibling_id", "type", "relative_of", "child_of", "order", "died_year", "died_month", "died_governorate",
      "died_cause", grep("childs_adult", colnames(adults), value = TRUE), "died_month_int", "aliveage_cat",
      "died_age_cat", "age_cat", "died_period")
    adults <- adults[, ! colnames(adults) %in% x1]
    
    # Convert dataset into 1 row = 1 period for each person (periods: pre-war, war, pandemic)
      # append three identical copies of the dataset, one for each period
      df <- rbind(adults, adults, adults)
      df <- df[order(df$adult_id), ]
      df[, "period"] <- rep(c("pre-war", "war", "pandemic"), times = nrow(adults))
      df[, "period"] <- factor(df$period, levels = c("pre-war", "war", "pandemic"))
      
      # identify observations for each period
      x1 <- which(df$period == "pre-war")
      x2 <- which(df$period == "war")
      x3 <- which(df$period == "pandemic")
      
      # determine age in days at start and end of each period
      df[, c("age_start", "age_end")] <- NA
        # pre-war period
        df[x1, "age_start"] <- ifelse(df[x1, "date_entry"] >= date_knots[1], NA, age_cutoff * 365.25)
        df[x1, "age_end"] <- ifelse(df[x1, "date_entry"] >= date_knots[1], NA,
          age_cutoff * 365.25 + pmin(df[x1, "date_exit"], date_knots[1] - 1) - df[x1, "date_entry"])
        
        # war period
        df[x2, "age_start"] <- ifelse(df[x1, "date_entry"] >= date_knots[2], NA, 
          pmax(age_cutoff * 365.25, age_cutoff * 365.25 + date_knots[1] - df[x2, "date_entry"] ) )
        df[x2, "age_end"] <- ifelse(df[x2, "date_entry"] >= date_knots[2], NA,    
          age_cutoff * 365.25 + pmin(df[x2, "date_exit"], date_knots[2] - 1) - df[x2, "date_entry"])
        
        # pandemic period
        df[x3, "age_start"] <- pmax(age_cutoff * 365.25, age_cutoff * 365.25 + date_knots[2] - df[x3, "date_entry"] )
        df[x3, "age_end"] <- age_cutoff * 365.25 + df[x3, "date_exit"] - df[x3, "date_entry"]
   
      # determine dates at start and end of each period
      df[, c("date_start", "date_end")] <- NA
        # pre-war period
        df[x1, "date_start"] <- ifelse(df[x1, "date_entry"] >= date_knots[1], NA, df[x1, "date_entry"] )
        df[x1, "date_end"] <- ifelse(df[x1, "date_entry"] >= date_knots[1], NA,
          pmin(df[x1, "date_exit"], date_knots[1] - 1))
        
        # war period
        df[x2, "date_start"] <- ifelse(df[x1, "date_entry"] >= date_knots[2], NA, 
          pmax(df[x1, "date_entry"], date_knots[1] ) )
        df[x2, "date_end"] <- ifelse(df[x2, "date_entry"] >= date_knots[2], NA,    
          pmin(df[x2, "date_exit"], date_knots[2] - 1))
        
        # pandemic period
        df[x3, "date_start"] <- pmax(df[x1, "date_entry"], date_knots[2] )
        df[x3, "date_end"] <- df[x3, "date_exit"]
       
      # reconstitute date format
      df[, "date_start"] <- as.Date(df[, "date_start"])     
      df[, "date_end"] <- as.Date(df[, "date_end"])     
        
      # eliminate period observations for which the person spent no time in the period
      df <- subset(df, ! is.na(age_start))
      df <- subset(df, age_end >= age_start)
   
      # identify in which period death occurs, if at all
        # figure this out in adults dataset...
        adults[, "period"] <- NA
        x1 <- which(adults$status == "deceased")
        adults[which(adults$died_date %in% c(ymd("1900-01-01") : (date_knots[1] - 1)) ), "period"] <- "pre-war"
        adults[which(adults$died_date %in% c(date_knots[1] : (date_knots[2] - 1)) ), "period"] <- "war"
        adults[which(adults$died_date %in% c(date_knots[2] : ymd("2100-01-01")) ), "period"] <- "pandemic"
    
        #...then merge into survival dataset
        x1 <- subset(adults, ! is.na(period))
        x1[, "died"] <- 1
        x1 <- x1[, c("adult_id", "period", "died")]
        df <- merge(df, x1, by = c("adult_id", "period"), all.x = TRUE)  
        df[which(is.na(df$died)), "died"] <- 0
        df <- df[order(df$adult_id, df$age_start), ]
        setdiff(adults$adult_id, unique(df$adult_id))
        table(df$died)

    # Final variable preparations  
      # create simpler period exposure (pre-war, war)
      df[, "period_simple"] <- df$period
      df[which(df$period_simple == "pandemic"), "period_simple"] <- "war"
      df[, "period_simple"] <- factor(df$period_simple, levels = c("pre-war", "war"))
      table(df$period_simple)    

      # create death outcome that is restricted to deaths in Yemen (other deaths are treated as censored observations)
      df[, "died_yemen"] <- ifelse(df$died == 1 & df$died_yemen == "yes", 1, 0)
      table(df$died_yemen)
        
      # set age back by age_cutoff years
      df[, c("age_start", "age_end")] <- df[, c("age_start", "age_end")] - age_cutoff * 365.25
      
      # make adult id a bit simpler
      x1 <- sort(unique(df$adult_id))
      x1 <- data.frame("adult_id" = x1, "id" = paste("id", 1:length(x1), sep = "") )
      df <- merge(df, x1, by = "adult_id", all.x = TRUE)
    
    # Output prepared dataset
    return(df)
  }  

  
  #...................................      
  ## Plot survival over time, by birth cohort and gender
    
    # Prepare dataset
    df <- f_prep(all, 50, c("parent"), date_knots)
  
    # Kaplan-Meier survival curve by birth cohort
    fit_km_birth <- survfit(Surv(as.numeric(date_start) / 365.25 + 1970, as.numeric(date_end) / 365.25 + 1970, died) ~ 
        birth_cohort, data = df)  
    plot1 <- ggsurvplot(fit_km_birth, conf.int = TRUE, break.time.by = 5, xlim = c(1990, 2023),
      conf.int.alpha = 0.1, palette = palette_cb[c(7, 6, 8, 4)],
      xlab = "Year", legend.labs = levels(df$birth_cohort), legend.title = "birth cohort:",
      font.x = c(11, "plain", "grey20"), font.y = c(11, "plain", "grey20"), font.tickslab = c(11, "plain", "grey20"))
    plot1 <- plot1$plot + geom_vline(xintercept = c(as.numeric(date_knots[1]) / 365.25 + 1970,
      as.numeric(date_knots[2]) / 365.25 + 1970), colour = "grey20", linetype = "longdash") +
      annotate(geom = "text", hjust = 0, x = c(as.numeric(date_knots[1] + 60) / 365.25 + 1970,
      as.numeric(date_knots[2] + 60) / 365.25 + 1970), y = c(0, 0), label = c("war", "pandemic"))
    plot1
      
    # Kaplan-Meier survival curve by gender
    fit_km_gender <- survfit(Surv(as.numeric(date_start) / 365.25 + 1970, as.numeric(date_end) / 365.25 + 1970, died) ~ 
        gender, data = df)  
    plot2 <- ggsurvplot(fit_km_gender, conf.int = TRUE, break.time.by = 5, xlim = c(1990, 2023),
      conf.int.alpha = 0.1, palette = palette_cb[c(7, 6)],
      xlab = "Year", legend.title = "gender:",
      font.x = c(11, "plain", "grey20"), font.y = c(11, "plain", "grey20"), font.tickslab = c(11, "plain", "grey20"))
    plot2 <- plot2$plot + geom_vline(xintercept = c(as.numeric(date_knots[1]) / 365.25 + 1970,
      as.numeric(date_knots[2]) / 365.25 + 1970), colour = "grey20", linetype = "longdash") +
      annotate(geom = "text", hjust = 0, x = c(as.numeric(date_knots[1] + 60) / 365.25 + 1970,
      as.numeric(date_knots[2] + 60) / 365.25 + 1970), y = c(0, 0), label = c("war", "pandemic"))
    plot2

    # Combine plots
    plot <- ggarrange(plot1, plot2, ncol = 2)
    plot
    ggsave("out_survival_elderly_combi.png", units = "cm", dpi = "print", width = 33, height = 15)
    
  #...................................      
  ## Fit Cox proportional hazards model
      # (outcome = died/died_yemen; age = time unit; period = exposure; gender, birth cohort = confounders)
    
    # Prepare dataset for model
    df <- f_prep(all, 50, c("parent"), date_knots)
  
    # Model 1 (exposure = war or pandemic periods; outcome = death)
    fit_fe <- coxph(Surv(age_start, age_end, died) ~ birth_cohort + gender + period , data = df)          
    fit_fe
    length(unique(df$id)) # number of individuals at risk
    broom::tidy(fit_fe, exponentiate= TRUE)
    cox.zph(fit_fe)
    plot(cox.zph(fit_fe)[3])

    # Model 2 (exposure = war or pandemic periods; outcome = death in Yemen)
    fit_fe <- coxph(Surv(age_start, age_end, died_yemen) ~ birth_cohort + gender + period , data = df)          
    fit_fe
    length(unique(df$id)) # number of individuals at risk
    broom::tidy(fit_fe, exponentiate= TRUE)
    cox.zph(fit_fe)
    plot(cox.zph(fit_fe)[3])

    # Model 3 (exposure = war and pandemic period; outcome = death)
    fit_fe <- coxph(Surv(age_start, age_end, died) ~ birth_cohort + gender + period_simple , data = df)          
    fit_fe
    length(unique(df$id)) # number of individuals at risk
    broom::tidy(fit_fe, exponentiate= TRUE)
    cox.zph(fit_fe)
    plot(cox.zph(fit_fe)[3])

    # Model 4 (exposure = war and pandemic period; outcome = death in Yemen)
    fit_fe <- coxph(Surv(age_start, age_end, died_yemen) ~ birth_cohort + gender + period_simple , data = df)          
    fit_fe
    length(unique(df$id)) # number of individuals at risk
    broom::tidy(fit_fe, exponentiate= TRUE)
    cox.zph(fit_fe)
    plot(cox.zph(fit_fe)[3])
    

  #...................................      
  ## Sensitivity analysis: simulation varying time to departure from Yemen among people who didn't die in Yemen

    # Define different proportions of people who left Yemen
    sens_props <- seq(1, 0, by = -0.25)
    names(sens_props) <- c("100%", "75%", "50%", "25%", "0% (baseline)")
      
    # Define different fractions of time that people who left spent in the analysis period before leaving
    sens_fracs <- seq(0, 1, by = 0.20)
    names(sens_fracs) <- c("0.00", "0.20", "0.40", "0.60", "0.80", "1.00 (baseline)")
    
    # Dataset of individuals, whether they died in Yemen and their age at exit from the cohort (age at entry = 0)
    df_id <- by(df, df$id, function(x) {return(c(unique(x[, "id"]), sum(x[, "died_yemen"]), max(x[, "age_end"]) ) )})
    df_id <- as.data.frame(do.call(rbind, df_id) )
    colnames(df_id) <- c("id", "died_yemen", "age_end")
    df_id[, c("died_yemen", "age_end")] <- sapply(df_id[, c("died_yemen", "age_end")], as.numeric)
    df_id <- df_id[order(df_id$id), ]

    # Columns to be retained in each simulation
    cols_sim <- c("id", "age_start", "age_end", "died", "died_yemen", "birth_cohort", "gender", "period", "period_simple")
    
    # Number of simulations
    n_sim <- 100
    
    # Simulation output
    out_all <- c()
    
    # For each proportion of people leaving...
    for (prop_k in names(sens_props) ) {
      
      # Define proportion leaving
      sens_prop <- sens_props[prop_k]
      
      # For each fraction of period spent in Yemen before leaving...
      for (frac_j in names(sens_fracs)) {
        
        # Define fraction
        sens_frac <- sens_fracs[frac_j]
      
        # Set up simulation output
        out <- data.frame(matrix(NA, nrow = n_sim * 3, ncol = 6))
        colnames(out) <- c("prop_leaving", "frac_period", "period", "hr", "p")
        out$prop_leaving <- prop_k
        out$frac_period <- frac_j
        out$period <- c(rep("war", n_sim), rep("pandemic", n_sim), rep("war + pandemic", n_sim))

        # For each simulation...
        for (i in 1:n_sim) {

          # prepare data for simulation
          df_id[, "leaver"] <- 0
          df_id[, "leave_when"] <- NA
          df_sim <- df[, cols_sim]
          
          # decide who leaves Yemen among those who didn't die in Yemen, and when they left
          df_id[which(df_id$died_yemen == 0), "leaver"] <- rbinom(sum(df_id$died_yemen == 0), 1, sens_prop)
          df_id[, "leave_when"] <- ifelse(df_id$leaver == 0, df_id$age_end, df_id$age_end * sens_frac)
                    
          # dataset to be simulated
          df_sim <- merge(df_sim, df_id[, c("id", "leaver", "leave_when")], by = "id", all.x = TRUE)

          # exclude periods beyond simulated age of exit
          df_sim <- df_sim[which(df_sim$age_start < df_sim$leave_when), ]
          
          # in remaining records, replace maximum exit age with simulated value
          x1 <- by(df_sim, df_sim$id, function(x) {return(cbind(x[, "id"], x[, "age_end"], (x[, "age_end"] == max(x[, "age_end"])) ) )} )
          x1 <- as.data.frame(do.call(rbind, x1))
          colnames(x1) <- c("id", "age_end", "is_max")
          df_sim <- merge(df_sim, x1, by = c("id", "age_end"), all.x = TRUE)
          df_sim$age_end <- ifelse(df_sim$is_max, df_sim$leave_when, df_sim$age_end)
          
          # run Cox model for both periods
          fit1 <- coxph(Surv(age_start, age_end, died_yemen) ~ birth_cohort + gender + period , data = df_sim)
          fit2 <- coxph(Surv(age_start, age_end, died_yemen) ~ birth_cohort + gender + period_simple , data = df_sim)
          
          # collect hazard ratios and p-values for association with war or pandemic periods
          out[i, c("hr", "p")] <- summary(fit1)$coefficients["periodwar", c("exp(coef)", "Pr(>|z|)")]
          out[i + n_sim, c("hr", "p")] <- summary(fit1)$coefficients["periodpandemic", c("exp(coef)", "Pr(>|z|)")]
          
          # collect p-value for association with war +- pandemic period
          out[i + n_sim * 2, c("hr", "p")] <- summary(fit2)$coefficients["period_simplewar", c("exp(coef)", "Pr(>|z|)")]
        }        

        # Add output for this combination of sensitivity parameters to the overall output
        out_all <- rbind(out_all, out)
      }
    }    
    
    # Format data for plotting
    out_plot <- subset(out_all, period == "war + pandemic" & prop_leaving != "100%" & frac_period != "1.00 (baseline)")
    out_plot$prop_leaving <- factor(gsub("[%]", "% migrated", out_plot$prop_leaving), 
      levels = c("0% migrated (baseline)", "25% migrated", "50% migrated", "75% migrated"),
      labels = c("0% migrated (baseline)", "25% migrated", "50% migrated", "75% migrated"))
    out_plot$frac_period <- factor(out_plot$frac_period, 
      levels = c("0.00", "0.20", "0.40", "0.60", "0.80"),
      labels = c("at start of period", "at 1/5 of period", "at 2/5 of period", "at 3/5 of period","at 4/5 of period"))    
    out_plot <- out_plot[order(out_plot$prop_leaving, out_plot$frac_period), ]

    # Compute output means
    sim_means <- aggregate(out_plot[, c("hr", "p")], by = out_plot[, c("prop_leaving", "frac_period", "period")], FUN = mean)
    plot_labels <- sim_means[, c("prop_leaving", "frac_period", "hr", "p")]
    plot_labels[, c("hr", "p")] <- sapply(plot_labels[, c("hr", "p")], as.numeric)
    plot_labels$hr <- round(plot_labels$hr, digits = 2)
    plot_labels$p <- round(plot_labels$p, digits = 3)
    plot_labels$label <- paste("HR = ", plot_labels$hr, " (p = ", plot_labels$p, ")", sep = "")
    plot_labels$label <- gsub("p = 0)", "p < 0.001)", plot_labels$label)
    
    # Visualise simulation outputs (omit pandemic and war periods separately) 
    plot <- ggplot(data = out_plot, 
      aes(x = hr, y = p, colour = prop_leaving, 
      size = prop_leaving) ) +
      geom_point(alpha = 0.5) +
      facet_wrap(prop_leaving ~ frac_period, scales = "free") +
      scale_x_continuous("mortality hazard ratio (war + pandemic period versus pre-war period") +
      scale_y_continuous("p-value", limits = c(NA, NA)) +
      theme_bw() +
      scale_colour_manual(values = palette_cb[c(4,7,6,8)]) +
      scale_size_manual(values = c(5,1,1,1)) +
      theme(legend.position = "none", axis.title = element_text(colour = "grey20")) +
      geom_text(aes(Inf, Inf, label = label), data = plot_labels, size = 3, hjust = 1, vjust = 1.3)
      
    plot
    ggsave("out_survival_sens.png", dpi = "print", unit = "cm", width = 25, height = 15)
    
    
    
#.........................................................................................      
### Estimating child mortality

  #[TO BE DONE]    
    
        
#.........................................................................................      
### Estimating adult sibling mortality
  # using R scripts and methods in https://desapublications.un.org/working-papers/r-scripts-computing-adult-and-maternal-mortality-dhs-sibling-survival-histories  

  #...................................      
  ## Select data
  siblings <- subset(all, type == "sibling")
      
  #...................................      
  ## Change and add respondent variable names to match UN R script
  
    # Cluster number (in this case == respondent id)
    x1 <- unique(siblings$resp_id)
    x1 <- data.frame("resp_id" = x1, "cluster" = 1:length(x1))
    siblings <- merge(siblings, x1, by = "resp_id", all.x = TRUE)
    siblings[, "v001"] <- siblings$cluster   

    # Sample weight (== 1/sample size == equal weights)
    siblings[, "v005"] <- 1/nrow(siblings)
    
    # Date respondent interviewed in century-month code
    siblings[, "v008"] <- 12*(year(as.Date.character(siblings$today)) - 1900) + month(as.Date.character(siblings$today))

    # Respondent's date of birth (not recorded, so make up dummy variable) in century-month code
    siblings[, "v011"] <- 12*(year(as.Date.character("1976-09-16")) - 1900) + month(as.Date.character("1976-09-16"))

    # Whether interview was completed
    siblings[, "v015"] <- 1
    
    # Date of month of interview
    siblings[, "v016"] <- lubridate::day(siblings$today)

  #...................................      
  ## Change and add sibling variable names to match the UN R script
  
    # Gender of sibling
    siblings[, "mm1"] <- ifelse(siblings$gender == "male", 1, 2)
    
    # Survival status of sibling (dead=0; alive=1; don’t know=8; missing=NA)
    siblings[, "mm2"] <- 1
    siblings[siblings$status == "deceased", "mm2"] <- 0
    siblings[siblings$status == "unknown", "mm2"] <- 8
    table(siblings$mm2)
    
    # Current age of sibling in integer years for siblings still alive
    siblings[, "mm3"] <- siblings$aliveage
    
    # Sibling’s date of birth imputed by DHS in century-month code (not recorded, so made up from age in years)
    siblings[siblings$status == "alive", "mm4"] <- 12*(2022 - siblings[siblings$status == "alive", "aliveage"] - 1900) + 6
    siblings[siblings$status == "deceased", "mm4"] <- 12*(siblings[siblings$status == "deceased", "died_year"] - 
        siblings[siblings$status == "deceased", "died_age"] - 1900) + 6
    siblings[siblings$status == "unknown", "mm4"] <- NA

    # Number of integer years that have passed since death for deceased siblings
    siblings[, "mm6"] <- NA
    siblings[siblings$status == "deceased", "mm6"] <- 2022 - siblings[siblings$status == "deceased", "died_year"]
    
    # Sibling’s age at death in integer years for deceased siblings
    siblings[, "mm7"] <- NA
    siblings[siblings$status == "deceased", "mm7"] <- siblings[siblings$status == "deceased", "died_age"]
      
    # Sibling’s date of death for deceased siblings imputed by DHS in century-month code
    siblings[, "mm8"] <- NA
    siblings[siblings$status == "deceased", "mm8"] <- 12*(siblings[siblings$status == "deceased", "died_year"] - 
        1900) + 6

    # Pregnancy status at death for deceased sisters (various coding schema across surveys) - Not recorded
    siblings[, "mm9"] <- 0

    # Sibling number in sibship 
    siblings[, "mmidx"] <- NA
    siblings <- siblings[order(siblings$resp_id, siblings$mm4), ]
    for (i in siblings$resp_id) {
      siblings[siblings$resp_id == i, "mmidx"] <- 1:nrow(siblings[siblings$resp_id == i, ])
    }
    
  #...................................      
  ## Reshape sibling variables to wide format as per UN R script requirement
  
    # Variables to drop in reshape
    x2 <- c("resp_id", "sibling_id", "type", "relative_of", "child_of", "order",
      "gender", "status", "aliveage", "aliveage_cat", grep("died_", colnames(siblings), value = TRUE),
      grep("childs_parent_", colnames(siblings), value = TRUE), "today", "age", "age_cat", "cluster")
    
    # Reshape wide
    x1 <- reshape(siblings, v.names = c("mm1", "mm2", "mm3", "mm4", "mm6", "mm7", "mm8", "mm9", "mmidx"),
      direction = "wide", sep = ".", idvar = c("v001", "v005", "v008", "v011", "v015", "v016"), 
      timevar = "mmidx", drop = x2 )
    
    # Fix variable names  
    for (i in c(1,2,3,4,6,7,8,9)) {
      for (j in 1:9) {
        colnames(x1)[colnames(x1) == paste("mm", i, ".", j, sep = "")] <- paste("mm", i, ".0", j, sep = "")
      } 
    }  
  
    for (i in 1:9) {
      colnames(x1)[colnames(x1) == paste("mmidx.", i, sep = "")] <- paste("mmidx.0", i, sep = "")
    } 
    
    # Save dataset
    write.csv(x1, "ind_recode.csv", row.names = FALSE)
  
                
  #...................................      
  ## Estimate sibling mortality
    
  # SurveyRefDate <- "02/03/2022"
    
    # Prepare dataset  
    source("FUNCTION.PrepForSiblingsDHS.R")
    df <- PrepForSiblingsDHS(ind.recode = "ind_recode.csv", SurveyRefDate = "10/05/2022")    
    
    # Estimate mortality rates
    source("FUNCTION.SiblingSurvival.R")
    # df <- siblings.pd
    
      # for last 7 years (since 2015)
      out <- SiblingSurvival(PreppedSibData = df, include.Respondent = FALSE, impute.Sex = FALSE, PregRelCode = c(2,3,6),
        recall.back = 7, recall.end = 0, stderror = FALSE, age.standardize = FALSE, pr.recode = NA)
      write.csv(out, "out_sibling_mortality_2015-2022.csv", row.names = FALSE)
    
      # for previous 7 years (2008-2014)
      out <- SiblingSurvival(PreppedSibData = df, include.Respondent = FALSE, impute.Sex = FALSE, PregRelCode = c(2,3,6),
        recall.back = 14, recall.end = 8, stderror = FALSE, age.standardize = FALSE, pr.recode = NA)
      write.csv(out, "out_sibling_mortality_2008-2014.csv", row.names = FALSE)

      # for all years since 2008
       out <- SiblingSurvival(PreppedSibData = df, include.Respondent = FALSE, impute.Sex = FALSE, PregRelCode = c(2,3,6),
        recall.back = 14, recall.end = 0, stderror = FALSE, age.standardize = FALSE, pr.recode = NA)
      write.csv(out, "out_sibling_mortality_2008-2022.csv", row.names = FALSE)
      
      
###NEED TO TRY AGE STANDARDISATION OPTION ONCE GET ACCESS TO DHS 2013 DATA
      
      
#.........................................................................................
### ENDS



  