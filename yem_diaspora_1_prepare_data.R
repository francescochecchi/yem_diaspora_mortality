#..........................................................................................
### ++++ RESPONDENT-DRIVEN SAMPLING MORTALITY SURVEY OF THE YEMENI DIASPORA (2022) ++++ ###
#..........................................................................................

#..........................................................................................
## ----------- R SCRIPT TO MODIFY, CLEAN AND MERGE DATASETS AHEAD OF ANALYSIS ----------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Jan 2023)
                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................
### Preparing the respondent dataset

  #...................................      
  ## Apply eligibility criteria
  
    # Apply criteria
    resp[, "eligible"] <- "yes"
    resp[resp$eligible.eligible_age == "no", "eligible"] <- "no - outside age"
    resp[resp$eligible.eligible_family == "no", "eligible"] <- "no - same family as other respondent"
    
    # Remove ineligible observations
    table(resp$eligible) 
    resp <- subset(resp, eligible == "yes")  

  #...................................      
  ## Clean up and save
    
    # Rename governorates
    resp[, "gov_en_ocha"] <- ""
    resp[resp$survey.govenorate_your.your_governorate == "abyan", "gov_en_ocha"] <- "Abyan"
    resp[resp$survey.govenorate_your.your_governorate == "aden", "gov_en_ocha"] <- "Aden"
    resp[resp$survey.govenorate_your.your_governorate == "al_bayda", "gov_en_ocha"] <- "Al Bayda"
    resp[resp$survey.govenorate_your.your_governorate == "al_hodeidah", "gov_en_ocha"] <- "Al Hodeidah"
    resp[resp$survey.govenorate_your.your_governorate == "hadramawt", "gov_en_ocha"] <- "Hadramawt"
    resp[resp$survey.govenorate_your.your_governorate == "ibb", "gov_en_ocha"] <- "Ibb"
    resp[resp$survey.govenorate_your.your_governorate == "sanaa", "gov_en_ocha"] <- "Sana'a"
    resp[resp$survey.govenorate_your.your_governorate == "sanaa_city", "gov_en_ocha"] <- "Sana'a City"
    resp[resp$survey.govenorate_your.your_governorate == "taiz", "gov_en_ocha"] <- "Ta'iz"
    
    # Rename respondent ID
    colnames(resp)[colnames(resp) == "KEY"] <- "resp_id"
    
    # Save dataset
    write.csv(resp, "out_respondent_data.csv", row.names = FALSE)  
  
  
#.........................................................................................
### Preparing the parent dataset (parents and parents-in-law of respondent)

  #...................................      
  ## Select the data and prepare for appending
    
    # Select respondent's mother data
    motheryour <- resp[, c("resp_id", grep("motheryour", colnames(resp), value = TRUE) )]
    motheryour[, "relative_of"] <- "respondent"
    motheryour[, "gender"] <- "female"
    colnames(motheryour) <- gsub("survey.motheryour.mother_", "", colnames(motheryour))

    # Select respondent's father data
    fatheryour <- resp[, c("resp_id", grep("fatheryour", colnames(resp), value = TRUE) )]
    fatheryour[, "relative_of"] <- "respondent"
    fatheryour[, "gender"] <- "male"
    colnames(fatheryour) <- gsub("survey.fatheryour.father_", "", colnames(fatheryour))
      
    # Select spouse's mother data
    motherspouse <- resp[, c("resp_id", grep("motherspouse", colnames(resp), value = TRUE) )]
    motherspouse[, "relative_of"] <- "spouse"
    motherspouse[, "gender"] <- "female"
    colnames(motherspouse) <- gsub("survey.ifspouse.motherspouse.motherspouse_", "", colnames(motherspouse))

    # Select spouse's father data
    fatherspouse <- resp[, c("resp_id", grep("fatherspouse", colnames(resp), value = TRUE) )]
    fatherspouse[, "relative_of"] <- "spouse"
    fatherspouse[, "gender"] <- "male"
    colnames(fatherspouse) <- gsub("survey.ifspouse.fatherspouse.fatherspouse_", "", colnames(fatherspouse))

  #...................................      
  ## Append together and clean up
        
    # Append all four datasets and exclude missing observations (no parent)         
    parents <- rbind(motheryour, fatheryour, motherspouse, fatherspouse)
    x1 <- which(parents$status == "")
    parents <- parents[-x1, ]  
    
    # Clean up
    colnames(parents)[colnames(parents) == "governorate"] <- "diegovernorate"
    colnames(parents)[colnames(parents) == "yemendeceased"] <- "dieyemen"
    colnames(parents)[colnames(parents) == "cod"] <- "diecause"   
    parents <- parents[, ! colnames(parents) %in% "note"]
    
    # Additional columns necessary for appending later
    parents[, "order"] <- NA
    parents[, "child_of"] <- NA
    parents[, "sibling_id"] <- NA
    parents[, "childs_parent_aliveage"] <- NA    
    parents[, "childs_parent_dieage"] <- NA    
    parents[, "childs_parent_dieyear"] <- NA  
    parents[, "childs_parent_diemonth"] <- NA  
    
#.........................................................................................
### Preparing the sibling dataset
            
  #...................................      
  ## Prepare the different sibling survival datasets for appending

    # Define relative of reference
    siblings1[, "relative_of"] <- "respondent"
    siblings2[, "relative_of"] <- "spouse"
    siblings3[, "relative_of"] <- "respondent"
    siblings4[, "relative_of"] <- "spouse"

    # Add gender
    siblings1[, "gender"] <- "male"
    siblings2[, "gender"] <- "male"
    siblings3[, "gender"] <- "female"
    siblings4[, "gender"] <- "female"
      
    # Modify column names
    colnames(siblings1) <- gsub("brother", "sibling", colnames(siblings1) )
    colnames(siblings2) <- gsub("brotherspouse", "sibling", colnames(siblings2) )
    colnames(siblings3) <- gsub("sister", "sibling", colnames(siblings3) )
    colnames(siblings4) <- gsub("sisterspouse", "sibling", colnames(siblings4) )
    
    
  #...................................      
  ## Append and clean up
          
    # Append sibling survival datasets together  
    siblings <- rbind(siblings1, siblings2, siblings3, siblings4)

    # Sort out unique identifiers
    colnames(siblings)[colnames(siblings) == "KEY"] <- "sibling_id"
    colnames(siblings)[colnames(siblings) == "PARENT_KEY"] <- "resp_id"
      
    # Make sure only siblings from eligible respondents are included
    x1 <- unique(resp$resp_id)       
    siblings <- subset(siblings, resp_id %in% x1)
    
    # Change column names
    colnames(siblings) <- gsub("sibling_", "", colnames(siblings))
    colnames(siblings)[colnames(siblings) == "id"] <- "sibling_id"
    colnames(siblings)[colnames(siblings) == "position"] <- "order"
    siblings$order <- siblings$order + 1
    colnames(siblings)[colnames(siblings) == "diedage"] <- "dieage"
    colnames(siblings)[colnames(siblings) == "governorate"] <- "diegovernorate"
    colnames(siblings)[colnames(siblings) == "yemendeceased"] <- "dieyemen"          
    colnames(siblings)[colnames(siblings) == "cod"] <- "diecause" 
  
    # Remove unnecessary variables and clean up
    x1 <- c("position_text", "position_text_ab", "position_which", "position_which_ab")
    siblings <- siblings[, ! colnames(siblings) %in% x1]
    rm(siblings1, siblings2, siblings3, siblings4)
      
    # Add variables necessary for appending later
    siblings[, "child_of"] <- NA
    siblings[, "childs_parent_aliveage"] <- NA    
    siblings[, "childs_parent_dieage"] <- NA    
    siblings[, "childs_parent_dieage"] <- NA  
    siblings[, "childs_parent_dieyear"] <- NA  
    siblings[, "childs_parent_diemonth"] <- NA  
    
    
#.........................................................................................
### Preparing the children dataset
            
  #...................................      
  ## Prepare the different children survival datasets for appending

    # Define relative of reference  
    children1[, "relative_of"] <- "respondent"
    children2[, "relative_of"] <- "spouse"
    children3[, "relative_of"] <- "respondent"
    children4[, "relative_of"] <- "spouse"

    # Define parent
    children1[, "child_of"] <- "brother"
    children2[, "child_of"] <- "brother"
    children3[, "child_of"] <- "sister"      
    children4[, "child_of"] <- "sister"      
        
    # MOdify column names    
    colnames(children1) <- gsub("brother_kid_", "", colnames(children1))
    colnames(children2) <- gsub("brotherspouse_kid_", "", colnames(children2))
    colnames(children3) <- gsub("sister_kid_", "", colnames(children3))
    colnames(children4) <- gsub("sisterspouse_kid_", "", colnames(children4))
 
  #...................................      
  ## Append, add sibling parents' IDs / ages and clean up

    # Append children survival datasets together  
    children <- rbind(children1, children2, children3, children4)

    # Sort out unique identifiers
    colnames(children)[colnames(children) == "KEY"] <- "resp_id"
    children$resp_id <- substr(children$resp_id, start = 1, stop = 41)
    colnames(children)[colnames(children) == "PARENT_KEY"] <- "sibling_id"
    children$sibling_id <- gsub("_kid", "", children$sibling_id)
    children$sibling_id <- gsub("spousebrother/spousebrother", "spousebrother/brotherspouse", children$sibling_id)
    children$sibling_id <- gsub("spousesisters/spousesister", "spousesisters/sisterspouse", children$sibling_id)
        
    # Make sure only children from eligible respondents are included
    x1 <- unique(resp$resp_id)       
    children <- subset(children, resp_id %in% x1)
    
    # Merge in the alive or death ages/times of the child's parent (from sibling dataset)
    x1 <- siblings[, c("sibling_id", "aliveage", "dieage", "dieyear", "diemonth")]
    colnames(x1) <- c("sibling_id", "childs_parent_aliveage", "childs_parent_dieage", 
      "childs_parent_dieyear", "childs_parent_diemonth")
    children <- merge(children, x1, by = "sibling_id", all.x = TRUE)
    
    # Change column names
    colnames(children)[colnames(children) == "position"] <- "order"
    children$order <- children$order + 1
    colnames(children)[colnames(children) == "diedage"] <- "dieage"
    colnames(children)[colnames(children) == "governorate"] <- "diegovernorate"
    colnames(children)[colnames(children) == "yemendeceased"] <- "dieyemen"          
    colnames(children)[colnames(children) == "cod"] <- "diecause"    
    
    # Remove unnecessary variables and clean up
    x1 <- c("position_text", "position_text_ab", "position_text_F_ab", "position_which", "position_which_ab",
      "position_which_gender_ab")
    children <- children[, ! colnames(children) %in% x1]
    rm(children1, children2, children3, children4)


#.........................................................................................
### Appending all datasets together and preparing for analysis
    
  #...................................      
  ## Append all individual datasets together
    
    # Denote type of relative
    parents[, "type"] <- "parent"
    siblings[, "type"] <- "sibling"
    children[, "type"] <- "child"
      
    # Append and clean up
    all <- rbind(parents, siblings, children)
    x1 <- c("resp_id", "sibling_id", "type", "relative_of", "child_of", "order", "gender", "status", "aliveage",
      grep("die", colnames(all), value = TRUE), grep("childs_parent", colnames(all), value = TRUE))
    all <- all[, unique(x1)]
    colnames(all) <- gsub("die", "died_", colnames(all))
 
  #...................................      
  ## Create age categories
    
    # Create age now (even for decedents)
      # add date of survey
      all <- merge(all, resp[, c("resp_id", "today")], by = "resp_id", all.x = TRUE)
      all$today <- ymd(all$today)
      
      # create age now
      all[, "age"] <- all$aliveage
    
      # calculate age now for decedents
      all[, "died_month_int"] <- match(all$died_month, tolower(month.abb))
      all[, "died_date"] <- ymd(paste(all$died_year, ifelse(is.na(all$died_month_int), 6, all$died_month_int),
        15, sep = "-") )
      all[all$status == "deceased", "age"] <- all[all$status == "deceased", "died_age"] +
        floor((all[all$status == "deceased", "today"] - all[all$status == "deceased", "died_date"]) / 365)

    # Create age now for children's parents (even for decedents)
      # create age now
      all[, "childs_parent_age"] <- all$childs_parent_aliveage
    
      # calculate age now for decedents
      all[, "childs_parent_died_month_int"] <- match(all$childs_parent_died_month, tolower(month.abb))
      all[, "childs_parent_died_date"] <- ymd(paste(all$childs_parent_died_year, 
        ifelse(is.na(all$childs_parent_died_month_int), 6, all$childs_parent_died_month_int),
        15, sep = "-") )
      x1 <- which(! is.na(all$childs_parent_died_age))
      all[x1, "childs_parent_age"] <- all[x1, "childs_parent_died_age"] + floor((all[x1, "today"] - 
          all[x1, "childs_parent_died_date"]) / 365)
      
    # Create age categories
    all[, "aliveage_cat"] <- cut(all$aliveage, breaks = c(0, 4, 14, 24, 34, 44, 100), 
      labels = c("0 to 4y", "5 to 14y", "15 to 24y", "25 to 34y", "35 to 44y", "45+y"), 
      include.lowest = TRUE, right = TRUE, ordered_result = TRUE)
    
    all[, "died_age_cat"] <- cut(all$died_age, breaks = c(0, 4, 14, 24, 34, 44, 100), 
      labels = c("0 to 4y", "5 to 14y", "15 to 24y", "25 to 34y", "35 to 44y", "45+y"), 
      include.lowest = TRUE, right = TRUE, ordered_result = TRUE)
    
    all[, "age_cat"] <- cut(all$age, breaks = c(0, 4, 14, 24, 34, 44, 100), 
      labels = c("0 to 4y", "5 to 14y", "15 to 24y", "25 to 34y", "35 to 44y", "45+y"), 
      include.lowest = TRUE, right = TRUE, ordered_result = TRUE)
            
  #...................................      
  ## Create further variables for deaths

    # Period of death
    all[, "died_period"] <- NA
    all[which((all$died_date < date_start)), "died_period"] <- "outside period"
    all[which((all$died_date %in% c(date_start : date_knots[1]))), "died_period"] <- "pre-war"
    all[which((all$died_date %in% c(date_knots[1] : (date_knots[2] - 1)))), "died_period"] <- "war"
    all[which((all$died_date %in% c(date_knots[2] : date_end))), "died_period"] <- "pandemic"
        
  #...................................      
  ## Save dataset
  write.csv(all, "out_individual_data.csv", row.names = FALSE)  

  
#.........................................................................................
### ENDS



  