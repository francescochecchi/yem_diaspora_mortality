#..........................................................................................
### ++++ RESPONDENT-DRIVEN SAMPLING MORTALITY SURVEY OF THE YEMENI DIASPORA (2022) ++++ ###
#..........................................................................................

#..........................................................................................
## --------- R SCRIPT TO DESCRIBE CHARACTERISTICS OF RESPONDENTS AND FAMILIES ---------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Jan 2023)
                                          # francesco.checchi@lshtm.ac.uk 



#.........................................................................................
### Describing general characteristics of the respondents and their families

  #...................................      
  ## Read data if not already in environment
  if (! exists("resp")) {resp <- read.csv("out_respondent_data.csv") }
  if (! exists("all")) {all <- read.csv("out_individual_data.csv") }
 
  
  #...................................      
  ## Number and density of respondents / population by governorate
    
    # Number and timespan of responses
    nrow(resp)
    range(resp$today)
  
    # Tabulate governorates
    x1 <- as.data.frame(table(resp$gov_en_ocha))
    colnames(x1) <- c("gov_en_ocha", "n_resp")
    x1[, "n_resp_lab"] <- paste("(", x1$n_resp, ")", sep = "")
    x1[x1$n_resp_lab == "(NA)", "n_resp_lab"] <- ""
    
    # Merge with population and compute density per million
    x1 <- merge(pop, x1, by = "gov_en_ocha", all.x = TRUE)
    x1[, "resp_density"] <- x1$n_resp * 1000000 / x1$pop_sep_2021
    x1[, "resp_density_sqrt"] <- sqrt(x1$resp_density)
    
    # Merge with shapefile
    x1[, "ADM1_EN"] <- x1$gov_en_ocha
    x1 <- merge(ocha_shape_gov, x1, by = "ADM1_EN", all.x = TRUE)
    
    # Specify governorate label positions
    pos_gov_x <- rep(0, times <- length(x1$ADM1_EN) )
    pos_gov_y <- rep(0, times <- length(x1$ADM1_EN) )
    names(pos_gov_x) <- x1$ADM1_EN
    names(pos_gov_y) <- x1$ADM1_EN
    pos_gov_y["Sana'a City"] <- 1.0    
    pos_gov_y["Sana'a"] <- -0.5
    pos_gov_y["Al Mahwit"] <- -0.2
    pos_gov_y["Al Hodeidah"] <- 0.3
    pos_gov_x["Al Hodeidah"] <- -0.3    
    pos_gov_y["Aden"] <- -0.5
    pos_gov_x["Sana'a City"] <- 1.0 
    pos_gov_x["Ibb"] <- -0.2
    pos_gov_x["Dhamar"] <- 0.2    
    pos_gov_x["Raymah"] <- -0.2  
    
    # Create and save map
    map1 <- tm_shape(x1) +
      tm_text("ADM1_EN", col = "grey20", size = 0.6, xmod = pos_gov_x, ymod = pos_gov_y, fontface = "bold") +
      tm_shape(x1) +
      tm_text("n_resp_lab", col = palette_cb[6], size = 0.6, xmod = pos_gov_x, ymod = pos_gov_y - 0.4, fontface = "bold") +
      tm_borders(col = "grey20", lwd = 2, alpha = 0.5) +
      tm_shape(x1) +
      tm_borders(col = palette_cb[7], lwd = 0.5) +
      tm_fill("resp_density_sqrt", alpha = 0.3, legend.show = FALSE)
    map1
    tmap_save(map1, "out_map_resp_by_gov.png", height = 15, width = 20, units = "cm", dpi = 300)
 
    # Create and save table
    x2 <- st_drop_geometry(x1)
    x2 <- as.data.frame(x2[, c("gov_en_ocha", "pop_sep_2021", "n_resp", "resp_density")])
    x2[is.na(x2$n_resp), "n_resp"] <- 0
    x2[is.na(x2$resp_density), "resp_density"] <- 0
    x2$pop_sep_2021 <- round(x2$pop_sep_2021, digits  = -3)
    x2$resp_density <- round(x2$resp_density, digits  = 1)
    write.csv(x2, "out_tab_resp_by_gov.csv", row.names = FALSE)      
    
       
  #...................................      
  ## General characteristics of the respondents
  
    # Number by age and gender
    ### [age and gender not collected]

    # Number born in Yemen
    table(resp$survey.govenorate_your.born_yemen)
    prop.table(table(resp$survey.govenorate_your.born_yemen))
    
    # Number from urban vs. rural place in Yemen
    x1 <- table(resp$survey.ses.urbanrural)
    x2 <- paste(x1, " (", round(prop.table(x1) * 100, digits = 1), "%)", sep = "")
    names(x2) <- names(x1)    
    x2 

    # Number who own different assets
      # identify assets
      x1 <- names(table(resp$survey.ses.assets))
      x1 <- sapply(x1, function (x) {strsplit(x, split = " ")})
      x1 <- unique(as.vector(unlist(x1)))

      # for each asset, create a TRUE/FALSE variable if owned
      for (i in x1) {resp[, i] <- grepl(i, resp$survey.ses.assets) }
      
      # for each asset, tabulate ownership as a table
      tab <- data.frame("asset" = x1, "freq" = NA, "percent" = NA)
      tab <- tab[order(tab$asset), ]
      for (i in tab$asset) {
        tab[tab$asset == i, "freq"] <- table(resp[, i])["TRUE"]
        tab[tab$asset == i, "percent"] <- round(prop.table(table(resp[, i]))["TRUE"] * 100, digits = 1)
      }
      tab
      tab[, "combi"] <- paste(tab$freq, " (", tab$percent, "%)", sep = "")
      write.csv(tab, "out_tab_asset_ownership.csv", row.names = FALSE)
      
    # Number whose house has different types of flooring
    x1 <- table(resp$survey.ses.floor)
    x2 <- paste(x1, " (", round(prop.table(x1) * 100, digits = 1), "%)", sep = "")
    names(x2) <- names(x1)    
    x2    
    
    # Number whose house has different types of sanitation
    x1 <- table(resp$survey.ses.sanitation)
    x2 <- paste(x1, " (", round(prop.table(x1) * 100, digits = 1), "%)", sep = "")
    names(x2) <- names(x1)    
    x2     
    
    # Number whose house uses different water sources
    x1 <- table(resp$survey.ses.water)
    x2 <- paste(x1, " (", round(prop.table(x1) * 100, digits = 1), "%)", sep = "")
    names(x2) <- names(x1)    
    x2    

    # Number who use different transportation means
      # identify vehicles
      x1 <- names(table(resp$survey.ses.transport))
      x1 <- sapply(x1, function (x) {strsplit(x, split = " ")})
      x1 <- unique(as.vector(unlist(x1)))

      # for each vehicle, create a TRUE/FALSE variable if used
      for (i in x1) {resp[, i] <- grepl(i, resp$survey.ses.transport) }
      
      # for each vehicle, tabulate ownership as a table
      tab <- data.frame("vehicle" = x1, "freq" = NA, "percent" = NA)
      tab <- tab[order(tab$vehicle), ]
      for (i in tab$vehicle) {
        tab[tab$vehicle == i, "freq"] <- table(resp[, i])["TRUE"]
        tab[tab$vehicle == i, "percent"] <- round(prop.table(table(resp[, i]))["TRUE"] * 100, digits = 1)
      }
      tab[, "combi"] <- paste(tab$freq, " (", tab$percent, "%)", sep = "")
      tab    
        
    # Number and mean / IQR of people sleeping in house
    x1 <- table(resp$survey.ses.sleeping)
    x2 <- paste(x1, " (", round(prop.table(x1) * 100, digits = 1), "%)", sep = "")
    names(x2) <- names(x1)    
    x2
    
    mean(resp$survey.ses.sleeping, na.rm = TRUE)
    quantile(resp$survey.ses.sleeping, c(0.025, 0.75), na.rm = TRUE)
    
  #...................................      
  ## Marital status of respondents
    
    # Marital status
    x1 <- table(resp$survey.spouse.marital_status)
    x2 <- paste(x1, " (", round(prop.table(x1) * 100, digits = 1), "%)", sep = "")
    names(x2) <- names(x1)    
    x2

    # Number of spouses
    x1 <- table(resp$survey.spouse.spouse_count)
    x2 <- paste(x1[-1], " (", round(prop.table(x1[-1]) * 100, digits = 1), "%)", sep = "")
    names(x2) <- names(x1[-1])    
    x2


#.........................................................................................
### Describing demographic characteristics of the respondents' families
    
  #...................................      
  ## Overall breakdown of relatives reported on   
    
    # Breakdown
    nrow(all)
    x1 <- table(all$type, all$relative_of)

    # Ratios per respondent or per spouse
    x1[, 1] / nrow(resp)
    x1[, 2] / table(resp$survey.spouse.marital_status)["spouse_yes"]
    
  #...................................      
  ## Age distribution of parents
    
    # Select and prepare data
    x1 <- subset(all, type == "parent")
    x1[, "parent"] <- ifelse(x1$gender == "female", "mother", "father")
    x1[, "relative_of"] <- paste(x1[, "relative_of"], "'s", sep = "")
  
    # Plot
    plot <- ggplot(x1, aes(x = age, colour = parent, fill = parent)) + 
      geom_bar(alpha = 0.5, position = "dodge") +
      scale_x_continuous("age (years)", expand = c(0, 0), limits = range(x1$age), breaks = seq(30, 100, by = 5)) +
      scale_y_continuous("number", expand = c(0, 0), breaks = seq(0, 12, by = 2)) +
      facet_wrap(relative_of~parent) +
      scale_fill_manual(values = palette_cb[c(6,7)]) +
      scale_colour_manual(values = palette_cb[c(6,7)]) +
      theme_bw() +
      theme(legend.position = "none", axis.title = element_text(colour = "grey20", size = 11))
    plot  
    ggsave("out_age_distribution_parents.png", width = 20, height = 12, units = "cm", dpi = "print")

    # Mean age per category of parent
    by(x1, x1[, c("relative_of", "parent")], function(x) {mean(x$age, na.rm = TRUE)})
    
    
  #...................................   
  ## Age and gender distribution of siblings
    
    # Respondent's siblings
    x1 <- subset(all, type == "sibling" & relative_of == "respondent")
    x1 <- as.data.frame(table(x1$age_cat, x1$gender))
    colnames(x1) <- c("age", "gender", "number")
    x1[x1$gender == "female", "number"] <- -x1[x1$gender == "female", "number"]
    plot1 <- ggplot(x1, aes(x = age, y = number, fill = gender)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme_bw() +
      scale_y_continuous("number of siblings", breaks = seq(-70, 70, by = 10), limits = c(-70, 70),
        labels = as.character(c(seq(70, 0, by = -10), seq(10, 70, by = 10)))) +
      scale_fill_manual(values = palette_cb[c(7, 6)]) +
      theme(axis.title.x = element_text(color="grey20", size=11), 
        axis.text.x = element_text(color = "grey20", size=11),
        axis.text.y = element_text(color = "grey20", size=11),
        axis.title.y = element_text(color="grey20", size=11),
        legend.text = element_text(color="grey20", size=11),
        legend.title = element_text(color="grey20", size=11),
        legend.position = "bottom",
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")
      )
    plot1

    # Spouse's siblings
    x1 <- subset(all, type == "sibling" & relative_of == "spouse")
    x1 <- as.data.frame(table(x1$age_cat, x1$gender))
    colnames(x1) <- c("age", "gender", "number")
    x1[x1$gender == "female", "number"] <- -x1[x1$gender == "female", "number"]
    plot2 <- ggplot(x1, aes(x = age, y = number, fill = gender)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme_bw() +
      scale_y_continuous("number of siblings", breaks = seq(-70, 70, by = 10), limits = c(-70, 70),
        labels = as.character(c(seq(70, 0, by = -10), seq(10, 70, by = 10)))) +
      scale_fill_manual(values = palette_cb[c(7, 6)]) +
      theme(axis.title.x = element_text(color="grey20", size=11), 
        axis.text.x = element_text(color = "grey20", size=11),
        axis.text.y = element_text(color = "grey20", size=11),
        axis.title.y = element_text(color="grey20", size=11),
        legend.text = element_text(color="grey20", size=11),
        legend.title = element_text(color="grey20", size=11),
        legend.position = "bottom",
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")          
      )
    plot2
   
    # Combined plot
    plot <- ggarrange(plot1, plot2, ncol = 2, labels = c("respondent's siblings", "spouse's siblings"), 
      common.legend = TRUE, legend = "bottom", font.label = list(size = 11))
    plot
    ggsave("out_age_gender_pyramids_siblings.png", width = 30, height = 15, units = "cm", dpi = "print")    

    # Mean age per category of sibling
    x1 <- subset(all, type == "sibling")
    by(x1, x1[, c("relative_of", "gender")], function(x) {mean(x$age, na.rm = TRUE)})

          
  #...................................      
  ## Age and gender distribution of nieces / nephews
    
    # Respondent's brothers' children
    x1 <- subset(all, type == "child" & relative_of == "respondent" & child_of == "brother")
    x1 <- as.data.frame(table(x1$age_cat, x1$gender))
    colnames(x1) <- c("age", "gender", "number")
    x1[x1$gender == "female", "number"] <- -x1[x1$gender == "female", "number"]
    plot1 <- ggplot(x1, aes(x = age, y = number, fill = gender)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme_bw() +
      scale_y_continuous("number of children", breaks = seq(-70, 70, by = 10), limits = c(-70, 70),
        labels = as.character(c(seq(70, 0, by = -10), seq(10, 70, by = 10)))) +
      scale_fill_manual(values = palette_cb[c(7, 6)]) +
      theme(axis.title.x = element_text(color="grey20", size=11), 
        axis.text.x = element_text(color = "grey20", size=11),
        axis.text.y = element_text(color = "grey20", size=11),
        axis.title.y = element_text(color="grey20", size=11),
        legend.text = element_text(color="grey20", size=11),
        legend.title = element_text(color="grey20", size=11),
        legend.position = "bottom",
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm") 
      )
    plot1

    # Respondent's sisters' children
    x1 <- subset(all, type == "child" & relative_of == "respondent" & child_of == "sister")
    x1 <- as.data.frame(table(x1$age_cat, x1$gender))
    colnames(x1) <- c("age", "gender", "number")
    x1[x1$gender == "female", "number"] <- -x1[x1$gender == "female", "number"]
    plot2 <- ggplot(x1, aes(x = age, y = number, fill = gender)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme_bw() +
      scale_y_continuous("number of children", breaks = seq(-70, 70, by = 10), limits = c(-70, 70),
        labels = as.character(c(seq(70, 0, by = -10), seq(10, 70, by = 10)))) +
      scale_fill_manual(values = palette_cb[c(7, 6)]) +
      theme(axis.title.x = element_text(color="grey20", size=11), 
        axis.text.x = element_text(color = "grey20", size=11),
        axis.text.y = element_text(color = "grey20", size=11),
        axis.title.y = element_text(color="grey20", size=11),
        legend.text = element_text(color="grey20", size=11),
        legend.title = element_text(color="grey20", size=11),
        legend.position = "bottom",
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm") 
      )
    plot2

    # Spouse's brothers' children
    x1 <- subset(all, type == "child" & relative_of == "spouse" & child_of == "brother")
    x1 <- as.data.frame(table(x1$age_cat, x1$gender))
    colnames(x1) <- c("age", "gender", "number")
    x1[x1$gender == "female", "number"] <- -x1[x1$gender == "female", "number"]
    plot3 <- ggplot(x1, aes(x = age, y = number, fill = gender)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme_bw() +
      scale_y_continuous("number of children", breaks = seq(-70, 70, by = 10), limits = c(-70, 70),
        labels = as.character(c(seq(70, 0, by = -10), seq(10, 70, by = 10)))) +
      scale_fill_manual(values = palette_cb[c(7, 6)]) +
      theme(axis.title.x = element_text(color="grey20", size=11), 
        axis.text.x = element_text(color = "grey20", size=11),
        axis.text.y = element_text(color = "grey20", size=11),
        axis.title.y = element_text(color="grey20", size=11),
        legend.text = element_text(color="grey20", size=11),
        legend.title = element_text(color="grey20", size=11),
        legend.position = "bottom",
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm") 
      )
    plot3
    
    # Spouse's sisters' children
    x1 <- subset(all, type == "child" & relative_of == "spouse" & child_of == "sister")
    x1 <- as.data.frame(table(x1$age_cat, x1$gender))
    colnames(x1) <- c("age", "gender", "number")
    x1[x1$gender == "female", "number"] <- -x1[x1$gender == "female", "number"]
    plot4 <- ggplot(x1, aes(x = age, y = number, fill = gender)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme_bw() +
      scale_y_continuous("number of children", breaks = seq(-70, 70, by = 10), limits = c(-70, 70),
        labels = as.character(c(seq(70, 0, by = -10), seq(10, 70, by = 10)))) +
      scale_fill_manual(values = palette_cb[c(7, 6)]) +
      theme(axis.title.x = element_text(color="grey20", size=11), 
        axis.text.x = element_text(color = "grey20", size=11),
        axis.text.y = element_text(color = "grey20", size=11),
        axis.title.y = element_text(color="grey20", size=11),
        legend.text = element_text(color="grey20", size=11),
        legend.title = element_text(color="grey20", size=11),
        legend.position = "bottom",
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm") 
      )
    plot4           
    
    # Combined plot
    plot <- ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2, labels = c("respondent's brothers' children", 
      "respondent's sisters' children", "spouse's brothers' children", "spouse's sisters' children"), 
      common.legend = TRUE, legend = "bottom", font.label = list(size = 11))
    plot
    ggsave("out_age_gender_pyramids_children.png", width = 30, height = 20, units = "cm", dpi = "print")    
      
    # Mean age per category of child
    x1 <- subset(all, type == "child")
    by(x1, x1[, c("relative_of", "gender")], function(x) {mean(x$age, na.rm = TRUE)})

    
  #...................................      
  ## Fertility patterns
      
    # Select sibling parents (living mothers aged 15-49yo who are siblings of respondent)
    x1 <- subset(all, type == "child" & child_of == "sister" & relative_of == "respondent" & 
        childs_parent_age %in% c(15:49) & is.na(childs_parent_died_age) )
    
    # Compute Total Fertility Rate
    nrow(x1) / length(unique(x1$sibling_id))
    

  #...................................      
  ## Characteristics of deaths

    # Breakdown of vital status
    nrow(all)
    table(all$status)
    prop.table(table(all$status))    
    
    # Breakdown of vital status by type of person
    table(all$type, all$status)
    prop.table(table(all$type, all$status), margin = 1)
    table(all$type)
    
    # Number of deaths by period and place of death
    table(all$died_period)
    table(all$died_yemen)
    table(all$died_period, all$died_yemen)
    prop.table(table(all$died_period, all$died_yemen), margin = 1)
    x1 <- subset(all, died_yemen == "yes")
    table(x1$type, x1$died_period)

    # Causes of death overall and by period (within Yemen only)
    x1 <- subset(all, died_yemen == "yes")
    table(x1$died_cause)
    prop.table(table(x1$died_cause))
    table(x1$died_cause, x1$died_period)
    

#.........................................................................................
### ENDS



  