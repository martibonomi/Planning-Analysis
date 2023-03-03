plot_DVH_comparison <- function(plans, plan_names, dvh, title){
  
  # --------------------------------------------------------------------------------- #
  # plans [list] -> list of plans you want to compare by plotting them (even one)
  # plan_names [chr] <- vector of names of your plans
  # dvh -> dvh you want to plot in comparison with the others
  # --------------------------------------------------------------------------------- #
  
  # get names of plans and dvh
  names(plans) <- plan_names
  dvh <- toupper(dvh)
  
  dvh_list <- list()
  dvh_df <- list()
  
  # loop over plans to get wanted dvh to plot
  for(plan in plan_names){
    
    # list of dvh you actually want to plot
    dvh_list[[plan]] <- list()
    dvh_list[[plan]][["Dose"]] <- plans[[plan]][["DVH data"]][,1]
    
      
    # select position of the organ in the dvh dataframe
    idx <- which(str_detect(colnames(plans[[plan]][["DVH data"]]), dvh))
      
    # selecting current dvh in the dataframe and current name
    if(dvh == "PTV" || dvh == "CTV" ){
      curr_dvh_name <- as.character((dvh))
    } else {
      curr_dvh_name <- as.character(str_to_title(dvh))
    }
      
    # selecting data of current DVH
    curr_dvh <- plans[[plan]][["DVH data"]][,idx]
      
    # adding current dvh to the dvh list
    dvh_list[[plan]][[curr_dvh_name]] <- curr_dvh
    
    dvh_list[[plan]] <- as.data.frame(dvh_list[[plan]])
    
    # gathering together dvh to plot 
    dvh_df[[plan]]  <- dvh_list[[plan]]  %>% 
      gather(key = "DHV_curve", value = "value", -Dose)
    
    # add label to the plan
    dvh_df[[plan]]$Plan <- plan
    
  }
  
  colors <- c("cornflowerblue","violetred", "orange", "chartreuse","hotpink3", "darkturquoise", "mediumorchid1")
  
  # create plot grid
  plot <- ggplot(dvh_df[[plan_names[1]]], aes(x=Dose, y=value)) + 
    ylim(0,105) + 
    xlab("Dose [%]") +
    ylab("Volume [%]") 
  
  if(title == TRUE){
    # create plot title
    title <- c()
    for(i in 1:length(plan_names)){
      if(i==length(plan_names)){
        title <- paste(title, plan_names[i], sep=" ")
      } else{
        title <- paste(title, plan_names[i], "vs.", sep=" ")
      }
    }
    plot <- plot + ggtitle(title)
  } else if(title == FALSE){
    plot <- plot
  } else{
    plot <- plot + ggtitle(title)
  }
  
  # plot dvh for each plan
  for(plan in plan_names){
    if(str_detect(plan, "underdose")==TRUE || str_detect(plan, "overdose")==TRUE){
      plot <- plot + geom_line(data=dvh_df[[plan]], aes(color = Plan), linewidth=0.4)
    }else{
      plot <- plot + geom_line(data=dvh_df[[plan]], aes(color = Plan), linewidth=0.8)
    }
  }
  
  return(plot)
  
}

plot_robustness <- function(robustness, plan_names, dvhs_stat, title){
  
  # ----------------------------------------------------------------------------------- #
  # robustness [list] <- list of robustness tables/dfs for which you want to plot the robustness spread
  # plan_names [chr] <- vector with names of plans for which you want to plot the robustness
  # dvhs_stat [logic] <- vector with logical conditions for DVHs to plot (either TRUE or FALSE)
  # ----------------------------------------------------------------------------------- #
  
  # for each robustness table in the input list, find robustness spread
  for(r in 1:length(robustness)){
    robustness[[r]] <- find_robustness_spread(robustness[[r]])
  }
  
  # set plan names
  names(robustness) <- plan_names
  dvhs <- toupper(names(dvhs_stat))
  
  dvhs_nom <- list()
  dvhs_min <- list()
  dvhs_max <- list()
  
  nom_df <- list()
  min_df <- list()
  max_df <- list()
  
  # loop over each plan to select just wanted DVHs
  for(plan in plan_names){
    
    # create nominal DVH list
    dvhs_nom[[plan]] <- list()
    dvhs_nom[[plan]][["Dose"]] <- robustness[[plan]]$Dose
    
    # create minima DVH list
    dvhs_min[[plan]] <- list()
    dvhs_min[[plan]][["Dose"]] <- robustness[[plan]]$Dose
    
    # create maxima DVH list
    dvhs_max[[plan]] <- list()
    dvhs_max[[plan]][["Dose"]] <- robustness[[plan]]$Dose
    
    # loop over DVHs to select the TRUE ones
    for(dvh_idx in 1:length(dvhs)){
      
      # if the organ is TRUE, I select the correspondent values
      if(dvhs_stat[[dvh_idx]]==TRUE){
        
        # select position of the organ in the dvh dataframe
        idx_nom <- which(str_detect(colnames(robustness[[plan]]), paste(dvhs[dvh_idx],"_nom",sep="")))
        idx_min <- which(str_detect(colnames(robustness[[plan]]), paste(dvhs[dvh_idx],"_min",sep="")))
        idx_max <- which(str_detect(colnames(robustness[[plan]]), paste(dvhs[dvh_idx],"_max",sep="")))
        
        # selecting current dvh in the dataframe and current name
        if(dvhs[dvh_idx] == "PTV" || dvhs[dvh_idx] == "CTV" ){
          curr_dvh_name <- as.character((dvhs[dvh_idx]))
        } else {
          curr_dvh_name <- as.character(str_to_title(dvhs[dvh_idx]))
        }
        
        # adding current nominal, minima and maxima DVHs to previous lists
        dvhs_nom[[plan]][[curr_dvh_name]] <- robustness[[plan]][,idx_nom]
        dvhs_min[[plan]][[curr_dvh_name]] <- robustness[[plan]][,idx_min]
        dvhs_max[[plan]][[curr_dvh_name]] <- robustness[[plan]][,idx_max]
      }
    }
    
    # transforming lists into dataframes
    dvhs_nom[[plan]] <- as.data.frame(dvhs_nom[[plan]])
    dvhs_min[[plan]] <- as.data.frame(dvhs_min[[plan]])
    dvhs_max[[plan]] <- as.data.frame(dvhs_max[[plan]])
    
    # gather nominal, minima and maxima curves together
    nom_df[[plan]]  <- dvhs_nom[[plan]]  %>% 
      gather(key = "DVH_nom", value = "value", -Dose)
    min_df[[plan]]  <- dvhs_min[[plan]]  %>% 
      gather(key = "DVH_min", value = "value", -Dose)
    max_df[[plan]]  <- dvhs_max[[plan]]  %>% 
      gather(key = "DVH_max", value = "value", -Dose)
    
    # adding plan label
    nom_df[[plan]]$Plan <- plan
    min_df[[plan]]$Plan <- plan
    max_df[[plan]]$Plan <- plan
    
    # adding type label
    nom_df[[plan]]$Curve <- "Nominal"
    min_df[[plan]]$Curve <- "Spread"
    max_df[[plan]]$Curve <- "Spread"
    
    # adding plan and type label
    nom_df[[plan]]$Plan_Curve <- paste(nom_df[[plan]]$Plan, nom_df[[plan]]$DVH_nom, sep=" - ")
    min_df[[plan]]$Plan_Curve <- paste(min_df[[plan]]$Plan, min_df[[plan]]$DVH_min, sep=" - ")
    max_df[[plan]]$Plan_Curve <- paste(max_df[[plan]]$Plan, max_df[[plan]]$DVH_max, sep=" - ")
    
  }
  
  colors <- c("cornflowerblue","violetred", "orange", "chartreuse","deeppink3", "darkturquoise", "mediumorchid1", "gold")
  
  # creating title for the plot
  if(title == TRUE){
    # create plot title
    title <- c()
    for(i in 1:length(plan_names)){
      if(i==length(plan_names)){
        title <- paste(title, plan_names[i], sep=" ")
      } else{
        title <- paste(title, plan_names[i], "vs.", sep=" ")
      }
    }
    plot <- plot + ggtitle(title)
  } else if(title == FALSE){
    plot <- plot
  } else{
    plot <- plot + ggtitle(title)
  }
  
  # creating basic grid for plot
  plot <- ggplot(nom_df[[plan_names[1]]], aes(x=Dose, y=value)) + 
    ylim(0,105) + 
    xlab("Dose [%]") +
    ylab("Volume [%]") +
    ggtitle(title)
  
  # plotting DVHs for each plan
  for(plan in plan_names){
    plot <- plot + geom_line(data=nom_df[[plan]], aes(color = Plan_Curve, linetype=Curve), linewidth=0.8) +
      geom_line(data=min_df[[plan]], aes(color = Plan_Curve, linetype=Curve), linewidth=0.4) +
      geom_line(data=max_df[[plan]], aes(color = Plan_Curve, linetype=Curve), linewidth=0.4)
  }
  
  #plot <- plot + scale_color_manual(name = "DVH curves", values = colors)
  
  return(plot)
  
}

plot_robustness <- function(robustness, plan_names, dvhs_stat, title){
  
  # ----------------------------------------------------------------------------------- #
  # robustness [list] <- list of robustness tables/dfs for which you want to plot the robustness spread
  # plan_names [chr] <- vector with names of plans for which you want to plot the robustness
  # dvhs_stat [logic] <- vector with logical conditions for DVHs to plot (either TRUE or FALSE)
  # ----------------------------------------------------------------------------------- #
  
  # for each robustness table in the input list, find robustness spread
  for(r in 1:length(robustness)){
    robustness[[r]] <- find_robustness_spread(robustness[[r]])
  }
  
  # set plan names
  names(robustness) <- plan_names
  dvhs <- toupper(names(dvhs_stat))
  
  dvhs_nom <- list()
  dvhs_min <- list()
  dvhs_max <- list()
  
  nom_df <- list()
  min_df <- list()
  max_df <- list()
  
  # loop over each plan to select just wanted DVHs
  for(plan in plan_names){
    
    # create nominal DVH list
    dvhs_nom[[plan]] <- list()
    dvhs_nom[[plan]][["Dose"]] <- robustness[[plan]]$Dose
    
    # create minima DVH list
    dvhs_min[[plan]] <- list()
    dvhs_min[[plan]][["Dose"]] <- robustness[[plan]]$Dose
    
    # create maxima DVH list
    dvhs_max[[plan]] <- list()
    dvhs_max[[plan]][["Dose"]] <- robustness[[plan]]$Dose
    
    # loop over DVHs to select the TRUE ones
    for(dvh_idx in 1:length(dvhs)){
      
      # if the organ is TRUE, I select the correspondent values
      if(dvhs_stat[[dvh_idx]]==TRUE){
        
        # select position of the organ in the dvh dataframe
        idx_nom <- which(str_detect(colnames(robustness[[plan]]), paste(dvhs[dvh_idx],"_nom",sep="")))
        idx_min <- which(str_detect(colnames(robustness[[plan]]), paste(dvhs[dvh_idx],"_min",sep="")))
        idx_max <- which(str_detect(colnames(robustness[[plan]]), paste(dvhs[dvh_idx],"_max",sep="")))
        
        # selecting current dvh in the dataframe and current name
        if(dvhs[dvh_idx] == "PTV" || dvhs[dvh_idx] == "CTV" ){
          curr_dvh_name <- as.character((dvhs[dvh_idx]))
        } else {
          curr_dvh_name <- as.character(str_to_title(dvhs[dvh_idx]))
        }
        
        # adding current nominal, minima and maxima DVHs to previous lists
        dvhs_nom[[plan]][[curr_dvh_name]] <- robustness[[plan]][,idx_nom]
        dvhs_min[[plan]][[curr_dvh_name]] <- robustness[[plan]][,idx_min]
        dvhs_max[[plan]][[curr_dvh_name]] <- robustness[[plan]][,idx_max]
      }
    }
    
    # transforming lists into dataframes
    dvhs_nom[[plan]] <- as.data.frame(dvhs_nom[[plan]])
    dvhs_min[[plan]] <- as.data.frame(dvhs_min[[plan]])
    dvhs_max[[plan]] <- as.data.frame(dvhs_max[[plan]])
    
    # gather nominal, minima and maxima curves together
    nom_df[[plan]]  <- dvhs_nom[[plan]]  %>% 
      gather(key = "DVH_nom", value = "value", -Dose)
    min_df[[plan]]  <- dvhs_min[[plan]]  %>% 
      gather(key = "DVH_min", value = "value", -Dose)
    max_df[[plan]]  <- dvhs_max[[plan]]  %>% 
      gather(key = "DVH_max", value = "value", -Dose)
    
    # adding plan label
    nom_df[[plan]]$Plan <- plan
    min_df[[plan]]$Plan <- plan
    max_df[[plan]]$Plan <- plan
    
    # adding type label
    nom_df[[plan]]$Curve <- "Nominal"
    min_df[[plan]]$Curve <- "Spread"
    max_df[[plan]]$Curve <- "Spread"
    
    # adding plan and type label
    nom_df[[plan]]$Plan_Curve <- paste(nom_df[[plan]]$Plan, nom_df[[plan]]$DVH_nom, sep=" - ")
    min_df[[plan]]$Plan_Curve <- paste(min_df[[plan]]$Plan, min_df[[plan]]$DVH_min, sep=" - ")
    max_df[[plan]]$Plan_Curve <- paste(max_df[[plan]]$Plan, max_df[[plan]]$DVH_max, sep=" - ")
    
  }
  
  colors <- c("cornflowerblue","violetred", "orange", "chartreuse","deeppink3", "darkturquoise", "mediumorchid1", "gold")
  
  # creating title for the plot
  if(title == TRUE){
    # create plot title
    title <- c()
    for(i in 1:length(plan_names)){
      if(i==length(plan_names)){
        title <- paste(title, plan_names[i], sep=" ")
      } else{
        title <- paste(title, plan_names[i], "vs.", sep=" ")
      }
    }
    plot <- plot + ggtitle(title)
  } else if(title == FALSE){
    plot <- plot
  } else{
    plot <- plot + ggtitle(title)
  }
  
  # creating basic grid for plot
  plot <- ggplot(nom_df[[plan_names[1]]], aes(x=Dose, y=value)) + 
    ylim(0,105) + 
    xlab("Dose [%]") +
    ylab("Volume [%]") +
    ggtitle(title)
  
  # plotting DVHs for each plan
  for(plan in plan_names){
    plot <- plot + geom_line(data=nom_df[[plan]], aes(color = Plan_Curve, linetype=Curve), linewidth=0.8) +
      geom_line(data=min_df[[plan]], aes(color = Plan_Curve, linetype=Curve), linewidth=0.4) +
      geom_line(data=max_df[[plan]], aes(color = Plan_Curve, linetype=Curve), linewidth=0.4)
  }
  
  #plot <- plot + scale_color_manual(name = "DVH curves", values = colors)
  
  return(plot)
  
}


read_robustness <- function(csv_file, organs){
  
  # ---------------------------------------------------- #
  # csv_file <- csv file of robustness, FIonA output
  # organs [chr] <- vector of organs of your plan
  # ---------------------------------------------------- #
  
  # read csv file
  robustness <- read.csv(csv_file)
  
  # get DVH names
  dvhs <- c("PTV", "CTV", organs)
  
  # rename first column as "Dose
  colnames(robustness)[1] <- "Dose"
  
  # rename columns of robustness table assigning correct numbers (num 5 is nominal)
  for(i in 1:length(dvhs)){
    
    # get column idxs for current DVH
    curr_idxs <- which(str_detect(colnames(robustness), regex(dvhs[i], ignore_case = TRUE)))
    
    # rename each column assigning correct numbers
    for(j in 1:length(curr_idxs)){
      colnames(robustness)[curr_idxs[j]] <- paste(dvhs[i], j, sep="_")
    }
    
  }
  
  # find NA values and substitute with 0
  robustness[is.na(robustness)] <- 0   
  rm(i,j,dvhs)
  
  return(robustness)
}


robustness <- read.csv("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/DIBH Lungs/Robustness DVHs/R_grid.csv")
organs <- c("Esophagus", "Heart", "Medulla", "Lungs", "PRV_SpinalCord")

rob <- read_robustness("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/DIBH Lungs/Robustness DVHs/R_grid.csv", organs = organs)