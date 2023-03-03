library(ggplot2)
library(tidyverse)
library(Matrix)
library(scales)
library(cowplot)
library(RCurl)
library(ggplot2)
library(devtools)
library(reconPlots)


setwd("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/Liver5")

plan1 <- read_plan("Plan 1/dvhs_nominal.csv")
plot_dvh(plan1, "Plan 1") 
plot_dvh_select(plan1, "Plan 1 ", PTV=TRUE, CTV=TRUE, Liver=TRUE, Stomach=TRUE)


plan2 <- read_plan("Liver5/plan2_dvhs.csv")
plot_dvh(plan2, "Plan 2")

comparison <- get_comparison("task3_table.csv", "Plan 1", "Plan 2")

# Plan 1
D2 <- find_Dv(plan1, 2, "Liver")
D5 <- find_Dv(plan1, 5, "Stomach")
V50 <- find_Vd(plan1, 50, "Liver")

# Plan 2
D2 <- find_Dv(plan2, 2, "Liver")
D5 <- find_Dv(plan2, 5, "Stomach")
V50 <- find_Vd(plan2, 50, "Liver")

plot_dvh_comparison(plans=list(plan2), plan_names = c("Plan 2"), dvhs_stat = c(PTV=TRUE,CTV=TRUE,STOMACH=TRUE,LIVER=TRUE))

r1 <- read_robustness("Plan 1/dvhs_robustness.csv", organs = c("liver","stomach"))
r2 <- read_robustness("Liver5/robustness_dvhs_p2.csv", organs = c("liver","stomach"))

plot_robustness(r)
r_spread <- find_robustness_spread(r)

p1_overdose <- read_plan("Liver5/plan1_overdose.csv")
p1_underdose <- read_plan("Liver5/plan1_underdose.csv")

p2_overdose <- read_plan("Liver5/plan2_overdose.csv")
p2_underdose <- read_plan("Liver5/plan2_underdose.csv")

r <- list(r1,r2)
names(r) <- c("Plan 1", "Plan 2")

plot_DVHs(plans = list(plan1,p1_overdose,p1_underdose), plan_names = c("Plan 1 - nominal", "Plan 1 - overdose", "Plan 1 - underdose"), dvhs_stat = c(PTV=TRUE,CTV=TRUE,STOMACH=TRUE,LIVER=TRUE), title="Underdose vs. Overdose")

plot_DVHs(plans = list(plan1,plan2), plan_names = c("Plan 1", "Plan 2"), dvhs_stat = c(PTV=TRUE, CTV=TRUE, LIVER=TRUE, STOMACH=TRUE)) 

reduction_time_percentage <- (dt_p1-dt_p3)/dt_p1

plan3 <- read_plan("Plan 3/dvhs_nominal.csv")

# read optimization table
opt_table <- read.csv("Liver5/best_opt.csv", header=TRUE,sep=";")
opt_table$Sigma <- as.character(opt_table$Sigma)
opt_table$Margin <- as.character(opt_table$Margin)
opt_table$Dose.at.Margin <- as.character(opt_table$Dose.at.Margin)
opt_table$Energy.layers <- as.character(opt_table$Energy.layers)

# plot the PTV V95 vs. spots
ggplot(opt_table, aes(x=PTV_V95, y=n_spots)) + 
  geom_point(aes(x=PTV_V95, y=n_spots, color=Configuration), size=4) +
  xlab("PTV V95 [%]") +
  ylab("Number of Spots")

# load dvhs for each configuration
R_grid <- read_plan("Liver5/DVHs optimization/Rgrid.csv")
H_grid <- read_plan("Liver5/DVHs optimization/Hgrid.csv")
R_air <- read_plan("Liver5/DVHs optimization/Rair.csv")
H_air <- read_plan("Liver5/DVHs optimization/Hair.csv")
R_water <- read_plan("Liver5/DVHs optimization/Rwat.csv")
H_water <- read_plan("Liver5/DVHs optimization/Hwat.csv")

plans <- list(R_grid, H_grid, R_air, H_air, R_water, H_water)
plan_names <- c("R grid", "H grid", "R air (ssf = 1.2)", "H air (ssf = 1.2)", "R water (ssf = 1.5)", "H water (ssf = 1.5)")

PTV <- plot_DVH_comparison(plans = plans, plan_names = plan_names, dvh = "PTV", title = "PTV comparison for the different configurations")
PTV + coord_cartesian(xlim=c(85,120))

liver <- plot_DVH_comparison(plans = plans, plan_names = plan_names, dvh = "liver", title = "Liver comparison for the different configurations")
liver + coord_cartesian(ylim=c(0,20))

stomach <- plot_DVH_comparison(plans = plans, plan_names = plan_names, dvh = "stomach", title = "Stomach comparison for the different configurations")
stomach + coord_cartesian(ylim=c(0,25))

# robustness spread
CTV_Rgrid <- read_robustness("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/Liver5/DVHs robustness/CTV_Rgrid.csv", organs = c())
CTV_Hgrid <- read_robustness("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/Liver5/DVHs robustness/CTV_Hgrid.csv", organs = c())
CTV_Rair <- read_robustness("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/Liver5/DVHs robustness/CTV_Rair.csv", organs = c())
CTV_Hair <- read_robustness("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/Liver5/DVHs robustness/CTV_Hair.csv", organs = c())
CTV_Rwat <- read_robustness("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/Liver5/DVHs robustness/CTV_Rwat.csv", organs = c())
CTV_Hwat <- read_robustness("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/Liver5/DVHs robustness/CTV_Hwat.csv", organs = c())

CTV_rob <- plot_robustness(robustness = list(CTV_Rgrid, CTV_Hgrid, CTV_Rair, CTV_Hair, CTV_Rwat, CTV_Hwat), plan_names = c("R grid", "H grid", "R air", "H air", "R water", "H water"), dvhs_stat = c(PTV=FALSE, CTV=TRUE, LIVER=FALSE, STOMACH=FALSE), title = "CTV robustness spread")
CTV_rob + coord_cartesian(xlim=c(85,120))



