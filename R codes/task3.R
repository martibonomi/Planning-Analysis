library(ggplot2)
library(tidyverse)
library(Matrix)
library(scales)
library(cowplot)
library(RCurl)
library(ggplot2)
library(devtools)
library(reconPlots)

plan1 <- read_plan("plan1_dvhs.csv")
plot_dvh(plan1, "Plan 1") 

plan2 <- read_plan("plan2_dvhs.csv")
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

plot_dvh_comparison(plan1, "Plan 1", plan2, "Plan 2")


img <- readPNG(source="Task3/P1_P2_comparison.png")
grid::grid.raster(img)
