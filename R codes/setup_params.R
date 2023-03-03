# Plan 1 - sigma=0.8, margin=0.2, dose_margin=0.9, e_layers=1

table_basic_1 <- data.frame("Configuration" = c("R Grid", "H Grid", "R Air", "H Air", "R Water", "H Water"),
                            "n_spots" = c(14435, 14866,	7474,	7435,	6515,	6460),
                            "PTV_V95" = c(93.75,	94.20,	95.14,	95.57,	96.67,	95.84),
                            "Plan" = rep(1, each=6),
                            "Sigma" = as.character(rep(0.8, each=6)),
                            "Margin" = as.character(rep(0.2, each=6)),
                            "Dose Margin" = as.character(rep(0.9, each=6)),
                            "Energy Layers" = as.character(rep(1, each=6)))


# Plan 3 - sigma=0.8, margin=0.2, dose_margin=0.9, e_layers=5

table_basic_3 <- data.frame("Configuration" = c("R Grid", "H Grid", "R Air", "H Air", "R Water", "H Water"),
                            "n_spots" = c(1533, 1520, 860, 873, 1134, 1107),
                            "PTV_V95" = c(89.99, 90.79, 78.49, 78.87, 87.23, 87.40),
                            "Plan" = rep(3, each=6),
                            "Sigma" = as.character(rep(0.8, each=6)),
                            "Margin" = as.character(rep(0.2, each=6)),
                            "Dose Margin" = as.character(rep(0.9, each=6)),
                            "Energy Layers" = as.character(rep(5, each=6)))

# Plan 3 - sigma=0.8, margin=0.2, dose_margin=0.9, e_layers=3

table_elayers_3 <- data.frame("Configuration" = c("R Grid", "H Grid", "R Air", "H Air", "R Water", "H Water"),
                              "n_spots" = c(1813,	2237,	1671,	1698,	1387,	1470),
                              "PTV_V95" = c(89.02,	91.87,	88.16,	87.85,	88.89,	88.60),
                              "Plan" = rep(3, each=6),
                              "Sigma" = as.character(rep(0.8, each=6)),
                              "Margin" = as.character(rep(0.2, each=6)),
                              "Dose Margin" = as.character(rep(0.9, each=6)),
                              "Energy Layers" = as.character(rep(3, each=6)))

# Plan 3 - sigma=0.4, margin=0.2, dose_margin=0.9, e_layers=5

table_sigma_04 <- data.frame("Configuration" = c("R Grid", "H Grid", "R Air", "H Air", "R Water", "H Water"),
                             "n_spots" = c(1197,	1524,	870,	887,	1150,	1112),
                             "PTV_V95" = c(87.22,	92.01,	78.78,	79.09,	88.04,	88.18),
                             "Plan" = rep(3, each=6),
                             "Sigma" = as.character(rep(0.4, each=6)),
                             "Margin" = as.character(rep(0.2, each=6)),
                             "Dose Margin" = as.character(rep(0.9, each=6)),
                             "Energy Layers" = as.character(rep(5, each=6)))

# Plan 3 - sigma=0.8, margin=0.4, dose_margin=0.9, e_layers=5

table_margin_04 <- data.frame("Configuration" = c("R Grid", "H Grid", "R Air", "H Air", "R Water", "H Water"),
                              "n_spots" = c(1131,	1515,	857,	864,	1117,	1089),
                              "PTV_V95" = c(88.93,	94.41,	79.01,	79.54,	89.50,	89.35),
                              "Plan" = rep(3, each=6),
                              "Sigma" = as.character(rep(0.8, each=6)),
                              "Margin" = as.character(rep(0.4, each=6)),
                              "Dose Margin" = as.character(rep(0.9, each=6)),
                              "Energy Layers" = as.character(rep(5, each=6)))

# Plots for comparison
sigma_comparison <- rbind(table_basic_3, table_sigma_04)
elayers_comparison <- rbind(table_basic_3, table_elayers_3)
margin_comparison <- rbind(table_basic_3, table_margin_04)


ggplot(sigma_comparison, aes(x=PTV_V95, y=n_spots)) + 
  geom_point(aes(x=PTV_V95, y=n_spots, color=Configuration, shape=Sigma), size=3) + 
  xlab("PTV V95") +
  ylab("Number of Spots") +
  ggtitle("PTV-V95 vs. # spots")

ggplot(elayers_comparison, aes(x=PTV_V95, y=n_spots)) + 
  geom_point(aes(x=PTV_V95, y=n_spots, color=Configuration, shape=Energy.Layers), size=3) +
  xlab("PTV V95") +
  ylab("Number of Spots") +
  ggtitle("PTV-V95 vs. # spots")

ggplot(margin_comparison, aes(x=PTV_V95, y=n_spots)) + 
  geom_point(aes(x=PTV_V95, y=n_spots, color=Configuration, shape=Margin), size=3) +
  xlab("PTV V95") +
  ylab("Number of Spots") +
  ggtitle("PTV-V95 vs. # spots")


opt_table <- read.csv("/Users/martinabonomi/Library/Mobile Documents/com~apple~CloudDocs/Planning/Plan 3 - optimization.csv", header=TRUE,sep=";")
opt_table$Sigma <- as.character(opt_table$Sigma)
opt_table$Margin <- as.character(opt_table$Margin)
opt_table$Dose.at.Margin <- as.character(opt_table$Dose.at.Margin)
opt_table$Energy.layers <- as.character(opt_table$Energy.layers)
opt_table$Spot.spacing <- as.character(opt_table$Spot.spacing)



ggplot(opt_table, aes(x=PTV_V95, y=n_spots)) + 
  geom_point(aes(x=PTV_V95, y=n_spots, color=Configuration), size=4) +
  xlab("PTV V95") +
  ylab("Number of Spots") +
  ggtitle("PTV-V95 vs. # spots")

