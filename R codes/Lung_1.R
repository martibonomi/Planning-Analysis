# set up paramenters for configuration

# GRID CONFIGURATION
grid <- list()

grid[["Fields"]] <- data.frame("Field" = c("F0", "F1", "F2"),
                               "Gantry" = c(130, 165, 160),
                               "Couch" = c(180, 0, 180),
                               "Nozzle" = c(15, 15, 15),
                               "Pre Absorber" = c("out", "out", "out"),
                               "Target" = c("PTV", "PTV", "PTV"))

grid[["Field parameters"]] <- data.frame("dT" = 0.47,
                                         "dU" = 0.47,
                                         "energy_layers" = 3,
                                         "structure_margin" = 0.9)

grid[["Shape"]] <- c("Rectilinear","Hexagonal")

grid[["Optimization parameters"]] <- data.frame("Sigma" = 0.2,
                                                "Margin" = 0.5,
                                                "Dose_at_margin" = 0.95,
                                                "Optimization" = "Improved optimization on GPU - Hotspot prevention and Range robust optimization",
                                                "n_iterations" = 150,
                                                "Contraints" = c("structure" = "PRV_spinalcord", "dose" = 50, "weight" = 3))
# AIR CONFIGURATION
air <- list()

air[["Fields"]] <- data.frame("Field" = c("F0", "F1", "F2"),
                              "Gantry" = c(130, 165, 160),
                              "Couch" = c(180, 0, 180),
                              "Nozzle" = c(15, 15, 15),
                              "Pre Absorber" = c("out", "out", "out"),
                              "Target" = c("PTV", "PTV", "PTV"))

air[["Field parameters"]] <- data.frame("energy_layers" = 3,
                                        "spot_spacing_factor" = 1.7)

air[["Shape"]] <- c("Rectilinear","Hexagonal")

air[["Optimization parameters"]] <- data.frame("Sigma" = 0.2,
                                               "Margin" = 0.5,
                                               "Dose_at_margin" = 0.95,
                                               "Optimization" = "Improved optimization on GPU - Hotspot prevention and Range robust optimization",
                                               "n_iterations" = 150,
                                               "Contraints" = c("structure" = "PRV_spinalcord", "dose" = 49, "weight" = 3))

# WATER CONFIGURATION
water <- list()

water[["Fields"]] <- data.frame("Field" = c("F0", "F1", "F2"),
                                "Gantry" = c(130, 165, 160),
                                "Couch" = c(180, 0, 180),
                                "Nozzle" = c(15, 15, 15),
                                "Pre Absorber" = c("out", "out", "out"),
                                "Target" = c("PTV", "PTV", "PTV"))

water[["Field parameters"]] <- data.frame("energy_layers" = 3,
                                        "spot_spacing_factor" = 1.5)

water[["Shape"]] <- c("Rectilinear","Hexagonal")

water[["Optimization parameters"]] <- data.frame("Sigma" = 0.2,
                                                 "Margin" = 0.5,
                                                 "Dose_at_margin" = 0.95,
                                                 "Optimization" = "Improved optimization on GPU - Hotspot prevention and Range robust optimization",
                                                 "n_iterations" = 150,
                                                 "Contraints" = c("structure" = "PRV_spinalcord", "dose" = 50, "weight" = 3))
