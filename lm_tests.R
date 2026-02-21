if(!("sf" %in% installed.packages()[,"Package"])) install.packages("sf") 
library(sf)

if(!("spdep" %in% installed.packages()[,"Package"])) install.packages("spdep") 
library(spdep)

if(!("spatialreg" %in% installed.packages()[,"Package"])) install.packages("spatialreg") 
library(spatialreg)

if(!("dplyr" %in% installed.packages()[,"Package"])) install.packages("dplyr") 
library(dplyr)

if(!("knitr" %in% installed.packages()[,"Package"])) install.packages("knitr") 
library(knitr)

if(!("kableExtra" %in% installed.packages()[,"Package"])) install.packages("kableExtra") 
library(kableExtra)

#SET WORKING DIRECTORY  
#setwd("")

data <- read.csv("data/bulgaria_data_regressions.csv")

data_sf <- st_as_sf(data, coords = c("centroid_x", "centroid_y"), crs = 4326)
data_sp <- as(data_sf, "Spatial")

coords_matrix <- as.matrix(data[, c("centroid_x", "centroid_y")])

neighbors <- knearneigh(coords_matrix, k = 4)
nb <- knn2nb(neighbors)
lw <- nb2listw(nb, style = "W")  # row-standardized weights

#Without secondary education regression
data_centered <- data

#Centering variables
data_centered$percentage_roma = data_centered$percentage_roma - mean(data_centered$percentage_roma)
data_centered$employment_rate = data_centered$employment_rate - mean(data_centered$employment_rate)
data_centered$teachers_1000_children = data_centered$teachers_1000_children - mean(data_centered$teachers_1000_children)
data_centered$rate_pop_school_age = data_centered$rate_pop_school_age - mean(data_centered$rate_pop_school_age)
data_centered$deprivation_rate = data_centered$deprivation_rate - mean(data_centered$deprivation_rate)
data_centered$popul_density = data_centered$popul_density - mean(data_centered$popul_density)
data_centered$rate_city_pop = data_centered$rate_city_pop - mean(data_centered$rate_city_pop)
data_centered$rate_lang_non_bg = data_centered$rate_lang_non_bg - mean(data_centered$rate_lang_non_bg)

#regressions
without_secondary_ols <- lm(without_secondary ~ 
                              rate_lang_non_bg +
                              rate_pop_school_age + 
                              teachers_1000_children + 
                              employment_rate +
                              popul_density +
                              rate_city_pop +
                              deprivation_rate, 
                            data = data_centered)

without_secondary_aic <- AIC(without_secondary_ols)

lm_tests <- lm.RStests(without_secondary_ols, lw, test = c("LMlag", "LMerr", "RLMlag", "RLMerr"))
lm_tests
