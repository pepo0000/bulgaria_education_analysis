if(!("sf" %in% installed.packages()[,"Package"])) install.packages("sf") 
library(sf)

if(!("spdep" %in% installed.packages()[,"Package"])) install.packages("spdep") 
library(spdep)

if(!("dplyr" %in% installed.packages()[,"Package"])) install.packages("dplyr") 
library(dplyr)

if(!("readr" %in% installed.packages()[,"Package"])) install.packages("readr") 
library("readr")

#SET WORKING DIRECTORY  
#setwd("")

data <- read.csv("data/bulgaria_data_regressions.csv")

data_sf <- st_as_sf(data, coords = c("centroid_x", "centroid_y"), crs = 4326)
data_sp <- as(data_sf, "Spatial")

coords_matrix <- as.matrix(data[, c("centroid_x", "centroid_y")])

neighbors <- knearneigh(coords_matrix, k = 4)
nb <- knn2nb(neighbors)
lw <- nb2listw(nb, style = "W")  # row-standardized weights

moran_test_roma <- moran.test(data$percentage_roma, lw)
moran_test_rate_lang_non_bg <- moran.test(data$rate_lang_non_bg, lw)


#Without secondary with roma percentage as X
lisa_without_secondary <- localmoran(data_sp$residuals_without_secondary, lw)

lisa_without_secondary_df <- as.data.frame(lisa_without_secondary)
lisa_without_secondary_df <- lisa_without_secondary_df %>% select(-Ii, -E.Ii, -Var.Ii, -Z.Ii)
colnames(lisa_without_secondary_df) <- c("lisa_without_secondary_p_value")

data <- bind_cols(data, lisa_without_secondary_df)

data <- data %>%
  mutate(
    res_std_without_secondary = scale(data$residuals_without_secondary)[,1],
    lag_res_without_secondary = lag.listw(lw, data$residuals_without_secondary),
    lag_std_without_secondary = scale(lag_res_without_secondary)[,1],
    cluster_without_secondary = case_when(
      res_std_without_secondary > 0 & lag_std_without_secondary > 0 & lisa_without_secondary_p_value <= 0.05 ~ "High-High",
      res_std_without_secondary < 0 & lag_std_without_secondary < 0 & lisa_without_secondary_p_value <= 0.05 ~ "Low-Low",
      res_std_without_secondary > 0 & lag_std_without_secondary < 0 & lisa_without_secondary_p_value <= 0.05 ~ "High-Low",
      res_std_without_secondary < 0 & lag_std_without_secondary > 0 & lisa_without_secondary_p_value <= 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    )
  )

#Without secondary with non BG speakers as X
lisa_without_secondary_tongues <- localmoran(data_sp$residuals_without_secondary_tongues, lw)

lisa_without_secondary_df_tongues <- as.data.frame(lisa_without_secondary_tongues)
lisa_without_secondary_df_tongues <- lisa_without_secondary_df_tongues %>% select(-Ii, -E.Ii, -Var.Ii, -Z.Ii)
colnames(lisa_without_secondary_df_tongues) <- c("lisa_without_secondary_tongues_p_value")

data <- bind_cols(data, lisa_without_secondary_df_tongues)

data <- data %>%
  mutate(
    res_std_without_secondary_tongues = scale(data$residuals_without_secondary_tongues)[,1],
    lag_res_without_secondary_tongues = lag.listw(lw, data$residuals_without_secondary_tongues),
    lag_std_without_secondary_tongues = scale(lag_res_without_secondary_tongues)[,1],
    cluster_without_secondary_tongues = case_when(
      res_std_without_secondary_tongues > 0 & lag_std_without_secondary_tongues > 0 & lisa_without_secondary_tongues_p_value <= 0.05 ~ "High-High",
      res_std_without_secondary_tongues < 0 & lag_std_without_secondary_tongues < 0 & lisa_without_secondary_tongues_p_value <= 0.05 ~ "Low-Low",
      res_std_without_secondary_tongues > 0 & lag_std_without_secondary_tongues < 0 & lisa_without_secondary_tongues_p_value <= 0.05 ~ "High-Low",
      res_std_without_secondary_tongues < 0 & lag_std_without_secondary_tongues > 0 & lisa_without_secondary_tongues_p_value <= 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    )
  )

#Roma LISA
lisa_roma <- localmoran(data_sp$percentage_roma, lw)

lisa_roma_df <- as.data.frame(lisa_roma)
lisa_roma_df <- lisa_roma_df %>% select(-Ii, -E.Ii, -Var.Ii, -Z.Ii)
colnames(lisa_roma_df) <- c("lisa_roma_p_value")

data <- bind_cols(data, lisa_roma_df)

data <- data %>%
  mutate(
    res_std_roma = scale(data$percentage_roma)[,1],
    lag_res_roma = lag.listw(lw, data$percentage_roma),
    lag_std_roma = scale(lag_res_roma)[,1],
    cluster_roma = case_when(
      res_std_roma > 0 & lag_std_roma > 0 & lisa_roma_p_value <= 0.05 ~ "High-High",
      res_std_roma < 0 & lag_std_roma < 0 & lisa_roma_p_value <= 0.05 ~ "Low-Low",
      res_std_roma > 0 & lag_std_roma < 0 & lisa_roma_p_value <= 0.05 ~ "High-Low",
      res_std_roma < 0 & lag_std_roma > 0 & lisa_roma_p_value <= 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    )
  )

#Non-Bulgarian speakers LISA
lisa_rate_lang_non_bg <- localmoran(data_sp$rate_lang_non_bg, lw)

lisa_rate_lang_non_bg_df <- as.data.frame(lisa_rate_lang_non_bg)
lisa_rate_lang_non_bg_df <- lisa_rate_lang_non_bg_df %>% select(-Ii, -E.Ii, -Var.Ii, -Z.Ii)
colnames(lisa_rate_lang_non_bg_df) <- c("lisa_rate_lang_non_bg_p_value")

data <- bind_cols(data, lisa_rate_lang_non_bg_df)

data <- data %>%
  mutate(
    res_std_rate_lang_non_bg = scale(data$rate_lang_non_bg)[,1],
    lag_res_rate_lang_non_bg = lag.listw(lw, data$rate_lang_non_bg),
    lag_std_rate_lang_non_bg = scale(lag_res_rate_lang_non_bg)[,1],
    cluster_rate_lang_non_bg = case_when(
      res_std_rate_lang_non_bg > 0 & lag_std_rate_lang_non_bg > 0 & lisa_rate_lang_non_bg_p_value <= 0.05 ~ "High-High",
      res_std_rate_lang_non_bg < 0 & lag_std_rate_lang_non_bg < 0 & lisa_rate_lang_non_bg_p_value <= 0.05 ~ "Low-Low",
      res_std_rate_lang_non_bg > 0 & lag_std_rate_lang_non_bg < 0 & lisa_rate_lang_non_bg_p_value <= 0.05 ~ "High-Low",
      res_std_rate_lang_non_bg < 0 & lag_std_rate_lang_non_bg > 0 & lisa_rate_lang_non_bg_p_value <= 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    )
  )


write_csv(data, "data/bulgaria_data_complete_lisa.csv")