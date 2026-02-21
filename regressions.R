if(!("stargazer" %in% installed.packages()[,"Package"])) install.packages("stargazer") 
library("stargazer")

if(!("dplyr" %in% installed.packages()[,"Package"])) install.packages("dplyr") 
library("dplyr")

if(!("tidyr" %in% installed.packages()[,"Package"])) install.packages("tidyr") 
library("tidyr")

if(!("purrr" %in% installed.packages()[,"Package"])) install.packages("purrr") 
library("purrr")

if(!("car" %in% installed.packages()[,"Package"])) install.packages("car") 
library("car")

if(!("lmtest" %in% installed.packages()[,"Package"])) install.packages("lmtest") 
library("lmtest")

if(!("sandwich" %in% installed.packages()[,"Package"])) install.packages("sandwich") 
library("sandwich")

if(!("readr" %in% installed.packages()[,"Package"])) install.packages("readr") 
library("readr")

if(!("spdep" %in% installed.packages()[,"Package"])) install.packages("spdep") 
library("spdep")

#SET WORKING DIRECTORY  
#setwd("")

#load data
data <- read.csv("data/bulgaria_data_centroids.csv")
data$without_secondary <- data$middle_educ_rate + data$primary_educ_rate
data$rate_pop_school_age <- (data$pop_school_age / data$total_population) * 100
data$popul_density <- data$total_population / data$AREA_KM2
data$log_population <- log(data$total_population)
data$teachers_per_school = data$total_teachers / data$total_schools


#getting VIF function
get_vif_df <- function(model) {
  v <- vif(model)
  df <- as.data.frame(v)
  names(df) <- "VIF"
  tibble::rownames_to_column(df, "Variable")
}

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
                              percentage_roma + 
                              rate_pop_school_age + 
                              teachers_1000_children + 
                              employment_rate +
                              popul_density +
                              rate_city_pop +
                              deprivation_rate, 
                            data = data_centered)


without_secondary_ols_table <- stargazer(without_secondary_ols,
                                           report="vcsp",
                                           type="html",
                                           out="without_secondary_ols_table.html",
                                           covariate.labels = c("Share of Roma population", "Share of population aged 7-19", "Teachers per 1000 children", "Employment rate", "Population density", "Urbanisation rate", "Share of population experiencing material and/or social deprivation"),
                                           dep.var.labels = "Share of population without secondary education",
                                           digits = 3,
                                           title = "Results of OLS regresion")

without_secondary_ols_tongues <- lm(without_secondary ~ 
                                      rate_lang_non_bg +
                                      rate_pop_school_age + 
                                      teachers_1000_children + 
                                      employment_rate +
                                      popul_density +
                                      rate_city_pop +
                                      deprivation_rate, 
                                    data = data_centered)

without_secondary_ols_tongues_table <- stargazer(without_secondary_ols_tongues,
                                         report="vcsp",
                                         type="html",
                                         out="without_secondary_ols_tongues_table.html",
                                         covariate.labels = c("Share of population who declare to not speak Bulgarian as their mother tongue", "Share of population aged 7-19", "Teachers per 1000 children", "Employment rate", "Population density", "Urbanisation rate", "Share of population experiencing material and/or social deprivation"),
                                         dep.var.labels = "Share of population without secondary education",
                                         title = "Results of OLS regresion")

#Calculating AIC and BIC measures to compare
without_secondary_aic <- AIC(without_secondary_ols, 
                             without_secondary_ols_tongues)
without_secondary_bic <- BIC(without_secondary_ols, 
                             without_secondary_ols_tongues)

#calculating VIF
models <- list(
  Model1 = without_secondary_ols,
  Model2 = without_secondary_ols_tongues
)

without_secondary_vif <- models %>%
  map(get_vif_df) %>%
  bind_rows(.id = "Model") %>%
  pivot_wider(
    names_from = Model,
    values_from = -Variable,
    values_fill = NA
  )

#heteroskedasticity test
bptest_without_secondary <- bptest(without_secondary_ols)

  

#join residuals
residuals_without_secondary <- resid(without_secondary_ols)
residuals_df <- as.data.frame(residuals_without_secondary)
data <- cbind(data, residuals_df)


#Join predicted values
predicted_y_without_secondary <- predict(without_secondary_ols)
predicted_df <- as.data.frame(predicted_y_without_secondary)
data <- cbind(data, predicted_df)

#Test spatial autocorrelation
data_moran <- data

coords_matrix <- as.matrix(data_moran[, c("centroid_x", "centroid_y")])

neighbors <- knearneigh(coords_matrix, k = 4)
nb <- knn2nb(neighbors)
lw <- nb2listw(nb, style = "W")  # row-standardized weights

moran_test_without_secondary <- moran.test(data_moran$residuals_without_secondary, lw)

#The residuals are heteroskedastic and spatially autocorellated. We need to
#carry out a spatial model too

#TONGUES
#heteroskedasticity test
bptest_without_secondary_tongues <- bptest(without_secondary_ols_tongues)


#displaying final table with robust errors
robust_se <- sqrt(diag(vcovHC(without_secondary_ols_tongues, type = "HC3")))

without_secondary_ols_tongues_table_robust_errors <- stargazer(without_secondary_ols_tongues,
                                         report="vcsp",
                                         type="html",
                                         out="without_secondary_ols_tongues_table_robust_errors.html",
                                         covariate.labels = c("Share of population who declare to not speak Bulgarian as their mother tongue", "Share of population aged 7-19", "Teachers per 1000 children", "Employment rate", "Population density", "Urbanisation rate", "Share of population experiencing material and/or social deprivation"),
                                         dep.var.labels = "Share of population without secondary education",
                                         digits = 3,
                                         title = "Results of OLS regresion with robust errors",
                                         se=list(robust_se))

#join residuals
residuals_without_secondary_tongues <- resid(without_secondary_ols_tongues)
residuals_df <- as.data.frame(residuals_without_secondary_tongues)
data <- cbind(data, residuals_df)


#Join predicted values
predicted_y_without_secondary_tongues <- predict(without_secondary_ols_tongues)
predicted_df <- as.data.frame(predicted_y_without_secondary_tongues)
data <- cbind(data, predicted_df)

#Test spatial autocorrelation
data_moran <- data

coords_matrix <- as.matrix(data_moran[, c("centroid_x", "centroid_y")])

neighbors <- knearneigh(coords_matrix, k = 4)
nb <- knn2nb(neighbors)
lw <- nb2listw(nb, style = "W")  # row-standardized weights

moran_test_without_secondary_tongues <- moran.test(data_moran$residuals_without_secondary_tongues, lw)

#The residuals are heteroskedastic and spatially autocorellated. We need to
#carry out a spatial model too


#--------------------------------------------------------------------------

#Unemployment regressions
data_centered <- data

#centering data
data_centered$percentage_roma = data_centered$percentage_roma - mean(data_centered$percentage_roma)
data_centered$log_average_salary = log(data_centered$average_salary) - mean(log(data_centered$average_salary))
data_centered$without_secondary = data_centered$without_secondary - mean(data_centered$without_secondary)
data_centered$deprivation_rate = data_centered$deprivation_rate - mean(data_centered$deprivation_rate)
data_centered$popul_density = data_centered$popul_density - mean(data_centered$popul_density)
data_centered$rate_city_pop = data_centered$rate_city_pop - mean(data_centered$rate_city_pop)


#regressions
unemployment_ols <- lm(unemployment_rate ~ 
                         percentage_roma + 
                         log_average_salary + 
                         without_secondary +
                         rate_city_pop +
                         deprivation_rate,
                       data = data_centered)

unemployment_ols_table <- stargazer(unemployment_ols, 
                                    report="vcsp",
                                    type="html",
                                    out="unemployment_ols_table.html",
                                    # covariate.labels = c(),
                                    # dep.var.labels = "",
                                    title = "Results of OLS regresion")

#heteroskedasticity test
bptest_unemployment <- bptest(unemployment_ols)

#displaying with robust errors
robust_se <- sqrt(diag(vcovHC(unemployment_ols, type = "HC3")))

unemployment_ols_table <- stargazer(unemployment_ols, 
                                    report="vcsp",
                                    type="html",
                                    out="unemployment_ols_table_robust_errors.html",
                                    # covariate.labels = c(),
                                    # dep.var.labels = "",
                                    title = "Results of OLS regresion",
                                    se= list(robust_se))


models <- list(
  Model1 = unemployment_ols
)

unemployment_vif <- models %>%
  map(get_vif_df) %>%
  bind_rows(.id = "Model") %>%
  pivot_wider(
    names_from = Model,
    values_from = -Variable,
    values_fill = NA
  )

#Join residuals
residuals_unemployment <- resid(unemployment_ols)
residuals_df <- as.data.frame(residuals_unemployment)
data <- cbind(data, residuals_df)


#Join predicted values
predicted_y_unemployment <- predict(unemployment_ols)
predicted_df <- as.data.frame(predicted_y_unemployment)
data <- cbind(data, predicted_df)

#Test spatial autocorrelation
data_moran <- data

coords_matrix <- as.matrix(data_moran[, c("centroid_x", "centroid_y")])

neighbors <- knearneigh(coords_matrix, k = 4)
nb <- knn2nb(neighbors)
lw <- nb2listw(nb, style = "W")  # row-standardized weights

moran_test_unemployment <- moran.test(data_moran$residuals_unemployment, lw)

#The residuals are heteroskedastic and spatially autocorellated. We need to
#carry out a spatial model too


#--------------------------------------------------------------------------

#health access regression
data_health <- data

data_health$rate_pop_over_65 = (data_health$pop_over_65 / data_health$total_population) * 100

data_centered <- data_health

data_centered$percentage_roma = data_centered$percentage_roma - mean(data_centered$percentage_roma)
data_centered$log_average_salary = log(data_centered$average_salary) - mean(log(data_centered$average_salary))
data_centered$rate_pop_over_65 = data_centered$rate_pop_over_65 - mean(data_centered$rate_pop_over_65)
data_centered$deprivation_rate = data_centered$deprivation_rate - mean(data_centered$deprivation_rate)
data_centered$log_population = data_centered$log_population - mean(data_centered$log_population)
data_centered$deprivation_rate = data_centered$deprivation_rate - mean(data_centered$deprivation_rate)
data_centered$popul_density = data_centered$popul_density - mean(data_centered$popul_density)
data_centered$rate_city_pop = data_centered$rate_city_pop - mean(data_centered$rate_city_pop)

health_access_ols <- lm(doctors_1000 ~ 
                          percentage_roma + 
                          log_average_salary + 
                          rate_pop_over_65 +
                          deprivation_rate +
                          rate_city_pop +
                          popul_density +
                          log_population,
                        data = data_centered)

health_access_ols_table <- stargazer(health_access_ols, 
                                     report="vcsp",
                                     type="html",
                                     out="health_access_ols_table.html",
                                     # covariate.labels = c(),
                                     # dep.var.labels = "",
                                     title = "Results of OLS regresion")


#health_access_aic <- AIC(health_access_ols, 
#                         health_access_ols_simple)
#health_access_bic <- BIC(health_access_ols, 
#                         health_access_ols_simple)

models <- list(
  Model1 = health_access_ols
)

health_access_vif <- models %>%
  map(get_vif_df) %>%
  bind_rows(.id = "Model") %>%
  pivot_wider(
    names_from = Model,
    values_from = -Variable,
    values_fill = NA
  )

#heteroskedasticity test
bptest_health_access <- bptest(health_access_ols)

#displaying with robust errors
robust_se <- sqrt(diag(vcovHC(health_access_ols, type = "HC3")))

unemployment_ols_table <- stargazer(health_access_ols, 
                                    report="vcsp",
                                    type="html",
                                    out="health_access_ols_ols_table_robust_errors.html",
                                    # covariate.labels = c(),
                                    # dep.var.labels = "",
                                    title = "Results of OLS regresion",
                                    se= list(robust_se))

#Join residuals
residuals_health_access <- resid(health_access_ols)
residuals_df <- as.data.frame(residuals_health_access)
data <- cbind(data, residuals_df)


#Join predicted values
predicted_y_health_access <- predict(health_access_ols)
predicted_df <- as.data.frame(predicted_y_health_access)
data <- cbind(data, predicted_df)

#Test spatial autocorrelation
data_moran <- data

coords_matrix <- as.matrix(data_moran[, c("centroid_x", "centroid_y")])

neighbors <- knearneigh(coords_matrix, k = 4)
nb <- knn2nb(neighbors)
lw <- nb2listw(nb, style = "W")  # row-standardized weights

moran_test_health_access <- moran.test(data_moran$residuals_health_access, lw)

#The residuals are heteroskedastic and spatially autocorellated. We need to
#carry out a spatial model too

#--------------------------------------------------------------------------

#Education access regressions
#teachers per 1000 children
data_teachers <- data

data_centered <- data_teachers

#centering data
data_centered$percentage_roma = data_centered$percentage_roma - mean(data_centered$percentage_roma)
data_centered$log_average_salary = log(data_centered$average_salary) - mean(log(data_centered$average_salary))
data_centered$rate_pop_school_age = data_centered$rate_pop_school_age - mean(data_centered$rate_pop_school_age)
data_centered$deprivation_rate = data_centered$deprivation_rate - mean(data_centered$deprivation_rate)
data_centered$schools_1000_children = data_centered$schools_1000_children - mean(data_centered$schools_1000_children)
data_centered$log_population = data_centered$log_population - mean(data_centered$log_population)
data_centered$rate_city_pop = data_centered$rate_city_pop - mean(data_centered$rate_city_pop)
data_centered$popul_density = data_centered$popul_density - mean(data_centered$popul_density)


#regressions
teachers_access_ols <- lm(teachers_1000_children ~ 
                            percentage_roma + 
                            log_average_salary + 
                            rate_pop_school_age +
                            deprivation_rate +
                            schools_1000_children +
                            log_population +
                            rate_city_pop,
                          data = data_centered)

teachers_access_ols_table <- stargazer(teachers_access_ols, 
                                       report="vcsp",
                                       type="html",
                                       out="teachers_access_ols_table.html",
                                       # covariate.labels = c(),
                                       # dep.var.labels = "",
                                       title = "Results of OLS regresion")

#teachers_access_aic <- AIC(teachers_access_ols, 
#                           teachers_access_ols_simple)
#teachers_access_bic <- BIC(teachers_access_ols, 
#                           teachers_access_ols_simple)

models <- list(
  Model1 = teachers_access_ols
)

teachers_access_vif <- models %>%
  map(get_vif_df) %>%
  bind_rows(.id = "Model") %>%
  pivot_wider(
    names_from = Model,
    values_from = -Variable,
    values_fill = NA
  )

#heteroskedasticity test
bptest_teacher_access <- bptest(teachers_access_ols)

#displaying with robust errors
robust_se <- sqrt(diag(vcovHC(teachers_access_ols, type = "HC3")))

teachers_access_ols_table <- stargazer(teachers_access_ols, 
                                    report="vcsp",
                                    type="html",
                                    out="teachers_access_ols_table_robust_errors.html",
                                    # covariate.labels = c(),
                                    # dep.var.labels = "",
                                    title = "Results of OLS regresion",
                                    se= list(robust_se))

#Join residuals
residuals_teacher_access <- resid(teachers_access_ols)
residuals_df <- as.data.frame(residuals_teacher_access)
data <- cbind(data, residuals_df)


#Join predicted values
predicted_y_teacher_access <- predict(teachers_access_ols)
predicted_df <- as.data.frame(predicted_y_teacher_access)
data <- cbind(data, predicted_df)

#Test spatial autocorrelation
data_moran <- data

coords_matrix <- as.matrix(data_moran[, c("centroid_x", "centroid_y")])

neighbors <- knearneigh(coords_matrix, k = 4)
nb <- knn2nb(neighbors)
lw <- nb2listw(nb, style = "W")  # row-standardized weights

moran_test_teacher_access <- moran.test(data_moran$residuals_teacher_access, lw)

#The residuals are heteroskedastic. So we infer using the robust errors. However,
#the Moran test is not significant. So, we keep it at that.


#--------------------------------------------------------------------------

#Teacher per school

data_centered <- data

data_centered$rate_lang_non_bg = data_centered$rate_lang_non_bg - mean(data_centered$rate_lang_non_bg)
data_centered$log_average_salary = log(data_centered$average_salary) - mean(log(data_centered$average_salary))
data_centered$rate_pop_school_age = data_centered$rate_pop_school_age - mean(data_centered$rate_pop_school_age)
data_centered$deprivation_rate = data_centered$deprivation_rate - mean(data_centered$deprivation_rate)
data_centered$log_population = data_centered$log_population - mean(data_centered$log_population)
data_centered$rate_city_pop = data_centered$rate_city_pop - mean(data_centered$rate_city_pop)
data_centered$popul_density = data_centered$popul_density - mean(data_centered$popul_density)



teachers_per_school_ols <- lm(teachers_per_school ~ 
                                percentage_roma + 
                                log_average_salary + 
                                rate_pop_school_age +
                                deprivation_rate +
                                rate_city_pop +
                                popul_density +
                                log_population,
                              data = data_centered)

teachers_per_school_ols_table <- stargazer(teachers_per_school_ols, 
                                           report="vcsp",
                                           type="html",
                                           out="teachers_per_school_ols_table.html",
                                           # covariate.labels = c(),
                                           # dep.var.labels = "",
                                           title = "Results of OLS regresion")

#teachers_per_school_aic <- AIC(teachers_per_school_ols, 
#                               teachers_per_school_ols_simple)
#teachers_per_school_bic <- BIC(teachers_per_school_ols, 
#                               teachers_per_school_ols_simple)

models <- list(
  Model1 = teachers_per_school_ols
)

teachers_per_school_vif <- models %>%
  map(get_vif_df) %>%
  bind_rows(.id = "Model") %>%
  pivot_wider(
    names_from = Model,
    values_from = -Variable,
    values_fill = NA
  )


#heteroskedasticity test
bptest_teacher_per_school <- bptest(teachers_per_school_ols)

#Join residuals
residuals_teachers_per_school <- resid(teachers_per_school_ols)
residuals_df <- as.data.frame(residuals_teachers_per_school)
data <- cbind(data, residuals_df)


#Join predicted values
predicted_y_teachers_per_school  <- predict(teachers_per_school_ols)
predicted_df <- as.data.frame(predicted_y_teachers_per_school )
data <- cbind(data, predicted_df)

#Test spatial autocorrelation
data_moran <- data

coords_matrix <- as.matrix(data_moran[, c("centroid_x", "centroid_y")])

neighbors <- knearneigh(coords_matrix, k = 4)
nb <- knn2nb(neighbors)
lw <- nb2listw(nb, style = "W")  # row-standardized weights

moran_test_teacher_per_school <- moran.test(data_moran$residuals_teachers_per_school, lw)

#Both the BP and the Moran I tests are not significant. Which means that 
#we keep the results of the OLS and we map those out


#--------------------------------------------------------------------------

#Crime rate regression
data_crime <- data

data_crime$log_population <- log(data_crime$total_population)

data_centered <- data_crime

data_centered$percentage_roma = data_centered$percentage_roma - mean(data_centered$percentage_roma)
data_centered$log_average_salary = log(data_centered$average_salary) - mean(log(data_centered$average_salary))
data_centered$employment_rate = data_centered$employment_rate - mean(data_centered$employment_rate)
data_centered$rate_pop_school_age = data_centered$rate_pop_school_age - mean(data_centered$rate_pop_school_age)
data_centered$deprivation_rate = data_centered$deprivation_rate - mean(data_centered$deprivation_rate)
data_centered$without_secondary = data_centered$without_secondary - mean(data_centered$without_secondary)
data_centered$log_population = data_centered$log_population - mean(data_centered$log_population)


crime_rate_ols <- lm(crime_per_1000 ~ 
                       percentage_roma + 
                       log_average_salary +
                       employment_rate +
                       rate_pop_school_age +
                       deprivation_rate +
                       log_population +
                       without_secondary,
                     data = data_centered)

crime_rate_ols_table <- stargazer(crime_rate_ols, 
                                  report="vcsp",
                                  type="html",
                                  out="crime_rate_ols_table.html",
                                  # covariate.labels = c(),
                                  # dep.var.labels = "",
                                  title = "Results of OLS regresion")

#crime_rate_aic <- AIC(crime_rate_ols, 
#                      crime_rate_ols_simple)
#crime_rate_bic <- BIC(crime_rate_ols, 
#                      crime_rate_ols_simple)

models <- list(
  Model1 = crime_rate_ols
)

crime_rate_vif <- models %>%
  map(get_vif_df) %>%
  bind_rows(.id = "Model") %>%
  pivot_wider(
    names_from = Model,
    values_from = -Variable,
    values_fill = NA
  )

#heteroskedasticity test
bptest_crime_rate <- bptest(crime_rate_ols)
                            
#Bad regression with low R squared. Many reasons for that. No real explanation
#behind crime rates. No statistical proof that the Roma contribute.
#Heteroskedastic residuals on top of everything.
#No furhter proceedings.

#--------------------------------------------------------------------------

#PCA computing
data_pca <- data

data_pca$rate_children = (data_pca$pop_children_total / data_pca$total_population) * 100
data_pca$rate_over_65 = (data_pca$pop_over_65 / data_pca$total_population) * 100

data_centered <- data_pca

data_centered$without_secondary = data_centered$without_secondary - mean(data_centered$without_secondary)
data_centered$unemployment_rate = data_centered$unemployment_rate - mean(data_centered$unemployment_rate)
data_centered$crime_per_1000 = data_centered$crime_per_1000 - mean(data_centered$crime_per_1000)
data_centered$doctors_1000 = data_centered$doctors_1000 - mean(data_centered$doctors_1000)
data_centered$deprivation_rate = data_centered$deprivation_rate - mean(data_centered$deprivation_rate)

pca_data <- data_centered %>% select(GISCO_ID, without_secondary, unemployment_rate, crime_per_1000, doctors_1000, deprivation_rate)

pca_data_noid <- pca_data %>% select(without_secondary, unemployment_rate, crime_per_1000, doctors_1000, deprivation_rate)

pca_data_nomissing <- na.omit(pca_data_noid)

pca_data_numeric <- pca_data_nomissing %>% mutate(across(everything(), ~ as.numeric(as.character(.))))

pca_data_scaled <- scale(pca_data_numeric)

pca_result <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

summary(pca_result)
pca_result$rotation  # Loadings (variable contributions)

pc1 <- pca_result$x[,1]
pc1_scores <- pca_data %>% select(GISCO_ID) %>% mutate(pca_comp1 = pc1)

data <- data %>% left_join(pc1_scores, by="GISCO_ID")

data_pca <- data

data_pca$rate_children = (data_pca$pop_children_total / data_pca$total_population) * 100
data_pca$rate_over_65 = (data_pca$pop_over_65 / data_pca$total_population) * 100

data_centered <- data_pca

data_centered$rate_lang_non_bg = data_centered$rate_lang_non_bg - mean(data_centered$rate_lang_non_bg)
data_centered$log_average_salary = log(data_centered$average_salary) - mean(log(data_centered$average_salary))
data_centered$avg_household_size = data_centered$avg_household_size - mean(data_centered$avg_household_size)
data_centered$rate_children = data_centered$rate_children - mean(data_centered$rate_children)
data_centered$rate_over_65 = data_centered$rate_over_65 - mean(data_centered$rate_over_65)
data_centered$rate_city_pop = data_centered$rate_city_pop - mean(data_centered$rate_city_pop)
data_centered$popul_density = data_centered$popul_density - mean(data_centered$popul_density)

pca_ols <- lm(pca_comp1 ~ 
                percentage_roma + 
                log_average_salary +
                avg_household_size +
                rate_children +
                rate_over_65 +
                rate_city_pop +
                popul_density,
              data = data_centered)

pca_ols_table <- stargazer(pca_ols, 
                           report="vcsp",
                           type="html",
                           out="pca_ols_table.html",
                           # covariate.labels = c(),
                           # dep.var.labels = "",
                           title = "Results of OLS regresion")

#pca_aic <- AIC(pca_ols, 
#               pca_ols_simple)
#pca_bic <- BIC(pca_ols, 
#               pca_ols_simple)

models <- list(
  Model1 = pca_ols
)

pca_vif <- models %>%
  map(get_vif_df) %>%
  bind_rows(.id = "Model") %>%
  pivot_wider(
    names_from = Model,
    values_from = -Variable,
    values_fill = NA
  )

#heteroskedasticity test
bptest_pca <- bptest(pca_ols)

#displaying with robust errors
robust_se <- sqrt(diag(vcovHC(pca_ols, type = "HC3")))

pca_ols_table <- stargazer(pca_ols, 
                           report="vcsp",
                           type="html",
                           out="pca_ols_table_robust_errors.html",
                           # covariate.labels = c(),
                           # dep.var.labels = "",
                           title = "Results of OLS regresion",
                           se= list(robust_se))

#Join residuals
residuals_pca <- resid(pca_ols)
residuals_df <- as.data.frame(residuals_pca)
data <- cbind(data, residuals_df)

#Join predicted values
predicted_y_pca <- predict(pca_ols)
predicted_df <- as.data.frame(predicted_y_pca)
data <- cbind(data, predicted_df)

#Test spatial autocorrelation
data_moran <- data

coords_matrix <- as.matrix(data_moran[, c("centroid_x", "centroid_y")])

neighbors <- knearneigh(coords_matrix, k = 4)
nb <- knn2nb(neighbors)
lw <- nb2listw(nb, style = "W")  # row-standardized weights

moran_test_pca <- moran.test(data_moran$residuals_pca, lw)

#Both BP and Moran I tests are significant, which means heteroskedastic and
#spatially non-stationary. We need to do a geographic analysis

data <- data %>% select(-GISCO_ID, -CNTR_CODE, -POP_2024, -POP_DENS_2, -AREA_KM2, -YEAR)

write_csv(data, "data/bulgaria_data_regressions.csv")