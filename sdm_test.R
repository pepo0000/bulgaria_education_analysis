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


round_table <- function(df,
                        coef_d = 3,
                        se_d   = 3,
                        z_d    = 2,
                        p_d    = 3) {
  df %>%
    dplyr::mutate(
      Coefficient     = round(Coefficient, coef_d),
      `Standard error`= round(`Standard error`, se_d),
      `Z-value`       = round(`Z-value`, z_d),
      `p-value`       = round(`p-value`, p_d)
    )
}

#SET WORKING DIRECTORY  
#setwd("")

data <- read.csv("data/bulgaria_data_regressions.csv")

sdm_model <- lagsarlm(
  without_secondary ~ rate_lang_non_bg + rate_pop_school_age + teachers_1000_children + 
    employment_rate + popul_density + rate_city_pop + deprivation_rate,
  data = data_centered, 
  listw = lw,
  type = "mixed"   # this fits the SDM
)
summary(sdm_model)

imp <- impacts(sdm_model, listw = lw, R = 1000)  # R = 1000 sims for CI
summary(imp, zstats=TRUE)

direct <- imp$res$direct
indirect <- imp$res$indirect
total <- imp$res$total

vars <- attr(imp, "bnames")  # should be same as colnames(imp$sres$direct)

#sres$direct is 1000 x 7 (samples x variables)
direct_se <- apply(imp$sres$direct, 2, sd)
indirect_se <- apply(imp$sres$indirect, 2, sd)
total_se <- apply(imp$sres$total, 2, sd)

# Names of variables
vars <- attr(imp, "bnames")  # should be same as colnames(imp$sres$direct)

impact_table_direct <- data.frame(
  Variable = vars,
  Coefficient = round(direct,4),
  Standard_Error = round(direct_se,4)
)

impact_table_indirect <- data.frame(
  Variable = vars,
  Coefficient = round(indirect,4),
  Standard_Error = round(indirect_se,4)
)

impact_table_total <- data.frame(
  Variable = vars,
  Coefficient = round(total,4),
  Standard_Error = round(total_se,4)
)

impact_table_direct <- impact_table_direct %>%
  dplyr::mutate(
    Z_value = Coefficient / Standard_Error,
    P_value = 2 * (1 - pnorm(abs(Z_value))),
    Significance = dplyr::case_when(P_value < 0.001 ~ "***",
                                    P_value < 0.01 ~ "**",
                                    P_value < 0.05 ~ "*",
                                    TRUE ~ "")
  )

impact_table_indirect <- impact_table_indirect %>%
  dplyr::mutate(
    Z_value = Coefficient / Standard_Error,
    P_value = 2 * (1 - pnorm(abs(Z_value))),
    Significance = dplyr::case_when(P_value < 0.001 ~ "***",
                                    P_value < 0.01 ~ "**",
                                    P_value < 0.05 ~ "*",
                                    TRUE ~ "")
  )

impact_table_total <- impact_table_total %>%
  dplyr::mutate(
    Z_value = Coefficient / Standard_Error,
    P_value = 2 * (1 - pnorm(abs(Z_value))),
    Significance = dplyr::case_when(P_value < 0.001 ~ "***",
                                    P_value < 0.01 ~ "**",
                                    P_value < 0.05 ~ "*",
                                    TRUE ~ "")
  )

var_labels <- c(
  rate_lang_non_bg = "Share of population who declare to not speak Bulgarian as their mother tongue",
  rate_pop_school_age = "Share of population aged 7-19",
  teachers_1000_children = "Teachers per 1000 children",
  employment_rate = "Employment rate",
  popul_density = "Population density",
  rate_city_pop = "Urbanisation rate",
  deprivation_rate = "Share of population experiencing material and/or social deprivation"
)

impact_table_direct$Variable <-
  dplyr::recode(impact_table_direct$Variable, !!!var_labels)

impact_table_indirect$Variable <-
  dplyr::recode(impact_table_indirect$Variable, !!!var_labels)

impact_table_total$Variable <-
  dplyr::recode(impact_table_total$Variable, !!!var_labels)

impact_table_direct <- impact_table_direct %>%
  dplyr::rename(
    `Standard error` = Standard_Error,
    `Z-value` = Z_value,
    `p-value` = P_value
  )

impact_table_indirect <- impact_table_indirect %>%
  dplyr::rename(
    `Standard error` = Standard_Error,
    `Z-value` = Z_value,
    `p-value` = P_value
  )

impact_table_total <- impact_table_total %>%
  dplyr::rename(
    `Standard error` = Standard_Error,
    `Z-value` = Z_value,
    `p-value` = P_value
  )

impact_table_direct  <- round_table(impact_table_direct)
impact_table_indirect <- round_table(impact_table_indirect)
impact_table_total   <- round_table(impact_table_total)


impact_table_direct %>%
  kable("html", caption = "Spatial Durbin Model direct impacts", row.names = FALSE, align = c("l", "c", "c", "c", "c", "r")) %>% 
  footnote(general = "*p<0.05; **p<0.01; ***p<0.001") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable("SDM_Impacts_direct_tongues.html")

impact_table_indirect %>%
  kable("html", caption = "Spatial Durbin Model indirect impacts", row.names = FALSE, align = c("l", "c", "c", "c", "c", "r")) %>%
  footnote(general = "*p<0.05; **p<0.01; ***p<0.001") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable("SDM_Impacts_indirect_tongues.html")

impact_table_total %>%
  kable("html", caption = "Spatial Durbin Model total impacts", row.names = FALSE, align = c("l", "c", "c", "c", "c", "r")) %>%
  footnote(general = "*p<0.05; **p<0.01; ***p<0.001") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  save_kable("SDM_Impacts_total_tongues.html")