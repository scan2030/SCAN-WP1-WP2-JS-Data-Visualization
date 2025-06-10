# SCAN2030: UC Incidence Analysis ----------------------------------------
# Author: Deliang Yang
# Purpose: Calculate Incidence and age-standardized Incidence for depression
# Input: Population data (e.g., Population all age sex_clean.xlsx), 
#        diagnosis data (e.g., IBD_DX converted/)
# Output: Incidence reports, age-specific Incidence, figures, and summary tables



# 1. Load Required Libraries ------------------------------------------------------
library(lubridate)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(openxlsx)  # Alternative: Use writexl if issues occur


# 2. Set Working Directory --------------------------------------------------------
path_to_root <- "~/shared_drive/Personal/Deliang Yang/IBD_UC/"
setwd(path_to_root)


# 3. Function Definitions ---------------------------------------------------------

# Function to unify the names of the features#
my_custom_name_repair <- function(x) tolower(gsub("\\.{1,}",'\\.',gsub("\n|  ", "", make.names(x))))
merge_files <- function(files){
  list_df <- lapply(files,readxl::read_xlsx,.name_repair=my_custom_name_repair) 
  df <- list_df %>% bind_rows(.)
  return(df)
}


# Function to calculate age
calc_age <- function(birthDate, refDate) {
  period <- as.period(interval(birthDate, refDate), unit = 'year')
  return(period$year)
}

# Function: Calculate Incidence for Each Age Group
calculate_age_inci <- function(newoccur_df) {
  boundary <- c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
  age_group_inci <- setNames(rep(0, length(boundary)), boundary)
  male_group_inci <- age_group_inci
  female_group_inci <- age_group_inci
  
  newoccur_df$date.of.birth.yyyy.mm.dd. <- as.Date(newoccur_df$date.of.birth.yyyy.mm.dd., format = "%Y-%m-%d")
  
  for (idx in seq_along(boundary[-length(boundary)])) {
    lower_bound <- boundary[idx]
    upper_bound <- boundary[idx + 1]
    
    
    subset <- newoccur_df %>%
      filter(lower_bound <= age & age< upper_bound)
    
    num_patient <- nrow(subset)
    print(sprintf("When age between %d and %d, the number of unique cases is %d", lower_bound, upper_bound, num_patient))
    
    # num_patient_1 <- nrow(subset_1)
    # print(sprintf("When age between %d and %d, the number of unique cases is %d", lower_bound, upper_bound, num_patient_1))
    
    num_male_patient <- nrow(subset %>% filter(sex. == "M"))
    
    num_population <- as.numeric(sum(population[paste0("Mid-year-", Year)][lower_bound:(lower_bound + 4), 1]) * 1000)
    num_male <- as.numeric(sum(population_male[paste0("Mid-year-", Year)][lower_bound:(lower_bound + 4), 1]) * 1000)
    
    age_group_inci[idx] <- num_patient / num_population * 10000
    male_group_inci[idx] <- num_male_patient / num_male * 10000
    female_group_inci[idx] <- (num_patient - num_male_patient) / (num_population - num_male) * 10000
  }
  
  # Process final age group (85+)
  subset <- newoccur_df %>% filter(age >= 85)
  num_patient <- nrow(subset)
  num_male_patient <- nrow(subset %>% filter(sex. == "M"))
  
  num_population <- as.numeric(population[paste0("Mid-year-", Year)][85, 1]) * 1000
  num_male <- as.numeric(population_male[paste0("Mid-year-", Year)][85, 1]) * 1000
  
  age_group_inci[length(boundary)] <- num_patient / num_population * 10000
  male_group_inci[length(boundary)] <- num_male_patient / num_male * 10000
  female_group_inci[length(boundary)] <- (num_patient - num_male_patient) / (num_population - num_male) * 10000
  
  return(data.frame("Starting Age" = names(age_group_inci),
                    "All.incidence" = unlist(age_group_inci),
                    "Male.incidence" = unlist(male_group_inci),
                    "Female.incidence" = unlist(female_group_inci)))
}

# Function: Calculate age-group population proportions
calculate_proportion <- function(result_df) {
  age_groups <- seq(10, 80, by = 5)  # Note: 85+ will be added separately
  total_popu_2022 <- mid_popu_all[["2022"]]
  total_male_2022 <- mid_popu_male[["2022"]]
  
  age_group_popu_2022 <- setNames(rep(0, length(age_groups) + 1), as.character(c(age_groups, "85+")))
  male_group_popu_2022 <- age_group_popu_2022
  female_group_popu_2022 <- age_group_popu_2022
  
  for (age in age_groups) {
    num_population <- sum(as.numeric(population %>%
                                       filter(Age %in% seq(age, age+4)) %>%
                                       pull("Mid-year-2022")) * 1000)
    
    num_male <- sum(as.numeric(population_male %>%
                                 filter(Age %in% seq(age, age+4)) %>%
                                 pull("Mid-year-2022")) * 1000)
    
    age_group_popu_2022[as.character(age)] <- num_population / total_popu_2022
    male_group_popu_2022[as.character(age)] <- num_male / total_male_2022
    female_group_popu_2022[as.character(age)] <- (num_population - num_male) / (total_popu_2022 - total_male_2022)
  }
  
  # d the 85+ age group
  num_population_85plus <- as.numeric(population %>%
                                        filter(Age == "≥ 85") %>%
                                        pull("Mid-year-2022")) * 1000
  
  num_male_85plus <- as.numeric(population_male %>%
                                  filter(Age == "≥ 85") %>%
                                  pull("Mid-year-2022")) * 1000
  
  age_group_popu_2022["85+"] <- num_population_85plus / total_popu_2022
  male_group_popu_2022["85+"] <- num_male_85plus / total_male_2022
  female_group_popu_2022["85+"] <- (num_population_85plus - num_male_85plus) / (total_popu_2022 - total_male_2022)
  
  # Append proportions to result_df
  result_df <- cbind(result_df, 
                     "Prop.all" = unlist(age_group_popu_2022),
                     "Prop.male" = unlist(male_group_popu_2022),
                     "Prop.female" = unlist(female_group_popu_2022))
  
  return(result_df)
}

# 4. Load and Preprocess Data --------------------------------------------------

#helper ##
my_custom_name_repair <- function(x) tolower(gsub("\\.{1,}",'\\.',gsub("\n|  ", "", make.names(x))))
merge_files <- function(files){
  list_df <- lapply(files,readxl::read_xlsx,.name_repair=my_custom_name_repair) #.name_repair: name column
  df <- list_df %>% bind_rows(.)
  return(df)
}

#1. merge all Dx ####
dxdirectory <- list.files("./IBD_DX converted", full.names = T)
dx.19932023 <- merge_files(dxdirectory)
colnames(dx.19932023) <- tolower(make.names(colnames(dx.19932023)))
alldx<-dx.19932023%>% filter(as.Date(reference.date.)<= as.Date("2022-12-31"))     
alldx_summary <- alldx %>%
  group_by(reference.key.) %>%
  summarize(
    diag_count = n(),  # 计算诊断记录数目
    firstdiag = min(reference.date.),  # 找到最早的诊断日期
    InciYear = year(min(reference.date.))
  )
#test
alldx_summary <-alldx_summary %>% filter()
#sensitivity: one Dx  -alldx_summary
table(alldx_summary$InciYear)
#2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 
#177  153  170  129  154  152  161  184  207  262  259  236  289  279  293  246  300  381  343  382  317  428  366

#2. subtypes-UC/CD
#根据诊断次数分类-找到reference key
# patients_with_multiple_diags 
patients_with_multiple_diags <- alldx_summary %>%
  filter(diag_count >= 2)
# with_one_diag
patients_with_one_diag <- alldx_summary %>%
  filter(diag_count < 2)

#2.1 patients with one diag ----
Dx_onediag_oneyear<-alldx %>% 
  filter(reference.key.%in%patients_with_one_diag$reference.key.) %>% mutate(type = case_when(
    startsWith(as.character(all.diagnosis.code.icd9.), "555") ~ "CD",
    startsWith(as.character(all.diagnosis.code.icd9.), "556") ~ "UC",
    TRUE ~ NA_character_
  ))

#2.2 patients_with_multiple_diags ----
Dx_multiple_diags<-alldx %>% 
  filter(reference.key.%in%patients_with_multiple_diags$reference.key.)
length(unique(Dx_multiple_diags$reference.key.)) 
Dx_multiple_diags <- Dx_multiple_diags %>%
  mutate(diagnosis_date = as.Date(reference.date.), # 确保日期列是Date类型
         type = case_when(
           grepl("^555", all.diagnosis.code.icd9.) ~ "CD",
           grepl("^556", all.diagnosis.code.icd9.) ~ "UC",
           TRUE ~ NA_character_
         ))%>%
  arrange(reference.key., desc(diagnosis_date))
# 计算每个病人的最晚诊断记录日期
latest_diagnosis <- Dx_multiple_diags %>%
  group_by(reference.key.) %>%
  summarize(latest_date = max(diagnosis_date))
# 确定每个病人最晚诊断记录往前推1年内的诊断记录
Dx_multiple_diags_oneyear<- Dx_multiple_diags %>%
  left_join(latest_diagnosis, by = "reference.key.") %>%
  filter(diagnosis_date > latest_date %m-% years(1) & diagnosis_date <= latest_date)
true_types <- Dx_multiple_diags_oneyear %>%
  group_by(reference.key.) %>%
  summarize(true_type = ifelse(n_distinct(type[type != ""]) == 1, first(type[type != ""]), 
                               ifelse(sum(type == "CD") > sum(type == "UC"), "CD",
                                      ifelse(sum(type == "UC") > sum(type == "CD"), "UC",
                                             first(type[type != ""])))))
Dx_multiple_diags <- Dx_multiple_diags %>%dplyr::select(-diagnosis_date,-type) %>% 
  arrange(reference.key.,reference.date.) %>% distinct(reference.key.,.keep_all = T) %>%
  left_join(true_types, by = "reference.key.") %>% mutate(type = true_type) %>% dplyr::select(-true_type)

#2.3 combine----
ibd_dx<-Dx_multiple_diags %>%rbind(Dx_onediag_oneyear) %>% 
  mutate(firstDiag = reference.date.)%>%
  mutate(InciYear = year(firstDiag)) %>% 
  mutate(Incimonth = month(firstDiag)) %>% 
  mutate(Inciage=as.numeric(as.Date(firstDiag)-as.Date(date.of.birth.yyyy.mm.dd.))/365.25) %>% 
  mutate(DeathYear = year(date.of.registered.death.)) %>% 
  mutate(Deathage=as.numeric(as.Date(date.of.registered.death.)-as.Date(date.of.birth.yyyy.mm.dd.))/365.25) 
# Identify first occurrence of each individual
incident <- ibd_dx %>% 
  filter(type=="UC") %>% 
  mutate(year = year(reference.date.))%>% 
  mutate(age = Inciage)



# 5. Incidence Calculation Loop ---------------------------------------------------

years <- 2003:2022
Overall_result <- data.frame(
  Metric = c("Total Cases", "New Cases", "Male Cases", "Female Cases",
             "Crude Incidence", "Male Incidence", "Female Incidence",
             "Age-Standardized Incidence", "Male Age-Standardized", "Female Age-Standardized")
)


population <- read_excel("./Population all age sex_clean.xlsx")
population_male <- read_excel("./population male_clean.xlsx")

# Extract the "All age" row and select only the Mid-year columns for 2003 to 2022
mid_years <- seq(2003, 2022)  # Define the years of interest
mid_columns <- paste0("Mid-year-", mid_years)  # Column names to extract

# Filter the "All age" row
all_age_row <- population[population$Age == "All age", ]
all_age_row_male <- population_male[population_male$Age == "All age", ]

# Create a named list for Mid-year population from 2003 to 2022
mid_popu_all <- as.list(setNames(as.numeric(all_age_row[mid_columns])*1000, mid_years))
mid_popu_male <- as.list(setNames(as.numeric(all_age_row_male[mid_columns]*1000), mid_years))

for (Year in years) {
  unique_df <- incident %>% filter(year == Year)
  newoccur_df <- unique_df %>% filter(year(reference.date.) == Year)
  
  result_df <- calculate_age_inci(newoccur_df)
  result_df <- calculate_proportion(result_df)
  
  crude_incidence <- nrow(newoccur_df) / mid_popu_all[[as.character(Year)]]
  male_crude_incidence <- nrow(newoccur_df %>% filter(sex. == "M")) / mid_popu_male[[as.character(Year)]]
  female_crude_incidence <- (nrow(newoccur_df) - nrow(newoccur_df %>% filter(sex. == "M"))) /
    (mid_popu_all[[as.character(Year)]] - mid_popu_male[[as.character(Year)]])
  
  # Compute age-standardized incidence
  age_standardized_inci <- sum(unlist(result_df$All.incidence) * unlist(result_df$Prop.all))
  male_standardized_inci <- sum(unlist(result_df$Male.incidence) * unlist(result_df$Prop.male))
  female_standardized_inci <- sum(unlist(result_df$Female.incidence) * unlist(result_df$Prop.female))
  
  Overall_result[[as.character(Year)]] <- c(nrow(unique_df), nrow(newoccur_df), nrow(newoccur_df %>% filter(sex. == "M")),
                                            nrow(newoccur_df %>% filter(sex. != "M")),
                                            crude_incidence, male_crude_incidence, female_crude_incidence,
                                            age_standardized_inci, male_standardized_inci, female_standardized_inci)
  
  write.xlsx(result_df, file = sprintf("./results/Data/%d_Age_Group_Incidence.xlsx", Year))
}

# Save Overall Incidence Summary
write.xlsx(Overall_result, file = "./results/Incidence.xlsx")

# 6. Incidence forecasting ---------------------------------------------------
library(forecast)
library(tidyverse)
library(ggplot2)
library(tseries)
library(stats)
library(lmtest)
library(car)
library(readxl)

#1.functions#####
# 定义函数来进行 ARIMA 模型拟合,选择最小AIC/BIC和 MSE 的模型组合
compare_arima_models <- function(data, forecast_years) {
  # 拟合 auto.arima 模型
  arima_model_aic <- auto.arima(data$rate, ic = "aic", trace = TRUE, test="kpss")
  arima_model_bic <- auto.arima(data$rate, ic = "bic", trace = TRUE, test= "kpss")
  model_log_aic <- auto.arima(data$ibd_log, ic = "aic", trace = TRUE, test= "kpss")
  model_log_bic <- auto.arima(data$ibd_log, ic = "bic", trace = TRUE, test= "kpss")
  
  # 计算 model_model_log_aic 在原始尺度上的拟合值和 MSE
  fitted_log_values_aic <- fitted(model_log_aic)
  fitted_values_aic <- exp(fitted_log_values_aic)
  fitted_log_values_bic <- fitted(model_log_bic)  
  fitted_values_bic <- exp(fitted_log_values_bic)  
  
  actual_values <- data$rate
  residuals_original_scale_aic <- actual_values - fitted_values_aic
  residuals_original_scale_bic <- actual_values - fitted_values_bic  
  
  squared_residuals_original_scale_aic <- residuals_original_scale_aic^2
  squared_residuals_original_scale_bic <- residuals_original_scale_bic^2
  
  mse_original_scale_aic <- mean(squared_residuals_original_scale_aic)
  mse_original_scale_bic <- mean(squared_residuals_original_scale_bic)
  
  # 计算 arima_model_aic 和 arima_model_bic 的残差和 MSE
  residuals_aic <- residuals(arima_model_aic)
  residuals_bic <- residuals(arima_model_bic)
  
  mse_aic <- mean(residuals_aic^2)
  mse_bic <- mean(residuals_bic^2)
  
  # 创建包含 MSE 值的命名向量
  mse_values <- c(mse_aic = mse_aic, mse_bic = mse_bic, 
                  mse_original_scale_aic = mse_original_scale_aic,
                  mse_original_scale_bic = mse_original_scale_bic)
  
  # 找出最小的 MSE 和对应的模型
  min_mse <- min(mse_values)
  best_model <- names(mse_values)[which.min(mse_values)]
  
  # 预测步骤
  last_year <- max(data$year)
  last_value <- tail(data$rate, 1)
  
  if (best_model == "mse_aic") {
    model_1 <- arima_model_aic
    forecasted_rate <- forecast(model_1, h = length(forecast_years), level = 95)
    forecast_df <- data.frame(
      year = c(last_year, forecast_years),
      rate_forecast = c(last_value, as.numeric(forecasted_rate$mean)),
      lower_PI = c(last_value, forecasted_rate$lower[, "95%"]),
      upper_PI = c(last_value, forecasted_rate$upper[, "95%"])
    )
  } else if (best_model == "mse_bic") {
    model_1 <- arima_model_bic
    forecasted_rate <- forecast(model_1, h = length(forecast_years), level = 95)
    forecast_df <- data.frame(
      year = c(last_year, forecast_years),
      rate_forecast = c(last_value, as.numeric(forecasted_rate$mean)),
      lower_PI = c(last_value, forecasted_rate$lower[, "95%"]),
      upper_PI = c(last_value, forecasted_rate$upper[, "95%"])
    )
  } else if (best_model == "mse_original_scale_aic") {
    forecasted_log <- forecast(model_log_aic, h = length(forecast_years))
    forecasted_rate <- exp(forecasted_log$mean)
    forecasted_rate_lower <- exp(forecasted_log$lower[, 2])
    forecasted_rate_upper <- exp(forecasted_log$upper[, 2])
    last_actual <- tail(data, 1)
    forecast_df <- tibble(
      year = c(last_actual$year, forecast_years),
      rate_forecast = c(last_actual$rate, as.numeric(forecasted_rate)),
      lower_PI = c(last_actual$rate, forecasted_rate_lower),
      upper_PI = c(last_actual$rate, forecasted_rate_upper)
    )
  }
  else if (best_model == "mse_original_scale_bic") {
    forecasted_log <- forecast(model_log_bic, h = length(forecast_years))
    forecasted_rate <- exp(forecasted_log$mean)
    forecasted_rate_lower <- exp(forecasted_log$lower[, 2])
    forecasted_rate_upper <- exp(forecasted_log$upper[, 2])
    last_actual <- tail(data, 1)
    forecast_df <- tibble(
      year = c(last_actual$year, forecast_years),
      rate_forecast = c(last_actual$rate, as.numeric(forecasted_rate)),
      lower_PI = c(last_actual$rate, forecasted_rate_lower),
      upper_PI = c(last_actual$rate, forecasted_rate_upper)
    )
  }
}

# Define years for the data
years <- 2003:2022
forecast_years <- 2023:2032

# Define the data from Overall_result
Overall_result_for_fore <- list(
  Total_Cases = as.numeric(Overall_result[1,-1]) ,
  Male_Cases = as.numeric(Overall_result[3,-1]) ,
  Female_Cases = as.numeric(Overall_result[4,-1]) ,
  Age_Standardized = as.numeric(Overall_result[8,-1]) ,
  Male_Age_Standardized = as.numeric(Overall_result[9,-1]) ,
  Female_Age_Standardized = as.numeric(Overall_result[10,-1]) 
)

# Forecasting for each metric and combining results into one data frame
forecast_results <- list()
for (metric_name in names(Overall_result_for_fore)) {
  metric_data <- Overall_result_for_fore[[metric_name]]
  data <- tibble(year = years, rate = metric_data, ibd_log = log(metric_data))
  forecast_df <- compare_arima_models(data, forecast_years)
  forecast_df$Metric <- metric_name
  forecast_results[[metric_name]] <- forecast_df
}

# Combine all forecasts into one table
combined_forecasts <- bind_rows(forecast_results, .id = "Metric")

# Combine all forecasts into one Excel file
write.csv(forecast_results, file = "Overall_Forecasts_2023_2032_inc.csv")
