# SCAN2030: Depression Prevalence Analysis ----------------------------------------
# Author: Deliang Yang
# Purpose: Calculate prevalence and age-standardized prevalence for depression
# Input:  Diagnosis data (e.g., converted_data_dx/new data/)
# Output: Prevalence reports, age-specific Prevalence, figures, and summary tables

# 1. Libraries and Environment Setup ----------------------------------------------
library(readxl)    # For reading Excel files
library(ggplot2)   # For creating plots
library(openxlsx)  # For writing Excel files

# Set the root directory where all relevant files and data are stored
path_to_root = "~/shared_drive/Personal/Deliang Yang/IBD_CD/"
setwd(path_to_root)

# 2. Population Data --------------------------------------------------------------
# Mid-year population estimates for all ages and males
# Source: https://www.censtatd.gov.hk/en/web_table.html?id=110-01002#
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

# Define age group boundaries for prevalence calculations
boundary = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)

# 3. Functions --------------------------------------------------------------------

# Function 1: Data Preprocessing --------------------------------------------------
pre_processing = function(conv_df) {
  # Rename columns for clarity
  colnames(conv_df)[c(1,4,2,15)] = c("Key", "Sex", "Birth", "RefDate")
  # Convert Birth and RefDate fields to date format
  conv_df = transform(conv_df, Birth = as.Date(Birth), RefDate = as.Date(RefDate))
  # Sort by Key and retain unique records
  ordered_df = conv_df[order(conv_df$Key), ]
  unique_df = ordered_df[!duplicated(ordered_df$Key), ]
  return(unique_df)
}

# Function 2: Calculate Prevalence by Age Group -----------------------------------
calculate_age_prev = function(unique_df) {
  # Initialize prevalence rates for each age group
  age_group_prev = list("10"=0, "15"=0, "20"=0, "25"=0, "30"=0, "35"=0, "40"=0, "45"=0,
                        "50"=0, "55"=0, "60"=0, "65"=0, "70"=0, "75"=0, "80"=0, "85"=0)
  male_group_prev = age_group_prev
  female_group_prev = age_group_prev
  
  idx = 1
  col_name = sprintf("Mid-year-%d", Year)
  
  while (idx < length(boundary)) {
    # Define age group boundaries
    lower_bound = boundary[idx]
    upper_bound = boundary[idx + 1]
    
    # Subset data for the current age group
    subset = unique_df%>%
      filter(lower_bound <= current_age & current_age< upper_bound)
    num_patient = nrow(subset)
    num_male_patient = nrow(subset[subset$Sex == "M", ])
    
    # Calculate population in the age group
    num_population = 0
    num_male = 0
    for (current_age in seq(lower_bound, by = 1, length.out = 5)) {
      num_population = num_population + as.numeric(population[col_name][current_age, 1]) * 1000
      num_male = num_male + as.numeric(population_male[col_name][current_age, 1]) * 1000
    }
    
    # Calculate prevalence rates
    age_group_prev[idx] = num_patient / num_population * 10000
    male_group_prev[idx] = num_male_patient / num_male * 10000
    female_group_prev[idx] = (num_patient - num_male_patient) / (num_population - num_male) * 10000
    
    idx = idx + 1
  }
  
  # Calculate prevalence for age group 85+
  subset = unique_df%>%
    filter(85 <= current_age )
  num_patient = nrow(subset)
  num_male_patient = nrow(subset[subset$Sex == "M", ])
  num_population = as.numeric(population[col_name][85, 1]) * 1000
  num_male = as.numeric(population_male[col_name][85, 1]) * 1000
  
  age_group_prev[length(boundary)] = num_patient / num_population * 10000
  male_group_prev[length(boundary)] = num_male_patient / num_male * 10000
  female_group_prev[length(boundary)] = (num_patient - num_male_patient) / (num_population - num_male) * 10000
  
  # Return a data frame with prevalence rates
  result_df = data.frame("Age.group" = names(age_group_prev), 
                         "All" = unlist(age_group_prev), 
                         "Male" = unlist(male_group_prev), 
                         "Female" = unlist(female_group_prev))
  return(result_df)
}

# Function 3: Calculate Population Proportions ------------------------------------
calculate_proportion = function(result_df) {
  # Initialize proportions for each age group
  age_group_popu_2022 = list("10"=0, "15"=0, "20"=0, "25"=0, "30"=0, "35"=0, "40"=0, "45"=0,
                             "50"=0, "55"=0, "60"=0, "65"=0, "70"=0, "75"=0, "80"=0, "85"=0)
  male_group_popu_2022 = age_group_popu_2022
  female_group_popu_2022 = age_group_popu_2022
  
  total_popu_2022 = mid_popu_all[["2022"]]
  total_male_2022 = mid_popu_male[["2022"]]
  
  idx = 1
  while (idx < length(boundary)) {
    lower_bound = boundary[idx]
    num_population = 0
    num_male = 0
    for (age in seq(lower_bound, by = 1, length.out = 5)) {
      num_population = num_population + as.numeric(population["Mid-year-2022"][age, 1]) * 1000
      num_male = num_male + as.numeric(population_male["Mid-year-2022"][age, 1]) * 1000
    }
    age_group_popu_2022[sprintf("%d", lower_bound)] = num_population / total_popu_2022
    male_group_popu_2022[sprintf("%d", lower_bound)] = num_male / total_male_2022
    female_group_popu_2022[sprintf("%d", lower_bound)] = (num_population - num_male) / (total_popu_2022 - total_male_2022)
    idx = idx + 1
  }
  
  # Calculate proportions for age group 85+
  num_population = as.numeric(population["Mid-year-2022"][85, 1]) * 1000
  num_male = as.numeric(population_male["Mid-year-2022"][85, 1]) * 1000
  
  age_group_popu_2022[["85"]] = num_population / total_popu_2022
  male_group_popu_2022[["85"]] = num_male / total_male_2022
  female_group_popu_2022[["85"]] = (num_population - num_male) / (total_popu_2022 - total_male_2022)
  
  result_df = cbind(result_df, 
                    "Prop.all" = unlist(age_group_popu_2022),
                    "Prop.male" = unlist(male_group_popu_2022),
                    "Prop.female" = unlist(female_group_popu_2022))
  return(result_df)
}

# Function 4: Plot Prevalence vs Age ----------------------------------------------
plot_prev_figure = function(age_group_prev) {
  # Create directories for saving results
  base_dir <- "./results"
  data_dir <- file.path(base_dir, "Data")
  figures_dir <- file.path(base_dir, "Figures", "Prevalence with age")
  
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
  
  png(file = sprintf("./results/Figures/Prevalence with age/Prevalence_with_age_%d.png", Year))
  
  prev_df = data.frame(matrix(unlist(age_group_prev), nrow = length(age_group_prev)))
  colnames(prev_df) = c("prevalence")
  prev_df["Midage"] = seq(12, by = 5, length = 16)
  
  corr = cor.test(prev_df$Midage, prev_df$prevalence)
  graph = ggplot(prev_df, aes(x = Midage, y = prevalence)) +
    geom_point(shape = 1) +
    geom_smooth(method = lm) +
    labs(
      title = sprintf("Relationship between prevalence and age in %d", Year),
      x = "Age",
      y = "Prevalence (%)"
    ) +
    annotate(
      "text", x = 60, y = 10,
      label = sprintf("Correlation coefficient: %f", corr$estimate)
    )
  print(graph)
  dev.off()
}

# 4. Prevalence Calculation Loop --------------------------------------------------
years = 2003:2022
Overall_result = data.frame(
  "Col" = c("num_unique", "num_male_patient", "num_female_patient", 
            "crude_prevalence", "male_prev", "female_prev", "age_standardized_prev", 
            "age_standardized_prev_male", "age_standardized_prev_female")
)

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
Dx_multiple_diags <- Dx_multiple_diags %>% dplyr::select(-diagnosis_date,-type) %>% 
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
View(ibd_dx)

ibd_dx <- ibd_dx %>% filter(type == "CD")

# Identify first diagnosis per patient
first_diagnosis <- ibd_dx %>% 
  arrange(reference.key., reference.date.) %>% 
  group_by(reference.key.) %>% 
  slice_head(n = 1)  

# Load survival data (assuming death dates exist)
survival_data <- ibd_dx %>%
  dplyr::select(reference.key., date.of.registered.death.) %>%
  distinct() %>%
  mutate(date.of.registered.death. = ymd(date.of.registered.death.),
         last_date = if_else(is.na(date.of.registered.death.), 
                             as.Date("2022-12-31"), 
                             date.of.registered.death.))%>%
  dplyr::select(-date.of.registered.death.)  # Remove unnecessary column

# Merge diagnosis and survival data
cohort <- first_diagnosis %>%
  left_join(survival_data, by = "reference.key.")%>%
  mutate(birth_year = year(date.of.birth.yyyy.mm.dd.),
         first_dx_year = year(reference.date.),
         last_year =  year(last_date),
         # Ensure last_year is not earlier than first_dx_year
         last_year = if_else(last_year < first_dx_year | is.na(last_year), first_dx_year, last_year),
  )

# Expand data to follow patients across years
prevalence_long <- cohort %>%
  rowwise() %>%
  mutate(year = list(seq(first_dx_year, last_year, by = 1))) %>%
  unnest(year) %>%
  mutate(
    current_age = year - birth_year,  # Update age dynamically
    # Update age group classification to match the new structure
    age_group = case_when(
      current_age < 65 ~ "<65",
      current_age >= 65 & current_age < 80 ~ "65-79",
      TRUE ~ "≥80"
    )
  )

# Count unique patients per year and age group
prevalence <- prevalence_long %>%
  group_by(year, age_group, sex.) %>%
  summarize(prevalent_cases = n_distinct(reference.key.), .groups = "drop")







for (Year in years) {
  conv_df = prevalence_long %>% filter(year == Year)
  unique_df = pre_processing(conv_df)
  
  # Crude prevalence
  num_unique = nrow(unique_df)
  crude_prevalence = num_unique / mid_popu_all[[sprintf("%d", Year)]] * 10000
  
  # Sex-specific prevalence
  num_male_patient = nrow(unique_df[unique_df$Sex == "M", ])
  num_female_patient = nrow(unique_df) - num_male_patient
  male_prev = num_male_patient / mid_popu_male[[sprintf("%d", Year)]] * 10000
  female_prev = num_female_patient / 
    (mid_popu_all[[sprintf("%d", Year)]] - mid_popu_male[[sprintf("%d", Year)]]) * 10000
  
  # Age-specific prevalence
  result_df = calculate_age_prev(unique_df)
  result_df = calculate_proportion(result_df)
  age_group_prev = result_df$All
  
  # Age-standardized prevalence
  age_standardized_prev = sum(unlist(age_group_prev) * unlist(result_df$Prop.all))
  age_standardized_prev_male = sum(result_df$Male * result_df$Prop.male)
  age_standardized_prev_female = sum(result_df$Female * result_df$Prop.female)
  
  # Save results and plot
  write.xlsx(result_df, sprintf("./results/Data/%d Age group Prevalence.xlsx", Year))
  plot_prev_figure(age_group_prev)
  
  result_summary = c(num_unique, num_male_patient, num_female_patient, crude_prevalence, 
                     male_prev, female_prev, age_standardized_prev, 
                     age_standardized_prev_male, age_standardized_prev_female)
  
  Overall_result[as.character(Year)] = result_summary
}

write.xlsx(Overall_result, "./results/Prevalence.xlsx")


# 6. Prevalence forecasting ---------------------------------------------------
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
  Male_Cases = as.numeric(Overall_result[2,-1]) ,
  Female_Cases = as.numeric(Overall_result[3,-1]) ,
  Age_Standardized = as.numeric(Overall_result[7,-1]) ,
  Male_Age_Standardized = as.numeric(Overall_result[8,-1]) ,
  Female_Age_Standardized = as.numeric(Overall_result[9,-1]) 
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
write.csv(forecast_results, file = "Overall_Forecasts_2023_2032_prev.csv")