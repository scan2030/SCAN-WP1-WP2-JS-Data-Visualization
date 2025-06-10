# SCAN2030: Kaplan-Meier Survival Analysis for Combined Incidence Cohort ------------
# Author: Deliang Yang
# Purpose: Calculate the Kaplan-Meier (KM) curve for the combined incidence cohort (2014-2022)
# Input: Diagnosis data (merged from multiple files), demographic details
# Output: KM curves (combined, stratified by sex and age group), survival data tables

# Required Libraries ---------------------------------------------------------------
library(readxl)
library(ggplot2)
library(openxlsx)
library(survival)
library(dplyr)

# 1. Directory Setup ---------------------------------------------------------------
# 1.1 PATH CONFIGURATION ---------------------------------------------------------
path_to_root <- "~/shared_drive/Personal/Deliang Yang/IBD_CD"
setwd(path_to_root)



# 2. Function Definitions ----------------------------------------------------------
# Function to calculate the age based on birthdate and reference date
calc_age<-function(birthDate, refDate){z
  period<-as.period(interval(birthDate, refDate), unit = 'year')
  return(period$year)
}

# Function to calculate the time difference in days between two dates
calc_time<-function(refDate, endDate){
  period<-as.period(interval(refDate, endDate), unit = 'day')
  period$day
}

my_custom_name_repair <- function(x) tolower(gsub("\\.{1,}",'\\.',gsub("\n|  ", "", make.names(x))))
merge_files <- function(files){
  list_df <- lapply(files,readxl::read_xlsx,.name_repair=my_custom_name_repair) 
  df <- list_df %>% bind_rows(.)
  return(df)
}


# 3. Incidence Calculation ---------------------------------------------------------
# Address saving all the dx files
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
  filter(type=="CD") %>% 
  mutate(year = year(reference.date.))%>% 
  mutate(age = Inciage)


incident_summary <- incident %>%
  group_by(year) %>%
  summarise(count = n(), .groups = 'drop')


# 4. Death Data Preparation --------------------------------------------------------
# Address saving all the dx files
list_template = list("2014"=0, "2015"=0, "2016"=0, "2017"=0, "2018"=0, "2019"=0,
                     "2020"=0, "2021"=0, "2022"=0)

# Construct dataframes to store results
result_death_df = data.frame("Year"=names(list_template))
result_mortality_df = data.frame("Year"=names(list_template))
count = 1  # Counter for combining cohorts


df_allrecords <- incident %>% filter(year >= 2014 & year <= 2022  )
# Ensure ExactDate and DeathDate are in Date format
df_allrecords$reference.date. <- as.Date(df_allrecords$reference.date.)  # Convert to Date format
df_allrecords$date.of.registered.death. <- as.Date(df_allrecords$date.of.registered.death.)  # Convert to Date format


# Define the follow-up end date properly
df_allrecords$follow_up_end <- ifelse(
  is.na(df_allrecords$date.of.registered.death.) | df_allrecords$date.of.registered.death. > as.Date("2022-12-31"),
  as.Date("2022-12-31"),  # Censor at 2022-12-31 if death date is NA or later than this date
  df_allrecords$date.of.registered.death.  # Otherwise, use the actual death date
)

# Convert follow_up_end to Date format explicitly
df_allrecords$follow_up_end <- as.Date(df_allrecords$follow_up_end, origin = "1970-01-01")

# Calculate Survival Time (in years)
df_allrecords$SurvTime <- as.numeric(difftime(df_allrecords$follow_up_end, df_allrecords$reference.date., units = "days")) / 365.25

# Ensure non-negative survival times
df_allrecords$SurvTime[df_allrecords$SurvTime < 0] <- NA  # Handle invalid cases

# Define Status (1 = died, 0 = censored at 2022-12-31)
df_allrecords$Status <- ifelse(!is.na(df_allrecords$date.of.registered.death.) & df_allrecords$date.of.registered.death. <= as.Date("2022-12-31"), 1, 0)

# Check structure to confirm follow_up_end is correctly formatted
str(df_allrecords$follow_up_end)

# View the first few rows to verify
head(df_allrecords[, c("reference.date.", "date.of.registered.death.", "follow_up_end", "SurvTime", "Status")])

# 5. Age Group Categorization ------------------------------------------------------
# Categorize patients into age groups
df_allrecords$AgeGroup <- cut(
  df_allrecords$age,
  breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 80, Inf),  # Define breakpoints for age groups
  labels = c("<20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80+"),  # Labels for the groups
  right = FALSE  # Intervals are closed on the left, open on the right
)


# Check the distribution of age groups
table(df_allrecords$AgeGroup)



# 6. Kaplan-Meier Curve for Combined Cohort ----------------------------------------
fit = survfit(Surv(SurvTime, Status) ~ 1, data = df_allrecords)

# Plot combined KM curve
png(file = "./results/Figures/KM_curve_combined.png")
plot(fit, xlab = "Years", ylab = "Survival Probability", 
     xlim = c(0, max(df_allrecords$SurvTime, na.rm = TRUE)),  # Start x-axis at 0,
     main = "Kaplan-Meier Curve (2014-2022 Combined Cohort)")
dev.off()

# Extract survival data for each AgeGroup
km_data <- data.frame(
  time = fit$time,        # Time points
  survival = fit$surv,    # Survival probabilities
  n_risk = fit$n.risk,    # Number at risk
  n_event = fit$n.event,  # Number of events
  lower_CI = fit$lower,   # Lower confidence interval
  upper_CI = fit$upper    # Upper confidence interval
)

# View the extracted KM data
head(km_data)

# Save the extracted data to a CSV file
write.csv(km_data, file = "./results/KM_curve_data_total.csv", row.names = FALSE)



# 7. Kaplan-Meier Curves by Sex ----------------------------------------------------
# Convert Sex to factor
df_allrecords$sex. <- as.factor(df_allrecords$sex.)
# Define colors for M and F
sex_colors <- c("M" = "blue", "F" = "red")
# Stratified KM curves by Sex
if ("sex." %in% colnames(df_allrecords)) {
  fit_sex = survfit(Surv(SurvTime, Status) ~ sex., data = df_allrecords)
  png(file = "./results/Figures/KM_curve_by_sex.png")
  plot(fit_sex, lty = 1:2,
       xlab = "Years", ylab = "Survival Probability", main = "Kaplan-Meier Curve by Sex  (2014-2022 Combined Cohort)", col = sex_colors,xlim = c(0, max(df_allrecords$SurvTime, na.rm = TRUE)))
  legend("topright", legend = levels(df_allrecords$sex.), col = sex_colors, lty = 1:length(levels(df_allrecords$sex.))
  )
  dev.off()
}


# Extract survival data for each AgeGroup
km_data_Sex <- data.frame(
  time = fit_sex$time,        # Time points
  survival = fit_sex$surv,    # Survival probabilities
  n_risk = fit_sex$n.risk,    # Number at risk
  n_event = fit_sex$n.event,  # Number of events
  lower_CI = fit_sex$lower,   # Lower confidence interval
  upper_CI = fit_sex$upper    # Upper confidence interval
)

# Add group variable if there are multiple strata (AgeGroup)
if (!is.null(fit_sex$strata)) {
  km_data_Sex$Sex <- rep(names(fit_sex$strata), fit_sex$strata)
}

# View the extracted KM data
head(km_data_Sex)

# Save the extracted data to a CSV file
write.csv(km_data_Sex, file = "./results/KM_curve_data_by_Sex.csv", row.names = FALSE)


# 8. Kaplan-Meier Curves by Age Group ----------------------------------------------
# Convert AgeGroup to factor
df_allrecords$AgeGroup <- as.factor(df_allrecords$AgeGroup)

# Define colors for Age Groups
AgeGroup_colors <- c("<20" = "purple", "20-30" = "blue", "30-40" = "green", "40-50" = "orange",
                     "50-60" = "red", "60-70" = "brown", "70-80" = "pink", "80+" = "black")

# Stratified KM curves by AgeGroup
if ("AgeGroup" %in% colnames(df_allrecords)) {
  fit_AgeGroup <- survfit(Surv(SurvTime, Status) ~ AgeGroup, data = df_allrecords)
  
  # Save the plot as a PNG file
  png(file = "./results/Figures/KM_curve_by_AgeGroup.png")
  
  # Plot with colors for each AgeGroup
  plot(fit_AgeGroup, col = AgeGroup_colors[levels(df_allrecords$AgeGroup)], 
       lty = 1:length(levels(df_allrecords$AgeGroup)),
       xlab = "Years", ylab = "Survival Probability", 
       main = "Kaplan-Meier Curve by Age Group (2014-2022 Combined Cohort)",
       xlim = c(0, max(df_allrecords$SurvTime, na.rm = TRUE)))
  
  # Add a legend with matching colors
  legend("topright", legend = levels(df_allrecords$AgeGroup), 
         col = AgeGroup_colors, lty = 1:length(levels(df_allrecords$AgeGroup)))
  
  dev.off()  # Close the PNG device
}
