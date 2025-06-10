# SCAN2030: UC Cost Analysis ----------------------------------------
# Author: Deliang Yang (Revised)
# Purpose: Calculate AE/IP/OP costs for prevalent prostate cancer cohort (2003-2022)
#          and forecast costs for 2023-2032
# Input: PC_dx.RDS (diagnosis data), AE/IP/OP service records (inpatient.RDS, AE.RDS, outpatient.RDS)
# Output: Cost analysis reports and forecasts

# 1. PACKAGE SETUP ------------------------------------------------------------
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(tidyr)
library(forecast)
library(ggplot2)
library(tseries)
library(stats)
library(lmtest)
library(car)

# 1.1 PATH CONFIGURATION ---------------------------------------------------------
path_to_root <- "~/shared_drive/Personal/Deliang Yang/IBD_UC"
setwd(path_to_root)

# Create a directory for results if it doesn't exist
if (!dir.exists("./results")) {
  dir.create("./results")
}

# Define analysis and forecasting years
ANALYSIS_YEARS <- 2003:2022
FORECAST_YEARS <- 2023:2032

# 2. DATA LOADING & PREPROCESSING ----------------------------------------------
# Source: Hospital Authority Reference Data
COST_AE <- 1230
COST_IP <- c(
  "Acute" = 5100,
  "HighDependency" = 13650,
  "IntensiveCare" = 24400,
  "Convalescent" = 5100,
  "Psychiatric" = 2340
)

# 2.1 Load Raw Data ----
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

ibd_dx <- ibd_dx %>% filter(type == "UC")

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


# 4. COHORT PREPARATION -------------------------------------------------------
# 4.1 Data Loading Functions ----
load_prevalent_data <- function(year) {
  prevalence_long %>%
    filter(year == !!year) %>%
    transmute(
      Key = reference.key.,
      Birth = as.Date(date.of.birth.yyyy.mm.dd.),
      DeathDate = as.Date(last_date),  # Use the calculated last date
      Sex = sex.,
      RefDate = as.Date(reference.date.),
      Year = year,
      Current_age = current_age  # Include dynamic age classification
    ) %>%
    distinct(Key, .keep_all = TRUE)
}

# Load data once
ae_data_full <- readRDS("./IBD_2000-2022cohort_1993-2022AE.RDS") %>%
  distinct()%>%
  rename(Key = 1, Institution = 2, AttendanceDate = 3) %>%
  mutate(AttendanceDate = as.Date(AttendanceDate, origin = "1899-12-30"))

# Custom function to handle the specific date transformation
# Improved function to handle multiple date/datetime formats
transform_date <- function(date_value) {
  # Handle NA values
  if(is.na(date_value)) return(NA_character_)
  
  # If already a character string in YYYY-MM-DD format (10 chars), return as is
  if(is.character(date_value) && nchar(date_value) == 10) 
    return(date_value)
  
  # For POSIXct/POSIXlt datetime objects
  if(inherits(date_value, "POSIXct") || inherits(date_value, "POSIXlt")) {
    # Extract the time component
    time_part <- format(date_value, "%H:%M:%S")
    
    # If time is 16:00:00, add one day
    if(time_part == "16:00:00") {
      return(format(as.Date(date_value) + 1, "%Y-%m-%d"))
    } else {
      # For other times, just use the date part
      return(format(as.Date(date_value), "%Y-%m-%d"))
    }
  }
  
  # For Date objects (without time)
  if(inherits(date_value, "Date")) {
    return(format(date_value, "%Y-%m-%d"))
  }
  
  # For any other format, try to convert to character
  return(as.character(date_value))
}


# Load and preprocess IP data with discharge date
ip_data_raw  <- readRDS("./IBD_2000-2022cohort_1993-2022Ip_updated.RDS") %>%
  #ip_data_full <- readRDS("./IBD_2000-2022cohort_1993-2022Ip.RDS") %>%
  dplyr::select(-no.of.episodes., -no.of.episodes.patient.based., -matches("diagnosis\\.rank\\.[0-9]+\\."))%>%
  mutate(across(
    starts_with("los.of.ward.care.type"), 
    ~{
      # First convert to numeric (handles any character values)
      x <- as.numeric(.)
      # Then replace NA with 0 and convert to integer
      as.integer(ifelse(is.na(x), 0, x))
    }
  ))%>%
  mutate(
    admission.date.yyyy.mm.dd. = sapply(admission.date.yyyy.mm.dd., transform_date),
    discharge.date.yyyy.mm.dd. = sapply(discharge.date.yyyy.mm.dd., transform_date)
  )%>%
  distinct()%>%
  left_join(first_diagnosis%>%dplyr::select(reference.key., firstDiag), by ="reference.key.")%>%
  filter(!(is.na(firstDiag)))%>%
  filter(!(firstDiag>discharge.date.yyyy.mm.dd.))%>% # Filter the record discharging earlier than the first diagnosis
  # Adjust admission date to match first diagnosis date when diagnosis occurs during hospital stay
  mutate(
    # Check if first diagnosis date falls between admission and discharge
    diagnosis_during_stay = (firstDiag > admission.date.yyyy.mm.dd.) & 
      (firstDiag <= discharge.date.yyyy.mm.dd.),
    
    # If diagnosis occurs during stay, use diagnosis date as admission date
    admission.date.yyyy.mm.dd. = ifelse(
      diagnosis_during_stay,
      as.character(firstDiag),  # Use first diagnosis date as admission date
      admission.date.yyyy.mm.dd.  # Keep original admission date
    )
  ) %>%
  # Remove the temporary column
  dplyr::select(-diagnosis_during_stay)

# Count records before processing
original_count <- nrow(ip_data_raw)
cat("Original record count:", original_count, "\n")

# Copy to working dataset
ip_data_processed <- ip_data_raw

# Iterate until no more records are dropped
repeat {
  # Record count before this iteration
  count_before <- nrow(ip_data_processed)
  
  # Apply the containment logic
  ip_data_processed <- ip_data_processed %>%
    arrange(reference.key., admission.date.yyyy.mm.dd., 
            discharge.date.yyyy.mm.dd., 
            desc(los.of.ward.care.type.acute.general.acute.),
            desc(los.of.ward.care.type.acute.general.high.dependency.),
            desc(los.of.ward.care.type.acute.general.intensive.care.),
            desc(los.of.ward.care.type.convalescent.rehabilitation.infirmary.),
            desc(los.of.ward.care.type.psychiatry.mentally.handicapped.))  %>%
    group_by(reference.key.) %>%
    mutate(
      # Add row ID for reference
      row_id = row_number(),
      
      # Identify contained records
      next_admission = lead(admission.date.yyyy.mm.dd.),
      next_discharge = lead(discharge.date.yyyy.mm.dd.),
      contains_next_record = !is.na(next_admission) & 
        (admission.date.yyyy.mm.dd. <= next_admission) & 
        (discharge.date.yyyy.mm.dd. >= next_discharge),
      
      # Mark records that should be dropped
      should_drop = lag(contains_next_record, default = FALSE)
    ) %>%
    ungroup() %>%
    # Remove the contained records
    filter(!should_drop) %>%
    # Clean up temporary columns
    dplyr::select(-row_id, -contains_next_record, -should_drop, -next_admission, -next_discharge)
  
  # Check if any records were dropped in this iteration
  count_after <- nrow(ip_data_processed)
  cat("Iteration removed", count_before - count_after, "records\n")
  
  # If no records were dropped, we're done
  if(count_before == count_after) break
}

# Handle partially overlapping records
ip_data_adjusted <- ip_data_processed %>%
  # Sort by patient ID and dates
  arrange(reference.key., admission.date.yyyy.mm.dd., discharge.date.yyyy.mm.dd.) %>%
  group_by(reference.key.) %>%
  mutate(
    # Get next record's admission date
    next_admission = lead(admission.date.yyyy.mm.dd.),
    
    # Check for partial overlap using the specified conditions
    partial_overlap = !is.na(next_admission) & 
      (next_admission >= admission.date.yyyy.mm.dd.) & 
      (next_admission < discharge.date.yyyy.mm.dd.),
    
    # Flag records that need adjustment (they follow a record with partial overlap)
    needs_adjustment = lag(partial_overlap, default = FALSE),
    
    # Store previous discharge date for adjustment calculation
    prev_discharge = lag(discharge.date.yyyy.mm.dd.),
    
    # Calculate original length of stay (in days)
    original_los = as.numeric(difftime(discharge.date.yyyy.mm.dd., admission.date.yyyy.mm.dd., units = "days")),
    
    # Calculate the days that need to be cut (for records needing adjustment)
    days_to_cut = ifelse(needs_adjustment, 
                         as.numeric(difftime(prev_discharge, admission.date.yyyy.mm.dd., units = "days")) + 1, 
                         0),
    
    # Calculate adjustment ratio (proportion of stay to keep)
    adjustment_ratio = ifelse(needs_adjustment & original_los > 0, 
                              (original_los - days_to_cut) / original_los, 
                              1)
  ) %>%
  # Now adjust the admission date and LOS fields for overlapping records
  mutate(
    # Adjust admission date (add 1 day to previous discharge date)
    admission.date.yyyy.mm.dd. = ifelse(
      needs_adjustment,
      format(as.Date(prev_discharge) + 1, "%Y-%m-%d"),
      admission.date.yyyy.mm.dd.
    ),
    
    # Adjust each LOS field by the calculated ratio
    los.of.ward.care.type.acute.general. = round(los.of.ward.care.type.acute.general. * adjustment_ratio),
    los.of.ward.care.type.acute.general.acute. = round(los.of.ward.care.type.acute.general.acute. * adjustment_ratio),
    los.of.ward.care.type.acute.general.high.dependency. = round(los.of.ward.care.type.acute.general.high.dependency. * adjustment_ratio),
    los.of.ward.care.type.acute.general.intensive.care. = round(los.of.ward.care.type.acute.general.intensive.care. * adjustment_ratio),
    los.of.ward.care.type.convalescent.rehabilitation.infirmary. = round(los.of.ward.care.type.convalescent.rehabilitation.infirmary. * adjustment_ratio),
    los.of.ward.care.type.convalescent.rehabilitation. = round(los.of.ward.care.type.convalescent.rehabilitation. * adjustment_ratio),
    los.of.ward.care.type.infirmary. = round(los.of.ward.care.type.infirmary. * adjustment_ratio),
    los.of.ward.care.type.psychiatry.mentally.handicapped. = round(los.of.ward.care.type.psychiatry.mentally.handicapped. * adjustment_ratio),
    los.of.ward.care.type.psychiatry. = round(los.of.ward.care.type.psychiatry. * adjustment_ratio),
    los.of.ward.care.type.mentally.handicapped. = round(los.of.ward.care.type.mentally.handicapped. * adjustment_ratio),
    los.of.ward.care.type.unclassfied. = round(los.of.ward.care.type.unclassfied. * adjustment_ratio)
  ) %>%
  # Remove temporary columns
  dplyr::select(-next_admission, -partial_overlap, -needs_adjustment, 
                -prev_discharge, -original_los, -days_to_cut, -adjustment_ratio) %>%
  ungroup()

# Count records with partial overlaps that were adjusted
adjustment_count <- ip_data_processed %>%
  arrange(reference.key., admission.date.yyyy.mm.dd.) %>%
  group_by(reference.key.) %>%
  mutate(
    next_admission = lead(admission.date.yyyy.mm.dd.),
    partial_overlap = !is.na(next_admission) & 
      (next_admission >= admission.date.yyyy.mm.dd.) & 
      (next_admission < discharge.date.yyyy.mm.dd.)
  ) %>%
  ungroup() %>%
  summarize(overlap_count = sum(partial_overlap, na.rm = TRUE)) %>%
  pull(overlap_count)

cat("Number of partially overlapping records adjusted:", adjustment_count, "\n")

# Final processing to add the relationship fields
ip_data_full <- ip_data_adjusted %>%
  arrange(reference.key., admission.date.yyyy.mm.dd., discharge.date.yyyy.mm.dd.) %>%
  group_by(reference.key.) %>%
  mutate(
    same_day_stay = admission.date.yyyy.mm.dd. == discharge.date.yyyy.mm.dd.,
    next_admission = lead(admission.date.yyyy.mm.dd.),
    next_discharge = lead(discharge.date.yyyy.mm.dd.),
    next_principle_dia = lead(principal.diagnosis.code.),
    days_to_next = as.numeric(difftime(next_admission, discharge.date.yyyy.mm.dd., units = "days"))
  ) %>%
  ungroup()

# # Show examples of adjusted records
# adjusted_examples <- ip_data_processed %>%
#   arrange(reference.key., admission.date.yyyy.mm.dd.) %>%
#   group_by(reference.key.) %>%
#   mutate(
#     next_admission = lead(admission.date.yyyy.mm.dd.),
#     partial_overlap = !is.na(next_admission) & 
#       (next_admission >= admission.date.yyyy.mm.dd.) & 
#       (next_admission < discharge.date.yyyy.mm.dd.)
#   ) %>%
#   filter(partial_overlap) %>%
#   dplyr::select(reference.key., 
#                 admission.date.yyyy.mm.dd., 
#                 discharge.date.yyyy.mm.dd., 
#                 next_admission) %>%
#   head(5)
# 
# if(nrow(adjusted_examples) > 0) {
#   cat("\nExamples of partial overlaps before adjustment:\n")
#   print(adjusted_examples)
#   
#   # Get the corresponding adjusted records
#   cat("\nSame records after adjustment:\n")
#   for(i in 1:nrow(adjusted_examples)) {
#     patient <- adjusted_examples$reference.key.[i]
#     next_admission <- adjusted_examples$next_admission[i]
#     
#     # Find the adjusted record in ip_data_full
#     adjusted_record <- ip_data_full %>%
#       filter(reference.key. == patient) %>%
#       arrange(admission.date.yyyy.mm.dd.) %>%
#       filter(row_number() == which(ip_data_processed$reference.key. == patient & 
#                                      ip_data_processed$next_admission == next_admission) + 1)
#     
#     print(adjusted_record %>% 
#             dplyr::select(reference.key., admission.date.yyyy.mm.dd., discharge.date.yyyy.mm.dd.))
#   }
# }
# 
# # Check if there are still any overlaps
# remaining_overlaps <- ip_data_full %>%
#   arrange(reference.key., admission.date.yyyy.mm.dd.) %>%
#   group_by(reference.key.) %>%
#   mutate(
#     next_admission = lead(admission.date.yyyy.mm.dd.),
#     still_overlapping = !is.na(next_admission) & 
#       (next_admission >= admission.date.yyyy.mm.dd.) & 
#       (next_admission < discharge.date.yyyy.mm.dd.)
#   ) %>%
#   filter(still_overlapping) %>%
#   nrow()
# 
# cat("\nRemaining overlaps after adjustment:", remaining_overlaps, "\n")

# # Final processing to add the relationship fields
# ip_data_full <- ip_data_processed %>%
#   arrange(reference.key., admission.date.yyyy.mm.dd., discharge.date.yyyy.mm.dd.) %>%
#   group_by(reference.key.) %>%
#   mutate(
#     same_day_stay = admission.date.yyyy.mm.dd. == discharge.date.yyyy.mm.dd.,
#     next_admission = lead(admission.date.yyyy.mm.dd.),
#     next_discharge = lead(discharge.date.yyyy.mm.dd.),
#     next_principle_dia = lead(principal.diagnosis.code.),
#     days_to_next = as.numeric(difftime(next_admission, discharge.date.yyyy.mm.dd., units = "days"))
#   ) %>%
#   ungroup()

# Summary of processing
cat("\nSUMMARY:\n")
cat("Original record count:", original_count, "\n")
cat("Final record count:", nrow(ip_data_full), "\n")
cat("Total records merged:", original_count - nrow(ip_data_full), "\n")



# Collect clean data
ip_data_full <- ip_data_full %>%
  rename(
    Key = reference.key., 
    AdmissionDate = admission.date.yyyy.mm.dd.,
    DischargeDate = discharge.date.yyyy.mm.dd.,  # Add discharge date
    Acute = los.of.ward.care.type.acute.general.acute.,
    HighDependency = los.of.ward.care.type.acute.general.high.dependency.,
    IntensiveCare = los.of.ward.care.type.acute.general.intensive.care.,
    Convalescent = los.of.ward.care.type.convalescent.rehabilitation.infirmary.,  # Note: Changed from 25 to 26 based on column description
    Psychiatric = los.of.ward.care.type.psychiatry.mentally.handicapped.    # Note: Changed from 28 to 29 based on column description
  ) %>%
  dplyr::select(Key, AdmissionDate, DischargeDate, Acute, HighDependency, IntensiveCare, Convalescent, Psychiatric) %>%
  mutate(
    across(c(Acute, HighDependency, IntensiveCare, Convalescent, Psychiatric), as.numeric),
    AdmissionDate = as.Date(AdmissionDate, origin = "1899-12-30"),
    DischargeDate = as.Date(DischargeDate, origin = "1899-12-30"),
    # Then handle numeric columns and replace NA with 0
    across(c(Acute, HighDependency, IntensiveCare, Convalescent, Psychiatric), 
           ~as.numeric(replace(., is.na(.), 0)))
  )%>%
  # Filter out records with missing discharge dates
  filter(!is.na(DischargeDate)) 

# Function to split IP records that cross January 1st

# Function to split IP records that cross January 1st
split_ip_records_by_year <- function(ip_data, cores = parallel::detectCores() - 1, 
                                     chunk_size = 1000, progress = TRUE) {
  library(parallel)
  library(lubridate)
  if (progress && requireNamespace("pbapply", quietly = TRUE)) library(pbapply)
  
  # Create a copy and add Year column
  ip_data$Year <- NA_integer_
  
  # First quickly identify which records need splitting (cross year boundaries)
  admission_years <- year(ip_data$AdmissionDate)
  discharge_years <- year(ip_data$DischargeDate)
  
  # Records that don't cross years - mark them with their year
  same_year_indices <- which(admission_years == discharge_years)
  ip_data$Year[same_year_indices] <- admission_years[same_year_indices]
  
  # Records that need splitting
  cross_year_indices <- which(admission_years != discharge_years)
  
  # If no records cross years, return early with the original data
  if(length(cross_year_indices) == 0) {
    return(ip_data)
  }
  
  # Debug: Check if there are any records with discharge < admission
  problem_indices <- which(ip_data$DischargeDate < ip_data$AdmissionDate)
  if(length(problem_indices) > 0) {
    warning(paste("Found", length(problem_indices), "records where DischargeDate < AdmissionDate. These will be skipped."))
    # Remove problematic records from processing
    cross_year_indices <- setdiff(cross_year_indices, problem_indices)
    if(length(cross_year_indices) == 0) {
      return(ip_data) # Nothing left to process
    }
  }
  
  # Extract only the records that need processing to reduce memory overhead
  records_to_process <- ip_data[cross_year_indices, ]
  
  # Create chunks of indices for better load balancing
  n_records <- length(cross_year_indices)
  chunk_size <- min(chunk_size, n_records) # Ensure chunk size isn't larger than data
  n_chunks <- ceiling(n_records / chunk_size)
  chunks <- split(seq_len(n_records), ceiling(seq_len(n_records) / chunk_size))
  
  # Function to process a chunk of records - will be used in parallel
  process_chunk <- function(indices) {
    # Store results for this chunk
    all_results <- list()
    
    for (i in indices) {
      record <- records_to_process[i, ]
      admission_year <- year(record$AdmissionDate)
      discharge_year <- year(record$DischargeDate)
      
      # Safety check: Make sure discharge year is not before admission year
      if (discharge_year < admission_year) {
        warning(paste("Record", i, "has discharge year before admission year. Skipping."))
        next
      }
      
      # Calculate total LOS
      total_days <- as.numeric(difftime(record$DischargeDate, 
                                        record$AdmissionDate, units = "days")) + 1
      
      # Safety check: Make sure total_days is positive
      if (total_days <= 0) {
        warning(paste("Record", i, "has zero or negative LOS. Skipping."))
        next
      }
      
      # For optimizing memory, pre-allocate space for results
      years_spanned <- as.integer(discharge_year - admission_year + 1)
      # Safety check: Make sure years_spanned is positive
      if (years_spanned <= 0) {
        warning(paste("Record", i, "has invalid years_spanned:", years_spanned, ". Skipping."))
        next
      }
      
      # Process each year in the span
      result_records <- vector("list", years_spanned)
      counter <- 1
      
      for (yr in admission_year:discharge_year) {
        new_record <- record
        
        # Set year-specific start and end dates efficiently using vectorized operations
        year_start <- if (yr == admission_year) record$AdmissionDate else as.Date(paste0(yr, "-01-01"))
        year_end <- if (yr == discharge_year) record$DischargeDate else as.Date(paste0(yr, "-12-31"))
        
        # Calculate days in this year segment
        days_in_segment <- as.numeric(difftime(year_end, year_start, units = "days")) + 1
        
        # Safety check: Ensure days_in_segment is positive
        if (days_in_segment <= 0) {
          warning(paste("Record", i, "year", yr, "has non-positive days_in_segment:", days_in_segment, ". Skipping year."))
          next
        }
        
        # Calculate proportion of stay in this year
        proportion <- days_in_segment / total_days
        
        # Adjust LOS values based on proportion - vectorized operation
        los_columns <- c("Acute", "HighDependency", "IntensiveCare", "Convalescent", "Psychiatric")
        new_record[los_columns] <- record[los_columns] * proportion
        
        # Set the year for this record segment
        new_record$Year <- yr
        
        # Add to results
        result_records[[counter]] <- new_record
        counter <- counter + 1
      }
      
      # Collect non-null results from this record
      all_results <- c(all_results, result_records[!sapply(result_records, is.null)])
    }
    
    return(all_results)
  }
  
  # Safer export of variables to parallel cluster
  safe_export <- function(cl, vars, env = environment()) {
    for (var in vars) {
      if (exists(var, envir = env)) {
        clusterExport(cl, var, envir = env)
      } else {
        stop(paste("Variable", var, "not found for export"))
      }
    }
  }
  
  # Set up parallel cluster with explicit memory management
  cl <- makeCluster(cores, type = ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK"))
  
  # Export only the needed data to workers to reduce memory overhead
  safe_export(cl, c("records_to_process"))
  
  # Load lubridate on all worker nodes
  clusterEvalQ(cl, {
    library(lubridate)
    gc() # Clean up memory at start
  })
  
  # Process records in parallel with progress feedback
  message(paste("Processing", n_records, "cross-year records in", n_chunks, "chunks using", cores, "cores"))
  
  # Use simpler parallel execution for debugging
  if (progress && requireNamespace("pbapply", quietly = TRUE)) {
    # Use pbapply for progress bar if requested and available
    split_records <- tryCatch({
      pbapply::pblapply(chunks, process_chunk, cl = cl)
    }, error = function(e) {
      stopCluster(cl)
      stop(paste("Error in parallel processing:", e$message))
    })
  } else {
    # Fall back to standard parLapply
    split_records <- tryCatch({
      parLapply(cl, chunks, process_chunk)
    }, error = function(e) {
      stopCluster(cl)
      stop(paste("Error in parallel processing:", e$message))
    })
  }
  
  # Clean up and stop the cluster
  tryCatch({
    clusterEvalQ(cl, gc()) # Clean memory before shutdown
    stopCluster(cl)
  }, error = function(e) {
    message("Note: Error while cleaning up cluster: ", e$message)
  })
  
  # Flatten the list of lists efficiently
  split_records_flat <- unlist(split_records, recursive = FALSE)
  
  # Check if we have any results
  if (length(split_records_flat) == 0) {
    warning("No records were processed successfully.")
    return(ip_data) # Return original data if no processing happened
  }
  
  # Combine with records that didn't need splitting
  message("Combining processed records...")
  result <- rbind(
    ip_data[same_year_indices, ],
    do.call(rbind, split_records_flat)
  )
  
  # Sort by Key and Year for cleaner output
  message("Sorting results...")
  result <- result[order(result$Key, result$Year), ]
  
  return(result)
}

# Apply the function to split IP records by year
ip_data_full_split <- split_ip_records_by_year(ip_data_full)





df_op_full <- readRDS("./IBD_2000-2022cohort_1993-2022OP.RDS")%>% distinct()
colnames(df_op_full)[1] = c("Key")
#colnames(df_op_full)[22:23] = c("service.group.eis.","service.type.code.eis.")
#colnames(df_op_full)[25] = c("op.eis.specialty.")
#colnames(df_op_full)[27:28] = c("specialty.code.opas.","sub.specialty.opas.")
colnames(df_op_full)[31:32] = c("Attended","AppointmentDate")
df_op_full = transform(df_op_full, AppointmentDate = as.Date(AppointmentDate, origin=as.Date("1899-12-30")))

# Initialize results storage
results_all_years <- data.frame(
  Year = integer(),
  AE_Cost = numeric(),
  IP_Acute = numeric(),
  IP_HighDependency = numeric(),
  IP_IntensiveCare = numeric(),
  IP_Convalescent = numeric(),
  IP_Psychiatric = numeric(),
  IP_Total = numeric(),
  OP_DayHospital = numeric(),
  OP_SOPC_Psych = numeric(),
  OP_SOPC_NonPsych = numeric(),
  OP_SOPC_Additional = numeric(),
  OP_GOPC = numeric(),
  OP_Comm_Psych = numeric(),
  OP_Comm_General = numeric(),
  OP_Comm_Ancillary = numeric(),
  OP_Dressing = numeric(),
  OP_Total = numeric(),
  Total_Cost = numeric()
)

cost_op_lib <- read.csv("~/shared_drive/Personal/Deliang Yang/PC/cost_op_lib.csv")
rematch <- read.csv("~/shared_drive/Personal/Deliang Yang/IBD_UC/cost_op_lib_rematch_ibd_uc.csv") %>% filter(fl == 1)

# 5. MAIN ANALYSIS LOOP ------------------------------------------------------
for (YEAR in ANALYSIS_YEARS) {
  message(sprintf("\nAnalyzing Year: %d", YEAR))
  
  # Generate cohort for current year
  prevalent_df <- load_prevalent_data(YEAR)
  
  combined_cohort <- prevalent_df %>%
    mutate(
      analysis_start = pmax(RefDate, as.Date(paste0(YEAR, "-01-01"))),
      analysis_end = pmin(
        coalesce(DeathDate, as.Date(paste0(YEAR, "-12-31"))),
        as.Date(paste0(YEAR, "-12-31"))
      ),
      los_days = as.numeric(difftime(analysis_end, analysis_start, units = "days"))
    )
  
  # 5.1 AE Cost Calculation ----
  ae_data <- ae_data_full %>%
    filter(year(AttendanceDate) == YEAR) %>%
    inner_join(combined_cohort, by = "Key") %>%
    filter(AttendanceDate >= analysis_start & AttendanceDate <= analysis_end)
  
  ae_cost <- COST_AE * nrow(ae_data)
  
  # 5.2 IP Cost Calculation ----
  # Use the split data where each record is properly assigned to its year
  ip_data <- ip_data_full_split %>%
    filter(Year == YEAR) %>%
    inner_join(combined_cohort, by = "Key") %>%
    # Ensure we only include stays that overlap with the analysis period
    filter(
      (AdmissionDate <= analysis_end) & (DischargeDate >= analysis_start)
    )
  
  # Calculate IP cost by subtype
  ip_costs_by_type <- sapply(
    c("Acute", "HighDependency", "IntensiveCare", "Convalescent", "Psychiatric"),
    function(type) sum(ip_data[[type]] * COST_IP[type], na.rm = TRUE)
  )
  
  ip_cost_total <- sum(ip_costs_by_type)
  
  # 5.3 OP COST CALCULATION ------------------------------------------------------
  # Filter OP data for current year
  df_op <- df_op_full %>%
    filter(year(AppointmentDate) == YEAR)
  
  # Select the records in the prevalent cohort
  df_op <- left_join(df_op, combined_cohort, by="Key")
  df_op <- df_op[!is.na(df_op$Sex),] # Drop those not in this cohort
  # Select reference date only in the follow up period
  df_op <- df_op[df_op$analysis_start <= df_op$AppointmentDate,]
  df_op <- df_op[df_op$AppointmentDate <= df_op$analysis_end, ]
  
  num_episode_op <- nrow(df_op)
  
  #### new op calculation
  #colnames(df_op)[22:29] = c("service.group.eis.","service.type.code.eis.","service.type.code.eis.description.","op.eis.specialty.",
   #                          "op.eis.sub.specialty.","specialty.code.opas.","sub.specialty.opas.","sub.specialty.description.opas.")
  df_op$reference_key <- df_op$Key
  df_op$appointment_date <- df_op$AppointmentDate
  
  # mapping
  temp_mapped <- df_op %>% 
    left_join(cost_op_lib %>% 
                filter(disease == "IBD") %>% 
                dplyr::select(service.group.eis., service.type.code.eis.,service.type.code.eis.description.,
                       op.eis.specialty., op.eis.sub.specialty.,specialty.code.opas.,sub.specialty.opas.,
                       sub.specialty.description.opas.,
                       cate_final, price) %>% 
                unique(),
              by = c("service.group.eis.", "service.type.code.eis.", 
                     "service.type.code.eis.description.",
                     "op.eis.specialty.", "op.eis.sub.specialty.",
                     "specialty.code.opas.",
                     "sub.specialty.opas.","sub.specialty.description.opas.")) %>% 
    dplyr::select(reference_key, appointment_date,
                  service.group.eis., service.type.code.eis.,service.type.code.eis.description.,
                  op.eis.specialty., op.eis.sub.specialty.,specialty.code.opas.,sub.specialty.opas.,
                  sub.specialty.description.opas.,
                  cate_final,price)
  
  remated <- temp_mapped %>% filter(is.na(price)) %>% 
    dplyr::select(service.group.eis., service.type.code.eis.,service.type.code.eis.description.,
                  op.eis.specialty., op.eis.sub.specialty.,specialty.code.opas.,sub.specialty.opas.,
                  sub.specialty.description.opas.) %>% 
    unique() %>% 
    left_join(rematch %>% 
                transmute(service.group.eis., service.type.code.eis.,service.type.code.eis.description.,
                          op.eis.specialty., op.eis.sub.specialty.,specialty.code.opas.,
                          cate_final, price) %>% unique(), by = c("service.group.eis.", "service.type.code.eis.", 
                                                                  "service.type.code.eis.description.",
                                                                  "op.eis.specialty.", "op.eis.sub.specialty.",
                                                                  "specialty.code.opas."))
  
  # combine
  final_op <- temp_mapped %>% 
    filter(!is.na(price)) %>% 
    rbind(temp_mapped %>% 
            filter(is.na(price)) %>% 
            dplyr::select(reference_key:sub.specialty.description.opas.) %>% 
            left_join(remated %>% 
                        dplyr::select(service.group.eis.: sub.specialty.opas., cate_final, price) %>% unique(), by = c("service.group.eis.", "service.type.code.eis.", 
                                                                                                                       "service.type.code.eis.description.",
                                                                                                                       "op.eis.specialty.", "op.eis.sub.specialty.",
                                                                                                                       "specialty.code.opas.",
                                                                                                                       "sub.specialty.opas.")))
  
  final_sum <- final_op %>% 
    group_by(cate_final) %>% 
    summarise(total_cost = sum(as.numeric(price))) %>% 
    full_join(cost_op_lib %>% 
                dplyr::select(cate_final) %>% unique(),
              by = "cate_final") %>% 
    mutate(total_cost = ifelse(is.na(total_cost),0,total_cost)) %>% 
    pivot_wider(values_from = total_cost, names_from = cate_final) %>% 
    cbind(data.frame(Total = sum(final_op[!(final_op$cate_final %in% c("Accident & Emergency Department", 
                                                                       "General wards")),]$price))) # despite AE and IP
  
  
  
  # 5.3.4 Total OP Cost ----
  cost_op_total <- final_sum$Total
  
  # 6. STORE CURRENT YEAR RESULTS ----------------------------------------------
  # Ensure all cost values are numeric
  year_results <- data.frame(
    Year = YEAR,
    AE_Cost = as.numeric(ae_cost) + final_sum$`Accident & Emergency Department`,
    IP_Acute = as.numeric(ip_costs_by_type["Acute"]),
    IP_HighDependency = as.numeric(ip_costs_by_type["HighDependency"]),
    IP_IntensiveCare = as.numeric(ip_costs_by_type["IntensiveCare"]),
    IP_Convalescent = as.numeric(ip_costs_by_type["Convalescent"]),
    IP_Psychiatric = as.numeric(ip_costs_by_type["Psychiatric"]),
    IP_Total = as.numeric(ip_cost_total) + final_sum$`General wards`,
    OP_DayHospital = final_sum$`Geriatric day hospital` + final_sum$`Psychiatric day hospital` + final_sum$`Rehabilitation day hospital`,
    OP_SOPC = final_sum$`Specialist clinic (including allied health clinic)`,
    OP_GOPC = final_sum$`General clinic`,
    OP_Comm_Psych = final_sum$`Community psychiatric nursing service (per visit)`,
    OP_Comm_General = final_sum$`Community nursing service (per visit)`,
    OP_Comm_AlliedHealth = final_sum$`Community allied health service (per visit)`,
    OP_Dressing = final_sum$`Clinic or hospital for injection or dressing`,
    OP_DayProcedure_onc = final_sum$`Clinical Oncology clinic`,
    OP_DayProcedure_oph = final_sum$`Ophthalmic clinic`,
    OP_Haemodialysis_otherAE = final_sum$`Acute Haemodialysis` + final_sum$`Chronic Haemodialysis` + final_sum$`Others in an ambulatory facility`,
    OP_Total = as.numeric(final_sum$Total),
    Total_Cost = as.numeric(ae_cost + ip_cost_total + cost_op_total)
  )
  
  # Append to all years results
  results_all_years <- rbind(results_all_years, year_results)
  
  # Save individual year results
  output_list <- list(
    "Cost Summary" = tibble(
      Service = c("AE", "IP", "OP"),
      Episodes = c(nrow(ae_data), nrow(ip_data), num_episode_op),
      TotalCost = c(ae_cost + final_sum$`Accident & Emergency Department`, 
                    ip_cost_total + final_sum$`General wards`, 
                    cost_op_total)
    ),
    "IP Cost Details" = tibble(
      ServiceType = c(names(COST_IP), "Wards from outpatients data"),
      UnitCost = c(COST_IP, 5100),
      TotalDays = c(colSums(ip_data[names(COST_IP)]),sum(final_op$cate_final %in% c("General wards"))),
      TotalCost = c(ip_costs_by_type,final_sum$`Accident & Emergency Department`)
    ),
    "OP Cost Details" = tibble(
      Category = c("Psychiatric Day Hospital", 
                   "Geriatric Day Hospital",
                   "Rehabilitation Day Hospital",
                   "SOPC", "GOPC", 
                   "Community Psychiatric", 
                   "Community General", 
                   "Community Allied Health", 
                   "Dressing & Injection",
                   "Day Procedure/Treatment at Clinical Oncology Clinic",
                   "Day Procedure/Treatment at Ophthalmic Clinic",
                   "Day procedure and treatment for Haemodialysis at a Renal Clinic/Centre or other ambulatory facility"),
      Cost = c(final_sum$`Psychiatric day hospital`,
               final_sum$`Geriatric day hospital`,
               final_sum$`Rehabilitation day hospital`, 
               final_sum$`Specialist clinic (including allied health clinic)`,
               final_sum$`General clinic`,
               final_sum$`Community psychiatric nursing service (per visit)`,
               final_sum$`Community nursing service (per visit)`,
               final_sum$`Community allied health service (per visit)`,
               final_sum$`Clinic or hospital for injection or dressing`,
               final_sum$`Clinical Oncology clinic`,
               final_sum$`Ophthalmic clinic`,
               final_sum$`Acute Haemodialysis` + final_sum$`Chronic Haemodialysis` + final_sum$`Others in an ambulatory facility`)
    )
  )
  write.xlsx(
    output_list,
    file = sprintf("./results/%d_cost_analysis.xlsx", YEAR),
    colNames = TRUE,
    borders = "columns",
    firstRow = TRUE
  )
  
  message(sprintf("Year %d processed and saved", YEAR))
}

# 7. SAVE HISTORICAL RESULTS ----------------------------------------------------
# 7.1 Save as CSV ----
write.csv(results_all_years, "./results/IBD_UC_Cost_Analysis_2003_2022_v5.csv", row.names = FALSE)

# 7.2 Create Wide-Format Summary ----
# Create a pivot table for easier comparison across years
wide_format_results <- results_all_years %>%
  pivot_longer(cols = -Year, names_to = "Cost_Type", values_to = "Amount") %>%
  pivot_wider(names_from = Year, values_from = Amount)

write.csv(wide_format_results, "./results/IBD_UC_Cost_Summary_Wide_Format_v3.csv", row.names = FALSE)

# 8. FORECASTING COSTS FOR 2023-2032 --------------------------------------------
# 7.1 Forecasting function
dum_pan = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1)
# Combine regressors
xreg <- as.matrix( data.frame(dum_pan = dum_pan))

future_dum_pan = c(0,0,0,0,0,0,0,0,0,0)
future_xreg <- as.matrix(data.frame(dum_pan = future_dum_pan))
library(forecast)
compare_arima_models <- function(data, training_years, forecast_years) {
  # Filter to use only training data
  training_data <- data %>% filter(year %in% training_years)
  
  # Function to capture and parse auto.arima trace output
  # AND select best model based on score gap and p value
  get_top_models <- function(data_series, ic = "aic", xreg = NULL) {
    # Capture all console output
    output <- capture.output({
      model <- forecast::auto.arima(data_series, ic = ic, trace = TRUE, 
                                    test = "kpss", xreg = xreg)
    })
    
    # Extract the best model line
    best_model_line <- output[grep("Best model:", output)]
    
    # Find all model lines (lines with a colon that aren't the Best model line)
    model_lines <- output[grep(":", output)]
    model_lines <- model_lines[!grepl("Best model:", model_lines)]
    
    # Parse model information
    model_info <- list()
    valid_models <- 0
    
    for(line in model_lines) {
      if(grepl("Inf", line)) next  # Skip models with Inf values
      
      # Extract model name and score
      parts <- strsplit(line, ":")[[1]]
      if(length(parts) < 2) next  # Skip if line doesn't have a colon
      
      model_name <- trimws(parts[1])
      score <- as.numeric(trimws(parts[2]))
      
      if(is.na(score)) next  # Skip if score is NA
      
      # Extract AR order (p)
      p_value <- 0
      # Match ARIMA(p,d,q) pattern
      if(grepl("ARIMA\\([0-9]+,[0-9]+,[0-9]+\\)", model_name)) {
        p_match <- regexpr("ARIMA\\(([0-9]+)", model_name)
        if(p_match > 0) {
          p_str <- substr(model_name, p_match + 6, p_match + 6)
          p_value <- as.numeric(p_str)
        }
      }
      
      # For "Regression with ARIMA(p,d,q) errors"
      if(grepl("Regression with ARIMA\\([0-9]+,[0-9]+,[0-9]+\\) errors", model_name)) {
        p_match <- regexpr("ARIMA\\(([0-9]+)", model_name)
        if(p_match > 0) {
          p_str <- substr(model_name, p_match + 6, p_match + 6)
          p_value <- as.numeric(p_str)
        }
      }
      
      model_info[[length(model_info) + 1]] <- list(
        name = model_name,
        score = score,
        p = p_value
      )
      valid_models <- valid_models + 1
    }
    
    # If no valid models found, return the auto.arima model
    if(valid_models == 0) {
      cat("\nNo valid models found in trace output. Using auto.arima's selection.\n")
      return(list(
        selected_model = model,
        model_info = "auto.arima default",
        comparison_done = FALSE
      ))
    }
    
    # If only one valid model found, return auto.arima model
    if(valid_models == 1) {
      cat("\nOnly one valid model found in trace output. Using auto.arima's selection.\n")
      return(list(
        selected_model = model,
        model_info = "auto.arima default (only one valid model)",
        comparison_done = FALSE
      ))
    }
    
    # Sort models by score (ascending)
    scores <- sapply(model_info, function(x) x$score)
    sorted_idx <- order(scores)
    
    sorted_names <- sapply(model_info[sorted_idx], function(x) x$name)
    sorted_scores <- scores[sorted_idx]
    sorted_p_values <- sapply(model_info[sorted_idx], function(x) x$p)
    
    # Get best and second best
    best_name <- sorted_names[1]
    second_best_name <- sorted_names[2]
    best_score <- sorted_scores[1]
    second_best_score <- sorted_scores[2]
    best_p <- sorted_p_values[1]
    second_best_p <- sorted_p_values[2]
    
    # Calculate gap between best and second best
    gap <- abs(best_score - second_best_score)
    
    # Print comparison information
    cat(sprintf("\nBest model: %s (score = %.4f, p = %d)", 
                best_name, best_score, best_p))
    cat(sprintf("\nSecond best: %s (score = %.4f, p = %d)", 
                second_best_name, second_best_score, second_best_p))
    cat(sprintf("\nScore gap: %.4f", gap))
    
    # If gap < 2 and second best has higher p, select second best
    selected_model_name <- best_name
    selected_p <- best_p
    selected_is_second_best <- FALSE
    
    if(gap < 2) {
      cat("\nScore gap < 2, comparing p values...")
      
      if(best_p < second_best_p) {
        cat(sprintf("\nSelected second best model due to higher p value (%d > %d)", 
                    second_best_p, best_p))
        
        selected_model_name <- second_best_name
        selected_p <- second_best_p
        selected_is_second_best <- TRUE
      } else {
        cat("\nKept best model as it already has equal or higher p value.")
      }
    } else {
      cat("\nScore gap >= 2, keeping best model by score.")
    }
    
    # Create selected model
    if(selected_is_second_best) {
      # Need to refit with the selected p and original d,q
      current_order <- arimaorder(model)
      new_order <- c(selected_p, current_order[2:3])
      
      # Check if we're working with log data
      is_log_data <- grepl("log", deparse(substitute(data_series)))
      
      if(is_log_data) {
        selected_model <- Arima(data_series, order = new_order, xreg = xreg)
      } else {
        selected_model <- Arima(data_series, order = new_order, xreg = xreg)
      }
      
      model_info_text <- sprintf("Second best model selected (gap=%.4f, p=%d)", gap, selected_p)
    } else {
      # Just use the auto.arima model
      selected_model <- model
      model_info_text <- sprintf("Best model kept (gap=%.4f, p=%d)", gap, selected_p)
    }
    
    return(list(
      selected_model = selected_model,
      model_info = model_info_text,
      comparison_done = TRUE,
      gap = gap,
      best_p = best_p,
      second_best_p = second_best_p,
      best_model_name = best_name,
      second_best_model_name = second_best_name
    ))
  }
  
  # Get selected models for each criterion
  cat("\nAnalyzing rate with AIC criterion:\n")
  rate_aic_result <- get_top_models(training_data$rate, ic = "aic", xreg = xreg)
  
  cat("\nAnalyzing rate with BIC criterion:\n")
  rate_bic_result <- get_top_models(training_data$rate, ic = "bic", xreg = xreg)
  
  cat("\nAnalyzing log-transformed data with AIC criterion:\n")
  log_aic_result <- get_top_models(training_data$ibd_log, ic = "aic", xreg = xreg)
  
  cat("\nAnalyzing log-transformed data with BIC criterion:\n")
  log_bic_result <- get_top_models(training_data$ibd_log, ic = "bic", xreg = xreg)
  
  # Get the selected models
  arima_model_aic <- rate_aic_result$selected_model
  arima_model_bic <- rate_bic_result$selected_model
  model_log_aic <- log_aic_result$selected_model
  model_log_bic <- log_bic_result$selected_model
  
  # Calculate MSE for each model on training data
  fitted_log_values_aic <- fitted(model_log_aic)
  fitted_values_aic <- exp(fitted_log_values_aic)
  fitted_log_values_bic <- fitted(model_log_bic)  
  fitted_values_bic <- exp(fitted_log_values_bic)  
  
  actual_values <- training_data$rate
  residuals_original_scale_aic <- actual_values - fitted_values_aic
  residuals_original_scale_bic <- actual_values - fitted_values_bic  
  
  squared_residuals_original_scale_aic <- residuals_original_scale_aic^2
  squared_residuals_original_scale_bic <- residuals_original_scale_bic^2
  
  mse_original_scale_aic <- mean(squared_residuals_original_scale_aic)
  mse_original_scale_bic <- mean(squared_residuals_original_scale_bic)
  
  residuals_aic <- residuals(arima_model_aic)
  residuals_bic <- residuals(arima_model_bic)
  
  mse_aic <- mean(residuals_aic^2)
  mse_bic <- mean(residuals_bic^2)
  
  # Create named vector with MSE values
  mse_values <- c(mse_aic = mse_aic, mse_bic = mse_bic, 
                  mse_original_scale_aic = mse_original_scale_aic,
                  mse_original_scale_bic = mse_original_scale_bic)
  
  # Find best model based on minimum MSE
  min_mse <- min(mse_values)
  best_model_type <- names(mse_values)[which.min(mse_values)]
  
  # Get the selected model based on best_model_type
  if(best_model_type == "mse_aic") {
    cat(sprintf("\n\nSelected model for forecasting: rate (AIC criterion)"))
    cat(sprintf("\nModel info: %s", rate_aic_result$model_info))
    model_1 <- arima_model_aic
  } else if(best_model_type == "mse_bic") {
    cat(sprintf("\n\nSelected model for forecasting: rate (BIC criterion)"))
    cat(sprintf("\nModel info: %s", rate_bic_result$model_info))
    model_1 <- arima_model_bic
  } else if(best_model_type == "mse_original_scale_aic") {
    cat(sprintf("\n\nSelected model for forecasting: log-transformed data (AIC criterion)"))
    cat(sprintf("\nModel info: %s", log_aic_result$model_info))
    model_1 <- model_log_aic
  } else if(best_model_type == "mse_original_scale_bic") {
    cat(sprintf("\n\nSelected model for forecasting: log-transformed data (BIC criterion)"))
    cat(sprintf("\nModel info: %s", log_bic_result$model_info))
    model_1 <- model_log_bic
  }
  
  # Print final selected model details
  cat(sprintf("\n\nFINAL SELECTED MODEL: %s", best_model_type))
  cat(sprintf("\nModel structure: %s\n", capture.output(print(model_1))[1]))
  
  # We need to generate 10 forecast slots (that will become 2023-2032)
  forecast_horizon <- 10
  
  # Forecast using the chosen model from the end of training period
  if (best_model_type == "mse_aic" || best_model_type == "mse_bic") {
    forecasted_rate <- forecast(model_1, h = forecast_horizon, level = 95, xreg = future_xreg)
    
    # Create forecast dataframe and map to 2023-2032
    forecast_df <- data.frame(
      year = forecast_years,  # Map first 10 slots to 2023-2032
      rate_forecast = as.numeric(forecasted_rate$mean),
      lower_PI = forecasted_rate$lower[, "95%"],
      upper_PI = forecasted_rate$upper[, "95%"]
    )
  } else {
    # Log-transformed models
    forecasted_log <- forecast(model_1, h = forecast_horizon, xreg = future_xreg)
    forecasted_rate <- exp(forecasted_log$mean)
    forecasted_rate_lower <- exp(forecasted_log$lower[, 2])
    forecasted_rate_upper <- exp(forecasted_log$upper[, 2])
    
    # Create forecast dataframe and map to 2023-2032
    forecast_df <- tibble(
      year = forecast_years,  # Map first 10 slots to 2023-2032
      rate_forecast = as.numeric(forecasted_rate),
      lower_PI = forecasted_rate_lower,
      upper_PI = forecasted_rate_upper
    )
  }
  
  return(forecast_df)
}
# 7.2 Prepare data for forecasting
# Define training years (2015-2019)
TRAINING_YEARS <- 2003:2022
FORECAST_YEARS <- 2023:2032

# Extract relevant cost columns to forecast
cost_data_for_forecasting <- list(
  AE_Cost = results_all_years$AE_Cost,
  IP_Total = results_all_years$IP_Total,
  OP_Total = results_all_years$OP_Total
)

# Generate forecasts for each cost type
forecast_results <- list()
for (cost_type in names(cost_data_for_forecasting)) {
  message(sprintf("\nForecasting %s for 2023-2032 (using 2015-2019 for training)", cost_type))
  
  cost_values <- cost_data_for_forecasting[[cost_type]]
  data <- tibble(
    year = ANALYSIS_YEARS, 
    rate = cost_values, 
    ibd_log = log(ifelse(cost_values <= 0, 1, cost_values)) # Handle potential zeros
  )
  
  # Generate forecast using modified function
  forecast_df <- compare_arima_models(data, TRAINING_YEARS, FORECAST_YEARS)
  forecast_df$Cost_Type <- cost_type
  forecast_results[[cost_type]] <- forecast_df
}

# 7.3 Combine all forecasts and save as CSV
combined_forecasts <- bind_rows(forecast_results, .id = "Cost_Type")
write.csv(combined_forecasts, "./results/Depression_Cost_Forecasts_2023_2032_v4.csv", row.names = FALSE)

# 7.4 Create a combined table with historical and forecast data
# Process historical data for merging (2014-2022)
historical_long <- results_all_years %>%
  filter(Year <= 2022) %>%
  dplyr::select(Year, AE_Cost, IP_Total, OP_Total) %>%
  pivot_longer(cols = c(AE_Cost, IP_Total, OP_Total), 
               names_to = "Cost_Type", 
               values_to = "Cost")

# Process forecast data for merging
forecast_long <- combined_forecasts %>%
  dplyr::select(year, Cost_Type, rate_forecast, lower_PI, upper_PI) %>%
  rename(Year = year, Cost = rate_forecast)

# Combine historical and forecast
combined_data <- bind_rows(
  historical_long %>% mutate(Type = "Historical", lower_PI = NA, upper_PI = NA),
  forecast_long %>% mutate(Type = "Forecast")
)
write.csv(combined_forecasts, "./results/IBD_UC_Cost_Forecasts_2023_2032_v3.csv", row.names = FALSE)

# 8.4 Create a combined table with historical and forecast data
# Process historical data for merging
historical_long <- results_all_years %>%
  dplyr::select(Year, AE_Cost, IP_Total, OP_Total) %>%
  pivot_longer(cols = c(AE_Cost, IP_Total, OP_Total), 
               names_to = "Cost_Type", 
               values_to = "Cost")

# Process forecast data for merging
forecast_long <- combined_forecasts %>%
  dplyr::select(year, Cost_Type, rate_forecast, lower_PI, upper_PI) %>%
  rename(Year = year, Cost = rate_forecast) %>%
  filter(Year > max(ANALYSIS_YEARS))  # Only keep future years

# Combine historical and forecast
combined_data <- bind_rows(
  historical_long %>% mutate(Type = "Historical", lower_PI = NA, upper_PI = NA),
  forecast_long %>% mutate(Type = "Forecast")
)

# Save combined data
write.csv(combined_data, "./results/IBD_UC_Costs_Historical_and_Forecast_2003_2032_v3.csv", row.names = FALSE)

# 8.5 Create wide format for combined data
combined_wide <- combined_data %>%
  dplyr::select(Year, Cost_Type, Cost, Type) %>%
  pivot_wider(
    names_from = c(Cost_Type, Type),
    values_from = Cost
  ) %>%
  arrange(Year)

write.csv(combined_wide, "./results/IBD_UC_Costs_Wide_Format_2003_2032_v3.csv", row.names = FALSE)

# 8.6 Create an Excel workbook with multiple sheets
excel_output <- list(
  "Historical_2003_2022" = results_all_years,
  "Forecast_2023_2032" = combined_forecasts %>% 
    pivot_wider(names_from = Cost_Type, 
                values_from = c(rate_forecast, lower_PI, upper_PI)),
  "Combined_2003_2032" = combined_wide,
  "AE_Cost" = filter(combined_data, Cost_Type == "AE_Cost") %>% 
    dplyr::select(Year, Cost, Type, lower_PI, upper_PI),
  "IP_Total" = filter(combined_data, Cost_Type == "IP_Total") %>% 
    dplyr::select(Year, Cost, Type, lower_PI, upper_PI),
  "OP_Total" = filter(combined_data, Cost_Type == "OP_Total") %>% 
    dplyr::select(Year, Cost, Type, lower_PI, upper_PI)
)

write.xlsx(
  excel_output,
  file = "./results/IBD_UC_Cost_Analysis_and_Forecast_2003_2032_v3.xlsx",
  colNames = TRUE,
  borders = "columns",
  firstRow = TRUE
)

# 9. Console Output ----
message("\nANALYSIS AND FORECASTING COMPLETE\n")
message("Saved files:")
message("1. Individual year reports: ./results/YYYY_cost_analysis.xlsx")
message("2. Historical data (2003-2022): ./results/IBD_UC_Cost_Analysis_2003_2022.csv")
message("3. Forecast data (2023-2032): ./results/IBD_UC_Cost_Forecasts_2023_2032.csv")
message("4. Combined historical and forecast: ./results/IBD_UC_Costs_Historical_and_Forecast_2003_2032.csv")
message("5. Complete Excel report: ./results/IBD_UC_Cost_Analysis_and_Forecast_2003_2032.xlsx")