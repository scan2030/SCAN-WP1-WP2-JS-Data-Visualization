# SCAN2030: UC Unmet Need Analysis ----------------------------------------
# Author: Yin Zhang, Deliang Yang
# Purpose: Calculate Unmet Need for UC
# Input: 
#   - Diagnosis data: Files from "./IBD_DX converted" directory
#   - Prescription data: IBD_2000-2024cohort_2000-2024Rx.RDS
#   - Procedure data: IBD_2000-2022cohort_1993-2022Px.RDS
#   - Hospital data: IBD_2000-2024cohort_2000-2024AE.RDS, IBD_2000-2024cohort_2000-2024Ip.RDS
#   - Patient records: IBD_1993-2024firstDiag_1993-2024death.RDS
#   - Population reference data for incidence calculations
# Output: Incidence reports, age-specific Incidence, figures, and summary tables

# 1. LIBRARIES --------------------------------------------------------
# Load required libraries for data manipulation and analysis
library(lubridate)   # For date handling and calculations
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation operations
library(ggplot2)     # For visualization
library(tidyr)       # For data reshaping
library(openxlsx)    # For exporting to Excel

# 2. WORKING DIRECTORY -----------------------------------------------
# Set the working directory to the project root
path_to_root <- "Z:/Personal/Deliang Yang/IBD_UC/"
setwd(path_to_root)

# 3. HELPER FUNCTIONS ------------------------------------------------
# Function to standardize column names by removing special characters
my_custom_name_repair <- function(x) {
  tolower(gsub("\\.{1,}", '\\.',gsub("\n|  ", "", make.names(x))))
}

# Function to merge multiple Excel files into a single dataframe
merge_files <- function(files) {
  list_df <- lapply(files, readxl::read_xlsx, .name_repair = my_custom_name_repair)
  df <- list_df %>% bind_rows(.)
  return(df)
}

# Function to calculate age from birthdate to reference date
calc_age <- function(birthDate, refDate) {
  period <- as.period(interval(birthDate, refDate), unit = 'year')
  return(period$year)
}


# 4. DATA LOADING AND PREPROCESSING -----------------------------------
# Load all diagnostic files and merge them
dxdirectory <- list.files("./IBD_DX converted", full.names = TRUE)
dx.19932023 <- merge_files(dxdirectory)%>% filter(as.Date(reference.date.)<= as.Date("2022-12-31"))
colnames(dx.19932023) <- tolower(make.names(colnames(dx.19932023)))
alldx <- dx.19932023

# Create summary of diagnostic records by patient
alldx_summary <- alldx %>%
  group_by(reference.key.) %>%
  summarize(
    diag_count = n(),                  # Count of diagnostic records per patient
    firstdiag = min(reference.date.),  # Earliest diagnosis date
    InciYear = year(min(reference.date.))  # Year of first diagnosis
  )

# Display distribution of incident cases by year
table(alldx_summary$InciYear)

# 5. IBD/UC CASE IDENTIFICATION ---------------------------------------
# Split patients by number of diagnoses for different processing
patients_with_multiple_diags <- alldx_summary %>%
  filter(diag_count >= 2)

patients_with_one_diag <- alldx_summary %>%
  filter(diag_count < 2)

# 5.1 Process patients with single diagnosis record
Dx_onediag_oneyear <- alldx %>% 
  filter(reference.key. %in% patients_with_one_diag$reference.key.) %>% 
  mutate(type = case_when(
    startsWith(as.character(all.diagnosis.code.icd9.), "555") ~ "CD",  # Crohn's Disease
    startsWith(as.character(all.diagnosis.code.icd9.), "556") ~ "UC",  # Ulcerative Colitis
    TRUE ~ NA_character_
  ))

# 5.2 Process patients with multiple diagnosis records
Dx_multiple_diags <- alldx %>% 
  filter(reference.key. %in% patients_with_multiple_diags$reference.key.)

# Determine diagnosis type for patients with multiple records
Dx_multiple_diags <- Dx_multiple_diags %>%
  mutate(
    diagnosis_date = as.Date(reference.date.), 
    type = case_when(
      grepl("^555", all.diagnosis.code.icd9.) ~ "CD",  # Crohn's Disease
      grepl("^556", all.diagnosis.code.icd9.) ~ "UC",  # Ulcerative Colitis
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(reference.key., desc(diagnosis_date))  # Sort by patient ID and date (newest first)

# Find most recent diagnosis date for each patient
latest_diagnosis <- Dx_multiple_diags %>%
  group_by(reference.key.) %>%
  summarize(latest_date = max(diagnosis_date))

# Consider diagnoses only within 1 year of most recent diagnosis
Dx_multiple_diags_oneyear <- Dx_multiple_diags %>%
  left_join(latest_diagnosis, by = "reference.key.") %>%
  filter(diagnosis_date > latest_date %m-% years(1) & diagnosis_date <= latest_date)

# Determine most likely diagnosis type based on frequency in the last year
true_types <- Dx_multiple_diags_oneyear %>%
  group_by(reference.key.) %>%
  summarize(true_type = ifelse(n_distinct(type[type != ""]) == 1, first(type[type != ""]), 
                               ifelse(sum(type == "CD") > sum(type == "UC"), "CD",
                                      ifelse(sum(type == "UC") > sum(type == "CD"), "UC",
                                             first(type[type != ""])))))

# Merge type determination back into main data
Dx_multiple_diags <- Dx_multiple_diags %>%
  dplyr::select(-diagnosis_date, -type) %>% 
  arrange(reference.key., reference.date.) %>% 
  distinct(reference.key., .keep_all = TRUE) %>%
  left_join(true_types, by = "reference.key.") %>% 
  mutate(type = true_type) %>% 
  dplyr::select(-true_type)

# 5.3 Combine single and multiple diagnosis patients
ibd_dx <- Dx_multiple_diags %>%
  rbind(Dx_onediag_oneyear) %>% 
  mutate(
    firstDiag = reference.date.,  # First diagnosis date
    InciYear = year(firstDiag),   # Year of incidence
    Incimonth = month(firstDiag), # Month of incidence
    Inciage = as.numeric(as.Date(firstDiag) - as.Date(date.of.birth.yyyy.mm.dd.)) / 365.25,  # Age at diagnosis
    DeathYear = year(date.of.registered.death.),  # Year of death if applicable
    Deathage = as.numeric(as.Date(date.of.registered.death.) - as.Date(date.of.birth.yyyy.mm.dd.)) / 365.25  # Age at death
  )

# Extract UC incident cases for analysis
incident <- ibd_dx %>% 
  filter(type == "UC") %>% 
  mutate(
    year = year(reference.date.),
    age = Inciage
  )

# 6. MEDICATION ANALYSIS (UC PATIENTS) --------------------------------
# Extract UC patients for medication analysis
uc_dx <- ibd_dx %>% filter(type == "UC")

# Load prescription data
Rx <- readRDS("./IBD_2000-2024cohort_2000-2024Rx.RDS") %>%
  distinct()

# Select key fields from UC patients
uc_dx_selected <- uc_dx %>% dplyr::select(reference.key., firstDiag, date.of.registered.death.)

# Get prescriptions after diagnosis for UC patients
Rx_afterDx_uc <- Rx %>%
  filter(reference.key. %in% uc_dx_selected$reference.key.) %>% 
  left_join(uc_dx_selected, by = "reference.key.") %>%
  filter(as.Date(prescription.start.date.) >= firstDiag)  # Only keep prescriptions after diagnosis

# Extract steroid prescriptions (oral or injectable)
steroidRx_uc <- Rx_afterDx_uc %>% 
  filter(grepl("prednisone|methylprednisolone|hydrocortisone|prednisolone|budesonide", 
               drug.name., ignore.case = TRUE)) %>% 
  filter(grepl("ORAL|INJECTION|PO|INJ", route., ignore.case = TRUE))

# 6.1 Identify long-term steroid use (≥3 months)
# Sort steroid prescriptions by date
steroidRx_uc_sorted <- steroidRx_uc %>%
  arrange(reference.key., as.Date(prescription.start.date.))

# Identify continuous steroid treatments (gap ≤14 days)
steroidRx_uc_sorted <- steroidRx_uc_sorted %>%
  group_by(reference.key.) %>%
  mutate(
    prev_end_date = lag(as.Date(prescription.end.date.)),  # Previous prescription end date
    gap_days = as.numeric(as.Date(prescription.start.date.) - prev_end_date, units = "days"),  # Gap in days
    group = cumsum(ifelse(is.na(gap_days) | gap_days > 14, 1, 0))  # Create new group if gap > 14 days
  ) %>%
  ungroup()

# Merge continuous steroid treatments
steroidRx_uc_merged <- steroidRx_uc_sorted %>%
  group_by(reference.key., group) %>%
  summarise(
    prescription.start.date. = min(prescription.start.date.),  # Earliest start date
    prescription.end.date. = max(prescription.end.date.),      # Latest end date
    .groups = "drop"
  )

# Calculate treatment duration and eligibility (≥90 days)
steroidRx_uc_merged <- steroidRx_uc_merged %>%
  mutate(
    duration = as.numeric(as.Date(prescription.end.date.) - as.Date(prescription.start.date.)) + 1,  # Duration in days
    eligible = case_when(
      duration >= 90 ~ as.Date(prescription.start.date.) + 89,  # Date when 90-day threshold is reached
      TRUE ~ as.Date(NA)  # Not eligible
    )
  )

# 6.2 Identify frequent steroid use (two treatments ≤365 days apart)
steroidRx_uc_merged <- steroidRx_uc_merged %>%
  arrange(reference.key., prescription.start.date.) %>%
  group_by(reference.key.) %>%
  mutate(
    next_start_date = lead(as.Date(prescription.start.date.)),  # Next treatment start date
    gap_days = as.numeric(next_start_date - as.Date(prescription.end.date.)),  # Days between treatments
    eligible2 = case_when(
      !is.na(gap_days) & gap_days <= 365 ~ next_start_date,  # Eligible if gap ≤365 days
      TRUE ~ as.Date(NA)  # Not eligible
    )
  ) %>%
  ungroup()

# 6.3 Identify IBD flare-ups after steroid treatment
# Load emergency department and inpatient data
AE <- readRDS("./IBD_2000-2024cohort_2000-2024AE.RDS")
Ip <- readRDS("./IBD_2000-2024cohort_2000-2024Ip.RDS")

# Process emergency department data
AE <- AE %>%
  mutate(
    discharge.date. = as.Date(discharge.date.yyyy.mm.dd., format = "%Y-%m-%d"),
    discharge.date.plus1 = as.Date(discharge.date.yyyy.mm.dd., format = "%Y-%m-%d") + 1  # Day after discharge
  ) %>% 
  select(reference.key., discharge.date., discharge.date.plus1)

# Extract IBD-related inpatient admissions
IBDIp <- Ip %>%
  filter(if_any(c(principal.diagnosis.code., diagnosis.rank.2.), 
                ~ grepl("^555|^556", ., ignore.case = TRUE)))  # IBD diagnosis codes

# Process inpatient data
IBDIp <- IBDIp %>%
  mutate(
    admission.date.yyyy.mm.dd. = as.Date(admission.date.yyyy.mm.dd., format = "%Y-%m-%d"),
    admission.date = as.Date(admission.date.yyyy.mm.dd.),
    discharge.date = as.Date(discharge.date.yyyy.mm.dd.)
  ) %>%
  arrange(reference.key., admission.date)

# Function to merge consecutive hospital stays
merge_hospital_stays <- function(data) {
  repeat {
    prev_nrow <- nrow(data)  # Record row count before merging
    
    data <- data %>%
      group_by(reference.key.) %>%
      arrange(admission.date) %>%
      mutate(
        prev_discharge = lag(discharge.date)  # Previous stay discharge date
      ) %>%
      mutate(
        # Need to merge if consecutive/overlapping stays
        need_merge = !is.na(prev_discharge) & (prev_discharge >= (admission.date - 1))
      ) %>%
      mutate(
        # Merge stays by keeping earlier admission and later discharge
        admission.date = ifelse(need_merge, lag(admission.date), admission.date),
        discharge.date = ifelse(need_merge, pmax(discharge.date, prev_discharge, na.rm = TRUE), discharge.date)
      ) %>%
      filter(!need_merge | is.na(need_merge)) %>%  # Remove redundant records
      ungroup() %>%
      select(-prev_discharge, -need_merge)
    
    # Exit loop if no more merges occurred
    if (nrow(data) == prev_nrow) break
  }
  
  return(data)
}

# Apply hospital stay merging function
IBDIp <- merge_hospital_stays(IBDIp)

# Identify emergency visits that led to IBD hospitalizations
IBDIp_filtered <- IBDIp %>%
  filter(reference.key. %in% AE$reference.key.) %>% 
  inner_join(AE, by = "reference.key.", multiple = "all") %>%
  filter(
    # Match A&E discharge to inpatient admission on same or next day
    admission.date.yyyy.mm.dd. == discharge.date. |
      admission.date.yyyy.mm.dd. == discharge.date.plus1
  ) %>%
  distinct() %>%
  mutate(flareup = admission.date.yyyy.mm.dd.) %>% 
  select(reference.key., flareup)

# Match flare-ups with steroid prescriptions
merged_data <- steroidRx_uc_merged %>%
  inner_join(IBDIp_filtered, by = "reference.key.", multiple = "all")

# Identify flare-ups within 90 days of steroid discontinuation
merged_data <- merged_data %>%
  mutate(
    time_diff = as.Date(flareup) - as.Date(prescription.end.date.),  # Days between steroid end and flare-up
    eligible3 = ifelse(time_diff >= 0 & time_diff <= 90, as.character(flareup), NA)  # Eligible if within 90 days
  ) %>%
  mutate(eligible3 = as.Date(eligible3))

# 6.4 Find earliest eligibility date across all three criteria
# Get earliest date for criteria 1 & 2
steroidRx_uc_earliest <- steroidRx_uc_merged %>%
  group_by(reference.key.) %>%
  summarise(
    earliest_eligible = if(all(is.na(eligible))) NA else min(eligible, na.rm = TRUE),    # Criterion 1
    earliest_eligible2 = if(all(is.na(eligible2))) NA else min(eligible2, na.rm = TRUE)  # Criterion 2
  ) %>%
  filter(!(is.na(earliest_eligible) & is.na(earliest_eligible2)))  # Remove if both NA

# Get earliest date for criterion 3
eligible3_earliest <- merged_data %>%
  group_by(reference.key.) %>%
  summarise(
    earliest_eligible3 = if(all(is.na(eligible3))) NA else min(eligible3, na.rm = TRUE)  # Criterion 3
  ) %>%
  filter(!is.na(earliest_eligible3))

# Combine all criteria and find overall earliest date
merged_earliest <- full_join(steroidRx_uc_earliest, eligible3_earliest, by = "reference.key.") %>%
  mutate(
    final_eligible = pmin(earliest_eligible, earliest_eligible2, earliest_eligible3, na.rm = TRUE),
    final_eligible_year = year(final_eligible)
  )

# Show distribution of eligibility by year
table(merged_earliest$final_eligible_year)

# Add eligibility information to UC patient data
uc_dx <- full_join(uc_dx, merged_earliest, by = "reference.key.")

# 7. SURGICAL PROCEDURES ANALYSIS -------------------------------------
# Load procedure data
Px <- readRDS("./IBD_2000-2022cohort_1993-2022Px.RDS")
firstDx_death <- readRDS("./IBD_1993-2024firstDiag_1993-2024death.RDS")

# Extract IBD-related surgical procedures
IBDPx <- Px %>% 
  filter(grepl("^45\\.0|^45\\.33|^45\\.5|^45\\.6|^45\\.7|^45\\.8|^45\\.9|^45\\.90|^45\\.91|^45\\.92|^45\\.93|^45\\.94|^45\\.95|^48\\.
                     |^48\\.0|^48\\.1|^48\\.2|^48\\.3|^48\\.4|^48\\.5|^48\\.6|^48\\.7|^48\\.8|^48\\.9|^46\\.0|^46\\.1|^46\\.2|^17\\.3",
               all.procedure.code., ignore.case = TRUE)) %>% 
  mutate(procedure = as.Date(procedure.date.yyyy.mm.dd.hh.mm.)) %>%  
  left_join(uc_dx, by = "reference.key.") %>% 
  filter(as.Date(procedure) >= firstDiag) %>%  # Only include procedures after diagnosis
  group_by(reference.key.) %>% 
  arrange(procedure) %>%  # Sort by procedure date
  distinct(reference.key., .keep_all = TRUE) %>%  # Keep earliest procedure
  select(reference.key., firstDiag, procedure) %>% 
  left_join(firstDx_death, by = "reference.key.") %>% 
  mutate(firstDiag = firstDiag.x)

# Extract biologic medication data for UC patients
bioRx_uc <- Rx_afterDx_uc %>%
  filter(grepl("infliximab|adalimumab|golimumab|ustekinumab|vedolizumab|tofacitinib", 
               drug.name., ignore.case = TRUE)) %>%
  mutate(
    bio = prescription.start.date.,
    bioend = prescription.end.date.
  ) %>%
  select(reference.key., bio, bioend) %>%
  group_by(reference.key.) %>%
  arrange(bio) %>%
  distinct(reference.key., .keep_all = TRUE) %>%  # Keep earliest biologic
  filter(reference.key. %in% uc_dx$reference.key.)

# Combine procedure and biologic data
IBDPx_bioRx_uc <- IBDPx %>% 
  filter(reference.key. %in% uc_dx$reference.key.) %>% 
  left_join(bioRx_uc, by = "reference.key.")

# Identify procedures occurring before biologic therapy (surgical unmet need)
IBDPx_bioRx_uc <- IBDPx_bioRx_uc %>%
  mutate(Px_unmet = case_when(
    is.na(bio) | procedure < bio ~ procedure,  # No biologic or procedure before biologic
    TRUE ~ NA_Date_
  ))

# 8. UNMET NEEDS ANALYSIS --------------------------------------------
# Combine eligibility and biologic therapy data
merged_earliest_Rx <- merged_earliest %>% full_join(bioRx_uc, by = "reference.key.")

# Classify unmet need types by time between eligibility and biologic start
merged_earliest_Rx <- merged_earliest_Rx %>%
  mutate(
    bio = as.Date(bio),
    final_eligible = as.Date(final_eligible)
  ) %>%
  mutate(
    `eligible to bio` = as.numeric(bio - final_eligible),  # Days between eligibility and biologic
    `unmet type` = case_when(
      is.na(final_eligible) & !is.na(bio) ~ "Direct use",      # Biologic without prior eligibility
      !is.na(final_eligible) & is.na(bio) ~ "not use",         # Eligible but no biologic
      `eligible to bio` <= 0 ~ "meet before eligible",         # Biologic started before eligibility
      `eligible to bio` >= 1 & `eligible to bio` <= 29 ~ "1-29",           # Biologic 1-29 days after eligibility
      `eligible to bio` >= 30 & `eligible to bio` <= 179 ~ "30-179",       # Biologic 30-179 days after eligibility
      `eligible to bio` >= 180 ~ "NOearlythan_180",            # Biologic ≥180 days after eligibility
      TRUE ~ NA_character_
    )
  )

# Prepare surgical data for combined analysis
IBDPx_bioRx_uc_filtered <- IBDPx_bioRx_uc %>%
  select(reference.key., firstDiag, procedure, Px_unmet)

# Merge all data sources
merged_data <- full_join(merged_earliest_Rx, IBDPx_bioRx_uc_filtered, by = "reference.key.") %>% 
  left_join(firstDx_death, by = "reference.key.")

# Comprehensive classification of unmet needs
merged_data <- merged_data %>%
  mutate(
    # Ensure consistent date formats
    final_eligible = as.Date(final_eligible),
    bio = as.Date(bio),
    Px_unmet = as.Date(Px_unmet, origin = "1970-01-01")
  ) %>%
  mutate(
    # Classify cases with surgery but no biologic or eligibility
    `unmet type` = case_when(
      is.na(final_eligible) & is.na(bio) ~ "surgery",
      TRUE ~ `unmet type`
    ),
    
    # Initial incidence date assignment based on case type
    incidence = case_when(
      is.na(final_eligible) & is.na(bio) ~ floor_date(Px_unmet, unit = "year"),  # Surgery date
      `unmet type` == "not use" & !is.na(date.of.registered.death.) ~ floor_date(final_eligible, unit = "year"),  # Death without biologic
      `unmet type` == "Direct use" ~ floor_date(bio, unit = "year"),  # Direct biologic use
      TRUE ~ NA_Date_
    )
  ) %>%
  mutate(
    # Identify deaths in non-biologic users
    `unmet type` = ifelse(`unmet type` == "not use" & !is.na(date.of.registered.death.), 
                          "not use death", `unmet type`)
  ) %>%
  mutate(
    # Prioritize earlier surgery over eligibility
    `unmet type` = ifelse(!is.na(final_eligible) & !is.na(Px_unmet) & Px_unmet < final_eligible, 
                          "surgery", `unmet type`),
    
    # Update incidence date based on final classification
    incidence = case_when(
      `unmet type` == "surgery" ~ floor_date(Px_unmet, unit = "year"),
      `unmet type` == "not use death" ~ floor_date(final_eligible, unit = "year"),
      `unmet type` == "Direct use" ~ floor_date(bio, unit = "year"),
      !is.na(Px_unmet) & (is.na(final_eligible) | Px_unmet < final_eligible) ~ floor_date(Px_unmet, unit = "year"),
      TRUE ~ floor_date(final_eligible, unit = "year")
    )
  )

# Handle direct biologic use cases with earlier surgery
merged_data <- merged_data %>%
  mutate(
    # For direct biologic users, check if surgery occurred first
    incidence = case_when(
      `unmet type` == "Direct use" & !is.na(Px_unmet) & Px_unmet < bio ~ Px_unmet,
      `unmet type` == "Direct use" ~ bio,
      TRUE ~ incidence
    )
  ) %>%
  mutate(
    # Update classification if surgery occurred before biologic
    `unmet type` = case_when(
      `unmet type` == "Direct use" & !is.na(Px_unmet) & Px_unmet < bio ~ "surgery",
      TRUE ~ `unmet type`
    )
  )

# Final adjustments for early biologic use cases
merged_data <- merged_data %>%
  mutate(
    # Handle cases where biologic used before formal eligibility
    incidence = case_when(
      `unmet type` == "meet before eligible" ~ bio,
      TRUE ~ incidence
    ),
    `unmet type` = case_when(
      `unmet type` == "meet before eligible" ~ "Direct use",
      TRUE ~ `unmet type`
    ),
    firstDiag = firstDiag.x
  )

# Extract year from incidence date for summary
merged_data$incidence_year = year(merged_data$incidence)

# Add first diagnosis information
firstdx <- uc_dx %>% select(reference.key., firstDiag)
merged_data <- merged_data %>% 
  select(-firstDiag) %>% 
  left_join(firstdx, by = "reference.key.")

# 9. RESULTS EXPORT AND SUMMARY --------------------------------------
# Create contingency table of incidence year vs. unmet need type
incidence_unmet_table <- table(merged_data$incidence_year, merged_data$`unmet type`)

# Convert to dataframe for easier export
incidence_unmet_df <- as.data.frame.matrix(incidence_unmet_table)

# Add year as explicit column
incidence_unmet_df <- incidence_unmet_df %>%
  mutate(incidence_year = rownames(incidence_unmet_df)) %>%
  relocate(incidence_year)  # Move year to first column

# Export detailed results to Excel
write.xlsx(incidence_unmet_df, "/Users/zhangyin/Library/CloudStorage/OneDrive-connect.hku.hk/IBD/WP1/unmet needs/incidence_unmet_table_uc.xlsx", rowNames = FALSE)
write.xlsx(merged_data, "/Users/zhangyin/Library/CloudStorage/OneDrive-connect.hku.hk/IBD/WP1/unmet needs/incidence_unmet_uc.xlsx", rowNames = FALSE)

# Create summary statistics for years 2014-2022
years = 2014:2022
incidence_summary <- incidence_unmet_df %>%
  rowwise() %>%
  mutate(
    Year = incidence_year,
    `Surgery_OR_Death` = surgery + `not use death`,         # Combine Surgery and Death categories
    `>=180` = NOearlythan_180 + `not use`,                  # Long delays and non-users
    Met = `Direct use`,                                      # Needs met promptly
    `1_29` = `1-29`,                                         # Short delay
    `<30` = Met + `1_29`,                                    # Quick response (≤30 days)
    `30_179` = `30-179`,                                     # Medium delay
    Total = sum(Met, `1_29`, `30_179`, `>=180`, Surgery_OR_Death),  # Total cases
    
    # Calculate proportions
    `Met_Proportion` = Met / Total,
    `<30_Proportion` = `<30` / Total,
    `30_179_Proportion` = `30_179` / Total,
    `>=180_Proportion` = `>=180` / Total,
    `Surgery_OR_Death_Proportion` = Surgery_OR_Death / Total
  ) %>%
  select(
    Year, Total,
    `<30`, `<30_Proportion`,
    `30_179`, `30_179_Proportion`,
    `>=180`, `>=180_Proportion`,
    Surgery_OR_Death, `Surgery_OR_Death_Proportion`
  ) %>%
  filter(Year %in% years)  # Limit to years 2014-2022

# Export summary data for visualization
write.csv(incidence_summary, "IBD_UC Unmet Need Plot Data.csv")