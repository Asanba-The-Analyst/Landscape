# =============================================================================
# MATERNAL IMMUNIZATION SAFETY SURVEILLANCE LANDSCAPE ANALYSIS
# Full Data Synthesis and Visualization Script
# =============================================================================

# Load required libraries
library(tidyverse)
library(readr)
library(openxlsx)
library(janitor)
library(ggplot2)
library(plotly)
library(gtsummary)
library(flextable)

# Set visualization theme
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                  plot.subtitle = element_text(size = 12, hjust = 0.5),
                  axis.title = element_text(face = "bold"),
                  legend.title = element_text(face = "bold"),
                  panel.grid.minor = element_blank()))

# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

# Load survey data (assuming the CSV is in a 'Data' folder)
survey_data <- read_csv("Data/LandscapeAnalysisKHR_DATA_2025-09-17_1234.csv")

# Data cleaning and preprocessing
clean_data <- survey_data %>%
  clean_names() %>%
  # Clean site names based on the data dictionary
  mutate(site_name = case_when(
    country == 1 & sname_ghana == 1 ~ "Dodowa Health Research Centre",
    country == 1 & sname_ghana == 2 ~ "Kintampo Health Research Centre",
    country == 2 & sname_kenya == 1 ~ "KWTRP",
    country == 2 & sname_kenya == 2 ~ "KEMRI-LSTM",
    TRUE ~ "Unknown Site"
  ))

# =============================================================================
# 2.1 SITE PROFILES
# =============================================================================

# Create a data frame for site profiles
site_profiles <- clean_data %>%
  select(
    site = form_id,
    country_region_county = country,
    site_type = datasys_g, # Using datasys_g/k to infer site type
    facility_type = facility_type,
    facility_level = level,
    population_served = population,
    catchment_size = estimate,
    geographic_coverage = scale
  ) %>%
  # Recode categorical variables based on the data dictionary
  mutate(
    country_region_county = case_when(
      country_region_county == 1 ~ "Ghana",
      country_region_county == 2 ~ "Kenya",
      TRUE ~ "Unknown"
    ),
    facility_type = case_when(
      facility_type == 1 ~ "Public",
      facility_type == 2 ~ "Private",
      facility_type == 3 ~ "Faith-based",
      TRUE ~ "Unknown"
    ),
    facility_level = case_when(
      facility_level == 1 ~ "Level 1",
      facility_level == 2 ~ "Level 2",
      facility_level == 3 ~ "Level 3",
      facility_level == 4 ~ "Level 4",
      facility_level == 5 ~ "Level 5",
      TRUE ~ "Unknown"
    ),
    population_served = case_when(
      population_served == 1 ~ "Urban",
      population_served == 2 ~ "Rural",
      population_served == 3 ~ "Both Rural and Urban",
      TRUE ~ "Unknown"
    ),
    geographic_coverage = case_when(
      geographic_coverage == 1 ~ "National",
      geographic_coverage == 2 ~ "County",
      geographic_coverage == 3 ~ "Regional",
      geographic_coverage == 4 ~ "Sub-County",
      geographic_coverage == 5 ~ "District",
      geographic_coverage == 6 ~ "Other",
      TRUE ~ "Unknown"
    )
  )

# =============================================================================
# 2.2 DATA SYSTEM ATTRIBUTES
# =============================================================================

### Data Capture Modality Heatmap
# Prepare data for the heatmap
data_capture_modality_data <- clean_data %>%
  select(
    site_id = form_id,
    anc = capture_anc,
    maternity = maternity,
    paediatric = paediatric,
    community = community,
    outpatient = outp,
    inpatient = inp
  ) %>%
  pivot_longer(
    cols = anc:inpatient,
    names_to = "service_area",
    values_to = "capture_mode"
  ) %>%
  mutate(
    service_area = case_when(
      service_area == "anc" ~ "ANC",
      service_area == "maternity" ~ "Maternity",
      service_area == "paediatric" ~ "Paediatric",
      service_area == "outpatient" ~ "Outpatient",
      service_area == "inpatient" ~ "Inpatient",
      service_area == "community" ~ "Community",
      TRUE ~ service_area
    ),
    capture_mode = case_when(
      capture_mode == 1 ~ "Paper only",
      capture_mode == 2 ~ "Digital only",
      capture_mode == 3 ~ "Hybrid",
      TRUE ~ "Unknown"
    )
  )

# Define custom colors as specified in the proposal
capture_colors <- c("Paper only" = "darkblue", "Digital only" = "green", "Hybrid" = "palegoldenrod","Unknown" = "gray")

# Create the heatmap for data capture modality
ggplot(data_capture_modality_data, aes(x = factor(site_id), y = service_area, fill = capture_mode)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(values = capture_colors) +
  labs(
    title = "Data Capture Modality Across Sites and Service Areas",
    x = "Sites",
    y = "Service Areas",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_fixed(ratio = 1)


### Core Variable Collection and Modality Heatmap
# Prepare data for the core variables heatmap
core_variable_data <- clean_data %>%
  select(
    site_id = form_id,
    hiv = hiv,
    hepb = hepb,
    hepc = hepc,
    syphilis = syphilis,
    maternal_dob = sys_dob,
    maternal_age = sys_age,
    gestational_age = gest,
    lmp = lmp
  ) %>%
  pivot_longer(
    cols = hiv:lmp,
    names_to = "core_variable",
    values_to = "collection_mode"
  ) %>%
  mutate(
    core_variable = case_when(
      core_variable == "hiv" ~ "HIV",
      core_variable == "hepb" ~ "Hepatitis B",
      core_variable == "hepc" ~ "Hepatitis C",
      core_variable == "syphilis" ~ "Syphilis",
      core_variable == "maternal_dob" ~ "Maternal DOB",
      core_variable == "maternal_age" ~ "Maternal age",
      core_variable == "gestational_age" ~ "Gestational age",
      core_variable == "lmp" ~ "LMP",
      TRUE ~ core_variable
    ),
    collection_mode = case_when(
      collection_mode == 1 ~ "Paper only",
      collection_mode == 2 ~ "Digital only",
      collection_mode == 9 ~ "Not collected",
      TRUE ~ "Unknown"
    )
  )

# Define custom colors as specified in the proposal
variable_colors <- c("Paper only" = "pink", "Digital only" = "lightskyblue", "Not collected" = "gray","Unknown" = "black")

# Create the heatmap for core variable collection
ggplot(core_variable_data, aes(x = factor(site_id), y = core_variable, fill = collection_mode)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(values = variable_colors) +
  labs(
    title = "Core Variable Collection and Modality Across Sites",
    x = "Sites",
    y = "Core Variables",
    fill = "Data Collection Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_fixed(ratio = 1)

# =============================================================================
# 2.3 DATA QUALITY & SHARING PRACTICES
# =============================================================================

# Create a data frame for data quality and sharing practices
data_quality_sharing <- clean_data %>%
  select(
    site = form_id,
    trained_in_data_quality = trained,
    data_quality_check_methods = checarr,
    data_quality_check_frequency = dataq_freq,
    mnch_indicator_review = mnch,
    data_refresh_frequency = data_refresh,
    data_sharing = dsharing,
    data_transmission = dtransfer
  ) %>%
  # Recode categorical variables based on the data dictionary
  mutate(
    trained_in_data_quality = case_when(
      trained_in_data_quality == 1 ~ "Yes",
      trained_in_data_quality == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    data_quality_check_methods = case_when(
      data_quality_check_methods == 1 ~ "Manual Check",
      data_quality_check_methods == 2 ~ "System Automation",
      TRUE ~ "Unknown"
    ),
    data_quality_check_frequency = case_when(
      data_quality_check_frequency == 1 ~ "Daily",
      data_quality_check_frequency == 2 ~ "Weekly",
      data_quality_check_frequency == 3 ~ "Monthly",
      data_quality_check_frequency == 4 ~ "Quarterly",
      data_quality_check_frequency == 5 ~ "Annually",
      data_quality_check_frequency == 6 ~ "Other",
      TRUE ~ "Unknown"
    ),
    mnch_indicator_review = case_when(
      mnch_indicator_review == 1 ~ "Yes",
      mnch_indicator_review == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    data_refresh_frequency = case_when(
      data_refresh_frequency == 1 ~ "Daily",
      data_refresh_frequency == 2 ~ "Weekly",
      data_refresh_frequency == 3 ~ "Monthly",
      data_refresh_frequency == 4 ~ "Quarterly",
      data_refresh_frequency == 5 ~ "Annually",
      data_refresh_frequency == 6 ~ "Other",
      TRUE ~ "Unknown"
    ),
    data_sharing = case_when(
      data_sharing == 1 ~ "Yes",
      data_sharing == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    data_transmission = case_when(
      data_transmission == 1 ~ "Manual",
      data_transmission == 2 ~ "Automated",
      TRUE ~ "Unknown"
    )
  )

# Create and format the table using gtsummary and flextable
data_quality_sharing_table <- data_quality_sharing %>%
  select(-site) %>% # Remove site ID for a general summary
  tbl_summary() %>%
  as_flex_table()

# Print the table to the console
data_quality_sharing_table

# =============================================================================
# 2.4 DATA STORAGE SYSTEMS AND PRACTICES
# =============================================================================

# Create a data frame for data storage systems and practices
data_storage_practices <- clean_data %>%
  select( location=site_name,
    site = form_id,
    system_type = system,
    data_storage_type = storge,
    data_storage_location = local, # 'local' and 'cloud' are separate columns
    data_integration_type = spysysint1,
    network_architecture = network,
    security_measures = dsecurity,
    backup_recovery = backup_freq
  ) %>%
  # Recode categorical variables based on the data dictionary
  mutate(
    system_type = case_when(
      system_type == 1 ~ "Proprietary",
      system_type == 2 ~ "Open Source",
      system_type == 3 ~ "In-House",
      TRUE ~ "Unknown"
    ),
    data_storage_type = case_when(
      data_storage_type == 1 ~ "SQL",
      data_storage_type == 2 ~ "NoSQL",
      data_storage_type == 3 ~ "Flat File",
      data_storage_type == 4 ~ "Other",
      TRUE ~ "Unknown"
    ),
    data_storage_location = case_when(
      data_storage_location == 1 ~ "Local",
      data_storage_location == 2 ~ "Cloud",
      TRUE ~ "Unknown"
    ),
    data_integration_type = case_when(
      data_integration_type == 1 ~ "Yes",
      data_integration_type == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    network_architecture = case_when(
      network_architecture == 1 ~ "Standalone",
      network_architecture == 2 ~ "Local Client-Server",
      network_architecture == 3 ~ "Cloud",
      TRUE ~ "Unknown"
    ),
    security_measures = case_when(
      security_measures == 1 ~ "Password Protection",
      security_measures == 2 ~ "Encryption",
      security_measures == 3 ~ "Access Control",
      security_measures == 4 ~ "Physical Security",
      security_measures == 5 ~ "Other",
      TRUE ~ "Unknown"
    ),
    backup_recovery = case_when(
      backup_recovery == 1 ~ "Daily",
      backup_recovery == 2 ~ "Weekly",
      backup_recovery == 3 ~ "Monthly",
      backup_recovery == 4 ~ "Quarterly",
      backup_recovery == 5 ~ "Annually",
      backup_recovery == 6 ~ "Other",
      TRUE ~ "Unknown"
    )
  )

# Create and format the table using gtsummary and flextable
data_storage_practices_table <- data_storage_practices %>%
  select(-site) %>% # Remove site ID for a general summary
  tbl_summary(by=location) %>%
  as_flex_table()

# Print the table to the console
data_storage_practices_table

# =============================================================================
# 3 SWOT ANALYSIS AND RECOMMENDATIONS
# =============================================================================

# The SWOT analysis and recommendations are qualitative summaries based on the
# quantitative analysis. This section provides a placeholder for the narrative.

# Strengths:
# - (Based on analysis) Sites with digital-only or hybrid data capture.
# - (Based on analysis) Sites that collect a wide range of core variables.
# - (Based on analysis) Sites with regular data capture routines.
# - (Based on analysis) Data standardization with ICD10/ICD11, MedDRA, etc.

# Weaknesses:
# - (Based on analysis) Over-reliance on paper-based systems.
# - (Based on analysis) Gaps in core variable collection (e.g., specific lab tests, gestational age).
# - (Based on analysis) Challenges such as system downtime, lack of training, or difficulty navigating the system.

# Opportunities:
# - (Based on analysis) Digitizing data collection in paper-based areas.
# - (Based on analysis) Standardizing data systems across sites.
# - (Based on analysis) Developing training programs to improve user satisfaction and data quality.

# Threats:
# - (Based on analysis) Lack of data security or backup practices.
# - (Based on analysis) Disparate data systems that hinder integration.
# - (Based on analysis) Incomplete data leading to poor research outcomes.

# Recommendations for Phase II:
# 1. Prioritize sites for a large-scale pregnancy registry based on the extent of their digital data collection and the completeness of their core variables.
# 2. Develop a common data model and backend system to harmonize data from different sites, starting with the most comprehensive ones.
# 3. Implement targeted training programs to address data quality and user satisfaction challenges identified in the analysis.

# =============================================================================
# EXPORTING RESULTS
# =============================================================================

# # Create a list of data frames to be exported
# all_data_frames <- list(
#   "Site_Profiles" = site_profiles,
#   "Data_Capture_Modality" = data_capture_modality_data,
#   "Core_Variables" = core_variable_data,
#   "Data_Quality_Sharing" = data_quality_sharing,
#   "Data_Storage_Practices" = data_storage_practices
# )
# 
# # Export all data frames to a single XLSX file with different sheets
# write.xlsx(all_data_frames, file = "LandscapeAnalysis_Synthesized_Results.xlsx")
# 
# # You can also export the flextable objects to a Word or HTML file
# # library(officer)
# # write_flex_table(data_quality_sharing_table, path = "DataQualitySharing.docx")
# # write_flex_table(data_storage_practices_table, path = "DataStoragePractices.docx")
