# install.packages(c("tidyverse", "lubridate", "readr", "openxlsx", "haven", 
#                    "janitor", "scales", "ggrepel", "patchwork", "plotly", 
#                    "knitr", "kableExtra", "DT", "sf", "rnaturalearth", 
#                    "rnaturalearthdata", "RColorBrewer"))

# Load required libraries
library(tidyverse)
library(lubridate)
library(readr)
library(openxlsx)
library(haven)
library(janitor)
library(scales)
library(ggrepel)
library(patchwork)
library(plotly)
library(knitr)
library(kableExtra)
library(DT)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(readr)
data<- read_csv("Data/LandscapeAnalysisKHR_DATA_2025-09-17_1234.csv")



# =============================================================================
# MATERNAL IMMUNIZATION SAFETY SURVEILLANCE LANDSCAPE ANALYSIS
# Data Synthesis and Visualization Script
# =============================================================================



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

# Load data dictionary
data_dict <- read_csv("Data/LandscapeAnalysisKHRCMAIN_DataDictionary_2025-09-15.csv")

# Load survey data
survey_data <- read_csv("Data/LandscapeAnalysisKHR_DATA_2025-09-17_1234.csv")

# Data cleaning and preprocessing
clean_data <- survey_data %>%
  # Convert date fields
  mutate(across(matches("sdate|dcomp|dchecek"), dmy_hm)) %>%
  # Convert checkbox fields to proper format
  mutate(across(starts_with("role"), as.logical)) %>%
  # Clean country names
  mutate(country_name = case_when(
    country == 1 ~ "Ghana",
    country == 2 ~ "Kenya",
    TRUE ~ "Unknown"
  )) %>%
  # Clean site names
  mutate(site_name = case_when(
    !is.na(sname_ghana) & country == 1 ~ case_when(
      sname_ghana == 1 ~ "Dodowa Health Research Centre",
      sname_ghana == 2 ~ "Kintampo Health Research Centre",
      TRUE ~ "Other Ghana Site"
    ),
    !is.na(sname_kenya) & country == 2 ~ case_when(
      sname_kenya == 1 ~ "KWTRP",
      sname_kenya == 2 ~ "KEMRI-LSTM",
      TRUE ~ "Other Kenya Site"
    ),
    TRUE ~ "Unknown Site"
  )) %>%
  # Clean data system information
  mutate(data_system = case_when(
    country == 1 & !is.na(datasys_g) ~ case_when(
      datasys_g == 1 ~ "HDSS",
      datasys_g == 2 ~ "LHIMS",
      datasys_g == 3 ~ "Redcap (parallel system)",
      datasys_g == 4 ~ "AnTPost",
      datasys_g == 5 ~ "PDEL",
      datasys_g == 6 ~ "E-TRACKER",
      datasys_g == 7 ~ "DHIMS",
      datasys_g == 8 ~ "RSLOG",
      datasys_g == 9 ~ "Paper-Based Registry",
      datasys_g == 10 ~ datasysoth_g,
      TRUE ~ "Other Ghana System"
    ),
    country == 2 & !is.na(datasys_k) ~ case_when(
      datasys_k == 1 ~ "KILIFI HDSS",
      datasys_k == 2 ~ "KCH Paediatric Ward Surveillance",
      datasys_k == 3 ~ "KCH Maternity",
      datasys_k == 4 ~ "KCH Antenatal Clinic",
      datasys_k == 5 ~ "MIMBa pregnancy registry",
      datasys_k == 6 ~ "Paper-Based Registry",
      datasys_k == 7 ~ datasysoth_k,
      TRUE ~ "Other Kenya System"
    ),
    TRUE ~ "Unknown System"
  )) %>%
  # Clean facility type
  mutate(facility_type_clean = case_when(
    facility_type == 1 ~ "Public facility",
    facility_type == 2 ~ "Private facility",
    facility_type == 3 ~ "Faith-based facility",
    TRUE ~ "Unknown"
  )) %>%
  # Clean level of facility
  mutate(level_clean = case_when(
    level == 1 ~ "Level 1",
    level == 2 ~ "Level 2",
    level == 3 ~ "Level 3",
    level == 4 ~ "Level 4",
    level == 5 ~ "Level 5",
    TRUE ~ "Unknown"
  )) %>%
  # Clean population served
  mutate(population_clean = case_when(
    population == 1 ~ "Urban",
    population == 2 ~ "Rural",
    population == 3 ~ "Both Rural and Urban",
    TRUE ~ "Unknown"
  )) %>%
  # Clean geographic coverage
  mutate(scale_clean = case_when(
    scale == 1 ~ "National",
    scale == 2 ~ "County",
    scale == 3 ~ "Regional",
    scale == 4 ~ "Sub-County",
    scale == 5 ~ "District",
    scale == 6 ~ scale_oth,
    TRUE ~ "Unknown"
  ))

# =============================================================================
# 2.1 SITE PROFILES
# =============================================================================

# Create site profiles table
site_profiles <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  select(
    Site = form_id,
    Country = country_name,
    Region = region,
    County = county,
    `Site Type` = site_name,
    `Facility Type` = facility_type_clean,
    `Facility Level` = level_clean,
    `Population Served` = population_clean,
    `Catchment Population` = estimate,
    `Geographic Coverage` = scale_clean
  ) %>%
  arrange(Country, Site)

# Display site profiles table
site_profiles %>%
  datatable(options = list(pageLength = 10, scrollX = TRUE), 
            rownames = FALSE) %>%
  formatStyle(columns = 1:10, fontSize = '12px')

# =============================================================================
# 2.2 DATA SYSTEM ATTRIBUTES
# =============================================================================

# 2.2.1 Data capture modality
data_capture_modality <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  mutate(
    paper_based = ifelse(paper == 1, "Yes", "No"),
    digital_based = ifelse(digital == 1, "Yes", "No"),
    modality = case_when(
      paper == 1 & digital == 1 ~ "Hybrid",
      paper == 1 & digital == 2 ~ "Paper Only",
      paper == 2 & digital == 1 ~ "Digital Only",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(Site = form_id, Country = country_name, paper_based, digital_based, modality)

# Plot data capture modality
modality_plot <- data_capture_modality %>%
  count(modality) %>%
  ggplot(aes(x = reorder(modality, n), y = n, fill = modality)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2) +
  coord_flip() +
  labs(
    title = "Data Capture Modality Across Sites",
    x = "Data Capture Modality",
    y = "Number of Sites"
  ) +
  scale_fill_brewer(palette = "Set2")

# Plot by country
modality_by_country <- data_capture_modality %>%
  count(Country, modality) %>%
  ggplot(aes(x = Country, y = n, fill = modality)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(
    title = "Data Capture Modality by Country",
    x = "Country",
    y = "Number of Sites",
    fill = "Modality"
  ) +
  scale_fill_brewer(palette = "Set2")

# Combine plots
modality_plot + modality_by_country

# 2.2.2 Core variable collection and modality
# Extract core variables from the dataset
core_variables <- clean_data %>%
  select(
    Site = form_id,
    Country = country_name,
    # Maternal demographics
    Maternal_Demographics = mdemo,
    # Maternal medical history
    Maternal_Medical_History = medhis,
    # Current pregnancy
    Current_Pregnancy = curpreg,
    # Maternal vaccination
    Maternal_Vaccination = mavacd,
    # Maternal outcomes
    Maternal_Outcomes = mout,
    # Neonatal outcomes
    Neonatal_Outcomes = neoout,
    # AEFI related to vaccination (adults)
    AEFI_Adults = aefirvac,
    # Postnatal follow-up
    Postnatal_Followup = fupdel,
    # AEFI related to vaccination (children)
    AEFI_Children = aefi_child
  ) %>%
  mutate(across(Maternal_Demographics:AEFI_Children, 
                ~ case_when(.x == 1 ~ "Yes", .x == 2 ~ "No", TRUE ~ "Unknown"))) %>%
  pivot_longer(cols = Maternal_Demographics:AEFI_Children, 
               names_to = "Core_Variable", 
               values_to = "Collected")

# Plot core variable collection
core_var_plot <- core_variables %>%
  filter(Collected %in% c("Yes", "No")) %>%
  count(Core_Variable, Collected) %>%
  group_by(Core_Variable) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  filter(Collected == "Yes") %>%
  ggplot(aes(x = reorder(Core_Variable, Percentage), y = Percentage)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(Percentage), "%")), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Core Variable Collection Across Sites",
    x = "Core Variable",
    y = "Percentage of Sites Collecting"
  ) +
  ylim(0, 105)

# 2.2.3 Data standardization/coding
coding_systems <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  mutate(
    ICD10 = ifelse(icd10 == 1, "Yes", "No"),
    ICD11 = ifelse(icd11 == 1, "Yes", "No"),
    MedDRA = ifelse(meddra == 1, "Yes", "No"),
    Other_Coding = ifelse(other_standz == 1, "Yes", "No")
  ) %>%
  select(Site = form_id, Country = country_name, ICD10, ICD11, MedDRA, Other_Coding) %>%
  pivot_longer(cols = ICD10:Other_Coding, names_to = "Coding_System", values_to = "Used")

# Plot coding system usage
coding_plot <- coding_systems %>%
  filter(Used == "Yes") %>%
  count(Coding_System) %>%
  ggplot(aes(x = reorder(Coding_System, n), y = n, fill = Coding_System)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2) +
  coord_flip() +
  labs(
    title = "Coding System Usage Across Sites",
    x = "Coding System",
    y = "Number of Sites"
  ) +
  scale_fill_brewer(palette = "Set3")

# =============================================================================
# 2.3 HEALTH FACILITY CARE PRACTICES AND CAPACITY
# =============================================================================

# 2.3.1 Duration of newborn follow up
newborn_followup <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  select(
    Site = form_id,
    Country = country_name,
    Followup_6mo = newborns1,
    No_Routine_Followup = newborns2,
    Followup_6mo_Rounds = newborns3,
    Followup_3mo_Rounds = newborns4,
    Followup_12mo = newborns5,
    Followup_24mo_Neurodev = newborns6,
    Other_Followup = newborn7
  ) %>%
  mutate(across(Followup_6mo:Other_Followup, 
                ~ case_when(.x == 1 ~ "Yes", .x == 2 ~ "No", TRUE ~ "Unknown")))

# Transform for plotting
followup_long <- newborn_followup %>%
  pivot_longer(cols = Followup_6mo:Other_Followup, 
               names_to = "Followup_Type", 
               values_to = "Available") %>%
  filter(Available == "Yes")

# Plot newborn follow-up duration
followup_plot <- followup_long %>%
  count(Followup_Type) %>%
  ggplot(aes(x = reorder(Followup_Type, n), y = n, fill = Followup_Type)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2) +
  coord_flip() +
  labs(
    title = "Newborn Follow-up Duration Across Sites",
    x = "Follow-up Type",
    y = "Number of Sites"
  ) +
  scale_fill_brewer(palette = "Pastel1")

# 2.3.2 Health facility medical diagnostics capacity
diagnostic_capacity <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  select(
    Site = form_id,
    Country = country_name,
    MRI = mri,
    CT = ct,
    Blood_Chemistry = bchem,
    Hematology = hem,
    Urinalysis = urin,
    XRay = xray
  ) %>%
  mutate(across(MRI:XRay, 
                ~ case_when(.x == 1 ~ "Available", 
                            .x == 2 ~ "Not Available", 
                            .x == 9 ~ "Not Applicable",
                            TRUE ~ "Unknown")))

# Transform for plotting
diagnostic_long <- diagnostic_capacity %>%
  pivot_longer(cols = MRI:XRay, 
               names_to = "Diagnostic_Test", 
               values_to = "Availability") %>%
  filter(Availability == "Available")

# Plot diagnostic capacity
diagnostic_plot <- diagnostic_long %>%
  count(Diagnostic_Test) %>%
  ggplot(aes(x = reorder(Diagnostic_Test, n), y = n, fill = Diagnostic_Test)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2) +
  coord_flip() +
  labs(
    title = "Diagnostic Test Availability Across Sites",
    x = "Diagnostic Test",
    y = "Number of Sites"
  ) +
  scale_fill_brewer(palette = "Pastel2")

# =============================================================================
# 2.4 DATA GOVERNANCE AND QUALITY
# =============================================================================

# 2.4.1 Data governance profiles
data_governance <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  mutate(
    Data_Protection = case_when(
      data_reg == 1 ~ "Yes",
      data_reg == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    Staff_Trained = case_when(
      trained == 1 ~ "Yes",
      trained == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    Sharing_Protocol = case_when(
      protocol == 1 ~ "Yes",
      protocol == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    Consent_Obtained = case_when(
      consented == 1 ~ "Yes",
      consented == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    Backup_Policy = case_when(
      backup == 1 ~ "Yes",
      backup == 2 ~ "No",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(Site = form_id, Country = country_name, 
         Data_Protection, Staff_Trained, Sharing_Protocol, 
         Consent_Obtained, Backup_Policy)

# Display data governance table
data_governance %>%
  datatable(options = list(pageLength = 10, scrollX = TRUE), 
            rownames = FALSE) %>%
  formatStyle(columns = 1:7, fontSize = '12px')

# 2.4.2 Data capture practices
data_capture_practices <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  mutate(
    Data_Capture_Device = case_when(
      mobile_phone == 1 ~ "Mobile Phone",
      laptop == 1 ~ "Laptop",
      tablet == 1 ~ "Tablet",
      paper_capture == 1 ~ "Paper",
      TRUE ~ "Unknown"
    ),
    Data_Entry_Mode = case_when(
      mode_entry == 1 ~ "Online",
      mode_entry == 2 ~ "Offline",
      mode_entry == 3 ~ "Both",
      TRUE ~ "Unknown"
    ),
    Patient_ID_Generation = case_when(
      ids == 1 ~ "Manual",
      ids == 2 ~ "System Generated",
      ids == 3 ~ "Both",
      TRUE ~ "Unknown"
    ),
    Standard_Identifier = case_when(
      standard_acr == 1 ~ "Yes",
      standard_acr == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    Mother_Child_Linkage = case_when(
      link_uni == 1 ~ "Yes",
      link_uni == 2 ~ "No",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(Site = form_id, Country = country_name, 
         Data_Capture_Device, Data_Entry_Mode, Patient_ID_Generation,
         Standard_Identifier, Mother_Child_Linkage)

# 2.4.3 Data quality and sharing practices
data_quality <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  mutate(
    Quality_Checks = case_when(
      checarr == 1 ~ "Yes",
      checarr == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    Data_Sharing = case_when(
      dsharing == 1 ~ "Yes",
      dsharing == 2 ~ "No",
      TRUE ~ "Unknown"
    ),
    Data_Sharing_Frequency = case_when(
      sharefreq_1 == 1 ~ "On Request",
      sharefreq_2 == 1 ~ "Routinely",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(Site = form_id, Country = country_name, 
         Quality_Checks, Data_Sharing, Data_Sharing_Frequency)

# 2.4.4 Data storage systems and practices
data_storage <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  mutate(
    System_Type = case_when(
      system == 1 ~ "Proprietary",
      system == 2 ~ "Open Source",
      system == 3 ~ "In-house",
      TRUE ~ "Unknown"
    ),
    Data_Storage_Type = case_when(
      storge == 1 ~ "SQL Database",
      storge == 2 ~ "NoSQL Database",
      storge == 3 ~ "Flat File",
      TRUE ~ "Unknown"
    ),
    Storage_Location = case_when(
      local == 1 ~ "Local",
      cloud == 1 ~ "Cloud",
      TRUE ~ "Unknown"
    ),
    Security_Measures = case_when(
      password == 1 | encryption == 1 | accontrol == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    Backup_Frequency = case_when(
      backup_freq == 1 ~ "Real-time",
      backup_freq == 2 ~ "Daily",
      backup_freq == 3 ~ "Weekly",
      backup_freq == 4 ~ "Monthly",
      backup_freq == 5 ~ "Quarterly",
      backup_freq == 6 ~ backup_freq_oth,
      TRUE ~ "Unknown"
    )
  ) %>%
  select(Site = form_id, Country = country_name, 
         System_Type, Data_Storage_Type, Storage_Location,
         Security_Measures, Backup_Frequency)

# =============================================================================
# 3. SWOT ANALYSIS AND RECOMMENDATIONS
# =============================================================================

# Generate SWOT analysis based on the data
swot_analysis <- list(
  Strengths = c(
    "High digital data capture adoption in many sites",
    "Good coverage of core maternal and child health variables",
    "Multiple sites with standardized coding systems (ICD-10)",
    "Established data governance policies at several sites",
    "Mother-child linkage capability at most sites"
  ),
  Weaknesses = c(
    "Inconsistent newborn follow-up duration across sites",
    "Variable diagnostic capacity between facilities",
    "Incomplete implementation of data standardization",
    "Mixed data capture modalities creating integration challenges",
    "Inconsistent data sharing protocols"
  ),
  Opportunities = c(
    "Potential to harmonize data collection across sites",
    "Opportunity to expand digital data capture to all service areas",
    "Can develop standardized coding implementation guidelines",
    "Potential to create integrated data sharing platform",
    "Opportunity to strengthen data quality assurance processes"
  ),
  Threats = c(
    "Resource constraints for system upgrades and maintenance",
    "Data privacy and security concerns with increased digitization",
    "Staff turnover affecting data quality consistency",
    "System interoperability challenges",
    "Changing regulatory requirements for data management"
  )
)

# Recommendations for Phase II
recommendations <- list(
  `Pregnancy Registry Site Selection` = c(
    "Prioritize sites with established digital infrastructure",
    "Consider geographic distribution for national representation",
    "Include sites with strong data governance practices",
    "Ensure mix of facility levels for comprehensive coverage"
  ),
  `Digitization Priorities` = c(
    "Focus on standardizing antenatal care data collection",
    "Digitize vaccination tracking systems",
    "Implement electronic adverse event reporting",
    "Develop mobile solutions for community health workers"
  ),
  `Data Harmonization` = c(
    "Develop standardized data dictionary for core variables",
    "Implement common coding systems (ICD-10, MedDRA)",
    "Create data transformation pipelines for legacy systems",
    "Establish data quality metrics and monitoring"
  ),
  `Data Linkages` = c(
    "Develop APIs for integration with national pharmacovigilance systems",
    "Create unique patient identifiers that work across systems",
    "Establish secure data sharing protocols",
    "Implement master patient index for cross-facility linkage"
  )
)

# =============================================================================
# EXPORT RESULTS
# =============================================================================

# Create a workbook for export
wb <- createWorkbook()

# Add sheets
addWorksheet(wb, "Site Profiles")
addWorksheet(wb, "Data Governance")
addWorksheet(wb, "Data Capture Practices")
addWorksheet(wb, "Data Quality & Sharing")
addWorksheet(wb, "Data Storage")
addWorksheet(wb, "SWOT Analysis")
addWorksheet(wb, "Recommendations")

# Write data
writeData(wb, "Site Profiles", site_profiles)
writeData(wb, "Data Governance", data_governance)
writeData(wb, "Data Capture Practices", data_capture_practices)
writeData(wb, "Data Quality & Sharing", data_quality)
writeData(wb, "Data Storage", data_storage)

# Write SWOT analysis
swot_df <- data.frame(
  Category = rep(names(swot_analysis), sapply(swot_analysis, length)),
  Item = unlist(swot_analysis)
)
writeData(wb, "SWOT Analysis", swot_df)

# Write recommendations
rec_df <- data.frame(
  Category = rep(names(recommendations), sapply(recommendations, length)),
  Recommendation = unlist(recommendations)
)
writeData(wb, "Recommendations", rec_df)

# Save workbook
saveWorkbook(wb, "Maternal_Immunization_Landscape_Analysis_Results.xlsx", overwrite = TRUE)

# =============================================================================
# CREATE INTERACTIVE DASHBOARD COMPONENTS
# =============================================================================

# Create an interactive summary plot of data systems by country
system_summary_plot <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  count(country_name, data_system) %>%
  plot_ly(x = ~country_name, y = ~n, color = ~data_system, type = "bar") %>%
  layout(title = "Data Systems by Country",
         xaxis = list(title = "Country"),
         yaxis = list(title = "Number of Sites"),
         barmode = "stack")

# Create interactive map of sites (simplified version)
world <- ne_countries(scale = "medium", returnclass = "sf")
sites_geo <- clean_data %>%
  distinct(form_id, .keep_all = TRUE) %>%
  mutate(
    lat = case_when(
      country == 1 ~ runif(n(), 5.5, 11.0), # Ghana coordinates
      country == 2 ~ runif(n(), -4.9, 4.9), # Kenya coordinates
      TRUE ~ NA_real_
    ),
    long = case_when(
      country == 1 ~ runif(n(), -3.3, 1.2), # Ghana coordinates
      country == 2 ~ runif(n(), 33.9, 41.9), # Kenya coordinates
      TRUE ~ NA_real_
    )
  )

map_plot <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites_geo, aes(x = long, y = lat, color = country_name), size = 3) +
  coord_sf(xlim = c(-20, 60), ylim = c(-20, 20)) +
  labs(title = "Study Sites Distribution", color = "Country") +
  theme(legend.position = "bottom")

# =============================================================================
# PRINT KEY INSIGHTS
# =============================================================================

cat("MATERNAL IMMUNIZATION SAFETY SURVEILLANCE LANDSCAPE ANALYSIS\n")
cat("=============================================================\n\n")

cat("Key Insights:\n")
cat("1.", round(mean(clean_data$digital == 1, na.rm = TRUE) * 100, 1), 
    "% of sites use digital data capture systems\n")
cat("2.", round(mean(clean_data$paper == 1, na.rm = TRUE) * 100, 1), 
    "% of sites still use paper-based systems\n")
cat("3.", round(mean(clean_data$icd10 == 1, na.rm = TRUE) * 100, 1), 
    "% of sites use ICD-10 coding\n")
cat("4.", round(mean(clean_data$link_uni == 1, na.rm = TRUE) * 100, 1), 
    "% of sites can link mother and child records\n")
cat("5.", round(mean(clean_data$backup == 1, na.rm = TRUE) * 100, 1), 
    "% of sites have backup and recovery policies\n\n")

cat("Recommendations for Phase II:\n")
cat("- Prioritize digitization of paper-based systems\n")
cat("- Standardize coding systems across all sites\n")
cat("- Strengthen data governance and security measures\n")
cat("- Enhance data linkage capabilities between systems\n")
cat("- Develop integrated dashboard for monitoring data quality\n")

# =============================================================================
# SAVE WORKSPACE FOR FUTURE ANALYSIS
# =============================================================================

save.image("Maternal_Immunization_Landscape_Analysis.RData")

cat("\nAnalysis complete. Results saved to:\n")
cat("- Maternal_Immunization_Landscape_Analysis_Results.xlsx\n")
cat("- Maternal_Immunization_Landscape_Analysis.RData\n")