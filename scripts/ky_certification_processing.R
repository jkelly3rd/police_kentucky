# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(peopleparser)

# Clean workspace
rm(list = ls())

# Set working and output directories
root_dir = getwd()

# Identify paths for raw multi-sheet state Excel file to import
persons_file = "data/original/Certified_Employed_POPS_-_All_Cert_Statuses_Employment_History_Training_History.xlsx"

# Identify paths for output CSV files
ky_index = "data/processed/ky-2023-index.csv"
ky_index_enhanced = "data/processed/ky-2023-index-enhanced.csv"
ky_original_persons = "data/processed/ky-2023-original-persons.csv"
ky_original_licenses = "data/processed/ky-2023-original-licenses.csv"

# Create a template dataframe for the officers index. 
# This will be used to ensure that the final dataframe has the correct structure.
template_public <- data.frame("person_nbr" = character(0),
                       "full_name" = character(0),
                       "first_name" = character(0),
                       "middle_name" = character(0),
                       "last_name" = character(0),
                       "suffix" = character(0),
                       "year_of_birth" = numeric(0),
                       "age" = numeric(0),
                       "agency" = character(0),
                       "type" = character(0),
                       "rank" = character(0),
                       "start_date" = as.Date(character(0)),
                       "end_date" = as.Date(character(0))
                       )

# Import Excel file for officers provided by state of Kentucky
# Date columns with hard NULL value will convert to NA
ky_officers <- read_excel(persons_file,
                          sheet = 2,
                          col_types = c("text", "text", "text",
                                        "text", "text", "text",
                                        "text", "text", "text",
                                        "date", "date"))
# Import Excel file for certificates provided by state of Kentucky
# Date columns with hard NULL value will convert to NA
ky_licenses <- read_excel(persons_file,
                          sheet = 1,
                          col_types = c("text", "text", "text",
                                        "text", "text", "text",
                                        "date", "text", "date",
                                        "text", "text", "date",
                                        "text", "text", "date",
                                        "text"))
# Note: This import fails to process some dates; all appear to be NULL values

# Initial rename of columns to be more consistent format with eventual work history index
# We are using state field appointment in rank column in index and license in type column in index
colnames(ky_licenses) <- c("full_name","gender","academy_id",
                           "pops_number","year_of_birth","agency",
                           "start_date","pops_certification","pops_cert_issued","pops_cert_status",
                           "tps_certification","tps_cert_issued","tps_cert_status",
                           "cso_certification","cso_cert_issued","cso_cert_status")
colnames(ky_officers) <- c("full_name","gender","certification",
                           "certification_status","academy_id","pops_number",
                           "year_of_birth","agency","title",
                           "start_date","end_date")

# Function to convert all character columns to upper case
convert_to_upper <- function(df) {
  df %>%
    mutate(across(where(is.character), str_to_upper))
}

# Apply the function to both data frames
ky_officers <- convert_to_upper(ky_officers)
ky_licenses <- convert_to_upper(ky_licenses)

# Output processed original files as CSV files
ky_officers %>% write_csv(ky_original_persons)
ky_licenses %>% write_csv(ky_original_licenses)


# PREPARE INDEX FILE
# We will treat Kentucky certification as type
# We will treat Kentucky title as rank
# We will use Kentucky's academy_id as the person_nbr
# Rename those three columns
ky_officers <- ky_officers %>% rename("person_nbr" = academy_id)
ky_officers <- ky_officers %>% rename("type" = certification)
ky_officers <- ky_officers %>% rename("rank" = title)
ky_licenses <- ky_licenses %>% rename("person_nbr" = academy_id)

## NAME CLEAN UP
# Function to parse full name using peopleparser parse.names function
ky_officers <- ky_officers %>% mutate(parsed_name = parse.names(full_name))
# Rename name fields
ky_officers <- ky_officers %>% mutate(first_name = parsed_name$first_name,
                                      middle_name = parsed_name$middle_name,
                                      last_name = parsed_name$last_name,
                                      suffix = parsed_name$suffix)
# drop all other fields with parsed_name
ky_officers <- ky_officers %>% select(-parsed_name)
# create a new full_name field that is the concatenation of first_name, middle_name, last_name, and suffix
ky_officers <- ky_officers %>% mutate(full_name = paste(first_name, middle_name, last_name, suffix, sep = " "))
# remove all double spaces
ky_officers <- ky_officers %>% mutate(full_name = gsub("  ", " ", full_name))
# change year of birth to numeric
ky_officers <- ky_officers %>% mutate(year_of_birth = as.numeric(year_of_birth))


# Now merge the cleaned Kentucky data into the work history index file
ky_officers_public <- bind_rows(template_public,ky_officers)

# Export csv of work history index for project
ky_officers_public %>% write_csv(ky_index_enhanced)

# Remove extra columns and export csv of standard work history index for project
ky_officers <- ky_officers_public %>% select(person_nbr, full_name, first_name, middle_name, last_name, suffix, year_of_birth, age, agency, type, rank, start_date, end_date)

# Export csv of standard work history index for project
ky_officers %>% write_csv(ky_index)





