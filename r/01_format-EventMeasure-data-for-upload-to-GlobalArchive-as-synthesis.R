# Before you use this script please read the below instructions!!
# This workflow assumes all of your metadata is correctly formatted, and has been cleaned by using CheckEM. CheckEM available here: https://marine-ecology.shinyapps.io/CheckEM/
# Please see the CheckEM usermanual for the correct format(https://globalarchivemanual.github.io/CheckEM/articles/manuals/CheckEM_user_guide.html)

# Load libraries -----
library('remotes')
options(timeout=9999999)
# remotes::install_github("GlobalArchiveManual/CheckEM") # Run this if you do not have CheckEM installed.
library(CheckEM)
library(tidyverse)
library(here)

# Set name for synthesis, will be used as prefix for your files to upload
name <- "test-synthesis"

# All data needs to be saved in a folder structure "data/raw/" for the script to work, or change the direcory when reading in files.

# Read in metadata ----
# Note check if you have used opcode, opcode or period and change the below code accordingly.


metadata <- read_metadata(here::here("data/raw/"), method = "BRUVs") %>% # Change here to "DOVs"
  dplyr::select(campaignid, opcode, 
                status, 
                longitude_dd, latitude_dd, 
                observer_count, observer_length,
                date_time, 
                location, site, 
                depth_m, 
                successful_count, 
                successful_length, 
                successful_habitat_forwards, 
                successful_habitat_backwards) %>%
  # rename(opcode = opcode) %>% # use this line if you need to rename opcode to opcode
  glimpse()

unique(metadata$campaignid)

write_csv(metadata, paste0("data/uploads/", name, "_metadata.csv"))

# Read in the maxn and length data ----
maxn <- read_points(here::here("data/raw/")) %>% 
  dplyr::mutate(species = ifelse(species %in% c("sp", "sp1", "sp2"), "spp", as.character(species))) %>%
  dplyr::group_by(campaignid, opcode, filename, periodtime, frame, family, genus, species, stage) %>% # If you have MaxN'd by stage (e.g. Adult, Juvenile) add stage here
  dplyr::mutate(number = as.numeric(number)) %>%
  dplyr::summarise(maxn = sum(number)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(campaignid, opcode, family, genus, species, stage) %>%
  dplyr::slice(which.max(maxn)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(maxn)) %>%
  dplyr::select(-frame) %>%
  tidyr::replace_na(list(maxn = 0)) %>%
  dplyr::mutate(maxn = as.numeric(maxn)) %>%
  dplyr::filter(maxn > 0) %>%
  dplyr::inner_join(metadata, by = join_by(campaignid, opcode)) %>%
  dplyr::filter(successful_count %in% c("Yes")) %>% 
  dplyr::filter(maxn > 0) %>%
  dplyr::select(campaignid, opcode, family, genus, species, maxn, stage) %>%
  dplyr::mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) %>%
  
  {message(paste(length(which(.$family %in% "Unknown")), "rows removed because family is 'Unknown'"));
    .} %>%
  dplyr::filter(!family %in% "Unknown")

length <- read_em_length(here::here("data/raw/")) %>%
  dplyr::mutate(species = ifelse(species %in% c("sp", "sp1", "sp2"), "spp", as.character(species))) %>%
  dplyr::select(-c(comment))%>% # there is a comment column in metadata, so you will need to remove this column from EM data
  dplyr::inner_join(metadata, by = join_by(opcode, campaignid)) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  dplyr::mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species)))  %>%
  
  {message(paste(length(which(.$family %in% "Unknown")), "rows removed because family is 'Unknown'"));
    .} %>%
  dplyr::filter(!family %in% "Unknown")

# Format data for upload to GA and final checks -----
codes <- australia_life_history %>%
  dplyr::select(family, genus, species, caab_code)

count_upload <- maxn %>%
  dplyr::rename(count = maxn) %>%
  left_join(codes)

length_upload <- length %>% # This includes only EM data, not generic length
  left_join(codes) %>%
  dplyr::rename(rms_mm = rms, range_mm = range, precision_mm = precision, count = number)

# Check missing caab codes (okay if from sp, sp1 etc.)
n_no_caab_count <- length(count_upload$species[which(is.na(count_upload$caab_code))])
sp_no_caab_count <- unique(count_upload$species[which(is.na(count_upload$caab_code))])
n_no_caab_length <- length(length_upload$species[which(is.na(length_upload$caab_code))])
sp_no_caab_length <- unique(length_upload$species[which(is.na(length_upload$caab_code))])

# Save GA upload data ----
write_csv(count_upload, paste0("data/uploads/", name, "_count.csv"))
write_csv(length_upload, paste0("data/uploads/", name, "_length.csv"))
