# A script to scrape the crime log for incidents at UNC Medical Center.

#load libraries
library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)
library(readr)
library(hms)
library(digest)

# save the json response url
url <- 'https://crimelog.unch.unc.edu/home/datatable'

#store a dataframe for IBR status defined on crime log home page
#https://www.uncmedicalcenter.org/uncmc/patients-visitors/hospital-police-department/crime-log/
ibr_status <- tibble(
  status = c('5', 'A', 'CA', 'CE', 'CO', 'I', 'L', 'U'),
  status_detail = c('Open (Office Investigation)', 'Active', 'Closed by Arrest', 'Closed by Exception', 
                  'Closed by Other Means', 'Inactive', 'Located (Missing Persons and Runaways only)', 'Unfounded') 
)

#store the current crime log
cat('...downloading latest crime log...\n')
current_crime_log <- fromJSON(url)$data %>% 
  as_tibble() %>%
  mutate(date = date(date),
         time = as_hms(ymd_hms(time))) %>% 
  left_join(ibr_status, by = 'status') %>% 
  mutate(first_visible = now(tzone = 'US/Eastern'),
         last_visible = now(tzone = 'US/Eastern'),
         last_updated = now(tzone = 'US/Eastern')) %>% 
  unite('hash_input', number:status_detail, na.rm = TRUE, remove = FALSE) %>% 
  rowwise() %>% 
  mutate(hash = digest(hash_input), .after = everything()) %>%
  ungroup() %>% 
  select(-hash_input)

#load previous crimelog
cat('...loading previously saved crime log...\n')
saved_crime_log <- read_csv('data/uncmc_crime_log.csv', col_types = 'cDtccccccTc')

#get all new entries
cat('...filtering for new crime log entries...\n')
new_entries <- current_crime_log %>%
  #add_row(number = '9999999', description = 'THIS IS A NEW ENTRY', hash = '999') %>% #for testing
  anti_join(saved_crime_log, by = 'number') %>% 
  mutate(first_visible = now(tzone = 'US/Eastern'))

#get all updated entries, according to the hash
cat('...updating altered crime log entries...\n')
changed_hashes <- current_crime_log %>%
  #add_row(number = '202310020', description = 'THIS IS AN UPDATED ENTRY', hash = '9191919') %>%#for testing
  anti_join(saved_crime_log, by = c('number', 'hash'))

#combine saved, new and updated entries
cat('...combining crime log entries...\n')
updated_crime_log <- saved_crime_log %>% 
  filter(!number %in% (changed_hashes %>% pull(number)) ) %>% 
  rbind(new_entries) %>%
  rbind(changed_hashes)

#save the crime log file
cat('...saving file...\n')
updated_crime_log %>%
  write_csv('data/uncmc_crime_log.csv')


