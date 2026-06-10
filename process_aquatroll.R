# Handle the reading and initial process of Aquatroll data from Dropbox

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)

# This only needs to be done once
troll_inventory <- read_csv("inventories/aquatroll_inventory copy.csv", col_types = "ccccddcccdddddd")

datadir <- "/TEMPEST_PNNL_Data/Current_data"
message("set directory")

dtoken <- readRDS("tokenfile.RDS")
message("loaded token file")

change_date <- "2021-03-10 00:00:00"

change_IDs <- c("PNNL_13", "PNNL_23", "PNNL_32")
change_instrument <- "TROLL600"

atroll <- compasstools::process_aquatroll_dir(datadir = datadir, "EST", dropbox_token = dtoken)

print(atroll)

atroll %>%
    mutate(Install = if_else(Timestamp >= change_date &
                                 Logger_ID %in% change_IDs &
                                 Instrument == change_instrument, 2, 1)) %>%
    left_join(troll_inventory, by = c("Logger_ID", "Instrument", "Install")) %>%
    readr::write_csv("aquatroll.csv")

