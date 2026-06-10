# Handle the reading and initial process of TEROS data from Dropbox

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(compasstools)

# This only needs to be done once
teros_inventory <- read_csv("inventories/TEROS_Network_Location copy.csv", col_types = "cccccddccccd")

datadir <- "/TEMPEST_PNNL_Data/Current_data"
message("set directory")

dtoken <- readRDS("tokenfile.RDS")
message("loaded token file")

process_dir <- function(datadir, pattern, read_function,
                        dropbox_token = NULL,
                        progress_bar = NULL, ...) {

    local <- is.null(dropbox_token)

    # Get our file list, either locally or in Dropbox
    if(local) {
        s_files <- list.files(datadir, pattern = pattern, full.names = TRUE)
    } else {
        # We don't want users to need rdrop2 to use this package (i.e. we don't
        # want to put it in DESCRIPTION's Imports:), so check for availability
        if(requireNamespace("rdrop2refreshtoken", quietly = TRUE)) {
            # Generate list of 'current' (based on token) files
            s_dir <- rdrop2refreshtoken::drop_dir(datadir, dtoken = dropbox_token)
            s_files <- grep(s_dir$path_display, pattern = pattern, value = TRUE)
        } else {
            stop("rdrop2 package is not available")
        }
    }

    # Function called by lapply below; handles progress bar and calls file reader
    f <- function(filename, read_function, token, total_files) {
        if(!is.null(progress_bar)) progress_bar(1 / total_files)
        # Read file, either locally or from Dropbox
        if(local) {
            read_function(filename, ...)
        } else {
            read_file_dropbox(filename, dropbox_token, read_function, ...)
        }
    }
    x <- lapply(s_files, f, read_function, dropbox_token, length(s_files))
    bind_rows(x)
}

teros_primitive <- compasstools::process_teros_dir(datadir = datadir, tz = "EST", dropbox_token = dtoken)

teros_primitive %>%
    left_join(teros_inventory,
              by = c("Logger" = "Data Logger ID",
                     "Data_Table_ID" = "Terosdata table channel")) %>%
    select(- `Date of Last Field Check`) %>%
    rename("Active_Date" = "Date Online (2020)",
           "Grid_Square" = "Grid Square") %>%
    filter(!is.na(ID)) ->
    teros

nomatch <- anti_join(teros, teros_inventory,
                     by = c("Logger" = "Data Logger ID",
                            "Data_Table_ID" = "Terosdata table channel"))
if(nrow(nomatch) > 0) {
    warning("There were logger/channel combinations that I couldn't find in teros_inventory.csv:")
}

# Cut the memory footprint of the TEROS data by almost half and return
teros %>%
    mutate(Depth = as.factor(Depth),
           ID = as.character(ID),
           Logger = as.character(Logger)) %>%
    select(Timestamp, Plot, ID, Grid_Square, Logger, variable, Depth, value) -> t

process_dir(datadir = datadir, dropbox_token = dtoken, pattern = "84_Teros21", read_datalogger_file) %>%
    pivot_longer(starts_with("Teros"), names_to = "channel") %>%
    filter(!is.na(value)) %>%
    rename(Timestamp = TIMESTAMP) %>%
    mutate(Timestamp = ymd_hms(Timestamp, tz = "EST")) %>%
    separate(channel, into = c("Data_Table_ID", "variable"), sep = ",") %>%
    mutate(Data_Table_ID = as.integer(gsub("Teros21(", "", Data_Table_ID, fixed = TRUE)),
           variable = as.integer(gsub(")", "", variable, fixed = TRUE)),
           variable = case_when(variable ==  1 ~ "MP",
                                variable == 2 ~ "TSOIL"),
           Plot = case_when(Data_Table_ID >= 1 & Data_Table_ID <= 8 ~ "Freshwater",
                            Data_Table_ID >= 9 & Data_Table_ID <= 16 ~ "Saltwater",
                            .default = NA),
           Depth = case_when(Data_Table_ID %in% c(1, 5, 9, 13) ~ 10,
                             Data_Table_ID %in% c(2, 6, 10, 14) ~ 30,
                             Data_Table_ID %in% c(3, 7, 11, 15) ~ 50,
                             Data_Table_ID %in% c(4, 8, 12, 16) ~ 70,
                             .default = NA),
           Grid_Square = NA)  %>%
    filter(variable != 3) %>%
    rename(ID = Data_Table_ID) %>%
    mutate(ID = paste0("Teros21-", ID),
           Depth = as.factor(Depth)) %>%
    select(Timestamp, Plot, ID, Grid_Square, Logger, variable, Depth, value) -> ert_21

process_dir(datadir = datadir, dropbox_token = dtoken, pattern = "84_Teros12", read_datalogger_file) %>%
    pivot_longer(starts_with("Teros"), names_to = "channel") %>%
    filter(!is.na(value)) %>%
    rename(Timestamp = TIMESTAMP) %>%
    mutate(Timestamp = ymd_hms(Timestamp, tz = "EST")) %>%
    separate(channel, into = c("Data_Table_ID", "variable"), sep = ",") %>%
    mutate(Data_Table_ID = as.integer(gsub("Teros12(", "", Data_Table_ID, fixed = TRUE)),
           variable = as.integer(gsub(")", "", variable, fixed = TRUE)),
           variable = case_when(variable ==  1 ~ "VWC",
                                variable == 2 ~ "TSOIL",
                                variable == 3 ~ "EC"),
           Plot = case_when(Data_Table_ID >= 1 & Data_Table_ID <= 8 ~ "Freshwater",
                            Data_Table_ID >= 9 & Data_Table_ID <= 16 ~ "Saltwater",
                            .default = NA),
           Depth = case_when(Data_Table_ID %in% c(1, 5, 9, 13) ~ 10,
                             Data_Table_ID %in% c(2, 6, 10, 14) ~ 30,
                             Data_Table_ID %in% c(3, 7, 11, 15) ~ 50,
                             Data_Table_ID %in% c(4, 8, 12, 16) ~ 70,
                             .default = NA),
           Grid_Square = NA)  %>%
    rename(ID = Data_Table_ID) %>%
    mutate(ID = paste0("Teros12-", ID),
           Logger = as.character(Logger),
           Depth = as.factor(Depth)) %>%
    select(Timestamp, Plot, ID, Grid_Square, Logger, variable, Depth, value) %>%
    bind_rows(t, ert_21) %>%
    readr::write_csv("teros.csv")

