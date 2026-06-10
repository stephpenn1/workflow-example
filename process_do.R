# Script to process DO sensors

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)

pattern <- "Pyro\\.dat$"

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

process_dir(datadir = datadir, pattern, read_datalogger_file, dropbox_token = dtoken) %>%
    pivot_longer(ch1_Status:ch4_PerO2, names_to = c("Channel", "Variable"), names_sep = "_", values_to = "Value") %>%
    separate(Logger, into = c("one", "Logger")) %>%
    mutate(Timestamp = ymd_hms(TIMESTAMP, tz = "EST"),
           Plot = case_when(Logger == "12" ~ "Control",
                            Logger == "21" ~ "Freshwater",
                            Logger == "33" ~ "Saltwater",
                            .default = Logger),
           Depth_cm = case_when(Channel == "ch1" ~ "5",
                                Channel == "ch2" ~ "15",
                                Channel == "ch3" ~ "30",
                                Channel == "ch4" ~ "50",
                                .default = Channel)) %>%
    filter(Variable %in% c("PerAirSat", "Temp")) %>%
    select(Logger, Plot, Timestamp, Variable, Value, Depth_cm) %>%
    filter(Logger != 13) %>%
    write_csv("do.csv")

