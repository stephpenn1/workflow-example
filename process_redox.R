## One-off script to work up initial redox data from Roy
##
## Peter Regier edited by Stephanie Pennington 06-10-2024
## 2024-05-10
##
# ########## #
# ########## #
library(janitor)
library(compasstools)

pattern <- "(ERT.*RedoxTEST|TEMPEST.*Redox15)"

datadir <- "/TEMPEST_PNNL_Data/Current_data"
message("set directory")

dtoken <- readRDS("tokenfile.RDS")
message("loaded token file")

# 1. Setup ---------------------------------------------------------------------

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

# 2. Read in data --------------------------------------------------------------


process_dir(datadir = datadir, pattern, read_datalogger_file, dropbox_token = dtoken) %>%
    clean_names() -> redox_raw

redox_raw %>%
    split(grepl("ERT-84", redox_raw$logger)) -> redox_split

redox_split$`FALSE` %>%
    separate(logger, into = c("one", "two", "plot"), sep = "_") %>%
    select(-one, -two) -> df

redox_split$`TRUE` -> df_ert

# 3. Format data ---------------------------------------------------------------

df %>%
    pivot_longer(cols = contains("redox_"), names_to = "sensor", values_to = "redox_mv") %>%
    separate(sensor, into = c("scrap", "ref", "sensor"), sep = "_") %>%
    mutate(sensor = as.numeric(sensor),
           depth_cm = case_when(sensor == "1" | sensor == "5" | sensor == "9" | sensor == "13" | sensor == "17" ~ 5,
                                sensor == "2" | sensor == "6" | sensor == "10" | sensor == "14" | sensor == "18" ~ 15,
                                sensor == "3" | sensor == "7" | sensor == "11" | sensor == "15" | sensor == "19" ~ 30,
                                sensor == "4" | sensor == "8" | sensor == "12" | sensor == "16" | sensor == "20" ~ 50,
                                .default = 0)) %>%
    select(-c(statname, scrap)) %>%
    ungroup() %>%
    mutate(Timestamp = lubridate::as_datetime(timestamp, tz = "EST"),
           plot = case_match(plot, "control" ~ "Control",
                             "fresh" ~ "Freshwater",
                             "salt" ~ "Saltwater")) %>%
    group_by(Timestamp, plot, depth_cm, ref) %>%
    summarize(mean_redox = mean(redox_mv, na.rm = T)) %>%
    rename(Plot = plot, Depth_cm = depth_cm, Redox = mean_redox, Ref = ref) %>%
    #drop empty sensors
    filter(Depth_cm != 0) -> df1

df_ert %>%
    pivot_longer(cols = contains("redox_"), names_to = "sensor", values_to = "redox_mv") %>%
    separate(sensor, into = c("scrap", "ref", "sensor"), sep = "_") %>%
    mutate(sensor = as.numeric(sensor),
           depth_cm = case_when(sensor <= 16 ~ 35,
                                sensor >= 17 & sensor <= 32 ~ 25,
                                sensor >= 33 & sensor <= 48 ~ 15,
                                sensor >= 49 ~ 5,
                                .default = NA),
           plot = case_when(ref == "ra" | ref == "rb" ~ "ERT - Freshwater",
                            ref == "rc" | ref == "rd" ~ "ERT - Saltwater"),
           .default = "ERT") %>%
    select(-c(statname, scrap)) %>%
    ungroup() %>%
    mutate(Timestamp = lubridate::as_datetime(timestamp, tz = "EST")) %>%
    group_by(Timestamp, plot, depth_cm, ref) %>%
    summarize(mean_redox = mean(redox_mv, na.rm = T)) %>%
    rename(Plot = plot, Depth_cm = depth_cm, Redox = mean_redox, Ref = ref) %>%
    select(Timestamp, Plot, Depth_cm, Ref, Redox) %>%
    bind_rows(df1) %>%
    write_csv("redox.csv")

