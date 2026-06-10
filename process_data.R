library(readr)
library(dplyr)
library(tidyr)
library(compasstools)
library(rdrop2refreshtoken)
library(rdrop2)

datadir <- "/TEMPEST_PNNL_Data/Current_data"
dtoken <- readRDS("tokenfile.RDS")

## ------------ SAPFLOW ------------

sf_inventory <- read_csv("inventories/sapflow_inventory copy.csv", col_types = "ccdcdddclc")

print(sf_inventory)

sf_raw <- compasstools::process_sapflow_dir(datadir, tz = "EST", dropbox_token = dtoken)

print(sf_raw)

sf_raw %>%
    left_join(sf_inventory, by = c("Logger", "Port")) %>%
    filter(!is.na(Tree_Code)) %>% # remove ports that don't have any sensors
    select(Plot, Timestamp, Record, BattV_Avg, Port, Value, Logger,
           Sapflow_ID = Tree_Code, Grid_Square, Out_Of_Plot, Species, Installation_Date) %>%
    mutate(Deep_Sensor = grepl("D", Sapflow_ID),
           Grid_Letter = substring(Grid_Square, 1, 1),
           Grid_Number = substring(Grid_Square, 2, 2)) %>%
    # TEMPORARY HACK -- JUNE 2024 -- REMOVE OBSOLETE SENSOR CODES
    filter(!Sapflow_ID %in% c("SD2", "CD6", "SD9", "CD10", "CD3", "SD14")) ->
    sapflow

nomatch_ports <- anti_join(sf_raw, sf_inventory, by = c("Logger", "Port"))

if(nrow(nomatch_ports) > 0) {
    warning("There were logger/port combinations that I couldn't find in sapflow_inventory.csv:")
}

# Cut the memory footprint of the sapflow data by almost half and return
select(sapflow, Plot, Timestamp, Value, Sapflow_ID, Logger, BattV_Avg, Out_Of_Plot, Species, Grid_Square) %>%
    readr::write_csv("sapflow.csv")

## ------------ TEROS ------------

teros_primitive <- compasstools::process_teros_dir(datadir, tz = "EST",
                                                   dropbox_token = token)

teros_primitive %>%
    left_join(teros_inventory,
              by = c("Logger" = "Data Logger ID",
                     "Data_Table_ID" = "Terosdata table channel")) %>%
    select(- `Date of Last Field Check`) %>%
    rename("Active_Date" = "Date Online (2020)",
           "Grid_Square" = "Grid Square") %>%
    mutate(Depth = as.factor(Depth)) %>%
    filter(!is.na(ID)) ->
    teros

nomatch <- anti_join(teros, teros_inventory,
                     by = c("Logger" = "Data Logger ID",
                            "Data_Table_ID" = "Terosdata table channel"))
if(nrow(nomatch) > 0) {
    warning("There were logger/channel combinations that I couldn't find in teros_inventory.csv:")
}

# Cut the memory footprint of the TEROS data by almost half and return
select(teros, Timestamp, Plot, ID, Grid_Square, Logger, variable, Depth, value) %>%
    readr:write_csv("teros.csv")

## ------------ TROLL ------------

change_date <- "2021-03-10 00:00:00"

change_IDs <- c("PNNL_13", "PNNL_23", "PNNL_32")
change_instrument <- "TROLL600"

atroll <- compasstools::process_aquatroll_dir(datadir, "EST",
                                              dropbox_token = token)

atroll %>%
    mutate(Install = if_else(Timestamp >= change_date &
                                 Logger_ID %in% change_IDs &
                                 Instrument == change_instrument, 2, 1)) %>%
    left_join(troll_inventory, by = c("Logger_ID", "Instrument", "Install")) %>%
    readr:write_csv("troll.csv")

# ## ------------ REDOX ------------

pattern <- "Redox15\\.dat$"

process_dir(datadir, pattern, read_datalogger_file, dropbox_token = token) %>%
    clean_names() %>%
    separate(logger, into = c("one", "two", "plot"), sep = "_") %>%
    select(-one, -two) -> df

# 3. Format data ---------------------------------------------------------------

set_depths <- function(data){
    data %>%
        pivot_longer(cols = contains("redox_"), names_to = "sensor", values_to = "redox_mv") %>%
        separate(sensor, into = c("scrap", "ref", "sensor"), sep = "_") %>%
        mutate(depth_cm = case_when(sensor == "1" | sensor == "5" | sensor == "9" | sensor == "13" | sensor == "17" ~ 5,
                                    sensor == "2" | sensor == "6" | sensor == "10" | sensor == "14" | sensor == "18" ~ 15,
                                    sensor == "3" | sensor == "7" | sensor == "11" | sensor == "15" | sensor == "19" ~ 30,
                                    sensor == "4" | sensor == "8" | sensor == "12" | sensor == "16" | sensor == "20" ~ 50,
                                    TRUE ~ 0)) %>%
        select(-c(statname, scrap))
}

df_raw <- set_depths(df)

df_raw %>%
    ungroup() %>%
    mutate(Timestamp = lubridate::as_datetime(timestamp, tz = "EST"),
           plot = case_match(plot, "control" ~ "Control",
                             "fresh" ~ "Freshwater",
                             "salt" ~ "Saltwater")) %>%
    group_by(Timestamp, plot, depth_cm, ref) %>%
    summarize(mean_redox = mean(redox_mv, na.rm = T)) %>%
    rename(Plot = plot, Depth_cm = depth_cm, Redox = mean_redox, Ref = ref) %>%
    readr:write_csv("redox.csv")
