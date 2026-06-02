# Example script
library(rdrop2refreshtoken)
library(compasstools)

datadir <- ""

message("Hello from R!")

writeLines(as.character(Sys.time()), "my-data.txt")

if(file.exists("tokenfile.RDS")) {
    message("token file exists!")
} else {
    stop("no token file :(")
}

rdrop2refreshtoken::drop_auth(new_user = FALSE, rdstoken = "tokenfile.RDS") -> token

print(rdrop2refreshtoken::drop_dir())


# sf_raw <- compasstools::process_sapflow_dir(datadir, tz = "EST",
#                                             token, progress)
#
# sf_raw %>%
#     left_join(sf_inventory, by = c("Logger", "Port")) %>%
#     filter(!is.na(Tree_Code)) %>% # remove ports that don't have any sensors
#     select(Plot, Timestamp, Record, BattV_Avg, Port, Value, Logger,
#            Sapflow_ID = Tree_Code, Grid_Square, Out_Of_Plot, Species, Installation_Date) %>%
#     mutate(Deep_Sensor = grepl("D", Sapflow_ID),
#            Grid_Letter = substring(Grid_Square, 1, 1),
#            Grid_Number = substring(Grid_Square, 2, 2)) %>%
#     # TEMPORARY HACK -- JUNE 2024 -- REMOVE OBSOLETE SENSOR CODES
#     filter(!Sapflow_ID %in% c("SD2", "CD6", "SD9", "CD10", "CD3", "SD14")) ->
#     sapflow
#
# nomatch_ports <- anti_join(sf_raw, sf_inventory, by = c("Logger", "Port"))
#
# if(nrow(nomatch_ports) > 0) {
#     warning("There were logger/port combinations that I couldn't find in sapflow_inventory.csv:")
# }
#
# # Cut the memory footprint of the sapflow data by almost half and return
# select(sapflow, Plot, Timestamp, Value, Sapflow_ID, Logger, BattV_Avg, Out_Of_Plot, Species, Grid_Square) %>%
#     writeLines("sapflow.csv")

