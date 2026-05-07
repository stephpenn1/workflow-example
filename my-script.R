# Example script
library(rdrop2refreshtoken)

message("Hello from R!")

writeLines(as.character(Sys.time()), "my-data.txt")

if(file.exists("tokenfile.RDS")) {
    message("token file exists!")
} else {
    stop("no token file :(")
}

rdrop2refreshtoken::drop_auth(new_user = FALSE, rdstoken = "tokenfile.RDS")

print(rdrop2refreshtoken::drop_dir())
