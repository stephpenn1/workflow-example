# Example script

message("Hello from R!")

writeLines(as.character(Sys.time()), "my-data.txt")

if(file.exists("tokenfile.RDS")) {
    message("token file exists!")
} else {
    stop("no token file :(")
}

token <- readRDS("tokenfile.RDS")

print(token)
