rm(list = ls())
library(tidyverse)

# Read files saved by Save_out_to_csv.R
risk_profiles <- dir(path = "D:/315Lab/DATA/DSSAT48/out", pattern = "*Risk", full.names = TRUE)
for(file in risk_profiles){
  file.name <- sapply(regmatches(file, regexec("out/(.*?)\\_Risk.csv", file)), `[`, 2)
  assign(file.name, read.csv(file))
  rm(list = c("file.name", "file"))
}

## Function: Check the empty value of files
find.na <- function(df){
  df.name <- deparse(substitute(df))
  filtered.df <- df[apply(df, 1, function(row) any(is.na(row))),]
  cat("Abnormal track:", nrow(filtered.df), "in", df.name, "\n")
  return(filtered.df)
}

# Take the object name of data frames
read.crops <- sapply(regmatches(risk_profiles ,regexec("out/(.*?)\\_Risk.csv", risk_profiles)), `[`, 2)
# Check abnormal samples
for(crop in read.crops) eval(parse(text = paste("find.na(", crop, ")", sep = "")))

### Combine all risk from each crop
# Generate a initial full.risk data frame
eval(parse(text = paste("assign(\"full.risk\", ",read.crops[1],")", sep = "")))
full.risk$crop = read.crops[1]
# Combine sequentially and add a new column crop.
for(crop in read.crops[2:8]){
  eval(parse(text = paste("assign(\"bind.sheet\",", crop, ")")))
  eval(parse(text = paste("bind.sheet$crop = ","\"", crop,"\"", sep = "")))
  eval(parse(text = paste("assign(\"full.risk\", rbind(full.risk, bind.sheet))", sep = ""))) 
  rm(list = "bind.sheet")
}
# Save file to rds file format
saveRDS(full.risk, file = "./data/experiment_results.rds")