rm(list = ls())
library(DSSAT)
library(lubridate)
library(tidyverse)
library(parallel)
library(doParallel)
library(foreach)
source('./functions/run_dssat.R')
source("./functions/ChangeSOL.R")
options(DSSAT.CSM = "C:/DSSAT48/DSCSM048.EXE")
# GridMatch <- read.csv(file = "D:/315Lab/DATA/TReAD/SOL_TReAD_grid.csv",
#                       fileEncoding = 'big5') %>% tibble()
GridMatch <- read.csv(file = "./WTHGEN/SOL_gridMatch.csv") %>% tibble()
GridMatch$number <- sprintf("%02d",GridMatch$number)

DSSAT_Path <- "./DSSAT48/"

# Parallel Processing
cl = makeCluster(8)
doParallel::registerDoParallel(cl)

clusterEvalQ(cl, {
  library(tidyverse)
  library(parallel)
  library(doParallel)
  library(foreach)
  source('./functions/run_dssat.R')
  source("./functions/ChangeSOL.R")
  
  GridMatch <- read.csv(file = "./WTHGEN/SOL_gridMatch.csv") %>% tibble()
  GridMatch$number <- sprintf("%02d",GridMatch$number)
  
  simulated_planting_date <- c()
  simulated_planting_date$Rice = "02-15"
  simulated_planting_date$Wheat = "09-15"
  simulated_planting_date$Maize = "09-15"
  simulated_planting_date$Soybean = "08-15"
  simulated_planting_date$Taro = "01-15"
  simulated_planting_date$Tomato = "07-15"
  simulated_planting_date$Cabbage = "07-05"
  simulated_planting_date$Sugarcane = "11-05"
  
  cde <- c()
  cde$Rice = "RI"; cde$Wheat = "WH"; cde$Maize = "MZ"; cde$Soybean = "SB"; cde$Taro = "TR"; cde$Cabbage = "CB"; cde$Sugarcane = "SC"; cde$Tomato = "TM";
  
  # When to simulate
  simulated_year = 1980:1981
  
  # Where to simulate
  simulated_grids_index = 1:5
  
  print("pass")
})

# Crops to Simulate
crops = c("Rice", "Wheat", "Maize", "Soybean", "Taro", "Cabbage", "Sugarcane", "Tomato")

# crops = c("Rice")
main = foreach::foreach(crop = crops) %dopar%{
  # Which Crops selected
  if(crop != "Sugarcane"){
    FileX_Sample_name <- dir(file.path(DSSAT_Path, crop,"Template"), pattern = paste0("AFTD1901.","*",cde[crop],"X$"), full.names = TRUE)
  }else{
    FileX_Sample_name <- dir(file.path(DSSAT_Path, crop,"Template"), pattern = paste0("AFTD1801.","*",cde[crop],"X$"), full.names = TRUE)
  }
  FileX_sample <- readLines(FileX_Sample_name)
  # Detect fileX names
  filex_rm <- dir(paste0(DSSAT_Path,crop), pattern = paste0("\\.",cde[crop],"X$"), full.names = TRUE)
  file.remove(filex_rm) # Remove previous fileX
  # Detect OUT names
  out_files_rm <- dir(paste0(DSSAT_Path,crop), pattern = paste0("\\.","OUT$"), full.names = TRUE)
  file.remove(out_files_rm)
  # Detect batch file
  batch_files_rm <- dir(paste0(DSSAT_Path,crop), pattern = "\\.v48", full.names = TRUE)
  file.remove(batch_files_rm)
  for(yrs in c(simulated_year)){
    Planting_date <- paste0(yrs, "-", simulated_planting_date[crop]) # This can change planting date in the file
    for(i in simulated_grids_index){
      sol <- GridMatch$code[i]
      wth <- GridMatch$matched_grid[i]
      year <- str_sub(Planting_date, 3, 4)
      serial <- GridMatch$number[i]

      X <- ChangeSOL(replace = sol, pdate = Planting_date, crop = crop)

      FileName <- paste0(wth, year, serial)

      write.table(X, file.path(DSSAT_Path, crop, paste0(FileName,".", cde[crop],"X")), quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    runDSSAT(paste0(DSSAT_Path,crop,"/"))
    # Detect fileX names
    rix_files_rm <- dir(paste0(DSSAT_Path,crop), pattern = paste0("\\.",cde[crop],"X$"), full.names = TRUE)
    # Remove previous fileX
    file.remove(rix_files_rm)

    # Create folders for containing OUT files
    if(!file.exists(paste0(DSSAT_Path,'out/'))){
      dir.create(paste0(DSSAT_Path,'out/'))
    }
    if(!file.exists(paste0(DSSAT_Path,'out/',substr(Planting_date, 1,4)))){
      dir.create(paste0(DSSAT_Path,'out/',substr(Planting_date, 1,4)))
    }
    if(!file.exists(paste0(DSSAT_Path,'out/',substr(Planting_date, 1,4),"/",crop))){
      dir.create(paste0(DSSAT_Path,'out/',substr(Planting_date, 1,4),"/",crop))
    }
    # Detect those OUT files in the previous simulation path
    out_files <- dir(paste0(DSSAT_Path, crop, "/"), pattern = "\\.OUT$")
    # Move to OUT folder
    for(l in seq_along(out_files)){
      file.rename(paste0(DSSAT_Path, crop, "/", out_files[l]),paste0(DSSAT_Path,'out/',substr(Planting_date, 1,4),"/", crop, "/", out_files[l]))
    }
  }
  return(0)
}
stopCluster(cl)

## Check .OUT at out file folder