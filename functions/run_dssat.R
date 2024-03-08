batch_cmd <- c()
batch_cmd$Rice = 'C:/DSSAT48/DSCSM048.EXE RICER048 B DSSBatch.v48'
batch_cmd$Maize = 'C:/DSSAT48/DSCSM048.EXE MZCER048 B DSSBatch.v48'
batch_cmd$Soybean = "C:/DSSAT48/DSCSM048.EXE CRGRO048 B DSSBatch.v48"
batch_cmd$Wheat = "C:/DSSAT48/DSCSM048.EXE CSCER048 B DSSBatch.v48"
batch_cmd$Taro = "C:/DSSAT48/DSCSM048.EXE TRARO048 B DSSBatch.v48"
batch_cmd$Tomato = "C:/DSSAT48/DSCSM048.EXE CRGRO048 B DSSBatch.v48"
batch_cmd$Sugarcane = "C:/DSSAT48/DSCSM048.EXE SCCAN048 B DSSBatch.v48"
batch_cmd$Cabbage = "C:/DSSAT48/DSCSM048.EXE CRGRO048 B DSSBatch.v48"

runDSSAT <- function(path){
  # set working directory
  simulate_path = tools::file_path_as_absolute(path);
  
  # Read File names
  somefiles <- dir(simulate_path, pattern = paste0(".*X$"), full.names = TRUE)
  
  ChangeBatch <- function(paths){
    batch <- readLines(file.path(simulate_path,'Template', 'DSSBatch.v48'))

    lines <- batch[str_detect(batch, paste0("D:/315Lab/Project/P03_ClimateChange/DSSAT48/",crop,"/AFTD1901.", cde[crop],"X"))]
    batch <- batch[!str_detect(batch, paste0("D:/315Lab/Project/P03_ClimateChange/DSSAT48/",crop,"/AFTD1901.", cde[crop],"X"))]
    n_paths <- length(paths)
    for(i in 1:n_paths){
      replaced_line <- str_replace(lines,
                                   str_pad(paste0("D:/315Lab/Project/P03_ClimateChange/DSSAT48/",crop,"/AFTD1901.", cde[crop],"X"), 97, side = "right"),
                                   str_pad(paths[i], 97, "right", " "))
      batch <- c(batch, replaced_line)
    }
    

    return(batch)
  }
  # make batch file
  bch <- ChangeBatch(somefiles)
  write.table(bch, file.path(simulate_path, "DSSBatch.v48"), quote = FALSE, row.names = FALSE, col.names = FALSE)

  # Perform the CMD
  setwd(simulate_path)
  cml = batch_cmd[crop]
  system(paste(cml))
  for(i in 1:2) setwd("../")
}