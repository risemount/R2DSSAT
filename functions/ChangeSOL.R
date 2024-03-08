# Soil ID Change
ChangeSOL <- function(soil_id = 'DH03368054', replace, pdate = '2019-02-15', crop){
  change_to_year <- str_sub(pdate,1,4)
  DSSAT_Path = tools::file_path_as_absolute(DSSAT_Path);
  # Default soil_id, year, wth is based on following file
  # Based on a template FileX
  if(crop != "Sugarcane"){
    FileX <- readLines(paste0(DSSAT_Path,"/", crop, "/Template/AFTD1901.",cde[crop],"X"))  
  }else{
    FileX <- readLines(paste0(DSSAT_Path,"/", crop, "/Template/AFTD1801.",cde[crop],"X"))
  }
  
  change_words <- function(x, pattern, replace){
    # find
    lines <- x[str_detect(x, pattern)]
    
    # sample size of selected lines
    n_change <- length(lines)
    
    lines_changed <- NULL # This will replace the original lines
    for(i in 1:n_change){
      lines_changed <- c(lines_changed, str_replace(lines[i], pattern, replace))
    }
    # Replace original lines
    x[str_detect(x, pattern)] <- lines_changed
    
    return(x)
  }
  # Prepare for replacing WTH
  rawWTH <- GridMatch %>% filter(code == soil_id) %>% pull(matched_grid)
  rawSerialNum <- GridMatch %>% filter(code == soil_id) %>% pull(number)
  if(crop != "Sugarcane"){
    rawWTH_code <- paste0(rawWTH, 19, rawSerialNum)
  }else{
    rawWTH_code <- paste0(rawWTH, 18, rawSerialNum)
  }
  
  rawFieldID <- paste0(rawWTH, str_pad((as.numeric(rawSerialNum)-1), 4, pad = '0'))
  if(crop != "Sugarcane"){
    rawPDATE <- "19046"
  }else{
    rawPDATE <- "18046"
  }
  
  WTH <- GridMatch %>% filter(code == replace) %>% pull(matched_grid) # WTH code
  SerialNum <- GridMatch %>% filter(code == replace) %>% pull(number) # serial number
  WTH_code <- paste0(WTH, str_sub(change_to_year, 3, 4), '01') # Replace WTH
  FieldID <- paste0(WTH, str_pad((as.numeric(SerialNum)-1), 4, pad = '0')) # Replace Field ID
  PDATE <- format(as.POSIXct(pdate, tz = 'UTC'), format = "%y%j")
  
  FileX <- change_words(FileX, soil_id, replace)
  FileX <- change_words(FileX, rawWTH_code, WTH_code)
  FileX <- change_words(FileX, rawFieldID, FieldID)
  FileX <- change_words(FileX, rawPDATE, PDATE)
  
  return(FileX)
}
