rm(list = ls())
library(tidyverse)
library(DSSAT)
options(DSSAT.CSM = "C:/DSSAT48/DSCSM048.EXE")
GridMatch <- read.csv(file = "D:/315Lab/DATA/TReAD/SOL_gridMatch.csv",
                      fileEncoding = 'big5') %>% tibble()
# Polarization function
polar <- function(df = NULL, x, y, k = 1){
  # x: yield of full irrigated
  # y: yield of drought condition
  if(is.null(df)){
    r <- sqrt(x^2+y^2)
    theta1 <- ifelse(x>0, (1-((atan(y/x)/pi)*180)/45), 1)
    theta <- ifelse(theta1<0,0,theta1)
    r = abs(r-max(r))/max(r)
    return(1-sqrt((1-k)*(1-r)*k*(1-theta)))  
  }else{
    r <- sqrt(df[,1]^2+df[,2]^2)
    theta1 <- ifelse(df[,1]>0, (1-((atan(df[,2]/df[,1])/pi)*180)/45), 1)
    theta <- ifelse(theta1<0,0,theta1)
    r = abs(r-max(r))/max(r)
    return(1-sqrt((1-r)^(2-k)*(1-theta)^(k))) 
  }
}
GridMatch$number <- sprintf("%02d",GridMatch$number)
DSSAT_Path <- "D:/315Lab/DATA/DSSAT48/"
summary_list = list()
# c("Rice", "Wheat", "Maize", "Soybean", "Taro", "Cabbage", "Sugarcane", "Tomato")
crops = c("Rice", "Wheat", "Maize", "Soybean", "Taro", "Cabbage", "Sugarcane", "Tomato")
for(crop in crops){
  pb = txtProgressBar(min = 1980, max = 2018,
                      title = "year",style = 3)
  for(year in 1980:2018){
    # Read Summary file
    summary_file <- read_output(file.path(DSSAT_Path, "out", year, crop, "Summary.OUT"))
    
    summary_data <- read_output(file.path(DSSAT_Path, "out", year, crop, "Summary.OUT")) %>%
      mutate(SOIL_ID = sapply(strsplit(SOIL_ID, " "), '[', 1)) %>%
      select(SOIL_ID, TRNO, HWAH) %>%
      mutate(TRNO = ifelse(TRNO == 1, "auto", "no")) %>%
      pivot_wider(names_from = "TRNO", values_from = "HWAH")
    summary_data
    
    yield_data <- GridMatch %>%
      rename(SOIL_ID = code) %>%
      left_join(., summary_data, by = 'SOIL_ID') %>%
      mutate(RISK = polar(x = auto, y = no),
             RISK2 = ifelse(auto == 0, 1, ifelse((auto-no)/auto > 0, (auto-no)/auto, 0))
      )
    summary_list[[paste("Y",year,sep = "")]] = yield_data
    setTxtProgressBar(pb, year)
  }
  close(pb)
  summary_one_crop = bind_rows(summary_list, .id = "year") %>%
    mutate(year = as.numeric(str_sub(year, 2, 5)))
  write.csv(summary_one_crop, file = file.path(DSSAT_Path,"out",paste0(crop,"_Risk.csv")),
            row.names = FALSE)
  cat("Export to", file.path(DSSAT_Path,"out",paste0(crop,"_Risk.csv")))
}
