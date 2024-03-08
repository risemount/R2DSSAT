setwd("D:/315Lab/DATA/TReAD/WTH/")

file.names <- dir(pattern = "AATD", full.names = TRUE, recursive = TRUE)
wth.dat <- NULL
for(i in seq_along(file.names)){
  bind.dat <- read.table(file.names[i], skip = 4, header = TRUE,
                        col.names = c("DATE", "SRAD", "TMAX", "TMIN", "RAIN"),
                        colClasses = c("character",rep("numeric", 4)))
  wth.dat <- rbind(wth.dat, bind.dat)
}


wth.features <- `rownames<-`(wth.dat[,c("SRAD", "TMAX", "TMIN", "RAIN")], wth.dat$DATE)
cols = as.factor(str_sub(rownames(wth.features),1,2))

library(kohonen)
wth.som <- as.matrix(scale(wth.features))
wth.grid <- somgrid(xdim = 10, ydim = 10, topo = "rectangular")

set.seed(2024)
wth.som.model <- som(X = wth.som, grid = wth.grid,
                     rlen = 2000)
plot(wth.som.model, type = "count")
par(mfrow = c(2,2))
for(v in c("RAIN", "TMAX", "TMIN", "SRAD")){
  plot(wth.som.model, type = "property",
       property = getCodes(wth.som.model)[,v],
       main = v)
}
graphics.off()
plot(wth.som.model, type = "code")
wth.predict <- predict(wth.som.model, wth.som)
head(wth.predict$unit.predictions[[1]])
wth.predict$predictions[[1]][wth.predict$unit.classif == 1,]
wth.features[wth.predict$unit.classif == 1,]
plot(wth.som.model, type = "changes")
