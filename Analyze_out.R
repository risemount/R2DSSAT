rm(list = ls())

# Set current file directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Import libary
library(tidyverse)

# Set font
windowsFonts(A=windowsFont("Times New Roman"))

# Read all experiment data
full.risk <- readRDS("./data/experiment_results.rds")
windowsFonts()
# Coordinate systems
# WGS84: 4326
# TWD97: 3824

# load dh.sol
load("DH_SOL.Rdata")

# Show parameters(LL, DUL, SAT) in plot
par(mfrow = c(3,6))
for(i in 1:18){
  plot(x = dh.sol[som.clusters %in% c(1,4),]$SLLL[[i]], y = rev(c(1:6)), type = "l", xlim = c(0., 0.5), col = "red",
       xlab = "value", ylab = "Depth", main = dh.sol[som.clusters %in% c(1,4),]$PEDON[i])
  points(x = dh.sol[som.clusters %in% c(1,4),]$SDUL[[i]], y = rev(c(1:6)), type = "l", xlim = c(0., 0.5), col = "green")
  points(x = dh.sol[som.clusters %in% c(1,4),]$SSAT[[i]], y = rev(c(1:6)), type = "l", xlim = c(0., 0.5), col = "blue")
}


###################################################
# Self-Organized Map for 3 parameters with depth
soil_water <- dh.sol[,c("PEDON","SLLL","SDUL","SSAT")] %>%
  unnest_wider(.,c("SLLL","SDUL","SSAT"), names_sep = "_") %>%
  column_to_rownames(var = "PEDON") %>%
  as.matrix()

# SOM dimesion
grid.som <- kohonen::somgrid(xdim = 10, ydim = 10,
                 topo = "rectangular")

# Perform SOM analysis
set.seed(2024)
soil.som <- kohonen::som(soil_water, grid = grid.som,
             rlen = 500) # rlen: replication

# 
par(mfcol = c(6,3))
kohonen::getCodes(soil.som) %>% as_tibble() %>%
  rownames_to_column(var = "group") %>%
  group_by(group) %>%
  nest(SLLL = starts_with("SLLL"),
       SDUL = starts_with("SDUL"),
       SSAT = starts_with("SSAT")) %>%
  ungroup() %>% select(-group) 


# Cluster of SOM
som.clusters <- soil.som$unit.classif


# Barplot of RISK in each cluster
full.risk %>%
  filter(year == 2016, crop == "Rice") %>%
  mutate(cluster = as.factor(som.clusters)) %>%
  group_by(cluster) %>%
  summarize(m = mean(RISK), sd = sd(RISK)) %>%
  ggplot(aes(x = cluster, y = m)) +
  geom_col(col = "black", fill = "white")

par(mfcol = c(6,3))
kohonen::getCodes(soil.som) %>% as_tibble() %>%
  rownames_to_column(var = "group") %>%
  group_by(group) %>%
  nest(SLLL = starts_with("SLLL"),
       SDUL = starts_with("SDUL"),
       SSAT = starts_with("SSAT")) %>%
  ungroup() %>% select(-group) 

###################################################

# Year, crop, SOIL_ID

### Risk matrix
# Column: crop
# row: SOIL_ID

# For single position
Soil = "DH03368204"
# Save heatmap to path
full.risk %>%
  mutate(RISK.reverse = RISK) %>%
  filter(SOIL_ID == Soil) %>%
  pivot_wider(names_from = year,values_from = RISK.reverse, id_cols = crop) %>%
  column_to_rownames(., var = "crop") %>%
  pheatmap::pheatmap(cluster_cols = FALSE,color = colorRampPalette(c("green","red"))(10),
                     main = Soil, filename = file.path("./figures/", paste0(Soil,".png")), width = 8,height = 5)

# For single year
crop.reverse.risk.matrix <- full.risk %>% 
  filter(year == 2000) %>%
  mutate(RISK.reverse = 1 - RISK) %>%
  pivot_wider(names_from = crop, values_from = RISK.reverse, id_cols = SOIL_ID) %>%
  select(-SOIL_ID) %>% as.matrix


# Read the county boundaries (shapefile)
tw.sf <- sf::st_read("./shapefiles/nlsc/COUNTY_MOI_1090820.shp") %>%
  sf::st_transform(., 4326) # Transform to WGS84

# Transform soil location to sf files
# (Choose one yead and crop for just 920 points)
sol.sf <- full.risk %>% filter(year == 2018, crop == "Rice") %>% 
  sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Merge the county of each 920 points
within <- sf::st_within(sol.sf, tw.sf, sparse = FALSE)
counties <- unlist(apply(within, 1, function(x) tw.sf$COUNTYNAME[x]))


# Combine the county and soil location
points_with_district <- sf::st_join(sol.sf, tw.sf)

the.point <- points_with_district %>% 
  filter(SOIL_ID == "DH03368204")
  # filter(latitude > 24, latitude < 24.3, longitude > 120.5, longitude < 121.5)

### Abnormal Soil grid point
the.point <- points_with_district[2,]
tw.sf %>%
  ggplot() + 
  geom_sf(aes(geometry = geometry)) +
  geom_sf(data = the.point, color = "red", size = 3.5, pch = 3) + 
  theme_bw() +
  coord_sf(xlim = c(121.52,121.56), ylim = c(25.29, 25.3)) # Specific range
# coord_sf(xlim = c(119.8, 122.2), ylim = c(21.7, 25.4)) # Full range of island

#### Risk map ####
map.crop = "Cabbage"
map.year = 2018
map.risk <- full.risk %>% filter(year == map.year, crop == map.crop)
ggplot() +
  geom_sf(data = tw.sf, aes(geometry = geometry), fill = alpha("white",0)) +
  geom_point(data = map.risk, aes(x=longitude, y=latitude, col = RISK),size=2, pch=15) +
  scale_colour_gradient(low="green", high = "red1",name ="Risk ratio",
                        limits = c(0,1)) +
  coord_sf(xlim = c(119.8, 122.2), ylim = c(21.7, 25.4)) + # Full range of island
  labs(x = "Longitude", y = "Latitude",title = paste(map.year, map.crop)) +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "A"),
        axis.title=element_text(size=16,family = "A"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks=seq(120,122,0.5),limits = c(120, 122))+
  scale_y_continuous(breaks=seq(21.5,25.5,0.5),limits = c(21.5,25.5))+
  guides(col = guide_colorbar(barwidth = 1, barheight = 15, title.position = "top")) + 
  theme_bw()+
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "A"))

full.risk %>%
  filter(year == 2018) %>%
  pivot_wider(id_cols = SOIL_ID, values_from = RISK, names_from = crop) %>%
  column_to_rownames(var = "SOIL_ID")

# Make nested tibble
longer.table <- function(x){
  pivot_longer(x, cols = 2:9,names_to = "crop", values_to = "Risk") %>%
    mutate(year_and_crop = paste(crop, year, sep = "_")) %>%
    select(year_and_crop, Risk, -year, -crop) %>%
    pivot_wider(names_from = "year_and_crop", values_from = "Risk")
}

som.risk <- full.risk %>%
  select(SOIL_ID, year, crop, RISK) %>%
  pivot_wider(names_from = crop, values_from = RISK) %>%
  nest(.by = SOIL_ID, .names_sep = "_") %>%
  mutate(data = map(data, ~ longer.table(.x))) %>%
  unnest(cols = data) %>% 
  column_to_rownames(var = "SOIL_ID") %>% as.matrix()

# SOM dimesion
grid.som <- kohonen::somgrid(xdim = 4, ydim = 4,
                             topo = "rectangular")
# Perform SOM analysis
set.seed(2024)
soil.som <- kohonen::som(som.risk, grid = grid.som,
                         rlen = 1000) # rlen: replication

# Trace plot
plot(soil.som,type = "changes")

wider.table <- function(x){
  pivot_wider(x, id_cols = year, names_from = crop, values_from = Risk) %>%
    column_to_rownames(var = "year")
}
heatmap.som <- kohonen::getCodes(soil.som) %>% as.tibble() %>%
  rownames_to_column(var = "cluster") %>%
  pivot_longer(cols = 2:313, names_to = "crop_and_year", values_to = "Risk") %>%
  mutate(crop = str_extract(crop_and_year, pattern = "^([^_]*)"),
         year = str_extract(crop_and_year, pattern = "(?<=_).*$")) %>%
  select(-crop_and_year) %>% nest(.by = cluster) %>%
  mutate(data = map(data, ~ wider.table(.x))) %>%
  mutate(hmp = map2(data, cluster, ~ pheatmap::pheatmap(.x, cluster_rows = FALSE, cluster_cols = FALSE,
                                              silent = TRUE, main = .y, breaks = seq(0,1,0.1),
                                              color = colorRampPalette(c("yellow", "red"))(10))[[4]]))
# Arrange Heatmaps
gg <- do.call(gridExtra::grid.arrange, c(heatmap.som[[3]], ncol = 5, nrow = 5))

# Save plots
ggsave("./heatmap_mat.png", gg, width = 15, height = 15)

# Sample Size
pheatmap::pheatmap(table(soil.som$unit.classif) %>% matrix(5,5,byrow = TRUE),
                   cluster_cols = FALSE, cluster_rows = FALSE, display_numbers = TRUE,
                   number_format = "%.f", fontsize_number = 12)


# Radar plot
library(fmsb)
full.risk %>% 
  group_by(year, SOIL_ID) %>%
  select(crop, RISK) %>%
  mutate(RISK = 1-RISK) %>%
  pivot_wider(names_from = crop, values_from = RISK) %>%
  ungroup() %>%
  select(-year, -SOIL_ID) %>%
  rbind(rep(1, 8), rep(0, 8), .) %>%
  slice(1:4) %>%
  fmsb::radarchart(.,1)


