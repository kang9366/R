if(!require(ggmap)){install.packages("ggmap")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(raster)){install.packages("raster")}
if(!require(maptools)){install.packages("maptools")}
if(!require(rgdal)){install.packages("rgdal")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(readxl)){install.packages("readxl")}
if (!require(gpclib)){install.packages("gpclib", type="source")}

library(ggmap)
library(ggplot2)
library(raster)
library(maptools)
library(rgdal)
library(dplyr)
library(readxl)

### load location data
map <- shapefile("./data/TL_SCCO_SIG.shp")

# convert location data to coordinates data
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

gpclibPermit()
map_df <- fortify(map, region='SIG_CD') 
head(map_df, n = 10)

# convert coordinates data converted from location data to data frame
map_data <- map@data
head(map_data)

# convert SIG_ID for integer
map_data$SIG_CD <- map_data %>% 
  dplyr::select(SIG_CD) %>% 
  unlist() %>% 
  as.integer()

# extract SIG_CD value under 11740 and arrange it for name of state and extract SIG_CD
map_data <- map_data %>% 
  filter(SIG_CD <= 11740) %>% 
  arrange(SIG_KOR_NM) %>% 
  dplyr::select(SIG_CD)

### processing fine dust data
data <- read_xlsx("seoul-pollution.xlsx")
names(data)[3] <- 'name'
names(data)[4] <- 'finedust'

gu <- data %>% 
  arrange(name)
str(gu)

mean_finedust <- gu %>% 
  group_by(name) %>% 
  summarise(mean(finedust))

dust_data <- data.frame(id = map_data, mean_findust = mean_finedust)

names(dust_data)[1] = 'id'
names(dust_data)[3] = 'mean_finedust'
dust_data

### Visualization
map_df$id <- map_df %>% 
  dplyr::select(id) %>% 
  unlist %>% 
  as.integer()

map_df <- map_df %>% 
  filter(id <= 11740)

data <- merge(map_df, dust_data, by = 'id')

g1 <- ggplot(data, aes(x=long, y=lat, group=group, fill = mean_finedust)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "#E6E7EA", high = "#6A5ACD") + 
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) + 
  labs(title = "서울시 초미세먼지", x = "", y = "") + 
  labs(fill = "초미세먼지(㎍/㎥)") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
  
g2 <- g1 + 
  geom_text(x = 126.976, y = 37.586, label = "종로구", size = 3) +
  geom_text(x = 127.02, y = 37.6, label = "성북구", size = 3) +
  geom_text(x = 127.0106, y = 37.64, label = "강북구", size = 3) +
  geom_text(x = 127.033, y = 37.67, label = "도봉구", size = 3) +
  geom_text(x = 127.075, y = 37.653, label = "노원구", size = 3) +
  geom_text(x = 126.995, y = 37.5624, label = "중구", size = 3) +
  geom_text(x = 127.04503, y = 37.5567, label = "성동구", size = 3) +
  geom_text(x = 126.978, y = 37.533, label = "용산구", size = 3) + 
  geom_text(x = 126.935, y = 37.5742, label = "서대문구", size = 3) +
  geom_text(x = 126.9241, y = 37.62, label = "은평구", size = 3) +
  geom_text(x = 127.049, y = 37.4998, label = "강남구", size = 3) +
  geom_text(x = 127.11, y = 37.51, label = "송파구", size = 3) +
  geom_text(x = 127.08296, y = 37.551883, label = "광진구", size = 3) +
  geom_text(x = 127.0904, y = 37.59, label = "중랑구", size = 3) +
  geom_text(x = 127.14, y = 37.55, label = "강동구", size = 3) +
  geom_text(x = 127.053, y = 37.584, label = "동대문구", size = 3) +
  geom_text(x = 127.008, y = 37.4905, label = "서초구", size = 3) +
  geom_text(x = 127.049, y = 37.4998, label = "강남구", size = 3) +
  geom_text(x = 126.9438, y = 37.502, label = "동작구", size = 3) +
  geom_text(x = 126.9398, y = 37.466, label = "관악구", size = 3) +
  geom_text(x = 126.9, y = 37.462, label = "금천구", size = 3) +
  geom_text(x = 126.91, y = 37.52, label = "영등포구", size = 3) +
  geom_text(x = 126.9, y = 37.5648, label = "마포구", size = 3) +
  geom_text(x = 126.844, y = 37.493, label = "구로구", size = 3) +
  geom_text(x = 126.849, y = 37.518, label = "양천구", size = 3) +
  geom_text(x = 126.816, y = 37.564, label = "강서구", size = 3)

ggsave("서울시 초미세먼지 시각화.jpg", plot = g2, width=12, height=9)