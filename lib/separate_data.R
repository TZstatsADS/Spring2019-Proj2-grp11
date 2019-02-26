data = read.csv('data/AirportData/JFK_reduced.csv')
data = data %>% subset(select = c('pickup_longitude', 'pickup_latitude', 'business_day', 'pickup_hour', 'farePerDistance'))
# data$WWH = ifelse(data$weekend, 2, 1 + 2 * !data$business_day)
data$WWH = ifelse(data$business_day == "True", 1, 2)

JFK_count_result = array(dim = c(195,24,2))
JFK_FPD_result = array(dim = c(195,24,2))
load('output/myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))
count_rank = rank(subdat@data$NTACode)

for (j in 1:2){
  for (h in 1:24){
    data.sel = data %>% subset(pickup_hour == h - 1 & WWH == j)
    
    dat = data.frame(Longitude = data.sel$pickup_longitude, Latitude = data.sel$pickup_latitude)
    coordinates(dat) <- ~ Longitude + Latitude
    proj4string(dat) <- CRS("+proj=longlat")
    dat <- spTransform(dat, proj4string(myShape1))
    r = over(dat, myShape1)
    r = r %>% subset(select = c('NTACode')) %>% cbind(data.sel)
    
    count = table(r$NTACode)[count_rank]
    FPD = tapply(r$farePerDistance, r$NTACode, mean)
    JFK_count_result[,h,j] = as.vector(count)
    JFK_FPD_result[,h,j] = as.vector(FPD)
  }
}

save(JFK_count_result, file = 'output/JFK_count_seperated.RData')
save(JFK_FPD_result, file = 'output/JFK_FPD_seperated.RData')

