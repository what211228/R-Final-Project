library(dplyr, help, pos = 2, lib.loc = NULL)

humidity <- read.table("humidity.txt",header = F,sep = ",")
temperature  <- read.table("temperature.txt",header = F,sep = ",")
precipitation <- read.table("precipitation.txt",header = F,sep = ",")
data<- read.csv("sample.csv",header = T,sep = ",")

names(humidity)<-c(1:30)
humidity<-t(humidity)
humidity<-data.frame(humidity)
humidity<-mutate(humidity,day=c(1:30))
names(humidity)<-c("humidity","pickup_day")


names(temperature)<-c(1:30)
temperature<-t(temperature)
temperature<-data.frame(temperature)
temperature<-mutate(temperature,day=c(1:30))
names(temperature)<-c("temperature","pickup_day")

# View(temperature)


names(precipitation)<-c(1:30)
precipitation<-t(precipitation)
precipitation<-data.frame(precipitation)
precipitation<-mutate(precipitation,day=c(1:30))
names(precipitation)<-c("precipitation","pickup_day")
# View(precipitation)

weather <- left_join(humidity,temperature,by="pickup_day")%>%
    left_join(precipitation,by="pickup_day")
# View(weather)


cleandata <- mutate(data,pickup_day = as.integer(substring(tpep_pickup_datetime,9,10),formate="%2d"))%>%
    mutate(dropoff_day = as.integer(substring(tpep_dropoff_datetime,9,10),formate="%2d"))%>%
    mutate(pickup_hour =as.integer(substring(tpep_pickup_datetime,12,13),formate="%2d"))%>%
    mutate(pickup_minute =as.integer(substring(tpep_pickup_datetime,15,16),formate="%2d"))%>%
    mutate(dropoff_hour = as.integer(substring(tpep_dropoff_datetime,12,13),formate="%2d"))%>%
    mutate(dropoff_minute = as.integer(substring(tpep_dropoff_datetime,15,16),formate="%2d"))


addweatherdata <- left_join(x=cleandata,y=weather,by="pickup_day")

View(addweatherdata)


write.table(addweatherdata, file = "cleandata.csv", sep = ",")