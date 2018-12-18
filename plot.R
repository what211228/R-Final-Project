library(ggplot2, help, pos = 2, lib.loc = NULL)
library(dplyr, help, pos = 2, lib.loc = NULL)
library(ggmap, help, pos = 2, lib.loc = NULL)

addweatherdata<- read.csv("cleandata.csv",header = T,sep = ",")
map <- readRDS("nycmap10.rds")
View(addweatherdata)

#1.試分析在紐約市不同區域以及不同時間點的上下車趨勢，並查找紐約市相關資料，提出影響計程車載客多寡趨勢的原因。

#每小時載客量
ggplot(addweatherdata, aes(x=pickup_hour,color=I("White"),fill=I("steelblue")))+
    geom_histogram(bins=24)

#每天載客
ggplot(addweatherdata, aes(x=pickup_day,color=I("White"),fill=I("springgreen4")))+
    geom_histogram(bins=30)


####ggmap尚未分析####
png(paste0(i,".png"), width=600, height=600)
ggmap(map)+
    geom_point(data=filter(addweatherdata,dropoff_hour==8),aes(x = dropoff_longitude,y = dropoff_latitude), alpha = 0.5, color = "red")+
    ggtitle(8)
dev.off()

ggmap(map)+
    geom_point(data=filter(addweatherdata,pickup_hour==8),aes(x = pickup_longitude,y = pickup_latitude), alpha = 0.5, color = "red")+
        ggtitle(8)
   


#2.試分析天氣因素如何影響紐約市黃牌計程車的載客量，並提出可能的解釋。

#下雨天載客
ggplot(addweatherdata, aes(x=pickup_day,color=I("White")))+
    geom_histogram(aes(fill=precipitation>0.00))

#溫度載客
ggplot(addweatherdata, aes(x=pickup_day,color=I("White")))+
    geom_histogram(aes(fill=temperature<20))

#濕度載客 30~60
ggplot(addweatherdata, aes(x=pickup_day,color=I("White")))+
    geom_histogram(aes(fill= 30<=precipitation&&precipitation<=60))



#(3) 試分析紐約的黃牌計程車司機如何可以獲得最多的小費 (tip)

#小費分佈圖已去除=0
filter(addweatherdata,tip_amount!=0)%>%
    ggplot()+
    geom_density(aes(x=tip_amount))+
    xlim(0,25)+
    geom_vline(aes(xintercept=mean(tip_amount)),
                color="blue", linetype="dashed", size=1)

#小費用以是否大於乘車人數做分佈圖（無相關）
filter(addweatherdata,tip_amount!=0)%>%
    ggplot()+
    geom_density(aes(x=tip_amount,color=passenger_count>mean(passenger_count)))+
    xlim(0,25)+
    geom_vline(aes(xintercept=mean(tip_amount)),
                color="blue", linetype="dashed", size=1)

#費用以是否晚班做分佈圖（無相關）
filter(addweatherdata,tip_amount!=0)%>%
    ggplot()+
    geom_density(aes(x=tip_amount,color=dropoff_hour==c(0,1,2,3,4,5)))+
    xlim(0,25)+
    geom_vline(aes(xintercept=mean(tip_amount)),
                color="blue", linetype="dashed", size=1)

# #旅程分佈圖已去除=0
# filter(addweatherdata,trip_distance!=0)%>%
#     ggplot()+
#     geom_density(aes(x=trip_distance))+
#     xlim(0,20)+
#     geom_vline(aes(xintercept=mean(trip_distance)),
#                 color="blue", linetype="dashed", size=1)

#有旅程==0但小費!=0
filter(addweatherdata,trip_distance==0)%>%select(tip_amount,trip_distance)%>%View

#小費用以是否大於平均旅程做分佈圖（有相關）
filter(addweatherdata,tip_amount!=0)%>%
    ggplot()+
    geom_density(aes(x=tip_amount,color=trip_distance>mean(trip_distance)))+
    xlim(0,25)+
    geom_vline(aes(xintercept=mean(tip_amount)),
                color="blue", linetype="dashed", size=1)

#小於平均旅程的小費分佈圖
filter(addweatherdata,tip_amount!=0)%>%filter(trip_distance<mean(trip_distance))%>%
    ggplot()+
    geom_density(aes(x=tip_amount,color=I("springgreen4")))+
    xlim(0,25)

#每次小費$
h <-group_by(addweatherdata,pickup_hour)%>%
    summarise(mean(tip_amount, na.rm = TRUE))
names(h)<-c("hour","tip_avg")


ggplot(h,aes(x=hour,y=tip_avg))+
    geom_line()+
    geom_point()


#每次距離   
j<-group_by(addweatherdata,pickup_hour)%>%
    summarise(mean(trip_distance, na.rm = TRUE))
names(j)<-c("hour","trip_distance_avg")

ggplot(j,aes(x=hour,y=trip_distance_avg))+
    geom_line()+
    geom_point()


#小費/距離
k<- left_join(h,j)
k<-mutate(k,perMileTip=tip_avg/trip_distance_avg)
ggplot(k,aes(x=hour,y=perMileTip))+
    geom_line()+
    geom_point()
