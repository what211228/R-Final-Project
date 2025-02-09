install.packages("devtools")
install.packages("gifski")
devtools::install_github("dgrtwo/gganimate")
library(ggplot2, help, pos = 2, lib.loc = NULL)
library(dplyr, help, pos = 2, lib.loc = NULL)
library(ggmap, help, pos = 2, lib.loc = NULL)
library(gganimate)

addweatherdata<- read.csv("cleandata.csv",header = T,sep = ",")
map <- readRDS("nycmap12.rds")
View(addweatherdata)

#1.試分析在紐約市不同區域以及不同時間點的上下車趨勢，並查找紐約市相關資料，提出影響計程車載客多寡趨勢的原因。

#每小時載客量
ggplot(addweatherdata, aes(x=pickup_hour,color=I("White"),fill=I("")))+
    geom_histogram(bins=24)

#每天載客
ggplot(addweatherdata, aes(x=pickup_day,color=I("White"),fill=I("darkorchid3")))+
    geom_histogram(bins=30)

#From to
ggmap(map)+
    geom_segment(data=addweatherdata, aes(x=dropoff_longitude, y=dropoff_latitude, xend=pickup_longitude, yend=pickup_latitude), 
     arrow=arrow(),alpha = 0.5)

#動圖
ggmap(map)+
    geom_point(data=addweatherdata,aes(x = pickup_longitude,y = pickup_latitude), alpha = 0.5, color = "red")+
    labs(title ='Pickup hour {frame_time}', x = 'longitude', y = 'latitude')+
    transition_time(pickup_hour)

ggmap(map)+
    geom_point(data=addweatherdata,aes(x = pickup_longitude,y = pickup_latitude), alpha = 0.5, color = "red")+
    labs(title ='Dropoff hour {frame_time}', x = 'longitude', y = 'latitude')+
    transition_time(dropoff_hour)
    
#2.試分析天氣因素如何影響紐約市黃牌計程車的載客量，並提出可能的解釋。

#下雨天載客
ggplot(addweatherdata, aes(x=pickup_day,color=I("White")))+
    geom_histogram(aes(fill=precipitation>0.00))+
    scale_fill_manual(values = c("tomato", "deepskyblue2"),name="是否雨天")

#溫度載客
ggplot(addweatherdata, aes(x=pickup_day,color=I("White")))+
    scale_fill_manual(values = c("tomato", "deepskyblue2"),name="是否舒適溫度")+
    geom_histogram(aes(fill=temperature<20))

#濕度載客 30~60
ggplot(addweatherdata, aes(x=pickup_day,color=I("White")))+
    scale_fill_manual(values = c("tomato", "deepskyblue2"),name="是否舒適濕度")+
    geom_histogram(aes(fill= 30<=precipitation&&precipitation<=60))


#(3) 試分析紐約的黃牌計程車司機如何可以獲得最多的小費 (tip)

#小費分佈圖已去除=0
filter(addweatherdata,tip_amount!=0)%>%
    ggplot()+
    geom_density(aes(x=tip_amount,color=I("brown3")))+
    xlim(0,25)+
    geom_vline(aes(xintercept=mean(tip_amount)),
                color="blue", linetype="dashed", size=1)

#小費用以是否大於乘車人數做分佈圖（無相關）
filter(addweatherdata,tip_amount!=0)%>%
    ggplot()+
    geom_density(aes(x=tip_amount,color=passenger_count>mean(passenger_count)))+
    xlim(0,25)+
    scale_color_manual(values = c("firebrick2", "deepskyblue2"),name="是否大於平均乘客數")+
    geom_vline(aes(xintercept=mean(tip_amount)),
                color="blue", linetype="dashed", size=1)

#費用以是否晚班做分佈圖（無相關）
filter(addweatherdata,tip_amount!=0)%>%
    ggplot()+
    geom_density(aes(x=tip_amount,color=dropoff_hour==c(0,1,2,3,4,5)))+
    xlim(0,25)+
    scale_color_manual(values = c("firebrick2", "deepskyblue2"),name="是否開夜車")+
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
    scale_color_manual(values = c("firebrick2", "deepskyblue2"),name="是否大於平均旅程")+
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


group_by(addweatherdata,pickup_hour)%>%View
    summarise(mean(tip_amount, na.rm = TRUE))%>%

ggplot(h,aes(x=hour,y=tip_avg,color=I("brown3")))+
    geom_line()+
    geom_point()


#每次距離   
j<-group_by(addweatherdata,pickup_hour)%>%
    summarise(mean(trip_distance, na.rm = TRUE))
names(j)<-c("hour","trip_distance_avg")

ggplot(j,aes(x=hour,y=trip_distance_avg,color=I("brown3")))+
    geom_line()+
    geom_point()


#小費/距離
k<- left_join(h,j)
k<-mutate(k,perMileTip=tip_avg/trip_distance_avg)
ggplot(k,aes(x=hour,y=perMileTip,,color=I("brown3")))+
    geom_line()+
    geom_point()


ggplot(addweatherdata)+
geom_density(aes(x=trip_distance,color=I("springgreen4")))

#付費比例（先求有，尚未求好版）
paysum <- data.frame(
  group = c("信用卡", "現金", "免費","協議","未知"),
  value = c(nrow(filter(addweatherdata,payment_type==1)),
            nrow(filter(addweatherdata,payment_type==2)),
            nrow(filter(addweatherdata,payment_type==3)),
            nrow(filter(addweatherdata,payment_type==4)),
            nrow(filter(addweatherdata,payment_type==5)))
  )
nrow(filter(addweatherdata,payment_type==1))
nrow(filter(addweatherdata,payment_type==2))
nrow(filter(addweatherdata,payment_type==3))

ggplot(paysum, aes(x="", y=value, fill=group))+
geom_bar(width = 1, stat = "identity")+
coord_polar("y", start=0)


#4.信用卡/現金

#距離
ggplot()+
    geom_density(data=filter(addweatherdata,payment_type==1),aes(x=trip_distance,color=I("red")))+
    geom_density(data=filter(addweatherdata,payment_type==2),aes(x=trip_distance,color=I("blue")))+
    xlim(0,25)

#總花費
ggplot()+
    geom_density(data=filter(addweatherdata,payment_type==1),aes(x=total_amount,color=I("red")))+
    geom_density(data=filter(addweatherdata,payment_type==2),aes(x=total_amount,color=I("blue")))+
    xlim(0,100)
