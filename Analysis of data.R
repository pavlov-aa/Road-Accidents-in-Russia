### Installing libraries and set up -----
library("dplyr")
library("lubridate")
library("RColorBrewer")
library("car")
library("pROC")

pal<-brewer.pal(8,"Set3")

### Saving short version of input data ----
all<-read.csv("all.csv")
attach(all)
all[] <- lapply(all, as.character) # all factor columns to char

# Selecting interesting variables
colnames(all)
df<-select(all,KartId,TSID.x,PersonID,date,Time,DTP_V,POG,RAN,K_TS,ndu,sdor,k_ul,s_pog,s_pch,osv,COORD_W,COORD_L,t_ts,marka_ts,m_ts,color,r_rul,g_v,m_pov,NPDD,S_T,POL,V_ST,ALCO,SOP_NPDD,SAFETY_BELT,S_SM,"role"=K_UCH.y,"year"=V16.y,"region"=V18)
rm(all)
write.csv(df,"df.csv")   

### Start of general analysis
attach(df)
df$died<-grepl("Скончался",S_T)+0
t<-aggregate(data=df,died~region+year,FUN=sum)
stat<-cbind(t[1:85,1:2],"2015"=t[1:85,3],"2016"=t[86:170,3],"2017"=t[171:255,3],"2018"=round(t[256:340,3]*12/11))
stat$year<-NULL
stat$chg<-stat$`2018`/stat$`2015`

# Best regions in dynamics
stat[order(stat$chg),][1:10,]

# Worst regions in dynamics
stat[order(-stat$chg),][1:10,]

# Most deaths in 2018 year here:
stat[order(-stat$`2018`),][1:10,]

## The same story with accidents:
t<-aggregate(data=df, KartId~region+year, function(x) length(unique(x)))
stat<-cbind(t[1:85,1:2],"2015"=t[1:85,3],"2016"=t[86:170,3],"2017"=t[171:255,3],"2018"=round(t[256:340,3]*12/11))
stat$year<-NULL
stat$chg<-stat$`2018`/stat$`2015`

# Best regions in dynamics
stat[order(stat$chg),][1:10,]

# Worst regions in dynamics
stat[order(-stat$chg),][1:10,]

# Most deaths in 2018 year here:
stat[order(-stat$`2018`),][1:10,]

ds<-df[df$region=="Краснодарский край",]
write.csv(ds,"kransodarskiy_krai.csv",row.names=F)
### Analysis of Krasnodarskiy Krai region ----

ds<-read.csv("kransodarskiy_krai.csv")
attach(ds)
summary(ds)

setwd("/home/alexander/Рабочий стол/images")
jpeg(filename="Distribution of driving experience in accidents in KK.jpeg",width=480,height=480)
hist(ds$V_ST,col=pal,breaks=100,xlab="Driving Experience, years",ylab="frequency",
     main="Distribution of driving experience in accidents in KK")
dev.off()
# 
jpeg(filename="Distribution of observed ALCO in accidents in KK.jpeg",width=480,height=480)
hist(ds$ALCO,col=pal,breaks=100,xlab="Alcohol intoxication, promile",ylab="frequency",
     main="Distribution of ALCO in accidents in KK")
dev.off()
# 


sds<-summary(ds)

# POL
jpeg(filename="Distribution of sex in accidents in KK.jpeg",width=480,height=480)
pie(table(ds$POL),main="Sex distribution",col=pal)
dev.off()
# V_ST
sds[1:7,28]
# ALCO:
sds[1:7,29]




ds$date<-as.Date(ds$date, "%d.%m.%Y")
ds$month<-month(ds$date)
ds$wday<-as.POSIXlt(ds$date)$wday+1

stat<-aggregate(data=ds,died~month+year,FUN=sum)

jpeg(filename="Deaths by month, 2015-2018.jpeg",width=480,height=480)
boxplot(stat$died~stat$month,main="Deaths by month, 2015-2018",xlab="month",ylab="frequency",col=pal)
dev.off()

jpeg(filename="Deaths by month during 2015-2018.jpeg",width=480,height=480)
boxplot(stat$died~stat$year,main="Deaths by month during 2015-2018",xlab="year",ylab="frequency",col=pal)
dev.off()

stat<-aggregate(data=ds,died~wday+month+year,FUN=sum)

jpeg(filename="Deaths by month during 2015-2018 - day of week.jpeg",width=480,height=480)
boxplot(stat$died~stat$wday,main="Deaths by month during 2015-2018",xlab="day of week",ylab="frequency",col=pal)
dev.off()



### Tests of association ----

ds_short<-ds[,c("KartId","DTP_V","POG","RAN","ndu","sdor","k_ul","s_pog","s_pch","osv","month","wday","year")]
ds_short<-distinct(ds_short)
ds_short$smb_died<-ifelse(ds_short$POG>0,1,0)
ds_short$accidents<-1

chisq.test(ds_short$wday,ds_short$smb_died) # 
chisq.test(ds_short$DTP_V,ds_short$smb_died)
chisq.test(ds_short$s_pog,ds_short$smb_died)
chisq.test(ds_short$s_pch,ds_short$smb_died) # not significant
chisq.test(ds_short$osv,ds_short$smb_died)
chisq.test(ds_short$DTP_V,ds_short$month)

### Plots and tables for tests ----
# 1)
t<-aggregate(data=ds_short,cbind(accidents,smb_died)~wday,FUN=sum)
t$ratio<-t$smb_died/t$accidents

jpeg(filename="Deaths to accidents ratio vs day of week.jpeg",width=480,height=480)
barplot(t$ratio,type="s",xlab="day of week",ylab="death ratio",main="Deaths to accidents ratio vs day of week",names.arg=t[,1],col=pal,ylim=c(0,0.2))
dev.off()

# 2)
t<-aggregate(data=ds_short,cbind(accidents,smb_died)~DTP_V,FUN=sum)
t$ratio<-t$smb_died/t$accidents
t[t$smb_died>5,]

# 3)
t<-aggregate(data=ds_short,cbind(accidents,smb_died)~s_pog,FUN=sum)
t$ratio<-t$smb_died/t$accidents
t<-t[t$smb_died>5,]
t<-t[-1,] # removing complex weather

jpeg(filename="Deaths to accidents ratio vs wheather conditions.jpeg",width=480,height=480)
barplot(t$ratio,type="s",xlab="wheather conditions",ylab="death ratio",main="Deaths to accidents ratio vs wheather conditions",names.arg=t[,1],col=pal,ylim=c(0,0.3))
dev.off()

# 4) -//-
# 5) 
t<-aggregate(data=ds_short,cbind(accidents,smb_died)~osv,FUN=sum)
t$ratio<-t$smb_died/t$accidents
t


dtpv<-data.frame(sort(table(ds_short$DTP_V)))
sum(dtpv$Freq[dtpv$Freq>50])/sum(dtpv$Freq)
jpeg(filename="Road accidents by type.jpeg",width=1000,height=1000)
pie(dtpv[dtpv$Freq>50,'Freq'],labels=dtpv[dtpv$Freq>50,'Var1'],main="Road accidents by type")
dev.off()
stat<-aggregate(data=ds_short, KartId~DTP_V+month+year,FUN=length)

jpeg(filename="Clashes between vehicles monthly.jpeg",width=480,height=480)
boxplot(stat[stat$DTP_V=="Столкновение",'KartId']~stat[stat$DTP_V=="Столкновение",'month'],col=pal,xlab="month",ylab="frequency",main="Clashes between vehicles")
dev.off()

jpeg(filename="Knocking down pedestrians monthly.jpeg",width=480,height=480)
boxplot(stat[stat$DTP_V=="Наезд на пешехода",'KartId']~stat[stat$DTP_V=="Наезд на пешехода",'month'],col=pal,xlab="month",ylab="frequency",main="Hitting pedestrians")
dev.off()

jpeg(filename="Hitting the obstacle monthly.jpeg",width=480,height=480)
boxplot(stat[stat$DTP_V=="Наезд на препятствие",'KartId']~stat[stat$DTP_V=="Наезд на препятствие",'month'],col=pal,xlab="month",ylab="frequency",main="Hitting the obstacle")
dev.off()

jpeg(filename="Rollovers monthly.jpeg",width=480,height=480)
boxplot(stat[stat$DTP_V=="Опрокидывание",'KartId']~stat[stat$DTP_V=="Опрокидывание",'month'],col=pal,xlab="month",ylab="frequency",main="Rollover")
dev.off()

jpeg(filename="Hitting cyclists monthly.jpeg",width=480,height=480)
boxplot(stat[stat$DTP_V=="Наезд на велосипедиста",'KartId']~stat[stat$DTP_V=="Наезд на велосипедиста",'month'],col=pal,xlab="month",ylab="frequency",main="Hitting Cyclists")
dev.off()

# Hitting pedestrians trend is another. Hypothesis - light is the main reason.

# What causes hitting pedestrian?

kul<-data.frame(table(ds_short$k_ul))


### Transforming data for logit regression -----
hp<-ds_short[ds_short$DTP_V=="Наезд на пешехода",]
hp$countryside<-ifelse(grepl("Вне НП",hp$k_ul)+0==1,1,0)
hp$village<-ifelse(grepl("Поселковые",hp$k_ul)+0==1,1,0)
hp$place.na<-ifelse(hp$k_ul=="",1,0)
hp$k_ul<-NULL

ndus<-data.frame(table(ndu))
hp$ndu.marking<-grepl("разметк",hp$ndu)+0
hp$ndu.signs<-grepl("знак",hp$ndu)+0
hp$ndu.lighting<-grepl("освещен",hp$ndu)+0
hp$ndu.road<-ifelse(grepl("обочин",hp$ndu)+grepl("сцепные",hp$ndu)+grepl("содержан",hp$ndu)+grepl("огражден",hp$ndu)+
  grepl("покрыти",hp$ndu)+grepl("препятстви",hp$ndu)+grepl("ТСОД",hp$ndu)+0>0,1,0)
hp$ndu.visibility<-grepl("видимост",hp$ndu)+0
hp$ndu.pedestrian.fencing<-ifelse(grepl("тротуар",hp$ndu)+grepl("обустройств",hp$ndu)+grepl("огражден",hp$ndu)+0>0,1,0)
hp$ndu<-NULL


pog<-data.frame(table(hp$s_pog))
hp$pog.clear<-grepl("Ясно",hp$s_pog)+0
hp$pog.fog<-grepl("Туман",hp$s_pog)+0
hp$pog.precipitation<-ifelse(grepl("Снегопад",hp$s_pog)+grepl("Дождь",hp$s_pog)+grepl("Пасмурно",hp$s_pog)+0>0,1,0)
hp$s_pog<-NULL

sdors<-data.frame(table(sdor))

hp$sdor.crosswalk<-grepl("пешеходный переход",hp$sdor)+0
hp$sdor.tunnel.or.bridge<-ifelse(grepl("тоннель",tolower(hp$sdor))+grepl("мост",tolower(hp$sdor))+0>0,1,0)
hp$sdor.low.speed.zone<-ifelse(grepl("остановк",tolower(hp$sdor))+grepl("гаражн",tolower(hp$sdor))+grepl("пешеходная зона",tolower(hp$sdor))+
                                 grepl("тротуар",tolower(hp$sdor))+grepl("выезд",tolower(hp$sdor))+grepl("внутридворовая",tolower(hp$sdor))+
                                 grepl("азс",tolower(hp$sdor))+grepl("автостоянк",tolower(hp$sdor))+0>0,1,0)
hp$sdor.intersection<-ifelse(grepl("перекресток",tolower(hp$sdor))+grepl("пересечени",tolower(hp$sdor))+grepl("переезд",tolower(hp$sdor))+0>0,1,0)
hp$sdor<-NULL


spch<-data.frame(table(s_pch))
hp$s_pch.dry<-ifelse(grepl("Сухое",hp$s_pch)+grepl("Пыльное",hp$s_pch)+0>0,1,0)
hp$s_pch.wet<-ifelse(grepl("Мокрое",hp$s_pch)+grepl("водой",hp$s_pch)+0>0,1,0)
hp$s_pch.snow<-ifelse(grepl("Заснеженное",hp$s_pch)+grepl("Гололедица",hp$s_pch)+grepl("снежн",hp$s_pch)+0>0,1,0)
hp$s_pch<-NULL


table(hp$osv)
hp$osv.daylight<-grepl("Светлое время",hp$osv)+0
hp$osv.twilight<-grepl("Сумерки",hp$osv)+0
hp$osv.night.with.light<-grepl("освещение включено",hp$osv)+0
hp$osv<-NULL


# Where and when is it dangerous?

dangerous<-aggregate(data=hp,cbind(POG,RAN,smb_died)~countryside+village+place.na+pog.clear+pog.fog+pog.precipitation+osv.daylight+osv.twilight+osv.night.with.light,FUN=sum)
dangerous2<-aggregate(data=hp,KartId~countryside+village+place.na+pog.clear+pog.fog+pog.precipitation+osv.daylight+osv.twilight+osv.night.with.light, function(x) length(unique(x)))
cases<-cbind(dangerous,"KartCnt"=dangerous2$KartId)
cases$deaths.to.cards<-cases$smb_died/cases$KartCnt
cases[order(-cases$deaths.to.cards),]
 
# Explain most dangerous cases in terms of death ratio and absolute value

### Logit-regression without personal variables ----
hp$wday<-as.character(hp$wday)
summary(hp)
hp$smb_died<-factor(hp$smb_died)

mylogit<-glm(smb_died~wday+countryside+village+place.na+ndu.marking+
               ndu.signs+ndu.lighting+ndu.road+ndu.visibility+
               ndu.pedestrian.fencing+pog.clear+pog.fog+pog.precipitation+
               sdor.crosswalk+sdor.tunnel.or.bridge+
               sdor.low.speed.zone+sdor.intersection+
               s_pch.dry+s_pch.wet+s_pch.snow+osv.daylight+osv.twilight+
               osv.night.with.light,data=hp,family="binomial")
vif(mylogit)
summary(mylogit)

mylogit$aic
mylogit2<-glm(smb_died~wday+countryside+village+place.na+ndu.marking+
                ndu.signs+ndu.lighting+ndu.road+ndu.visibility+
                ndu.pedestrian.fencing+pog.fog+sdor.crosswalk+sdor.tunnel.or.bridge+
                sdor.low.speed.zone+sdor.intersection+
                s_pch.snow+osv.daylight+osv.twilight+
                osv.night.with.light,data=hp,family="binomial")

vif(mylogit2)
summary(mylogit2)
mylogit2$aic

mylogit3<-glm(smb_died~countryside+place.na+
                +ndu.lighting+
                +pog.fog+sdor.crosswalk+
                sdor.low.speed.zone+
                s_pch.wet+osv.daylight+
                osv.night.with.light,data=hp,family="binomial")
summary(mylogit3)
vif(mylogit3)
mylogit3$aic

probs<-predict(mylogit3,type="response")
predicted.classes<-ifelse(probs>0.5,1,0)
jpeg(filename="Different distributions of estimated probs of death for alive and dead.jpeg",width=600,height=600)
boxplot(probs~hp$smb_died,col=pal,main="Different distributions of estimated probs of death for alive and dead")
dev.off()
auc(hp$smb_died,probs)

### Adding variables related to person and vehicle ------
#tts<-data.frame(table(ds$t_ts))
ds$a.class<-grepl("A-класс",ds$t_ts)+0
ds$b.class<-grepl("B-класс",ds$t_ts)+0
ds$c.class<-grepl("C-класс",ds$t_ts)+0
ds$d.class<-grepl("D-класс",ds$t_ts)+0
ds$e.class<-grepl("E-класс",ds$t_ts)+0

ds$vehicle.age<-ds$year-ds$g_v

#npdds<-data.frame(table(ds$NPDD))
#sop_npdds<-data.frame(table(ds$SOP_NPDD))

ds$npdd.broken.car<-ifelse(grepl("техническ",ds$NPDD)+grepl("техническ",ds$SOP_NPDD)+0>0,1,0)
ds$npdd.maneuver<-ifelse(grepl("разворот",ds$NPDD)+grepl("разворот",ds$SOP_NPDD)+
                           grepl("задним",ds$NPDD)+grepl("задним",ds$SOP_NPDD)+
                           grepl("перестроени",ds$NPDD)+grepl("перестроени",ds$SOP_NPDD)+
                           grepl("обгон",ds$NPDD)+grepl("обгон",ds$SOP_NPDD)+
                           grepl("встречн",ds$NPDD)+grepl("встречн",ds$SOP_NPDD)+
                           0>0,1,0)
ds$npdd.speed<-ifelse(grepl("скорост",ds$NPDD)+grepl("скорост",ds$SOP_NPDD)+0>0,1,0)
ds$npdd.priority<-ifelse(grepl("проезд",ds$NPDD)+grepl("проезд",ds$SOP_NPDD)+0>0,1,0)
ds$npdd.distance<-ifelse(grepl("интервал",ds$NPDD)+grepl("интервал",ds$SOP_NPDD)+
                           grepl("дистанци",ds$NPDD)+grepl("дистанци",ds$SOP_NPDD)+0>0,1,0)
ds$npdd.no.right<-ifelse(grepl("категори",ds$SOP_NPDD)+grepl("незарегистрирован",ds$SOP_NPDD)++grepl("права",ds$SOP_NPDD)+0>0,1,0)
ds$npdd.alco<-ifelse(grepl("алкогол",ds$SOP_NPDD)+grepl("освидетельств",ds$SOP_NPDD)+0>0,1,0)
ds$npdd.drugs<-ifelse(grepl("наркоти",ds$SOP_NPDD)+0>0,1,0)
ds$npdd.tired<-ifelse(grepl("переутомлен",ds$SOP_NPDD)+grepl("отдых",ds$SOP_NPDD)+0>0,1,0)
ds$npdd.mtpl<-ifelse(grepl("ОСАГО",ds$SOP_NPDD)+0>0,1,0)


ds<-ds[ds$DTP_V=="Наезд на пешехода",]
ds<-ds[ds$role=="Водитель",]
ds<-ds[ds$K_TS==1,]

ds$TSID.x<-NULL
ds$PersonID<-NULL
ds$date<-NULL
ds$Time<-NULL
ds[,6:16]<-NULL
ds$RAN<-NULL
ds$K_TS<-NULL
ds$DTP_V<-NULL
ds[,4:8]<-NULL
ds[,7:13]<-NULL
ds$POG<-NULL
ds$month<-NULL
ds$wday<-NULL
data<-merge(ds, hp, by = "KartId")
data$DTP_V<-NULL
data$POG<-NULL
data$RAN<-NULL
data$KartId<-NULL

### Logit-regression with all variables -----
mylogit4<-glm(data=data,smb_died~wday+countryside+village+place.na+ndu.marking+
                ndu.signs+ndu.lighting+ndu.road+ndu.visibility+
                ndu.pedestrian.fencing+pog.clear+pog.fog+pog.precipitation+
                sdor.crosswalk+sdor.tunnel.or.bridge+
                sdor.low.speed.zone+sdor.intersection+
                s_pch.dry+s_pch.wet+s_pch.snow+osv.daylight+osv.twilight+
                osv.night.with.light+color+ALCO+a.class+b.class+c.class+d.class+
                e.class+vehicle.age+npdd.broken.car+npdd.maneuver+npdd.speed+
                npdd.priority+npdd.distance+npdd.no.right+npdd.alco+npdd.drugs+
                npdd.tired+npdd.mtpl,family="binomial")
summary(mylogit4)
vif(mylogit4)
mylogit4$aic

mylogit5<-glm(data=data,smb_died~countryside+place.na+
                +ndu.lighting+
                +pog.fog+sdor.crosswalk+
                sdor.low.speed.zone+
                s_pch.wet+osv.daylight+
                osv.night.with.light+color+ALCO+d.class+
                +vehicle.age+npdd.broken.car+npdd.maneuver+npdd.speed+
                npdd.distance+npdd.no.right+npdd.alco+npdd.drugs+
                npdd.mtpl
                ,family="binomial")
summary(mylogit5)
vif(mylogit5)
mylogit5$aic


mylogit6<-glm(data=data,smb_died~countryside+place.na+
                +ndu.lighting+
                +pog.fog+sdor.crosswalk+
                sdor.low.speed.zone+
                s_pch.wet+osv.daylight+
                osv.night.with.light+npdd.maneuver+npdd.speed+
                +npdd.distance+npdd.mtpl
              ,family="binomial")
summary(mylogit6)
vif(mylogit6)
mylogit6$aic

probs<-predict(mylogit6,type="response")
predicted.classes<-ifelse(probs>0.5,1,0)
jpeg(filename="Different distributions of estimated probs of death for alive and dead (logit6).jpeg",width=600,height=600)
boxplot(probs~data$smb_died,col=pal,main="Different distributions of estimated probs of death for alive and dead")
dev.off()
auc(data$smb_died,probs)

