install.packages("data.table")
install.packages("tibble")
install.packages("bit64")
install.packages("chron")

library("data.table")
library("tibble")
library("bit64")
library("chron")

options(stringAsFactors = T)
list.files(path="~/data2/2015/csv_normalized")
# (1) Make sure where your files are located
csv_files <- list.files (path       = "~/data2/2015/csv_normalized", 
                         pattern    = "*.csv", 
                         full.names = T)

main_info <- as_tibble (rbindlist (lapply (csv_files[171:255], fread)))

ts<-as_tibble (rbindlist (lapply (csv_files[86:170], fread)))

people_1 <- as_tibble (rbindlist (lapply (csv_files[1:27], fread)))
people_2 <- as_tibble (rbindlist (lapply (csv_files[29:36], fread)))
people_3 <- as_tibble (rbindlist (lapply (csv_files[38], fread)))
people_4 <- as_tibble (rbindlist (lapply (csv_files[39], fread)))
people_5 <- as_tibble (rbindlist (lapply (csv_files[41:85], fread)))
people_6 <- as_tibble (rbindlist (lapply (csv_files[c(28,37,40)], fread)))

people<-rbind(people_1,people_2,people_3,people_4,people_5,people_6)
rm(people_1,people_2,people_3,people_4,people_5,people_6)

write.csv(main_info,'dtp_2015.csv',row.names = F)
write.csv(ts,'ts_2015.csv',row.names = F)
write.csv(people,'people_2015.csv',row.names = F)

# 2016
csv_files <- list.files (path       = "~/data2/2016/csv_normalized", 
                         pattern    = "*.csv", 
                         full.names = T)

main_info <- as_tibble (rbindlist (lapply (csv_files[171:255], fread)))
ts<-as_tibble (rbindlist (lapply (csv_files[86:170], fread)))
people <- as_tibble (rbindlist (lapply (csv_files[1:85], fread)))

write.csv(main_info,'dtp_2016.csv',row.names = F)
write.csv(ts,'ts_2016.csv',row.names = F)
write.csv(people,'people_2016.csv',row.names = F)


# 2017
csv_files <- list.files (path       = "~/data2/2017/csv_normalized", 
                         pattern    = "*.csv", 
                         full.names = T)

main_info <- as_tibble (rbindlist (lapply (csv_files[171:255], fread)))
ts<-as_tibble (rbindlist (lapply (csv_files[86:170], fread)))
people <- as_tibble (rbindlist (lapply (csv_files[1:85], fread)))

write.csv(main_info,'dtp_2017.csv',row.names = F)
write.csv(ts,'ts_2017.csv',row.names = F)
write.csv(people,'people_2017.csv',row.names = F)



# 2018
csv_files <- list.files (path       = "~/data2/2018tillNov/csv_normalized", 
                         pattern    = "*.csv", 
                         full.names = T)
main_info <- as_tibble (rbindlist (lapply (csv_files[171:255], fread)))
ts<-as_tibble (rbindlist (lapply (csv_files[86:170], fread)))
people <- as_tibble (rbindlist (lapply (csv_files[1:85], fread)))

write.csv(main_info,'dtp_2018.csv',row.names = F)
write.csv(ts,'ts_2018.csv',row.names = F)
write.csv(people,'people_2018.csv',row.names = F)


dtp2015<-read.csv("dtp_2015.csv")
dtp2015[] <- lapply(dtp2015, as.character)

dtp2016<-read.csv("dtp_2016.csv")
dtp2016[] <- lapply(dtp2016, as.character)

dtp2017<-read.csv("dtp_2017.csv")
dtp2017[] <- lapply(dtp2017, as.character)

dtp2018<-read.csv("dtp_2018.csv")
dtp2018[] <- lapply(dtp2018, as.character)

dtp_all<-rbind(dtp2015,dtp2016,dtp2017,dtp2018)
###

ts2015<-read.csv("ts_2015.csv")
ts2016<-read.csv("ts_2016.csv")
ts2017<-read.csv("ts_2017.csv")
ts2018<-read.csv("ts_2018.csv")
ts_all<-rbind(ts2015,ts2016,ts2017,ts2018)

people2015<-read.csv("people_2015.csv")
people2016<-read.csv("people_2016.csv")
people2017<-read.csv("people_2017.csv")
people2018<-read.csv("people_2018.csv")
people_all<-rbind(people2015,people2016,people2017,people2018)

write.csv(people_all,'people_all.csv',row.names = F)
write.csv(dtp_all,'dtp_all.csv',row.names = F)
write.csv(ts_all,'ts_all.csv',row.names = F)


dtp_and_ts<-merge(x = dtp_all, y = ts_all, by = "KartId", all = TRUE)
all<-merge(x = dtp_and_ts, y = people_all, by = "KartId", all = TRUE)
colnames(all)
all_filtered<-all[all$TSID.x==all$TSID.y,]
all<-all_filtered[!is.na(all_filtered$V18),]

write.csv(all,"all.csv",row.names=F)
rm(dtp_all,dtp_and_ts,dtp2015,dtp2016,dtp2017,dtp2018,people2015,people2016,people2017,people2018,ts2015,ts2016,ts2017,ts2018,all_filtered,all1,people_all,ts_all)
