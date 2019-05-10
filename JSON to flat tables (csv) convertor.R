library("jsonlite")
library("RJSONIO")
library("plyr")
library("dplyr")
options(stringsAsFactors = F)

dirs<-c("2015","2016","2017","2018tillNov")

for (papka in 1:4) {

setwd(paste0("/home/alexander/Рабочий стол/dissertation/data/",dirs[papka]))  
files<-list.files()

for (file in 1:85){


setwd(paste0("/home/alexander/Рабочий стол/dissertation/data/",dirs[papka]))  
json_file<-fromJSON(files[file])
json_file<-fromJSON(json_file)
json_file$region_name

### GETTING MAIN INFORMATION ----
main_info<-lapply(json_file$cards, function(x) c(x['KartId'],x['rowNum'],x['date'],
                                                 x['Time'],x['District'],x['DTP_V'],
                                                 x['POG'],x['RAN'],x['K_TS'],x['K_UCH']))
main_info <- data.frame(matrix(unlist(main_info), nrow=length(main_info), byrow=T),stringsAsFactors=FALSE)
ndu<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['ndu']))
sdor<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['sdor']))
n_p<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['n_p']))
street<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['n_p']))
house<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['house']))
dor<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['dor']))
km<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['km']))
m<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['m']))
k_ul<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['k_ul']))
dor_k<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['dor_k']))
dor_z<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['dor_z']))
s_pog<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['s_pog']))
s_pch<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['s_pch']))
osv<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['osv']))
change_org_motion<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['change_org_motion']))
s_dtp<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['s_dtp']))
COORD_W<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['COORD_W']))
COORD_L<-lapply(json_file$cards, function(x) c(x[['infoDtp']]['COORD_L']))

ndu<-sapply(ndu, paste, collapse = " ")
sdor<-sapply(sdor, paste, collapse = " ")
n_p<-sapply(n_p, paste, collapse = " ")
street<-sapply(street, paste, collapse = " ")
house<-sapply(house, paste, collapse = " ")
dor<-sapply(dor, paste, collapse = " ")
km<-sapply(km, paste, collapse = " ")
m<-sapply(m, paste, collapse = " ")
k_ul<-sapply(k_ul, paste, collapse = " ")
dor_k<-sapply(dor_k, paste, collapse = " ")
dor_z<-sapply(dor_z, paste, collapse = " ")
s_pog<-sapply(s_pog, paste, collapse = " ")
s_pch<-sapply(s_pch, paste, collapse = " ")
osv<-sapply(osv, paste, collapse = " ")
change_org_motion<-sapply(change_org_motion, paste, collapse = " ")
s_dtp<-sapply(s_dtp, paste, collapse = " ")
COORD_W<-sapply(COORD_W, paste, collapse = " ")
COORD_L<-sapply(COORD_L, paste, collapse = " ")

main_info<-cbind(main_info,ndu,sdor,n_p,street,house,dor,km,m,k_ul,dor_k,dor_z,s_pog,s_pch,osv,change_org_motion,s_dtp,COORD_W,COORD_L)
colnames(main_info)<-c("KartId","rowNum","date","Time","District","DTP_V","POG","RAN","K_TS","K_UCH",
                  "ndu","sdor","n_p","street","house","dor","km","m","k_ul","dor_k","dor_z","s_pog","s_pch","osv",
                  "change_org_motion","s_dtp","COORD_W","COORD_L")

rm(ndu,sdor,n_p,street,house,dor,km,m,k_ul,dor_k,dor_z,s_pog,s_pch,osv,change_org_motion,s_dtp,COORD_W,COORD_L)
### END OF COLLECTING MAIN INFO -----
### START OF COLLECTING 2 LVL INFO -----


####  IF SEVERAL TS in DTP:

ts_counter<-lapply(json_file$cards, function(x) length(c(x[['infoDtp']][['ts_info']])))
ts_counter<-unlist(ts_counter)
max_ts_n<-max(ts_counter)

cnt<-1:max_ts_n
info_about_ts<-list()
info_about_people<-list()
person<-1

for (i in 1:max_ts_n){
events<-which(ts_counter>cnt[i]-1)

cards<-json_file$cards
selected_cards<-cards[events]

n_ts<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['n_ts']))
ts_s<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['ts_s']))
t_ts<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['t_ts']))
marka_ts<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['marka_ts']))
m_ts<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['m_ts']))
color<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['color']))
r_rul<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['r_rul']))
g_v<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['g_v']))
m_pov<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['m_pov']))
t_n<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['t_n']))
f_sob<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['f_sob']))
o_pf<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]]['o_pf']))

n_ts<-sapply(n_ts, paste, collapse = " ")
ts_s<-sapply(ts_s, paste, collapse = " ")
t_ts<-sapply(t_ts, paste, collapse = " ")
marka_ts<-sapply(marka_ts, paste, collapse = " ")
m_ts<-sapply(m_ts, paste, collapse = " ")
color<-sapply(color, paste, collapse = " ")
r_rul<-sapply(r_rul, paste, collapse = " ")
g_v<-sapply(g_v, paste, collapse = " ")
m_pov<-sapply(m_pov, paste, collapse = " ")
t_n<-sapply(t_n, paste, collapse = " ")
f_sob<-sapply(f_sob, paste, collapse = " ")
o_pf<-sapply(o_pf, paste, collapse = " ")


info_about_ts[[i]]<-data.frame(cbind("KartId"=main_info$KartId[events],"TSID"=i,n_ts,ts_s,t_ts,marka_ts,m_ts,color,r_rul,g_v,m_pov,t_n,f_sob,o_pf))
rm(n_ts,ts_s,t_ts,marka_ts,m_ts,color,r_rul,g_v,m_pov,t_n,f_sob,o_pf)
}


info_about_ts<-bind_rows(info_about_ts)
rm(selected_cards,i,events)


### END OF COLLECTING INFO ABOUT TS ----
### START OF COLLECTING INFO ABOUT PEOPLE ----
k<-1

for(i in 1:max_ts_n){


events<-which(ts_counter>cnt[i]-1)

cards<-json_file$cards
selected_cards<-cards[events]

uch_counter<-lapply(selected_cards, function(x) length(c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']])))


uch_counter<-unlist(uch_counter)
max_uch_n<-max(uch_counter)

cnt2<-1:max_uch_n

for (j in 1:max_uch_n){

  events_dop<-events[which(uch_counter>cnt2[j]-1)]
  selected_cards<-cards[events_dop]


KartIds<-lapply(selected_cards, function(x) c(x[['KartId']]))
K_UCH<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['K_UCH']))
NPDD<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['NPDD']))
S_T<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['S_T']))
POL<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['POL']))
V_ST<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['V_ST']))
ALCO<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['ALCO']))
SOP_NPDD<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['SOP_NPDD']))
SAFETY_BELT<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['SAFETY_BELT']))
S_SM<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['S_SM']))
N_UCH<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['N_UCH']))
S_SEAT_GROUP<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['S_SEAT_GROUP']))
INJURED_CARD_ID<-lapply(selected_cards, function(x) c(x[['infoDtp']][['ts_info']][[i]][['ts_uch']][[j]]['INJURED_CARD_ID']))

KartIds<-sapply(KartIds, paste, collapse = " ")
K_UCH<-sapply(K_UCH, paste, collapse = " ")
NPDD<-sapply(NPDD, paste, collapse = " ")
S_T<-sapply(S_T, paste, collapse = " ")
POL<-sapply(POL, paste, collapse = " ")
V_ST<-sapply(V_ST, paste, collapse = " ")
ALCO<-sapply(ALCO, paste, collapse = " ")
SOP_NPDD<-sapply(SOP_NPDD, paste, collapse = " ")
SAFETY_BELT<-sapply(SAFETY_BELT, paste, collapse = " ")
S_SM<-sapply(S_SM, paste, collapse = " ")
N_UCH<-sapply(N_UCH, paste, collapse = " ")
S_SEAT_GROUP<-sapply(S_SEAT_GROUP, paste, collapse = " ")
INJURED_CARD_ID<-sapply(INJURED_CARD_ID, paste, collapse = " ")


temp<-data.frame(cbind("KartId"=KartIds,"TSID"=i,"PersonID"=j,K_UCH,NPDD,S_T,POL,V_ST,ALCO,SOP_NPDD,SAFETY_BELT,S_SM,N_UCH,S_SEAT_GROUP,INJURED_CARD_ID))


if (length(colnames(temp))==15) { info_about_people[[k]]<-temp;k<-k+1}



}
}

info_about_people<-bind_rows(info_about_people)
#info_about_people[[36]]


setwd(paste0("/home/alexander/Рабочий стол/dissertation/data2/",dirs[papka],"/csv_normalized/")) 
print(cat("papka nomer ",papka," file nomer ",file))

nmi<-length(colnames(main_info))
main_info[,nmi+1]<-json_file[["year"]]
main_info[,nmi+2]<-json_file[["region_code"]]
main_info[,nmi+3]<-json_file[["region_name"]]

niats<-length(colnames(info_about_ts))
info_about_ts[,niats+1]<-json_file[["year"]]
info_about_ts[,niats+2]<-json_file[["region_code"]]
info_about_ts[,niats+3]<-json_file[["region_name"]]

niap<-length(colnames(info_about_people))
info_about_people[,niap+1]<-json_file[["year"]]
info_about_people[,niap+2]<-json_file[["region_code"]]
info_about_people[,niap+3]<-json_file[["region_name"]]

year<-json_file[["year"]]
region_name<-json_file[["region_name"]]
region_code<-json_file[["region_code"]]

write.csv(main_info,paste0("main_info_",year,"_",region_code,"_",region_name,".csv"))
write.csv(info_about_ts,paste0("info_about_ts_",year,"_",region_code,"_",region_name,".csv"))
write.csv(info_about_people,paste0("info_about_people_",year,"_",region_code,"_",region_name,".csv"))

}}

