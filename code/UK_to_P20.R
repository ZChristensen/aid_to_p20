list.of.packages <- c("data.table","ggplot2","plyr","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/aid_to_p20")
setwd(wd)

load("data/recipient_donor_year.RData")

crs=crs[which(crs$Year>2000),]
povcalcuts=read.csv("https://raw.githubusercontent.com/ZChristensen/poverty_trends/master/data/P20incometrends.csv")
crs$RecipientName[which(crs$RecipientName=="China (People's Republic of)")]="China"
crs$RecipientName[which(crs$RecipientName=="Viet Nam")]="Vietnam"
crs$RecipientName[which(crs$RecipientName=="Democratic Republic of the Congo")]="Congo, Democratic Republic of"
crs$RecipientName[which(crs$RecipientName=="Congo")]="Congo, Republic of"
crs$RecipientName[which(crs$RecipientName=="Gambia")]="Gambia, The"
crs$RecipientName[which(crs$RecipientName=="Yemen")]="Yemen, Republic of"
crs$RecipientName[which(crs$RecipientName=="Kyrgyzstan")]="Kyrgyz Republic"
crs$RecipientName[which(crs$RecipientName=="Venezuela")]="Venezuela, Republica Bolivariana de"
crs$RecipientName[which(crs$RecipientCode==247)]="Cote d'Ivoire"
crs$RecipientName[which(crs$RecipientName=="West Bank and Gaza Strip")]="West Bank and Gaza"
crs$RecipientName[which(crs$RecipientName=="Egypt")]="Egypt, Arab Republic of"
crs$RecipientName[which(crs$RecipientName=="Former Yugoslav Republic of Macedonia")]="Macedonia, former Yugoslav Republic of"
crs$RecipientName[which(crs$RecipientName=="Saint Lucia")]="St. Lucia"
crs$RecipientName[which(crs$RecipientName=="Swaziland")]="Eswatini"
crs$RecipientName[which(crs$RecipientName=="Iran")]="Iran, Islamic Republic of"
crs$RecipientName[which(crs$RecipientName=="Micronesia")]="Micronesia, Federated States of"

setnames(crs,"RecipientName","CountryName")
setnames(crs,"Year","RequestYear")
povcal=povcalcuts[,c("P20pop","CountryName","RequestYear","ExtPovHC","P20Headcount","pop")]
uniquerecip=unique(povcalcuts$CountryName)
years=c(1981:2017)
expand.df=expand.grid(uniquerecip,years)
names(expand.df)=c("CountryName","RequestYear")
povcal.expand=merge(povcal,expand.df,by=c("CountryName","RequestYear"),all=T)


dat=data.table(povcal.expand)[,.(
  RequestYear=RequestYear
  ,P20Headcount=na.approx(P20Headcount,rule=2)
  ,pop=na.approx(pop,rule=2)
),by=.(CountryName)]

povcal=join(crs,dat,by=c("CountryName","RequestYear"))



povcal$aid_per_personc=povcal$commitment_value/povcal$pop
povcal$aid_per_persond=povcal$disbursement_value/povcal$pop
povcal$highP20=2
povcal$highP20[which(povcal$P20Headcount>.2)]=3
povcal$highP20[which(povcal$CountryName %in% c(
  "Europe, regional"
  , "South of Sahara, regional"
  , "Africa, regional"
  , "West Indies, regional" 
  , "North & Central America, regional"          
  , "America, regional"
  , "Asia, regional"                       
  , "Oceania, regional"                    
  , "Far East Asia, regional" 
  , "Middle East, regional"                
  , "South America, regional"   
  , "North of Sahara, regional"            
  , "Central Asia, regional"   
  , "South & Central Asia, regional" 
  , "South Asia, regional"
))] =1
povcal$highP20[which(povcal$CountryName %in% c("Afghanistan","Somalia","Eritrea","Democratic People's Republic of Korea"))]=1

povcal$highP20=factor(povcal$highP20,levels=c(1,2,3),labels=c("regional aid", "other countries","high P20 headcounts"))



# povcal=povcal[which(povcal$RequestYear %in% c(2015, 2013, 2012, 2011, 2010, 2008, 2005, 2002)),]

aidpercap = data.table(povcal)[
  ,.(
    commitment_value=mean(aid_per_personc,na.rm=T)
    ,disbursement_value=mean(aid_per_persond,na.rm=T)
  )
  ,by=.(DonorName,RequestYear)
  ]

# UK=ggplot(povcal[which(povcal$DonorName %in% c("United Kingdom") & RequestYear==2015)], aes(log(aid_per_personc), P20Headcount, colour=RequestYear))+geom_point()+ggtitle("United Kingdom")
# US=ggplot(povcal[which(povcal$DonorName %in% c("United States") & RequestYear==2015)], aes(log(aid_per_personc), P20Headcount, colour=RequestYear))+geom_point()+ggtitle("United States")
# FR=ggplot(povcal[which(povcal$DonorName %in% c("France") & RequestYear==2015)], aes(log(aid_per_personc), P20Headcount, colour=RequestYear))+geom_point()+ggtitle("France")
# DE=ggplot(povcal[which(povcal$DonorName %in% c("Germany") & RequestYear==2015)], aes(log(aid_per_personc), P20Headcount, colour=RequestYear))+geom_point()+ggtitle("Germany")
# CA=ggplot(povcal[which(povcal$DonorName %in% c("Canada") & RequestYear==2015)], aes(log(aid_per_personc), P20Headcount, colour=RequestYear))+geom_point()+ggtitle("Canada")
ggplot(povcal[which(povcal$DonorName %in% c("United States","United Kingdom", "France","Germany","Canada","Japan") & RequestYear==2015)]
       ,aes(log(aid_per_personc), ExtPovHC, colour=RequestYear))+
      geom_point()+
      ggtitle("Aid Commitments vs Share in Poverty 2015")+
      theme_bw()+
      ylab("Share of recipient \npopulation in extreme poverty")+
      xlab("ODA commitments per capita (logged)")+
      theme(legend.position="none")+
      facet_wrap(~DonorName,ncol=3)
      
ggplot(povcal[which(povcal$DonorName %in% c("United Kingdom") & RequestYear==2015)]
       ,aes(log(aid_per_personc), ExtPovHC, colour=RequestYear))+
  geom_point()+
  ggtitle("UK Aid Commitments vs Share in Poverty")+
  theme_bw()+
  ylab("Share of recipient \npopulation in extreme poverty")+
  xlab("2015 ODA commitments per capita (logged)")+
  theme(legend.position="none")

ggplot(povcal[which(povcal$DonorName %in% c("United Kingdom") & RequestYear==2015)]
       ,aes(log(aid_per_personc), ExtPovHC, colour=RequestYear))+
  geom_point()+
  ggtitle("UK Aid Commitments vs Share in Poverty")+
  theme_bw()+
  ylab("Share of recipient \npopulation in extreme poverty")+
  xlab("2015 ODA commitments per capita (logged)")+
  theme(legend.position="none")

aidtotalhighP20 = data.table(povcal)[
  ,.(
    commitment_value=sum(commitment_value,na.rm=T)
    ,disbursement_value=sum(commitment_value,na.rm=T)
  )
  ,by=.(DonorName,RequestYear,highP20)
  ]

aidtotal = data.table(povcal)[
  ,.(
    commitment_value_total=sum(commitment_value,na.rm=T)
    ,disbursement_value_total=sum(commitment_value,na.rm=T)
  )
  ,by=.(DonorName,RequestYear)
  ]

aidtotalhighP20=join(aidtotalhighP20,aidtotal,by=c("DonorName","RequestYear"))
aidtotalhighP20$share_commitments=aidtotalhighP20$commitment_value/aidtotalhighP20$commitment_value_total
aidtotalhighP20=aidtotalhighP20[order(DonorName,RequestYear,highP20),]
aidtotalhighP20$highP20=factor(aidtotalhighP20$highP20)

ggplot(data=aidtotalhighP20[which(RequestYear>2000 & DonorName %in% c("United Kingdom")),], aes(y=share_commitments,x=RequestYear,fill=highP20))+
  geom_bar(stat="identity")+
  ylab("Total Aid Commitments")+
  xlab("")+
  theme(legend.title=element_blank())+
  ggtitle("UK ODA to countries with high P20 headcounts")

ggplot(data=aidtotalhighP20[which(RequestYear>2000 & DonorName %in% c("United Kingdom") & highP20==1),], aes(y=commitment_value,x=RequestYear))+
  geom_bar(stat="identity")+
  ylab("Total Aid Commitments (USD millions)")+
  xlab("")+
  theme(legend.title=element_blank())+
  ggtitle("UK ODA to countries with high P20 headcounts")

ggplot(data=aidtotalhighP20[which(RequestYear>2000 & DonorName %in% c("United Kingdom","United States","France","Germany","Japan","Canada")),], aes(y=commitment_value,x=RequestYear))+
  geom_bar(stat="identity")+
  ylab("Share Total Aid Commitments")+
  xlab("")+
  theme_bw()+
  theme(legend.title=element_blank())+
  ggtitle("ODA to countries left behind")+
  facet_wrap(~DonorName,ncol=3,scales="free_x")+
  scale_y_continuous(labels=scales::dollar)

ggplot(data=aidtotalhighP20[which(RequestYear>2000 & DonorName %in% c("United Kingdom","United States","France","Germany","Japan","Canada")),], aes(y=share_commitments,x=RequestYear,fill=highP20,group=highP20))+
  geom_area(stat="identity")+
  ylab("Share Total Aid Commitments")+
  xlab("")+
  theme_bw()+
  theme(legend.title=element_blank())+
  ggtitle("ODA to countries with high P20 headcounts")+
  facet_wrap(~DonorName,ncol=3,scales="free_x")+
  scale_y_continuous(labels=scales::percent)
