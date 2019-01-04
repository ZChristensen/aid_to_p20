list.of.packages <- c("data.table","ggplot2","plyr")
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
crs$RecipientName[which(crs$RecipientName=="China (People's Republic of)")]="China"
crs$RecipientName[which(crs$RecipientName=="Viet Nam")]="Vietnam"
crs$RecipientName[which(crs$RecipientName=="Democratic Republic of the Congo")]="Congo, Democratic Republic of the"
crs$RecipientName[which(crs$RecipientName=="Congo")]="Congo, Republic of"
crs$RecipientName[which(crs$RecipientName=="Gambia")]="Gambia, The"
crs$RecipientName[which(crs$RecipientName=="Yemen")]="Yemen, Republic of"
crs$RecipientName[which(crs$RecipientName=="Kyrgyzstan")]="Kyrgyz Republic"
crs$RecipientName[which(crs$RecipientName=="Venezuela")]="Venezuela, Republica Bolivariana de"
crs$RecipientName[which(crs$RecipientName==unique(crs$RecipientName[78]))]="Cote d'Ivoire"
crs$RecipientName[which(crs$RecipientName=="West Bank and Gaza Strip")]="West Bank and Gaza"
crs$RecipientName[which(crs$RecipientName=="Egypt")]="Egypt, Arab Republic of"
crs$RecipientName[which(crs$RecipientName=="Former Yugoslav Republic of Macedonia")]="Macedonia, former Yugoslav Republic of"
crs$RecipientName[which(crs$RecipientName=="Saint Lucia")]="St. Lucia"
crs$RecipientName[which(crs$RecipientName=="Swaziland")]="Eswatini"
crs$RecipientName[which(crs$RecipientName=="Iran")]="Iran, Islamic Republic of"
crs$RecipientName[which(crs$RecipientName=="Micronesia")]="Micronesia, Federated States of"

setnames(crs,"RecipientName","CountryName")
setnames(crs,"Year","RequestYear")

clb=c(
  "Afghanistan"
  ,"Benin"                    
  ,"Burundi"
  ,"Central African Republic" 
  ,"Chad"                     
  ,"Congo, Republic of"                    
  ,"Congo, Democratic Republic of the"
  ,"Eritrea"
  ,"Gambia, The" 
  ,"Guinea"
  ,"Guinea-Bissau"            
  ,"Haiti"
  ,"Lesotho"                  
  ,"Liberia" 
  ,"Madagascar" 
  ,"Malawi" 
  ,"Mali"
  ,"Micronesia, Federated States of"
  ,"Mozambique"               
  ,"Niger"
  ,"Nigeria"
  ,"Papua New Guinea"   
  ,"Somalia"
  ,"South Sudan"
  ,"Sudan" 
  ,"Syrian Arab Republic"  
  ,"Togo" 
  ,"Uganda"
  ,"Yemen, Republic of"
  ,"Zambia" 
)

crs$clb=0
crs$clb[which(crs$CountryName %in% clb)]=1


aidtotals=data.table(crs)[
  ,.(
    commitment_value_total=sum(commitment_value,na.rm=T)
    ,disbursement_value_total=sum(disbursement_value,na.rm=T)
  )
  ,by=.(DonorName,RequestYear)
  ]
aidbyclb = data.table(crs)[
  ,.(
    commitment_value=sum(commitment_value,na.rm=T)
    ,disbursement_value=sum(disbursement_value,na.rm=T)
  )
  ,by=.(DonorName,RequestYear,clb)
  ]
aidbyclb=join(aidbyclb,aidtotals,by=c("DonorName","RequestYear"))
aidbyclb$commitment_share=aidbyclb$commitment_value/aidbyclb$commitment_value_total
aidbyclb$disbursement_share=aidbyclb$disbursement_value/aidbyclb$disbursement_value_total
aidbyclb$RequestYear=as.numeric(aidbyclb$RequestYear)
aidbyclb$clb=factor(aidbyclb$clb,levels=c(0,1),labels=c("Other countries","Countries left behind"))

ggplot(data=aidbyclb[which(aidbyclb$RequestYear>2010 & DonorName %in% c("United Kingdom")),], aes(y=commitment_value,x=RequestYear,fill=clb))+
  geom_bar(stat="identity")+
  ylab("Total Aid Commitments (USD millions)")+
  xlab("")+
  ggtitle("UK ODA to countries left behind and other countries")
  # scale_fill_continuous(name="",
  #                    breaks=c(1, 0),
  #                    labels=c("Countries left behind", "Other countries"),
  #                    guide="legend")

ggplot(data=aidbyclb[which(aidbyclb$RequestYear>2000 & DonorName %in% c("United Kingdom")),], aes(y=commitment_share,x=RequestYear,fill=clb))+
  geom_bar(stat="identity")+
  ylab("Share Total Aid Commitments")+
  xlab("")+
  theme(legend.title=element_blank())+
  ggtitle("UK ODA to countries left behind")
