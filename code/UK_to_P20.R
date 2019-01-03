list.of.packages <- c("data.table","ggplot2")
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

# load("E:/git/p20_hum_cap/project_data/crs.RData")
# 
# 
# # crs = subset(crs,DonorName=="United Kingdom")
# crs = subset(crs,FlowName %in% c("ODA Grants","ODA Loans"))
# 
# crs = data.table(crs)[
#   ,.(
#     commitment_value=sum(usd_commitment_defl,na.rm=T)
#     ,disbursement_value=sum(usd_disbursement_defl,na.rm=T)
#   )
#   ,by=.(DonorName,RecipientCode,RecipientName,Year)
#   ]
# 
# save(crs,file="data/recipient_donor_year.RData")
load("data/recipient_donor_year.RData")

crs=crs[which(crs$Year>2000),]
povcalcuts=read.csv("https://raw.githubusercontent.com/ZChristensen/poverty_trends/master/data/P20incometrends.csv")
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
povcal=povcalcuts[,c("P20pop","CountryName","RequestYear","ExtPovHC","P20Headcount","pop")]
povcal=join(crs,povcal,by=c("CountryName","RequestYear"))
povcal$aid_per_person_in_p20=povcal$commitment_value/povcal$P20pop

ggplot(povcal, aes(P20pop, disbursement_value, colour=DonorName))+geom_point()

UK2015=povcal[which(povcal$DonorName=="United Kingdom" & povcal$RequestYear==2015),]
plot(log(UK2015$aid_per_person_in_p20),UK2015$P20Headcount)
plot(log(UK2015$aid_per_person_in_p20),UK2015$P20pop)
