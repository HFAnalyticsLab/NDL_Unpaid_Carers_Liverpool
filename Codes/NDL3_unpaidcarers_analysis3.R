#---------------------------
# script to derive the list of informal carers
# from Liverpool and Wirral
# and their 'index' date
#
# carers may be registered as informal carers at their gp
# hence (carer_min_date in liverpool and gp_start date in wirral)
# or receive support from social services as carers (client type = carer)
# with starting date asc_min_date or asc_start_date
# or both

#author R. Piroddi
#date: April, 2023

setwd('K:/Information & Analysis/Work Areas/UoL_analysis/Projects/NDL3_unpaidcarers')


library(data.table)
library(sf) 
library(dplyr)
library(ggplot2)
library(janitor)
library(readxl)
library(statar)

library(ggthemes)
library(RColorBrewer)
library(colorspace)

library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(gee)
library(ggplot2)
library("survival")
library(survminer)
library(readr)
library(MatchIt)
library(survey)
library(tableone)
library(naniar)
library(gdata)
library(gtools)

`%notin%` <- Negate(`%in%`)

#IMPORTANT - optional code below compiles .csv to creat tables: w_gp_all 
#w_asc_all, w_gp_all, l_asc_all1, l_asc_all2


#create ASC tables
#----ASC DATA---#
#compile data from multiple .csv files
fileDT = data.table(fn=list.files('K:/Information & Analysis/Work Areas/UoL_analysis/Data/SourceData/ASC'))
fileDT <- fileDT[!grepl('cm|addit|Wirral', fn)]
fileDT[, id := as.character(.I)]
p <- "K:/Information & Analysis/Work Areas/UoL_analysis/Data/SourceData/ASC/"
fileDT[, contents := .(lapply(fn, function(n) fread(file.path(p, n))))]

bkp1 <- fileDT
#fileDT <- bkp1


#create vectors from list items
linkpseudo <- unlist(sapply(fileDT$contents, "[[", 3))
#check
#t <- table(linkpseudo)
#t[t>100]
#ERROR REPEATED ID CHECK '5A55D265479E09113D9B531E64D237942539FEAF7489B3684B6F6E8637C97C71' 
type <- unlist(sapply(fileDT$contents, "[[", 12))
#start date
std <- sapply(fileDT$contents, "[[", 19)
std <- ymd(do.call("c",std))
#end date
edt <- sapply(fileDT$contents, "[[", 20)
edt <- ymd(do.call("c",edt))
edt[is.na(edt)] <- '2021-12-31'
edt[grep('1899|1900', edt)] <- '2021-12-31'
#birth year
bt_yr <- unlist(sapply(fileDT$contents, "[[", 6))  
bt_yr <- as.numeric(paste(bt_yr))
#has carer?
#has_carer <- unlist(sapply(fileDT$contents, "[[", 11))
#gender
gender <- unlist(sapply(fileDT$contents, "[[", 4))
gender[gender==1] <- 'Male'
gender[gender==2] <- 'Female'
gender[gender==0] <- 'Unknown'
event_type <- unlist(sapply(fileDT$contents, "[[", 16))


#vectors to data.frame
asc_L <- data.frame('linkpseudo'=linkpseudo, 'gender'=gender, 'type'=type,  'event_type'=event_type,'birth_year'=bt_yr, 'event_start_date'=std, 'event_end_date'=edt)

#REMOVE ERROR IDs (e.g. '1','2', and is repeated: '5A55D265479E09113D9B531E64D237942539FEAF7489B3684B6F6E8637C97C71')
setDT(asc_L)
#remove errors
asc_L <- asc_L[!is.na(linkpseudo),]
asc_L <- asc_L[!linkpseudo %in% '1',]
asc_L <- asc_L[!linkpseudo %in% '2',]
asc_L <- asc_L[!linkpseudo %in% '5A55D265479E09113D9B531E64D237942539FEAF7489B3684B6F6E8637C97C71',]

asc_L <- asc_L[gender  %in% c('Male', 'Female', 'Unknown')]


#limit dates

asc_L[,carer_min_date := min(event_start_date), by=linkpseudo]
asc_L$carer_min_date <- as.Date(asc_L$carer_min_date)#format='%Y-%m-%d'
asc_L[,carer_max_date := max(event_end_date), by=linkpseudo]
asc_L$carer_max_date <- as.Date(asc_L$carer_max_date, format='%Y-%m-%d')
asc_L[is.na(carer_max_date)]$carer_max_date <- as.Date('2016-12-31', format='%Y-%m-%d')

asc_L[,carer_min_year := as.numeric(format(carer_min_date, format('%Y')))]
asc_L[,age_min_carer := carer_min_year-birth_year]
setDT(asc_L)
#remove who became carer before date range
asc_L <- asc_L[!carer_min_year < 2016]



#-----------------------------------------
# load data

# wirral data has two simple lists
# liverpool carers data is within the gp whole population data
# and adult social care data comes in two batches

w_gp_all <- fread("./data/wirral_gp_carers_min_date_2016_2021.csv")
w_asc_all <- fread("./data/wirral_asc_carers.csv")

l_gp_all <- fread("./data/gp_liverpool_carercohort_1.csv")
l_asc_all1 <- fread("./data/ASC_liverpool_carercohort_2.csv")
l_asc_all2 <- fread("./data/ASC_additional.csv")


setnames(w_gp_all, names(w_gp_all), tolower(names(w_gp_all)))
setnames(w_asc_all, names(w_asc_all), tolower(names(w_asc_all)))

setnames(l_gp_all, names(l_gp_all), tolower(names(l_gp_all)))
setnames(l_asc_all1, names(l_asc_all1), tolower(names(l_asc_all1)))
setnames(l_asc_all2, names(l_asc_all2), tolower(names(l_asc_all2)))


#--------------------------------------
# wirral

# 2 simple lists

w_gp_all[, carer_min_date:=as.IDate(carer_min_date, "%d/%m/%Y")]         
w_asc_all[, asc_start_date:=as.IDate(asc_start_date)]

w_gp<-w_gp_all[year(carer_min_date)>2015 & year(carer_min_date)<2022,]
w_asc<-w_asc_all[year(asc_start_date)>2015 & year(asc_start_date)<2022,]
w_gp[, wirral_gp:=1]
w_gp <- w_gp[!duplicated(cm_pseudo)]
# 6823 unique people
w_asc <- w_asc[!duplicated(cm_pseudo)]
# 4962 unique people
w_asc[, wirral_asc:=1]

w_carers <- merge(w_gp,w_asc,by="cm_pseudo", all.x=TRUE,all.y=TRUE) 
# 11662 unique people
table(w_carers$wirral_gp, w_carers$wirral_asc,useNA = "ifany" )
nrow(w_carers[!is.na(carer_min_date) & !is.na(asc_start_date)])
num_w_carers_gp_and_asc <- nrow(w_carers[!is.na(carer_min_date) & !is.na(asc_start_date)])
# 123 - people that are known as carers in both gp and asc 
# low 

# join w_carers with
# ./scripts/Wirral_cldat_2021.csv
# to get the characteristics of the carers


#-------------------------------
# liverpool


# data had more complex structure
#l_gp_all <- fread("gp_liverpool_carercohort_1.csv")
# this file contains all people registered in gps in liverpool

l_gp<-l_gp_all[ !is.na(carer_min_date) & year(carer_min_date)>2015 & year(carer_min_date)<2022 & age_min_carer>17]
# 10522 unique people

l_asc_1<-l_asc_all1[type=="Carer" & year(carer_min_date)>2015 & year(carer_min_date)<2022,]
l_asc_1 <- l_asc_1[!duplicated(linkpseudo)]
# 3338 unique people

l_asc_2<-l_asc_all2[!'client type'=="NULL" & year(asc_min_date)>2015 & year(asc_min_date)<2022,]
l_asc_2 <- l_asc_2[!duplicated(linkpseudo)]
# 2471 unique people

l_asc <- merge(l_asc_1[,.(linkpseudo,asc_min_date_1 = carer_min_date)], l_asc_2[,.(linkpseudo,asc_min_date_2 = asc_min_date)], by="linkpseudo", all.x=TRUE, all.y=TRUE)

l_asc[ , asc_min_date:= asc_min_date_1]

l_asc[is.na(asc_min_date_1) , asc_min_date:= asc_min_date_2]

l_asc <- l_asc[,.(linkpseudo, asc_min_date)]
# 4501 unique people
l_gp[, liv_gp:=1]
l_asc[, liv_asc:=1]
l_carers <- merge(l_gp[,.(linkpseudo,gp_start_date = carer_min_date, liv_gp)],l_asc,by="linkpseudo", all.x=TRUE,all.y=TRUE) 
# 14010 unique people

num_l_carers_gp_and_asc <- nrow(l_carers[!is.na(gp_start_date) & !is.na(asc_min_date)])
# 1013 - people that are known as carers in both gp and asc 

# for liverpool the file "gp_liverpool_carercohort_1.csv"
# has all the characteristics
table(l_carers$liv_gp, l_carers$liv_asc, useNA = "ifany")

l_carers[, place:="Liverpool"]
w_carers[, place:="Wirral"]
#LSOAs
gp_data_W[,c(1,11)]
#l_carers <- merge(l_carers, gp_data_L[,c(3,33)], by='linkpseudo')
#w_carers <- merge(w_carers, gp_data_W[,c(1,11)], by.x='cm_pseudo', by.y = 'Pseudo_NHS_Number')
ls<-list(l_carers, w_carers)
all_carers<-rbindlist(ls, fill = T)
all_carers[, gp:=as.numeric(wirral_gp==1|liv_gp==1)]
all_carers[, asc:=as.numeric(wirral_asc==1|liv_asc==1)]
with(all_carers, table(gp, asc, useNA = "ifany"))

#integrate linkpseudo and cm_pseudo
all_carers[, c('linkpseudo1', 'linkpseudo_na' ) := .(fcoalesce(linkpseudo, cm_pseudo), +(!is.na(linkpseudo)))]

uniqueN(all_carers$linkpseudo1) #25672

# wirral
# ------------------------
# 6823 unique people registered at gp as carers
# 4962 unique people receiving social care support as carers
# 11662 unique people in total
# 123 - people that are known as carers in both gp and asc 

#liverpool
#---------------------------------------
# 10522 unique people registered at gp as carers
# 4501 unique people receiving social care support as carers
# 14010 unique people in total
# 1013 - people that are known as carers in both gp and asc

#--------------
# total cohort
# 25672 

#-----------

#IMD
sheets <- excel_sheets('K:/Information & Analysis/Work Areas/UoL_analysis/Projects/NDL3_unpaidcarers/data/IMD_Eng_Wls.xlsx')
imd <- read_excel('K:/Information & Analysis/Work Areas/UoL_analysis/Projects/NDL3_unpaidcarers/data/IMD_Eng_Wls.xlsx', grep('IMD', sheets))
names(imd)[2] <- 'name'
imd <- setDT(imd)[grepl('Wirral|Liverpool', imd$name),][,c(1,6)]
names(imd) <- c('lsoa_11', 'imd_decile')

#LSOA polys
sp_lsoa <- st_read("shp/Wirral_Liverpool_LSOA_bnds.shp")
#sp_lsoa <- sp_lsoa[grep('Liverpool|Wirral',sp_lsoa$LSOA11NM), ]
names(sp_lsoa)[1] <- 'lsoa_11'



#clinical data
#Wirral
cl_W <- fread('K:/Information & Analysis/Work Areas/UoL_analysis/Data/CM/datasets/Wirral_cldat_2021.csv')

gp_data_W <- cl_W



#need to age groups for denominators
agebreaks <- c(18, 30, 40, 50, 60, 70, 80, 110)
agelabels <- c('18-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')
#setDT(gp_asc)[ , age_group_2022 := cut(as.numeric(age.2022), breaks = agebreaks, right = FALSE, labels = agelabels)]
gp_data_W$age.2022 <- as.numeric(gp_data_W$age.2022)
setDT(gp_data_W)[ , age_group_2022 := cut(age.2022, breaks = agebreaks, right = FALSE, labels = agelabels)]


gp_data_W[Frailty==1 | Dementia==1 , frailty_or_dementia := 1]

#LIverpool
#
#gp_data_L <- fread('K:/Information & Analysis/Work Areas/UoL_analysis/Data/MainCohort/liverpoolcohort_characteristics_v5.csv')
#setDT(gp_data_L)
#bkp <- gp_data_L

#find carer date
gp_data_L <- l_gp_all
gp_data_L$carerdate <- dmy(gp_data_L$carerdate)
gp_data_L$carerdate <- as.Date(gp_data_L$carerdate)
gp_data_L[,carer_min_date := min(carerdate), by=linkpseudo]
gp_data_L[,carer_max_date := max(carerdate), by=linkpseudo]
gp_data_L[,carer_min_year := as.numeric(format(carer_min_date, format='%Y'))]

#age became carer
gp_data_L[, age_min_carer:=as.numeric(carer_min_year-date.of.birth)]

#age
gp_data_L[, age.2022:=as.numeric(2022-date.of.birth)]

#need to age groups for denominators
agebreaks <- c(18, 30, 40, 50, 60, 70, 80, 110)
agelabels <- c('18-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')
#setDT(gp_asc)[ , age_group_2022 := cut(as.numeric(age.2022), breaks = agebreaks, right = FALSE, labels = agelabels)]
gp_data_L$age.2022 <- as.numeric(gp_data_L$age.2022)
setDT(gp_data_L)[ , age_group_2022 := cut(age.2022, breaks = agebreaks, right = FALSE, labels = agelabels)]


#add carer flags to GP data EDIT
#gp_data_L[linkpseudo %in% l_gp$linkpseudo, carer_gp_flag := 1]
#gp_data_L[linkpseudo %in% l_asc$linkpseudo, carer_asc_flag := 1]
#id codes for Wirral?
#gp_data_W[Pseudo_NHS_Number %in% w_gp$cm_pseudo, carer_gp_flag := 1]
#gp_data_W[Pseudo_NHS_Number %in% w_asc$cm_pseudo, carer_asc_flag := 1]

#join relevant columns
#need to add to join: IMD decile, carer_min_date, age_min_carer, age_group_min_carer
gp_data_Lx <- gp_data_L[,c(3,6,8,9,11,33,43,47,48,50,51,54,59,66)]
names(gp_data_Lx) <- c('linkpseudo1', 'birth_year', 'gender', 'ethnic_group', 'pUPRN', 'lsoa_11',
                       'frailty_or_dementia', "learning_disabilities", "physical_disability",
                       "copd","cancer","cvd", "depression","rheumatology")
gp_data_Lx[,region_name := 'Liverpool']

gp_data_Wx <- gp_data_W[,c(1,5,7,8,11,12,42,44,47,56,64,73,88,275)]
names(gp_data_Wx) <- c('linkpseudo1', 'birth_year', 'gender', 'ethnic_group', 'lsoa_11', 'pUPRN',
                       'cancer', "learning_disabilities", "physical_disability",
                       "depression", "cvd","rheumatology","copd","frailty_or_dementia")

gp_data_Wx[,region_name := 'Wirral']


gp_asc_LWx <- rbind(gp_data_Lx, gp_data_Wx)

names(all_carers)[12] <- 'carer_gp_flag'
names(all_carers)[13] <- 'carer_asc_flag'

#CHECK: do IDs from lists match clinical IDs?
uniqueN(w_carers$cm_pseudo) #10325
sum(w_carers$cm_pseudo %in% gp_data_W$Pseudo_NHS_Number==TRUE) #10325
uniqueN(l_carers$linkpseudo) #13667
sum(l_carers$linkpseudo %in% gp_data_L$linkpseudo==TRUE) #13667

uniqueN(all_carers$linkpseudo1)

#(11662-10325)+(14010-13667)=1680 missing from clinical records

#FIX lots of LSOAs missing...


gp_asc_LWx <- merge(gp_asc_LWx, all_carers, by='linkpseudo1', all.x = T, all.y = T)
#total with either cm_pseudo or linkpseudo = 25,674
#CHECK and carers with cm_pseudo AND linkpseudo?
uniqueN(gp_asc_LWx[carer_gp_flag==1|carer_asc_flag==1 & is.na(linkpseudo)]$cm_pseudo) #NO
uniqueN(gp_asc_LWx[carer_gp_flag==1|carer_asc_flag==1 & is.na(cm_pseudo)]$linkpseudo) #NO

#
gp_asc_LWx[is.na(carer_asc_flag)]$carer_asc_flag <- 0
gp_asc_LWx[is.na(carer_gp_flag)]$carer_gp_flag <- 0

uniqueN(gp_asc_LWx[carer_asc_flag==1|carer_gp_flag==1]$linkpseudo1) #25672


#CHECK: all carers have a DoB/year
uniqueN(gp_asc_LWx[carer_gp_flag==1|carer_asc_flag==1 & is.na(birth_year)]$linkpseudo1)
uniqueN(gp_asc_LWx[linkpseudo1 %in% l_carers$linkpseudo & !is.na(birth_year)]$linkpseudo1)
uniqueN(gp_asc_LWx[linkpseudo1 %in% w_carers$cm_pseudo & !is.na(birth_year)]$linkpseudo1)

#3145 carers DON'T have a birth year
#Possibly they are missing from the record...
#DoBs are in clinical data... so where did they go?
uniqueN(gp_data_W[Pseudo_NHS_Number %in% w_carers$cm_pseudo & !is.na(Year_of_Birth)])#10325
uniqueN(gp_data_L[linkpseudo %in% l_carers$linkpseudo & !is.na(date.of.birth)]) #13667

#get earliest date CHECK COLUMNS ARE CORRECT ONES
x <- gp_asc_LWx[,c(17, 19, 23,25)]
x[,min_carer_date_all := apply(x, 1, min, na.rm=T)]

gp_asc_LWx$carer_min_date_all <- x[,5]

gp_asc_LWx$min_carer_date_all <- as.Date(x$min_carer_date_all,format = '%Y-%m-%d')
gp_asc_LWx[,min_carer_year_all := as.numeric(format(min_carer_date_all, format='%Y'))]
#check all the dates are there
uniqueN(gp_asc_LWx[carer_gp_flag==1 & is.na(min_carer_date_all)]$linkpseudo1) #=0, no NAs
uniqueN(gp_asc_LWx[carer_asc_flag==1 & is.na(min_carer_date_all)]$linkpseudo1) #=0, no NAs
#
uniqueN(gp_asc_LWx[carer_gp_flag==1|carer_asc_flag==1]$linkpseudo1) # 25672
uniqueN(gp_asc_LWx[carer_gp_flag==1|carer_asc_flag==1 & is.na(birth_year)]$linkpseudo1) #22527

#age.2022
gp_asc_LWx[,age.2022 := 2022-as.numeric(birth_year)] 
#NOTE 
agebreaks <- c(18, 30, 40, 50, 60, 70, 80, 110)
agelabels <- c('18-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')
gp_asc_LWx[ , age_group := cut(age.2022, breaks = agebreaks, right = FALSE, labels = agelabels)]


#age/groups
#NOTE birth_year formats are different - just extract year to avoid NAs
#check do all carers have a birth year
#nrow(gp_asc_LWx[is.na(birth_year) & carer_asc_flag==1]) =0
#nrow(gp_asc_LWx[is.na(birth_year) & carer_gp_flag==1]) =0
gp_asc_LWx[,birth_year := as.numeric(str_sub(birth_year, 1, 4))]
gp_asc_LWx[, carer_min_age := min_carer_year_all-as.numeric(birth_year)]

agebreaks <- c(18, 30, 40, 50, 60, 70, 80, 110)
agelabels <- c('18-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')
gp_asc_LWx[ , age_group_min_carer := cut(carer_min_age, breaks = agebreaks, right = FALSE, labels = agelabels)]

#gender tidy
gp_asc_LWx[gender %in% 'F']$gender <- 'Female'
gp_asc_LWx[gender %in% 'M']$gender <- 'Male'

#tidy ethnic groups
gp_asc_LWx[grepl('Not', ethnic_group)]$ethnic_group <- 'Unknown'
gp_asc_LWx[nchar(ethnic_group)==0]$ethnic_group  <- 'Unknown'
gp_asc_LWx[grep('Other Ethnic', ethnic_group)]$ethnic_group <- 'Other ethnic groups'

table(gp_asc_LWx[carer_asc_flag==1|carer_gp_flag==1]$ethnic_group)

#imd
gp_asc_LWx <- merge(gp_asc_LWx, imd, by='lsoa_11', all.x = T, all.y = T)

#check the counts 
gp_asc_LWx[is.na(carer_gp_flag)]$carer_gp_flag <- 0
gp_asc_LWx[is.na(carer_asc_flag)]$carer_asc_flag <- 0
uniqueN(gp_asc_LWx[carer_gp_flag==1 & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo1) #GP 17225
uniqueN(gp_asc_LWx[carer_asc_flag==1 & carer_gp_flag==0 & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo11) #ASC & !GP 6668
uniqueN(gp_asc_LWx[carer_asc_flag==1 & carer_gp_flag==1 & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo11) #ASC & GP 1132
uniqueN(gp_asc_LWx[carer_asc_flag==1 | carer_gp_flag==1 & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo11) #ASC | GP 23,893

#Liverpool
uniqueN(gp_asc_LWx[carer_asc_flag==1 & region_name %in% 'Liverpool' & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo11) #GP 4,143
uniqueN(gp_asc_LWx[(carer_asc_flag==1 & carer_gp_flag==0) & region_name %in% 'Liverpool' & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo11) #ASC & !GP 3,132
uniqueN(gp_asc_LWx[(carer_asc_flag==1 & carer_gp_flag==1)& region_name %in% 'Liverpool' & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo11) #ASC & GP 1,011
uniqueN(gp_asc_LWx[(carer_asc_flag==1 | carer_gp_flag==1) & region_name %in% 'Liverpool' & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo11) #ASC | GP 13,652
#Wirral
uniqueN(gp_asc_LWx[region_name %in% 'Wirral' & carer_asc_flag==1 & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo11) #GP 3,657
uniqueN(gp_asc_LWx[(carer_asc_flag==1 & carer_gp_flag==0) & region_name %in% 'Wirral' & (min_carer_year_all>2015 & min_carer_year_all<2022)]$linkpseudo11) #ASC & !GP 3,536
uniqueN(gp_asc_LWx[(carer_asc_flag==1 & carer_gp_flag==1)& region_name %in% 'Wirral']) #ASC & GP 121
uniqueN(gp_asc_LWx[(carer_asc_flag==1 | carer_gp_flag==1) & place %in% 'Wirral']) #ASC | GP 11,662




#carers cohorts by age group, gender, ethnic group
#asc
carer_cohorts_asc <- gp_asc_LWx[(carer_asc_flag==1&carer_gp_flag==0) & min_carer_year_all>2015 & min_carer_year_all<2022][, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(age_group_min_carer, ethnic_group, gender)]
carer_cohorts_asc[, regd := 'asc']
#gp
carer_cohorts_gp <- gp_asc_LWx[carer_gp_flag==1&carer_asc_flag==0 & min_carer_year_all>2015 & min_carer_year_all<2022][, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(age_group_min_carer, ethnic_group, gender)]
carer_cohorts_gp[, regd:= 'gp']
#gp&asc
carer_cohorts_gp_and_asc <- gp_asc_LWx[carer_gp_flag==1&carer_asc_flag==1 &  min_carer_year_all>2015 & min_carer_year_all<2022][, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(age_group_min_carer, ethnic_group, gender)]
carer_cohorts_gp_and_asc[, regd:= 'gp_and_asc']
#gp|sc
carer_cohorts_gp_or_asc <- gp_asc_LWx[carer_gp_flag==1|carer_asc_flag==1 &  min_carer_year_all>2015 & min_carer_year_all<2022][, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(age_group_min_carer, ethnic_group, gender)]
carer_cohorts_gp_or_asc[, regd:= 'gp_or_asc']

carer_cohorts_table <- rbind(carer_cohorts_asc, carer_cohorts_gp, carer_cohorts_gp_and_asc, carer_cohorts_gp_or_asc)
names(carer_cohorts_table)[1] <- 'age_group'

#tidy
gp_asc_LWx[is.na(ethnic_group)]$ethnic_group <- 'Unknown'
gp_asc_LWx[is.na(gender) | gender %in% 'U' | gender %in% 'Not specified' | nchar(gender)==0]$gender <- 'Unknown'
carer_cohorts_table[is.na(gender) | gender %in% 'U' | gender %in% 'Not specified' | nchar(gender)==0]$gender <- 'Unknown'

carer_cohorts_table[is.na(ethnic_group)]$ethnic_group <- 'Unknown'

#count all adults (sampled)
denoms <- gp_asc_LWx[age.2022>17 & age.2022<90, .(count_all=uniqueN(linkpseudo1)), by=.(gender, age_group, ethnic_group)]

carer_cohorts_table <- merge(carer_cohorts_table, denoms, by=c('ethnic_group', 'age_group', 'gender'), all.x=T, all.y=T)

sum(carer_cohorts_table[regd %in% 'gp_or_asc']$count) #25672

#rates
carer_cohorts_table[, rate := round(count*100/count_all,2)]

#write
fwrite(carer_cohorts_table, './tables/carer_counts_rates_table.csv')
#supress
hf_table <- carer_cohorts_table
hf_table[count<=5]$count <- NA
fwrite(hf_table, './tables/HF_carer_counts_table.csv')

#--plots--#
#carer rate pyramid plot
carer_cohorts_table$rate <- ifelse(carer_cohorts_table$gender == "Male", -1*carer_cohorts_table$rate, carer_cohorts_table$rate)
ggplot(carer_cohorts_table[regd %in% 'gp_or_asc' & !is.na(age_group) & ethnic_group %notin% 'Unknown' & gender %in% c('Male', 'Female')],
       aes(x = rate,
           y = age_group          ,
           fill = gender)) +
  geom_bar(position="stack", stat="identity") +
  geom_col() +
  xlab("Carers per 100") + ylab("Age group") +
  theme(legend.title=element_blank()) +
  #scale_x_continuous(breaks=seq(-10,0,10)),labels=abs(seq(-10,0,10)) + 
  facet_wrap(~ethnic_group) 
ggsave("./figures/carer_count_age_ethnic_gender.png", width = 8, height = 6, units = 'in')

#age group, gender and imd
carer_imd <- gp_asc_LWx[carer_asc_flag==1|carer_gp_flag==1][, list(
  count=uniqueN(linkpseudo1)),
  by=.(age_group_min_carer, imd_decile, gender)]
names(carer_imd)[1] <- 'age_group'

sum(carer_imd$count) #25672

denoms <- gp_asc_LWx[gender %in% c('Female', 'Male'), .(count_all=uniqueN(linkpseudo1)), by=.(gender, age_group, imd_decile)]

#setDT(age_ttls_imd)
carer_imd <- merge(carer_imd, denoms, by=c('age_group', 'gender', 'imd_decile'), all.x=T, all.y=F)
carer_imd[, rate := round(count*100/count_all, 2)]
carer_imd <- carer_imd[!is.na(carer_imd$imd_decile)]
carer_imd <- carer_imd[!is.na(carer_imd$age_group)]


fwrite(carer_imd, 'tables/carer_gp_asc_imd.csv')
#supress
hf_table <- carer_imd
hf_table[count<=5]$count <- NA
fwrite(hf_table, './tables/HF_carers_count_rates_age_groups_IMD.csv ')

ggplot(data=carer_imd, aes(x=factor(imd_decile), y=count, fill=age_group)) + 
  geom_bar(position="dodge", stat="identity") +
  guides(fill = guide_legend(title = "Age groups")) +
  labs(x = 'IMD decile (1=highest)') +
  labs(y = 'Total carers') +
  facet_wrap(~gender)
ggsave("./figures/carer_imd.png", width = 8, height = 6, units = 'in')

#---look back---#
#---OPTIONAL code to create the popl table: 'age_groups_gender_LSOA_2016_2021.csv'--#
#NOTE need popl totals for each year

#years needed
yr <- c('2016', '2017', '2018', '2019', '2020') #2021 not currently available (?)

for(i in 1:length(yr)) {
  #get data (match year to pattern)
  l <-list.files('C:/Users/OBrienJ2/Downloads', pattern=paste(yr[i]), full.names=TRUE)
  sheets <- readxl::excel_sheets(l)
  d1 <- read_excel(paste(l), grep('Female', sheets))
  d2 <- read_excel(paste(l), grep('Males', sheets))
  #need to remove top few rows from Excel files
  #d1 <- d1[-c(1:4),]
  #d2 <- d2[-c(1:4),]
  setDT(d1)
  setDT(d2)
  names(d1)[1] <- 'lsoa_11'
  names(d2)[1] <- 'lsoa_11'
  d1 <- d1[lsoa_11 %in% sp_lsoa$lsoa_11]
  d2 <- d2[lsoa_11 %in% sp_lsoa$lsoa_11]
  #need to know which cols to sum (i.e. ages are all double figures, but rm ones we don't want)
  x1 <- which(as.logical(nchar(names(d1))==2) & !grepl(c('10|11|12|13|14|15|16|17'), names(d1)))
  x2 <- which(as.logical(nchar(names(d2))==2) & !grepl(c('10|11|12|13|14|15|16|17'), names(d2)))
  d1 <- d1[,c(1,..x1)]
  d2 <- d2[,c(1,..x2)]
  #Females
  d1[, '18-29' := rowSums(.SD), .SDcols = 2:13]
  d1[, '30-39' := rowSums(.SD), .SDcols = 14:23]
  d1[, '40-49' := rowSums(.SD), .SDcols = 24:33]
  d1[, '50-59' := rowSums(.SD), .SDcols = 34:43]
  d1[, '60-69' := rowSums(.SD), .SDcols = 44:53]
  d1[, '70-79' := rowSums(.SD), .SDcols = 54:63]
  d1[, '80+' := rowSums(.SD), .SDcols = 64:73]
  d1 <- d1[,c(1,74:80)] #only useful cols
  #totals
  TF <- colSums(d1[,-1])
  TF <- append(paste('total_Female_', yr[i], sep=''), TF)
  d1[, gender := 'Female']
  #Males
  d2[, '18-29' := rowSums(.SD), .SDcols = 2:13]
  d2[, '30-39' := rowSums(.SD), .SDcols = 14:23]
  d2[, '40-49' := rowSums(.SD), .SDcols = 24:33]
  d2[, '50-59' := rowSums(.SD), .SDcols = 34:43]
  d2[, '60-69' := rowSums(.SD), .SDcols = 44:53]
  d2[, '70-79' := rowSums(.SD), .SDcols = 54:63]
  d2[, '80+' := rowSums(.SD), .SDcols = 64:73]
  d2 <- d2[,c(1,74:80)] #only useful cols
  #totals 
  TM <- colSums(d2[,-1])
  TM <- append(paste('total_Male_', yr[i], sep=''), TM)
  d2[, gender := 'Male']
  #
  d3 <- rbind(d1, d2)
  d3[,year := yr[i]]
  #data to named object
  assign(paste('TotFM_', yr[i], sep=''), rbind(TF, TM))
  assign(paste('age_', yr[i], '_FM', sep=''), d3)
  
  #TotFM_2016...
  #age_2016_FM, age_2017_FM, age_2018_FM, age_2019_FM, age_2020_FM
  
}

#estimation pending data
age_2021_FM <- age_2020_FM
age_2021_FM$year <- '2021'
TotFM_2021 <- TotFM_2020
TotFM_2021[1] <- 'total_Female_2021'
TotFM_2021[2] <- 'total_Male_2021'

age_groups_LSOA_gender_2016_2021 <- rbind(age_2016_FM,age_2017_FM,age_2018_FM,age_2019_FM,age_2020_FM,age_2021_FM) 
age_groups_TOTALS_gender_2016_2021 <- rbind(TotFM_2016,TotFM_2017,TotFM_2018,TotFM_2019,TotFM_2020,TotFM_2021) 


#add cols
age_groups_TOTALS_gender_2016_2021 <- as.data.frame(age_groups_TOTALS_gender_2016_2021)
setDT(age_groups_TOTALS_gender_2016_2021)
age_groups_TOTALS_gender_2016_2021[grep('Fe', V1), gender := 'Female']
age_groups_TOTALS_gender_2016_2021[grep('Ma', V1), gender := 'Male']
age_groups_TOTALS_gender_2016_2021$year <- stringr::str_sub(age_groups_TOTALS_gender_2016_2021$V1, -4,-1)

fwrite(age_groups_LSOA_gender_2016_2021, '.tables/age_groups_gender_LSOA_2016_2021.csv')
fwrite(age_groups_TOTALS_gender_2016_2021, '.tables/age_groups_gender_TOTALS_2016_2021.csv')

#---end of optional code to create table: 'age_groups_gender_LSOA_2016_2021.csv'

age_groups_gender_LSOA_2016_2021 <- read.csv('age_groups_gender_LSOA_2016_2021.csv')
setDT(sp_lsoa)
setDT(age_groups_gender_LSOA_2016_2021)
age_groups_gender_LSOA_2016_2021 <- age_groups_gender_LSOA_2016_2021[lsoa_11 %in% sp_lsoa[grepl('Liverpool', LSOA11NM)]$lsoa_11]
names(age_groups_gender_LSOA_2016_2021) <- c('lsoa_11', '18_29', '30_39', '40_49', '50_59', '60_69', '70_79',  '80plus', 'gender', 'year')
melt <- melt(age_groups_gender_LSOA_2016_2021, id.vars = c("year", 'gender'), measure.vars = c("18_29", "30_39", "40_49",  "50_59", "60_69", "70_79","80plus"))

age_groups_gender_LSOA_2016_2021_TOTALS <- aggregate(melt, value~year+gender+variable, FUN=sum)
names(age_groups_gender_LSOA_2016_2021_TOTALS) <- c('year', 'gender', 'age_group', 'total')


#look-back age and gender
#asc only
look_back_asc <- gp_asc_LWx[carer_asc_flag==1&carer_gp_flag==0][, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(min_carer_year_all, age_group_min_carer, gender)]
look_back_asc[, regd := 'asc']

#gp only
look_back_gp <- gp_asc_LWx[carer_asc_flag==0&carer_gp_flag==1][, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(min_carer_year_all, age_group_min_carer, gender)]
look_back_gp[, regd := 'gp']
#gp and asc
look_back_gp_asc <- gp_asc_LWx[carer_asc_flag==1&carer_gp_flag==1][, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(min_carer_year_all, age_group_min_carer, gender)]
look_back_gp_asc[, regd := 'gp_and_asc']

look_back_incidence_age_gender <- rbind(look_back_gp, look_back_asc, look_back_gp_asc)
look_back_incidence_age_gender <- look_back_incidence_age_gender[order(min_carer_year_all)]
names(look_back_incidence_age_gender)[1] <- 'year'
names(look_back_incidence_age_gender)[2] <- 'age_group'

sum(look_back_incidence_age_gender$count) #25672

age_groups_gender_LSOA_2016_2021_TOTALS$age_group <- gsub('_', '-', age_groups_gender_LSOA_2016_2021_TOTALS$age_group)
age_groups_gender_LSOA_2016_2021_TOTALS$age_group <- gsub('plus', '+', age_groups_gender_LSOA_2016_2021_TOTALS$age_group)
look_back_incidence_age_gender <- merge(age_groups_gender_LSOA_2016_2021_TOTALS, look_back_incidence_age_gender, by=c('year','age_group', 'gender'), all.x=T, all.y=T)

look_back_incidence_age_gender[,rate:=round(count*100/total,2)]

#write
fwrite(look_back_incidence_age_gender, './tables/look_back_incidence.csv')
#supress
hf_table <- look_back_incidence_age_gender
hf_table[count<=5]$count <- NA
fwrite(hf_table, './tables/HF_look_back_age_gender.csv')#write

#look back plots
ggplot(look_back_incidence_age_gender[regd %in% 'gp' & gender %in% c('Male', 'Female') & !is.na(age_group)], aes(year , rate, fill = age_group)) +
  geom_bar(stat = "identity", position = 'dodge') +
  guides(fill = guide_legend(title = "Age group")) +
  labs(x="Year", y="Carers (unique event) per 100 gen. popl.") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  facet_wrap(~gender)
ggsave("./figures/look_back_incidence_GP_age_gender.jpeg", width = 8, height = 6, units = 'in')

#look back plots
ggplot(look_back_incidence_age_gender[regd %in% 'asc' & gender %in% c('Male', 'Female') & !is.na(age_group)], aes(year , rate, fill = age_group)) +
  geom_bar(stat = "identity", position = 'dodge') +
  guides(fill = guide_legend(title = "Age group")) +
  labs(x="Year", y="Carers (unique event) per 100 gen. popl.") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  facet_wrap(~gender)
ggsave("./figures/look_back_incidence_ASC_age_gender.jpeg", width = 8, height = 6, units = 'in')


#get look-back totals (cohorts)

x <- aggregate(count~year+regd, look_back_incidence_age_gender, FUN=sum)
setDT(x)
y <- dcast(x, year~regd, value.var = "count")
fwrite(y, 'tables/look_back_totals.csv')


#look back incidence (total new carers each year)
#carer events and dates (liverpool only)
gp_asc_LWx <- merge(gp_asc_LWx, l_asc_all1[,c(1,3,4,6)], by.x='linkpseudo1', by.y='linkpseudo', all.x=F,all.y = F, allow.cartesian = T)

look_back_carers <-  gp_asc_LWx[(carer_asc_flag==1|carer_gp_flag==1) & gender %in% c('Male', 'Female') & !is.na(age_group_min_carer)  & (min_carer_year_all>2015 & min_carer_year_all<2021)][, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(min_carer_year_all, age_group_min_carer, gender)]
look_back_carers <- look_back_carers[order(min_carer_year_all, age_group_min_carer, gender)]

fwrite(look_back_carers, 'tables/look_back_events.csv')
names(look_back_carers) <- c('year', 'age_group', 'gender', 'count')
      
#ons age group pops
ons_age <- read.csv('./tables/age_groups_gender_LSOA_2016_2021.csv')
names(ons_age) <- c('lsoa_11', '18-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+', 'gender', 'year')
setDT(ons_age)
m <- melt(ons_age[year>2015 & year<2021], id.vars = c("gender", 'year'), measure.vars = c("18-29", "30-39",  "40-49", "50-59", "60-69", "70-79", "80+"))
age_ttls <- aggregate(value~gender+variable+year, data = m, FUN = sum)
names(age_ttls)[2] <- 'age_group'
names(age_ttls)[4] <- 'total'
look_back_carers <- merge(look_back_carers, age_ttls, by=c('year', 'age_group', 'gender'), all.x=T, all.y=F)
look_back_carers[, rate := round(count*100/total, 2)]
look_back_carers <- look_back_carers[!is.na(total)]

fwrite(look_back_carers, 'tables/look_back_age_group_rate.csv')

hf_table <- look_back_carers
hf_table[count<=5]$count <- NA
fwrite(hf_table, './tables/HF_look_back_carers_age_gender.csv')


ggplot(look_back_carers, aes(year, rate, fill = age_group)) +
  geom_bar(stat = "identity", position = 'dodge') +
  guides(fill = guide_legend(title = "Age group")) +
  labs(x="Year", y="Carers per 100") + 
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  facet_wrap(~gender)
ggsave("./figures/look_back_ASC_new_carers.png", width = 8, height = 6, units = 'in')

hf_table <- look_back_carers
hf_table[count<=5]$count <- NA
fwrite(hf_table, './tables/HF_look_back_carers_age_gender.csv')


#prevalence - total carers who had carer event each year
#event dates only available for Liverpool
gp_asc_LWx[type %in% 'Carer', event_year := as.numeric(format(event_start_date, format='%Y'))]

look_back_prevalence <- gp_asc_LWx[type %in% 'Carer', list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(event_year, age_group_min_carer, gender)]

look_back_prevalence <- look_back_prevalence[order(event_year)]
names(look_back_prevalence) <- c('year', 'age_group', 'gender', 'count')
look_back_prevalence <- look_back_prevalence[!is.na(look_back_prevalence$age_group)]
look_back_prevalence <- look_back_prevalence[year>2015 & year<2022]

setDF(sp_lsoa)
lsLPL <- sp_lsoa[grep('Liv', sp_lsoa$LSOA11NM),]$lsoa_11

m <- melt(ons_age[year>2015 & year<2021 & lsoa_11 %in% lsLPL], id.vars = c("gender", 'year'), measure.vars = c("18-29", "30-39",  "40-49", "50-59", "60-69", "70-79", "80+"))
age_ttls <- aggregate(value~gender+variable+year, data = m, FUN = sum)
names(age_ttls)[2] <- 'age_group'
names(age_ttls)[4] <- 'total'

look_back_prevalence <- merge(age_ttls, look_back_prevalence, by=c('year', 'age_group', 'gender'), all.x=T, all.y=T)
setDT(look_back_prevalence)
look_back_prevalence[, rate := round(count*100/total,2)]

fwrite(look_back_prevalence, './tables/look_back_prevalence_age_gender_LPL_ONLY.csv')
#supress
hf_table <- look_back_prevalence
hf_table[count<=5]$count <- NA
fwrite(hf_table, './tables/HF_look_back_prevalence_age_gender_LPL_ONLY.csv')


#plot prevalence [ONS 2021 popl not available at time of analysis]
ggplot(data=look_back_prevalence[!year %in% '2021'], aes(x=as.factor(year), y=rate, fill=age_group)) + 
  geom_bar(position="dodge", stat="identity") +
  guides(fill = guide_legend(title = "")) +
  labs(x = 'Year') +
  labs(y = 'Rate per 100') +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  facet_wrap(~gender)
ggsave("./figures/look_back_prevalence_LPL_ONLY.png", width = 8, height = 6, units = 'in')

#percentage of population who are carers
names(look_back_carers) <- c('year', 'age_group', 'gender', 'count')
look_back_carers <- merge(look_back_carers, age_ttls, by=c('year', 'age_group', 'gender'))
look_back_carers[,perc := round(count*100/total,2)]

#write
fwrite(look_back_carers, './tables/look_back_percentage.csv')
#supress
hf_table <- look_back_carers
hf_table[count<=5]$count <- NA
fwrite(hf_table, './tables/HF_look_back_carer_percentage.csv')#write

#look back plots
ggplot(look_back_carers, aes(year , perc, fill = age_group)) +
  geom_bar(stat = "identity", position = 'dodge') +
  guides(fill = guide_legend(title = "Age group")) +
  labs(x="Year", y="Carers per 100 (general popl.)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  facet_wrap(~gender)
ggsave("./figures/look_back_incidence_percentage.jpeg", width = 8, height = 6, units = 'in')

#percentage of popl who are carers by age, sex, IMD
#age group, gender and imd
carer_imd_year <- gp_asc_LWx[(carer_asc_flag==1|carer_gp_flag==1) & gender %in% c('Female', 'Male') & !is.na(imd_decile.x) & min_carer_year_all>2015 & min_carer_year_all<2022][, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(min_carer_year_all, age_group, imd_decile.x, gender)]
names(carer_imd_year) <- c('year', 'age_group', 'imd_decile', 'gender', 'count')

ons_age_imd <- merge(ons_age, imd, by='lsoa_11')
m <- melt(ons_age_imd, id.vars = c("gender", 'year', 'imd_decile'), measure.vars = c("18-29", "30-39",  "40-49", "50-59", "60-69", "70-79", "80+"))
age_ttls_imd <- aggregate(value~gender+variable+year+imd_decile, data = m, FUN = sum)
names(age_ttls_imd) <- c('gender', 'age_group', 'year', 'imd_decile', 'total')
carer_imd_year <- merge(carer_imd_year, age_ttls_imd, by=c('age_group', 'year', 'gender', 'imd_decile'))

#FROM HERE - merge and plot...
carer_imd_year[, perc := round(count*100/total,2)]

#write
fwrite(carer_imd_year, './tables/carer_imd_year_percentage.csv')
#supress
hf_table <- look_back_carers
hf_table[count<=5]$count <- NA
fwrite(hf_table, './tables/HF_carer_imd_year_percentage.csv')#write

#CHECK THIS IS WRITTEN...

ggplot(carer_imd_year[year %in% '2020'], aes(as.factor(imd_decile), perc, fill = age_group)) +
  geom_bar(stat = "identity", position = 'dodge') +
  guides(fill = guide_legend(title = "Age group")) +
  labs(x="Year", y="Carers per 100 (general popl.)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  facet_wrap(~gender)
ggsave("./figures/carer_imd_year_percentage.jpeg", width = 8, height = 6, units = 'in')



#geography
carer_lsoa <- gp_asc_LWx[carer_asc_flag==1|carer_gp_flag==1, list(
  count=uniqueN(linkpseudo1, na.rm=T)),
  by=.(gender, lsoa_11)]

denoms <- gp_asc_LWx[, .(count_all=uniqueN(linkpseudo1)), by=.(lsoa_11, gender)]


carer_lsoa <- merge(carer_lsoa, denoms, by=c('gender', 'lsoa_11'))
carer_lsoa <- setDT(carer_lsoa)[order(lsoa_11)]
carer_lsoa[,rate := round(count*100/count_all, 2)]
carer_lsoa <- carer_lsoa[lsoa_11 %in% sp_lsoa$lsoa_11]

fwrite(carer_lsoa, './tables/count_lsoa.csv')
#supress
hf_table <- carer_lsoa
hf_table[count<=5]$count <- NA
fwrite(hf_table, './tables/HF_count_lsoa.csv')

#plot carer LSOA counts
plot_mf <- sp_lsoa
plot_mf <- merge(plot_mf, carer_lsoa, by='lsoa_11')
ggplot() +
  geom_sf(data = sp_lsoa, fill = 'transparent', col ='grey70', lwd=0.07) +
  geom_sf(data = plot_mf, aes(fill = rate), lwd=0) +
  scale_fill_viridis_c(name="Per 100", alpha = 1) +
  #guides(fill = guide_legend(title = "")) +
  theme(legend.text=element_text(size=12))+
  facet_wrap(~gender) +
  theme_void()
ggsave("./figures/carer_rates_gender_LSOA_LW.png", width = 8, height = 6, units = 'in')


#NOTE only 179 carers with Contact and any follow-up date

#days since Request and next carer events
u <- unique(gp_asc_LWx[event_type %in% 'Contact']$linkpseudo1) #10257
carer_days <- data.frame()
for(i in 1:length(u)) {
  #subset by carer id
  x <- carer_events[linkpseudo1 %in% u[i]]
  #get the earliest Contact date
  dt1 <- max(x[event_type %in% 'Contact']$event_start_date)
  #get next assessment or service date after the Contact date
  dt2 <- min(x[event_type %notin% 'Contact' & event_start_date > as.Date(dt1)]$event_start_date)
  if(is.infinite(dt2)==TRUE) { next }
  dy <- as.numeric(difftime(dt2, dt1, units = "days"))
  df <- cbind('linkpseudo1'=as.character(u[i]), 'Contact'=paste(dt1), 'Service'=paste(dt2), 'days'=dy)
  carer_days <- rbind(carer_days, df)
}

carer_days <- merge(carer_days, gp_asc_LWx[,c(1,2,4, 35)], by='linkpseudo1', all.x=T, all.y = F)

fwrite(carer_days, 'tables/days_between_Rqust_Srvc.csv')

#mean days waited...
carer_days$days <- as.numeric(carer_days$days)
x <- aggregate(days ~ gender+imd_decile, carer_days, FUN=mean)
x$days <- round(x$days, 2)

fwrite(x, 'tables/days_waited_Rq_Cnt_mean.csv')
fwrite(x, 'tables/HF_days_waited_Rq_Cnt_mean.csv')
#carers households
#box plot
setDT(carer_days)
ggplot(carer_days[gender %in% c('Male', 'Female') & !is.na(imd_decile) & days<100], aes(x=as.factor(imd_decile), y=days)) +
  geom_boxplot(fill='white', alpha=0.6) +
  theme(legend.position="none")+
  labs(x = 'IMD decile (1=highest)') +
  labs(y = 'Days') +
  facet_wrap(~gender)
ggsave("./figures/days_diff_box_IMD_plot.png", width = 8, height = 6, units = 'in')


#regressions

#Regression analysis
#age (life stage) dummies
gp_asc_LWx[age.2022>18 & age.2022<45, adult_lowmid := 1 ] #adult lower middle-aged
gp_asc_LWx[is.na(adult_lowmid)]$adult_lowmid <- 0
gp_asc_LWx[age.2022>=45 & age.2022<=64, adult_uppmid := 1 ] #adult upper middle-aged
gp_asc_LWx[is.na(adult_uppmid)]$adult_uppmid <- 0
gp_asc_LWx[age.2022>=65, adult_old := 1 ] #adult older age
gp_asc_LWx[is.na(adult_old)]$adult_old <- 0

#ethnic group dummies
gp_asc_LWx[(carer_asc_flag==1|carer_gp_flag==1) & ethnic_group %in% 'White', carer_White := 1]
gp_asc_LWx[(carer_asc_flag==1|carer_gp_flag==1) %in% 'Black or Black British', carer_Black := 1]
gp_asc_LWx[(carer_asc_flag==1|carer_gp_flag==1) %in% 'Asian or Asian British', carer_Asian := 1]
gp_asc_LWx[(carer_asc_flag==1|carer_gp_flag==1) %in% 'Other ethnic groups', carer_ethnic_Other := 1]
gp_asc_LWx[(carer_asc_flag==1|carer_gp_flag==1) %in% 'Unknown', carer_ethnic_Unkown := 1]

#gender dummies
gp_asc_LWx[gender %in% 'Female', carer_Female := 1]
gp_asc_LWx[gender %in% 'Male', carer_Male := 1]

#ethnic group dummies (general/non-carers)
gp_asc_LWx[ethnic_group %in% 'White', ethn_White := 1]
gp_asc_LWx[is.na(ethn_White)]$ethn_White <- 0
gp_asc_LWx[grepl('Asian', ethnic_group), ethn_AsnBrts := 1]
gp_asc_LWx[is.na(ethn_AsnBrts)]$ethn_AsnBrts <- 0
gp_asc_LWx[grepl('Black', ethnic_group), ethn_BlckBrts := 1]
gp_asc_LWx[is.na(ethn_BlckBrts)]$ethn_BlckBrts <- 0
gp_asc_LWx[grepl('Mixed', ethnic_group), ethn_Mxd := 1]
gp_asc_LWx[is.na(ethn_Mxd)]$ethn_Mxd <- 0
gp_asc_LWx[grepl('Other', ethnic_group), ethn_Othr := 1]
gp_asc_LWx[is.na(ethn_Othr)]$ethn_Othr <- 0

#household conditions
#count up households - then make separate table with totals
#don't include highly duplicated pUPRNs (lots of errors?)
t <- table(gp_asc_LWx$pUPRN)

#all household composition
households <- gp_asc_LWx[pUPRN %notin% names(t[t>100]), list(
  N=uniqueN(linkpseudo1, na.rm=T), 
  carerN=uniqueN(linkpseudo1[carer_asc_flag==1|carer_gp_flag==1], na.rm=T), 
  lowm=uniqueN(linkpseudo1[adult_lowmid==1], na.rm=T),
  uppm=uniqueN(linkpseudo1[adult_uppmid==1], na.rm=T),
  old=uniqueN(linkpseudo1[adult_old==1], na.rm=T),
  White=uniqueN(linkpseudo1[ethn_White==1], na.rm=T),
  Asian=uniqueN(linkpseudo1[ethn_AsnBrts==1], na.rm=T),
  Black=uniqueN(linkpseudo1[ethn_BlckBrts==1], na.rm=T),
  Mixed=uniqueN(linkpseudo1[ethn_Mxd==1], na.rm=T),
  frailty_or_dementia=uniqueN(linkpseudo1[frailty_or_dementia==1], na.rm=T),
  learning_disabilities=uniqueN(linkpseudo1[learning_disabilities==1], na.rm=T),
  physical_disability=uniqueN(linkpseudo1[physical_disability==1], na.rm=T),
  copd=uniqueN(linkpseudo1[copd==1], na.rm=T),
  cvd=uniqueN(linkpseudo1[cvd==1], na.rm=T),
  cancer=uniqueN(linkpseudo1[cancer==1], na.rm=T),
  rheumatology=uniqueN(linkpseudo1[rheumatology==1], na.rm=T),
  depression=uniqueN(linkpseudo1[depression==1], na.rm=T)),
  by=.(pUPRN)]

#tidy
households <- households[!pUPRN %in% '']
households <- households[!duplicated(households),]

#limit household size
households <- households[N<10]
households <- households[!duplicated(households),]

#imd
households <- merge(households, gp_asc_LWx[,c(18, 37)], by='pUPRN')

#household dummies

#mult-gen household
households[lowm>=1 & uppm>=1 & old>=1 , multi_gen_flag :=  1]
households[is.na(multi_gen_flag)]$multi_gen_flag <- 0

#65+ multiple sharing household
households[lowm==0 & uppm==0 & old>=2 , old_multi_flag :=  1]
households[is.na(old_multi_flag)]$old_multi_flag <- 0

#lowmid alone
households[lowm==1 & uppm==0 & old==0 , LowMid_single_flag :=  1]
households[is.na(LowMid_single_flag)]$LowMid_single_flag <- 0

#uppmid alone
households[lowm==0 & uppm==1 & old==0 , UppMid_single_flag :=  1]
households[is.na(UppMid_single_flag)]$UppMid_single_flag <- 0

#65+ single (without recently deceased)
households[(lowm==0 & uppm==0 & old==1) & deceased==0 , old_single_flag :=  1]
households[is.na(old_single_flag)]$old_single_flag <- 0


#ethnic group flags
households[White>=1 & Asian==0 & Black==0 & Mixed==0, all_White :=  1]
households[is.na(all_White)]$all_White <- 0
households[White>=1 & (Asian>=1 | Black>=1 | Mixed>=1), WhiteMixed :=  1]
households[is.na(WhiteMixed)]$WhiteMixed <- 0
households[White==0 & Asian>=1 & Black==0 & Mixed==0 , all_Asian :=  1]
households[is.na(all_Asian)]$all_Asian <- 0
households[White==0 & Asian==0 & Black>=1 & Mixed==0 , all_Black :=  1]
households[is.na(all_Black)]$all_Black <- 0
households[White==0 & Asian==0 & Black==0 & Mixed>=1 , all_Mixed :=  1]
households[is.na(all_Mixed)]$all_Mixed <- 0

#conditions dummies
households[frailty_or_dementia>=1 , Frlt_or_Dmnt_flag :=  1]
households[is.na(Frlt_or_Dmnt_flag)]$Frlt_or_Dmnt_flag <- 0
households[learning_disabilities>=1 , Lrng_Dsbs_flag :=  1]
households[is.na(Lrng_Dsbs_flag)]$Lrng_Dsbs_flag <- 0
households[physical_disability>=1 , Phys_Dsbs_flag :=  1]
households[is.na(Phys_Dsbs_flag)]$Phys_Dsbs_flag <- 0
households[copd>=1 , copd_flag :=  1]
households[is.na(copd_flag)]$copd_flag <- 0
households[cvd>=1 , cvd_flag :=  1]
households[is.na(cvd_flag)]$cvd_flag <- 0
households[cancer>=1 , cancer_flag :=  1]
households[is.na(cancer_flag)]$cancer_flag <- 0
households[rheumatology>=1 , rheumt_flag :=  1]
households[is.na(rheumt_flag)]$rheumt_flag <- 0
households[depression>=1 , deprss_flag :=  1]
households[is.na(deprss_flag)]$deprss_flag <- 0

#compare households by type/main conditions (carer&non)
#model household composition

#households with >1 person, conditions
m1 <- glmer(carer_flag ~ cvd_flag + rheumt_flag + deprss_flag + Frlt_or_Dmnt_flag +
              (1 | pUPRN), data = households[N>1], 
            family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

coeffs <- coef(summary(m1)) # get estimates, etc...
write.csv(coeffs, "tables/household_model_coeffs.csv") # export



jpeg('figures/households_glmm.jpg')
sjPlot::plot_model(m1, show.values=T, title="Effects of household factors on carer flag",
                   axis.labels=c("Physical Dsbs (child)", "Learning Dsbs (child)", "Fraily or dementia", "Depression", "Rheumatology","CVD"))
dev.off()

#aggregate and plot carers' households
carers_hshds <- households[pUPRN %in% gp_asc_LWx[carer_gp_flag==1|carer_asc_flag==1]$pUPRN, list(
  N=uniqueN(pUPRN, na.rm=T), 
  sum_mult_old=sum(old_multi_flag, na.rm=T),
  sum_LowMid_single=sum(LowMid_single_flag, na.rm=T),
  sum_UppMid_single=sum(UppMid_single_flag, na.rm=T),
  sum_multi_gen=sum(multi_gen_flag, na.rm=T),
  sum_all_White=sum(all_White, na.rm=T),
  sum_WhiteMixed=sum(WhiteMixed, na.rm=T),
  sum_all_Asian=sum(all_Asian, na.rm=T),
  sum_all_Black=sum(all_Black, na.rm=T),
  sum_all_Mixed=sum(all_Mixed, na.rm=T)),
  by=.(imd_decile)]

carers_hshds <- carers_hshds[order(imd_decile)]

#rates
x <- round(carers_hshds[,3:11]*100/carers_hshds$N,2)
names(x) <- gsub('sum', 'pc', names(x))

carers_hshds <- cbind(carers_hshds, x)

fwrite(carers_hshds, 'tables/carers_households_counts_rates.csv')
hf_table <- carers_hshds
hf_table[,3:11][hf_table[,3:11] <6] <- NA
fwrite(hf_table, 'HF_final_report_Mar23/HF_carers_households_counts_rates.csv')

#households pie charts
#df of totals
#crs hs
hc_age <- carers_hshds[ , lapply(.SD, sum), .SDcols = 3:6]
hc_age <- data.frame(t(hc_age))
hc_age$type <- rownames(hc_age)
names(hc_age)[1] <- 'total_crs'
setDT(age_hs)
m <- melt(age_hs, id='type')


#pie
p <- ggplot(m, aes(x="", y=value, fill=type)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  facet_wrap(~variable) +
  geom_text(aes(label = paste0(value , "%")), position = position_stack(vjust=0.5)) +
  theme_void() 
p + scale_fill_discrete(labels=c('Single 18-44', 'Multiple 65+', 'Multi-generational', 'Single 65+', 'Single 45-64'))
ggsave("./figures/carers_households_size_pies.png", width = 8, height = 6, units = 'in')

carers_hshds_conditions <- households[carerN>0, list(
  N=uniqueN(pUPRN, na.rm=T), 
  sum_cvd=sum(cvd_flag),
  sum_copd=sum(copd_flag),
  sum_depression=sum(deprss_flag),
  sum_cancer=sum(cancer_flag),
  sum_frailty_dementia=sum(Frlt_or_Dmnt_flag),
  sum_phys_disb=sum(Phys_Dsbs_flag),
  sum_lrng_disb=sum(Lrng_Dsbs_flag)),
  by=.(imd_decile)]

#perc
x <- round(carers_hshds_conditions[,3:9]*100/carers_hshds_conditions$N,2)
names(x) <- gsub('sum', 'pc', names(x))
carers_hshds_conditions <- cbind(carers_hshds_conditions, x)

carers_hshds_conditions <- carers_hshds_conditions[!is.na(imd_decile)]
carers_hshds_conditions <- carers_hshds_conditions[order(imd_decile)]

fwrite(carers_hshds_conditions, 'tables/households_PH_conditions_IMD.csv')
#supress
hf_table <- carers_hshds_conditions
hf_table[,3:9][hf_table[,3:9] <= 5] <- NA
fwrite(hf_table, './tables/households_PH_conditions_IMD.csv')



