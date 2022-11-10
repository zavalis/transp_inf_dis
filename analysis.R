# Key data and analyses of the sample

library(dplyr)

# read df
df=read.delim('./input/output.csv',sep=',')


# TABLE 2
# Study sample
## Year of Publication
nrow(df[df$Publication.Year==2019,])
nrow(df[df$Publication.Year==2021,])

## stats of primary comparisons

### First hypothesis 2019 v 2021 overall
fisher.test(table(df$Publication.Year, df$is_open_code))
fisher.test(table(df$Publication.Year, df$is_open_data))
fisher.test(table(df$Publication.Year, df$is_register_pred))
fisher.test(table(df$Publication.Year, df$is_coi_pred))
fisher.test(table(df$Publication.Year, df$is_fund_pred))

### 2019 non COVID-19 v 2021 non COVID-19
hypo2=df[df$COVID.Year%in%c('2019', '2021 FALSE'),]
fisher.test(table(hypo2$COVID.Year, hypo2$is_open_code))
fisher.test(table(hypo2$COVID.Year, hypo2$is_open_data))
fisher.test(table(hypo2$COVID.Year, hypo2$is_register_pred))
fisher.test(table(hypo2$COVID.Year, hypo2$is_coi_pred))
fisher.test(table(hypo2$COVID.Year, hypo2$is_fund_pred))

### 2021 non COVID-19 v 2021 COVID-19
hypo3=df[df$COVID.Year%in%c('2021 TRUE', '2021 FALSE'),]
fisher.test(table(hypo3$COVID.Year, hypo3$is_open_code))
fisher.test(table(hypo3$COVID.Year, hypo3$is_open_data))
fisher.test(table(hypo3$COVID.Year, hypo3$is_register_pred))
fisher.test(table(hypo3$COVID.Year, hypo3$is_coi_pred))
fisher.test(table(hypo3$COVID.Year, hypo3$is_fund_pred))

#TABLE 4

# get the p-values for each indicator for the first hypothesis stratified for journal 
for (i in unique(df$Journal.Book)){
 	temp=df%>%filter(Journal.Book==i)
 	#print(shtemp)
#temp=df%>%filter(Journal.Book=='Clin Infect Dis')
 	code=temp %>%group_by(Publication.Year) %>%summarize(true = sum(is_open_code, na.rm = TRUE), false=sum(is_open_code==FALSE,na.rm=TRUE))
 	data=temp %>%group_by(Publication.Year) %>%summarize(true = sum(is_open_data, na.rm = TRUE), false=sum(is_open_data ==FALSE,na.rm=TRUE))
 	#print(code[,2:3])
 	#print(data[,2:3])
 	register=temp %>%group_by(Publication.Year) %>%summarize(true = sum(is_register_pred, na.rm = TRUE), false=sum(is_register_pred ==FALSE,na.rm=TRUE))
 	coi=temp %>%group_by(Publication.Year) %>%summarize(true = sum(is_coi_pred, na.rm = TRUE), false=sum(is_coi_pred ==FALSE,na.rm=TRUE))
 	fund=temp %>%group_by(Publication.Year) %>%summarize(true = sum(is_fund_pred, na.rm = TRUE), false=sum(is_fund_pred ==FALSE,na.rm=TRUE))
 	p_code=fisher.test(code[,2:3])$p
 	p_data=fisher.test(data[,2:3])$p
 	p_register=fisher.test(register[,2:3])$p
 	p_coi=fisher.test(coi[,2:3])$p
 	p_fund=fisher.test(fund[,2:3])$p

	test_list=c(p_code,p_data,p_register,p_coi,p_fund)
	#print(p_code)
	k=1
	print(i)
	print(test_list)
	

 }
 
 
 #hypo2
 hypo2=df[df$COVID.Year%in%c('2019', '2021 FALSE'),]
  for (i in unique(hypo2$Journal.Book)){
 	temp=hypo2%>%filter(Journal.Book==i)
 	#print(shtemp)
#temp=df%>%filter(Journal.Book=='Clin Infect Dis')
 	code=temp %>%group_by(COVID.Year) %>%summarize(true = sum(is_open_code, na.rm = TRUE), false=sum(is_open_code==FALSE,na.rm=TRUE))
 	data=temp %>%group_by(Publication.Year) %>%summarize(true = sum(is_open_data, na.rm = TRUE), false=sum(is_open_data ==FALSE,na.rm=TRUE))
 	register=temp %>%group_by(COVID.Year) %>%summarize(true = sum(is_register_pred, na.rm = TRUE), false=sum(is_register_pred ==FALSE,na.rm=TRUE))
 	coi=temp %>%group_by(COVID.Year) %>%summarize(true = sum(is_coi_pred, na.rm = TRUE), false=sum(is_coi_pred ==FALSE,na.rm=TRUE))
 	fund=temp %>%group_by(COVID.Year) %>%summarize(true = sum(is_fund_pred, na.rm = TRUE), false=sum(is_fund_pred ==FALSE,na.rm=TRUE))
 	#print(code[,2:3])
 	#print(data[,2:3])
 	p_code=fisher.test(code[,2:3])$p
 	p_data=fisher.test(data[,2:3])$p
 	p_register=fisher.test(register[,2:3])$p
 	p_coi=fisher.test(coi[,2:3])$p
 	p_fund=fisher.test(fund[,2:3])$p

	test_list=c(p_code,p_data,p_register,p_coi,p_fund)
	#print(p_code)
	k=1
	print(i)
	print(test_list)
	

 }
 
 #hypo 3
 hypo3=df[df$COVID.Year%in%c('2021 TRUE', '2021 FALSE'),]
 #remove infection and immunity as it doesn't have any covid publications in our sample
 hypo3=hypo3%>%filter(!Journal.Book=='Infect Immun')

  for (i in unique(hypo3$Journal.Book)){
 	
 	temp=hypo3%>%filter(Journal.Book==i)
 	#print(shtemp)
#temp=df%>%filter(Journal.Book=='Clin Infect Dis')

 	code=temp %>%group_by(COVID.Year) %>%summarize(true = sum(is_open_code, na.rm = TRUE), false=sum(is_open_code==FALSE,na.rm=TRUE))
 	data=temp %>%group_by(COVID.Year) %>%summarize(true = sum(is_open_data, na.rm = TRUE), false=sum(is_open_data ==FALSE,na.rm=TRUE))
 	register=temp %>%group_by(COVID.Year) %>%summarize(true = sum(is_register_pred, na.rm = TRUE), false=sum(is_register_pred ==FALSE,na.rm=TRUE))
 	coi=temp %>%group_by(COVID.Year) %>%summarize(true = sum(is_coi_pred, na.rm = TRUE), false=sum(is_coi_pred ==FALSE,na.rm=TRUE))
 	fund=temp %>%group_by(COVID.Year) %>%summarize(true = sum(is_fund_pred, na.rm = TRUE), false=sum(is_fund_pred ==FALSE,na.rm=TRUE))
 	p_code=fisher.test(code[,2:3])$p
 	p_data=fisher.test(data[,2:3])$p
 	p_register=fisher.test(register[,2:3])$p
 	p_coi=fisher.test(coi[,2:3])$p
 	p_fund=fisher.test(fund[,2:3])$p

	test_list=c(p_code,p_data,p_register,p_coi,p_fund)
	#print(p_code)
	k=1
	print(i)
	print(test_list)
	

 }
 
 
# Manual validation

 # Correlates of transparency in the manually assessed sample
## possibly split into meta-data, sample characteristics, study design...?

## Characteristics
manual=read.delim('./input/manual_characteristics/manual_extraction_cleaned.csv',sep=',')
manual$sample_size=as.numeric(manual$sample_size)
median(manual$sample_size,na.rm=TRUE)
samplesize=(manual%>%filter(is_open_code==TRUE)%>%select(sample_size))
median(samplesize$sample_size,na.rm=TRUE)
IQR(samplesize$sample_size,na.rm=TRUE)

samplesize=(manual%>%filter(is_open_data==TRUE)%>%select(sample_size))
median(samplesize$sample_size,na.rm=TRUE)
IQR(samplesize$sample_size,na.rm=TRUE)

samplesize=(manual%>%filter(is_register_pred==TRUE)%>%select(sample_size))
median(samplesize$sample_size,na.rm=TRUE)
IQR(samplesize$sample_size,na.rm=TRUE)


# this code is then applied to all included columns in 
t=manual[!is.na(manual$type_dis),];table(t$is_open_code);table(t$is_open_data);table(t$is_register_pred)

table(manual$clinical)



# Manual validation (not reporting the specific FP and FN but just the overall)
df_val=read.delim('./input/manual_validation/sample_validation_completed.csv',sep=',')

table(df_val$nothingtd_coi)/162
table(df_val$nothingtd_fund)/182


overlapsamplemanual=intersect(manual293$DOI,sample$DOI)

pmid_overlap=(df%>%filter(DOI%in%overlapsamplemanual)%>%select(pmid))
df_val=(df_val%>%filter(!pmid%in% pmid_overlap$pmid))


# validation section
validationfun=function(indicator,df_val){
if (indicator=='code' |indicator== 'data'){indicatorz=paste0('is_open_',indicator)}else{indicatorz=paste0('is_',indicator,'_pred')}
valcol=paste0('validate_',indicator)
congruence=ifelse(df_val[[indicatorz]]==df_val[[valcol]],"Yes","No")
#return(c(valcol,indicatorz))
df_val$congruence=congruence
#return (df_val)
#indicatorz=paste0(is_open_,)
fp=nrow(df_val[df_val$congruence=='No' & df_val[[indicatorz]]==TRUE,])/nrow(df_val[df_val[[indicatorz]]==TRUE,])

fpN=nrow(df_val[df_val[[indicatorz]]==TRUE,])
fpn=nrow(df_val[df_val$congruence=='No' & df_val[[indicatorz]]==TRUE,])


fn=nrow(df_val[df_val$congruence=='No' & df_val[[indicatorz]]==FALSE,])/nrow(df_val[df_val[[indicatorz]]==FALSE,])

fnN=nrow(df_val[df_val[[indicatorz]]==FALSE,])
fnn=nrow(df_val[df_val$congruence=='No' & df_val[[indicatorz]]==FALSE,])

ind=nrow(df[df[[indicatorz]]==TRUE,])/nrow(df)
adjusted_ind=(1-fp)*ind +fn *(1-ind)

d=c(FP=fp,FPn=fpn,FPN=fpN,FN=fn,FNn=fnn,FNN=fnN,Indicator=ind, Adjusted_indicator=adjusted_ind)

return(d)
}

validationfun('code', df_val)
validationfun('data', df_val)
validationfun('register', df_val)
validationfun('coi', df_val)
validationfun('fund', df_val)
 
