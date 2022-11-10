# META-ANALYSES
library(meta)
library(stringr)
library(dplyr)

df=read.delim('./input/output.csv',sep=',')

# function for the forest plot taking in the raw data
## FIXED EFFECTS META-ANALYSIS
forestplot=function(testdf, col, control,experimental, indicator){
	
	my_sym=sym(indicator)
	hypo=sym(col)
	
	eventdf=df%>%group_by(Journal.Book)%>%summarise(Events.c = sum(!!my_sym &
	!!hypo==control), Events.e=sum(!!my_sym& !!hypo==experimental) )

	group1=as.character(control)
	group2=as.character(experimental)
	
	
	eventdf['Total.c']= testdf[group1]
	eventdf['Total.e']= testdf[group2]
	
	group1=str_replace(group1,'FALSE', 'n-Cov')
	
	group2=str_replace(group2,'TRUE', 'Cov')
	group2=str_replace(group2,'FALSE', 'n-Cov')
	
	filename=paste0('./output/forestplots/fixed_effects/fixed_forestplot_',indicator,'_',group1,group2,'.pdf')
	
	eventdf$Events.c=as.double(eventdf$Events.c)
	eventdf$Total.c=as.double(eventdf$Total.c)
	eventdf$Events.e=as.double(eventdf$Events.e)
    eventdf$Total.e=as.double(eventdf$Total.e)
    
    # changes the entire 2x2 table in cases of 0
    eventdf=eventdf%>%mutate(across(.cols=2:5, ~if_else(Events.e==0|Events.c==0, .x+0.5,.)))
		
	m.bin =metabin(event.e = Events.e, 
                 n.e = Total.e,
                 event.c =Events.c,
                 n.c = Total.c,
                 studlab = Journal.Book,
                 data = eventdf,
                 sm = "OR",
                 label.e=group2,
                 label.c=group1,
                 method = "Inverse",
                 fixed = TRUE,
                 random = FALSE,
                # method.tau = "DL",
) 
    #return(eventdf)            
    pdf(file=filename, width=10,height=5) # Open PDF device with specific file name
    forest.meta(m.bin,print.I2.ci=TRUE,)
    dev.off()
    #return(eventdf)
    }


# hypothesis 1
h1=as.data.frame.matrix(table(df$Journal.Book,df$Publication.Year))

# hypothesis 3
COVIDYNC=df[df$COVID.Year%in%c('2019','2021 FALSE'),]
h2=as.data.frame.matrix(table(COVIDYNC $Journal.Book, COVIDYNC $COVID.Year))

# hypothesis 2
COVIDY2021=df[df$COVID.Year%in%c('2021 TRUE','2021 FALSE'),]
h3=as.data.frame.matrix(table(COVIDY2021$Journal.Book, COVIDY2021$COVID.Year))

# argument 1 is the summary table chosen according to the hypotheses above
# argument 2 is the selected column
# argument 3 and 4 is the control and experimental group respectively
# argument 5 is the indicator you want to use
 
 ## for the first hypothesis   
forestplot(h1,'Publication.Year',2019,2021,'is_open_code')
forestplot(h1,'Publication.Year',2019,2021,'is_open_data')
forestplot(h1,'Publication.Year',2019,2021,'is_register_pred')
forestplot(h1,'Publication.Year',2019,2021,'is_coi_pred')
forestplot(h1,'Publication.Year',2019,2021,'is_fund_pred')

## for the second hypothesis
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_open_code')
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_open_data')
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_register_pred')
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_coi_pred')
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_fund_pred')

## for the third hypothesis
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_open_code')
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_open_data')
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_register_pred')
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_coi_pred')
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_fund_pred')



# RANDOM EFFECTS Meta-Analysis
forestplot=function(testdf, col, control,experimental, indicator){
	
	my_sym=sym(indicator)
	hypo=sym(col)
	
	eventdf=df%>%group_by(Journal.Book)%>%summarise(Events.c = sum(!!my_sym &
	!!hypo==control), Events.e=sum(!!my_sym& !!hypo==experimental) )

	group1=as.character(control)
	group2=as.character(experimental)
	
	
	eventdf['Total.c']= testdf[group1]
	eventdf['Total.e']= testdf[group2]
	
	group1=str_replace(group1,'FALSE', 'n-Cov')
	
	group2=str_replace(group2,'TRUE', 'Cov')
	group2=str_replace(group2,'FALSE', 'n-Cov')
	
	filename=paste0('./output/forestplots/random_effects/random_forestplot_',indicator,'_',group1,group2,'.pdf')
	
	eventdf$Events.c=as.double(eventdf$Events.c)
	eventdf$Total.c=as.double(eventdf$Total.c)
	eventdf$Events.e=as.double(eventdf$Events.e)
    eventdf$Total.e=as.double(eventdf$Total.e)
    
    # changes the entire 2x2 table in cases of 0
    eventdf=eventdf%>%mutate(across(.cols=2:5, ~if_else(Events.e==0|Events.c==0, .x+0.5,.)))
		
	m.bin =metabin(event.e = Events.e, 
                 n.e = Total.e,
                 event.c =Events.c,
                 n.c = Total.c,
                 studlab = Journal.Book,
                 data = eventdf,
                 sm = "OR",
                 label.e=group2,
                 label.c=group1,
                 method = "Inverse",
                 fixed = FALSE,
                 random = TRUE,
                # method.tau = "DL",
) 
    #return(eventdf)            
    pdf(file=filename, width=10,height=5) # Open PDF device with specific file name
    forest.meta(m.bin,print.I2.ci=TRUE,)
    dev.off()
    #return(eventdf)
    }


# hypothesis 1
h1=as.data.frame.matrix(table(df$Journal.Book,df$Publication.Year))

# hypothesis 3
COVIDYNC=df[df$COVID.Year%in%c('2019','2021 FALSE'),]
h2=as.data.frame.matrix(table(COVIDYNC $Journal.Book, COVIDYNC $COVID.Year))

# hypothesis 2
COVIDY2021=df[df$COVID.Year%in%c('2021 TRUE','2021 FALSE'),]
h3=as.data.frame.matrix(table(COVIDY2021$Journal.Book, COVIDY2021$COVID.Year))

# argument 1 is the summary table chosen according to the hypotheses above
# argument 2 is the selected column
# argument 3 and 4 is the control and experimental group respectively
# argument 5 is the indicator you want to use
 
 ## for the first hypothesis   
forestplot(h1,'Publication.Year',2019,2021,'is_open_code')
forestplot(h1,'Publication.Year',2019,2021,'is_open_data')
forestplot(h1,'Publication.Year',2019,2021,'is_register_pred')
forestplot(h1,'Publication.Year',2019,2021,'is_coi_pred')
forestplot(h1,'Publication.Year',2019,2021,'is_fund_pred')

## for the second hypothesis
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_open_code')
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_open_data')
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_register_pred')
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_coi_pred')
forestplot(h2,'COVID.Year','2019','2021 FALSE','is_fund_pred')

## for the third hypothesis
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_open_code')
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_open_data')
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_register_pred')
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_coi_pred')
forestplot(h3,'COVID.Year','2021 FALSE','2021 TRUE','is_fund_pred')