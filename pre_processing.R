# PRE-PROCESSING

library(dplyr)
library(tibble)
library(ddpcr)



code_df=read.delim('./input/codesharing.csv', sep=',')
rest_df=read.delim('./input/resttransp.csv', sep=',')
combined=merge(x = code_df, y = rest_df, by = "pmid", all = TRUE)

search=read.delim('./input/search.csv', sep=',')

colnames(search)[1]='pmid'
colnames(search)[9]='pmcid_uid'

search_combined=merge(x = search, y = combined, by = "pmid", all = TRUE)

select_combined=search_combined %>%
  select(c(1,11,16,17,21,24,23,25,37,38,65,66,111,112,141,7,6,3))

df=select_combined[(select_combined['is_research.x']==TRUE | select_combined['is_review.x']==TRUE),]

df[is.na(df$is_open_code),]['is_open_code']=FALSE
df[is.na(df$is_open_data),]['is_open_data']=FALSE

# Adds COVID column
covpapers=read.delim('./input/COVID_sample.csv',sep=',')

int=intersect(df['pmid'][df$Publication.Year!=2019,],covpapers$PMID)

df$COVID <- with(df, ifelse(pmid %in% int, 'TRUE', 'FALSE'))

# test if there are NA values in the pmid column
df[is.na(df$pmid),]

# Paste together COVID status and Publication Year.
cols <- c( 'Publication.Year' , 'COVID'  )

df$COVID.Year <- apply( df[ , cols ] , 1 , paste , collapse = " " )

# turn 2019 FALSE into 2019 simply
df=df %>%
  mutate(
    dplyr::across(
      .cols = COVID.Year, 
      .fns = ~ dplyr::if_else(stringr::str_detect(.x, "2019"), '2019', .x)
    )
  )

# validation of CID and JID papers
df_temp=df[df$Journal.Book %in% c('Clin Infect Dis','J Infect Dis'),]
df_temp=(df_temp[df_temp$is_register_pred == 'FALSE' & df_temp$is_open_code == 'FALSE'& df_temp$is_open_data == 'FALSE' & df_temp$is_fund_pred == 'FALSE'& df_temp$is_coi_pred=='FALSE', ])

# output it to use in the manual data correction of the CID and JID papers
write.csv(df_temp,'./input/manual_correction/correction_CID_JID.csv')

# get input from the manual data correction
manual293=read.delim('./input/manual_correction/correction_CID_JID_completed.csv',sep=',')

df_final=merge_dfs_overwrite_col(df, manual293, bycol = "DOI")
  
write.csv(df_final,"./input/output.csv", row.names = FALSE)

# VALIDATION FORM
# get manual extraction form for all indicators
sample=read.delim('./input/sample_metadata.csv', sep=',')
sample_pmids=as.list(sample$PMID)
sample_df=df_final%>%filter(pmid %in% sample_pmids)

sample_validation=sample_df%>% select(c(1,3,7,8,9,10,11,12,13,14,15,16))
sample_validation =add_column(sample_validation, validate_data = '', .after = 4)
sample_validation =add_column(sample_validation, validate_code = '', .after = 7)
sample_validation =add_column(sample_validation, method = '', .after = 8)
sample_validation =add_column(sample_validation, validate_coi = '', .after = 11)
sample_validation =add_column(sample_validation, validate_fund = '', .after = 14)
sample_validation =add_column(sample_validation, validate_reg= '', .after = 17)
sample_validation =add_column(sample_validation, comments= '', .after = 18)

write.csv(sample_validation,'./input/manual_validation/sample_validation.csv')

# read here to perform the analyses below

