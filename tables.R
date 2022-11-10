# TABLE CREATION IN CSV

# read df
df=read.delim('./input/output.csv',sep=',')


# TABLE 1
# to get the sum of journals' publications
table(df$Journal.Book)

## overall transparency
write.csv(df  %>% 
  summarise_at(c("is_open_code", "is_open_data","is_register_pred", "is_coi_pred","is_fund_pred"), ~paste0(sum(.x), '(', round(sum(.x) * 100 / n()), ')')),'./output/tables/table_transp_overall.csv')

### by journal
write.csv(df %>% group_by(Journal.Book) %>% 
  summarise_at(c("is_open_code", "is_open_data","is_register_pred", "is_coi_pred","is_fund_pred"), ~paste0(sum(.x), '(', round(sum(.x) * 100 / n()), ')')),'./output/tables/table_transp_journals.csv') # change the nas.
  
  
 # TABLE 2
 
 # transparency for each of the groups in the transparency
   write.csv(  df %>%group_by(COVID.Year) %>% 
  summarize_at(c("is_open_code", "is_open_data","is_register_pred", "is_coi_pred","is_fund_pred"),~paste0(sum(.x), '(', round(sum(.x) * 100 / n()), ')')),'./output/tables/primary_comp_table.csv')  
  
     write.csv(  df %>%group_by(Publication.Year) %>% 
  summarize_at(c("is_open_code", "is_open_data","is_register_pred", "is_coi_pred","is_fund_pred"),~paste0(sum(.x), '(', round(sum(.x) * 100 / n()), ')')),'./output/tables/primary_comp_table_years.csv')  
  
  # TABLE 3 WAS EXTRACTED FROM THE FOREST PLOTS.
  # THE CODE FOR THE FOREST PLOTS IS FOUND UNDER meta_analyses.R
  
  
  # TABLE 4
cols <- c( 'Journal.Book' ,'COVID.Year' )

df$COVID.Year.Journal <- apply( df[ , cols ] , 1 , paste , collapse = " " )
write.csv(df %>%group_by(COVID.Year.Journal) %>% 
  summarize_at(c("is_open_code", "is_open_data","is_register_pred", "is_coi_pred","is_fund_pred"),~paste0(sum(.x), '(', round(sum(.x) * 100 / n()), ')')),'./output/tables/stratified_perjournal_comp.csv')
  
  # SUPPLEMENTARY TABLES WERE CREATED MANUALLY
  
  