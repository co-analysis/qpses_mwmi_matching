## Load Libraries
source("load_libs.R")

## Load cleaned data from QPSES & MWMI
#--------#
join_qpses = readRDS(file="data/output_data/clean_data_qpses.RDS")
join_mwmi = readRDS(file="data/output_data/clean_data_mwmi.RDS")
#--------#

#--------------------------------------------------------------------------------#
## Check name matching between QPSES and MWMI
full_join(
  join_qpses %>% select(ends_with("_norm"),qpses_dept,qpses_body) %>% distinct(.),
  join_mwmi %>% select(ends_with("_norm"),mwmi_dept,mwmi_body) %>% distinct(.)
) %>%
  filter(is.na(mwmi_dept)|is.na(mwmi_body)) %>% arrange(dept_norm,body_norm) %>% print(n=Inf)


# join_qpses %>% select(ends_with("_norm"),qpses_dept,qpses_body) %>% distinct(.) %>%
#   filter(grepl("archive",body_norm))
# ##
# join_mwmi %>% select(ends_with("_norm"),mwmi_dept,mwmi_body) %>% distinct(.) %>% 
#   filter(grepl("archive",body_norm))
# 
# mwmi_names_norm %>% filter(grepl("archive",body_norm))
#--------------------------------------------------------------------------------#

df_match <- full_join(
  # select only relevant columns from each dataset
  join_qpses %>% select(t,tm,dept_norm,body_norm,measure,starts_with("qpses")), 
  join_mwmi %>% select(tm,dept_norm,body_norm,measure, mwmi_value, mwmi_dept,mwmi_body,org_type)
) %>% 
  select(tm,t,ends_with("_norm"),org_type, measure,ends_with("_value"), qpses_scope, qpses_dept,qpses_body,mwmi_dept,mwmi_body) %>%
  distinct(.) %>% # ensures no duplicates are being created
  arrange(tm,dept_norm,body_norm) %>%
  #
  group_by(body_norm) %>%
  fill(qpses_scope, .direction="updown") %>% # fill in qpses_scope column for non-qpses months
  ungroup() %>%
  #
  group_by(tm,dept_norm,body_norm) %>% # fills in mwmi_names for months with no returns
  fill(mwmi_dept,.direction="updown") %>%
  fill(mwmi_body,.direction="updown") %>%
  ungroup() %>%
  #
  filter(qpses_scope=="y")

#---------------#
# Pull out orginal names from QPSES and MWMI files, to match on later
df_OG_names <- df_match %>% select(tm,dept_norm,body_norm,qpses_dept:mwmi_body) %>% distinct(.)
#---------------#

#--------------------------------------------------------------------------------------------------------------------------------------#

## Create "wide" dataset with row for each source/measure
## Ensures that each organisation has full month coverage
df_match2_wide <- df_match %>%
  select(tm:qpses_scope) %>%
  pivot_longer(ends_with("_value"), names_to="source", values_to="value_body") %>%
  mutate(source=str_to_upper(str_remove(source,"_value"))) %>%
  arrange(tm,dept_norm,body_norm) %>%
  #
  select(tm,ends_with("_norm"),qpses_scope,source,measure,value_body) %>%
  pivot_wider(names_from = tm, values_from = value_body)

#--------------------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------#
most_recent_qpses_q = max(join_qpses$tm) ## Most recent QPSES 
#---------------------------------------#

df_match3_qpses_check <- df_match2_wide %>%
  pivot_longer(starts_with("20"), names_to="tm", values_to="value_body") %>%
  relocate(tm,.before=measure) %>%
  relocate(measure,.after=source) %>%
  arrange(tm,dept_norm,body_norm) %>%
  #
  ## Removing MOG rename for DLUHC/MHCLG
  filter(!(grepl("level",dept_norm) & tm>=202406)) %>%
  filter(!(grepl("local gov",dept_norm) & tm<202406)) %>%
  filter(dept_norm!="communities and local government") %>% 
  #
  pivot_wider(names_from = source, names_prefix = "raw_value_", values_from = value_body) %>% 
  #
  #
  group_by(body_norm,measure) %>%
  mutate(latest_qpses_month_by_body=ifelse(!is.na(raw_value_QPSES),tm,NA)) %>%
  mutate(latest_qpses_month_by_body=as.numeric(max(latest_qpses_month_by_body, na.rm=TRUE))) %>%
  mutate(next_qpses_month_by_body=ifelse(grepl(12,latest_qpses_month_by_body),latest_qpses_month_by_body+91,latest_qpses_month_by_body+3)) %>% 
  #
  mutate(qpses_remove_check=case_when(
    tm<=latest_qpses_month_by_body ~ "keep", # Keep data before latest QPSES data for org
    is.na(raw_value_QPSES) & tm==next_qpses_month_by_body & tm<=most_recent_qpses_q ~ "remove", # Remove data when NA in QPSES and quarter is before most recent_qpses_q
    T~NA)) %>%
  #
  fill(qpses_remove_check,.direction = "down") %>%
  mutate(qpses_remove_check = ifelse(tm>latest_qpses_month_by_body & (is.na(raw_value_QPSES) & is.na(raw_value_MWMI)) & qpses_remove_check=="keep", NA, qpses_remove_check)) %>% 
  fill(qpses_remove_check,.direction = "updown") %>% 
  #
  ungroup() %>%
  arrange(tm,dept_norm,body_norm) %>%
  filter(qpses_remove_check=="keep")

#--------------------------------------------------------------------------------------------------------------------------------------#

df_match4_value_compare <- df_match3_qpses_check %>% 
  select(-(latest_qpses_month_by_body:qpses_remove_check)) %>% 
  mutate(dif=raw_value_MWMI/raw_value_QPSES, dif_n=raw_value_QPSES-raw_value_MWMI) %>%
  #
  ## Test if difference between QPSES and MWMI is less than 5% or within 5 FTE/headcount 
  mutate(matching=ifelse(dif<=.95|dif>=1.05, "n","y")) %>%
  mutate(matching=ifelse(((dif_n>=0&dif_n<=5) | (dif_n<=0&dif_n>=-5)), "y",matching)) %>%
  mutate(matching=ifelse(!is.na(raw_value_QPSES) & is.na(raw_value_MWMI), "n", matching)) %>% 
  group_by(measure,dept_norm,body_norm) %>%
  fill(matching, .direction="down") %>%
  ungroup() 
 


df_match4_value_compare %>% filter(measure=="FTE") %>% filter(grepl("export",body_norm))
#--------------------------------------------------------------------------------------------------------------------------------------#

df_complete <- df_match4_value_compare %>%
  #
  mutate(tm=as.numeric(tm)) %>% 
  filter(tm>=2024) %>% 
  #----------------------------------------#
  #
  group_by(measure,dept_norm,body_norm) %>%
  # Create column with any available data for month in question
  mutate(value_available=ifelse(!is.na(raw_value_QPSES),raw_value_QPSES,NA)) %>%
  mutate(value_available=ifelse(is.na(value_available) & matching=="y", raw_value_MWMI, value_available)) %>%
  fill(value_available, .direction="down") %>% 
  #
  select(-dif,-dif_n,-matching) %>%
  #
  # Take the latest "available" data from column created above and roll forward where data is unavailable 
  group_by(measure,dept_norm,body_norm) %>%
  mutate(value_available = ifelse(tm==max(df_match4_value_compare$tm) & is.na(value_available), lag(value_available), value_available)) %>%
  fill(value_available, .direction="down") %>%
  ungroup() %>% 
  rename("value_final"=value_available)

#--------------------------------------------------------------------------------------------------------------------------------------#

df_complete_long <- df_complete %>%   
  mutate(tm=as.numeric(tm)) %>%
  #
  left_join(df_OG_names) %>% # Match on original QPSES/MWMI names from files
  distinct(.) %>%
  #mutate(across(qpses_dept:qpses_body, ~ifelse(source=="raw_QPSES",.,NA))) %>%
  #mutate(across(mwmi_dept:mwmi_body, ~ifelse(source=="raw_MWMI",.,NA))) %>%
  #select(tm,ends_with("_norm"),qpses_scope,measure,source,value,starts_with("qpses_"),starts_with("mwmi_")) %>%
  #
  filter(!is.na(value_final)) %>% 
  #
  filter(!(body_norm=="defence electronics and components agency" & tm>202403)) %>% 
  #
  filter(!(grepl("level",dept_norm) & tm>=202406)) %>%
  filter(!(grepl("local gov",dept_norm) & tm<202406)) %>%
  filter(dept_norm!="communities and local government")

#----------------------------------------#

## Create "total employment" data based on new data columns
# df_totals_to_join <- df_complete_long %>% 
#   filter(body_norm=="total employment") %>% select(dept_norm:raw_value_QPSES) %>%
#   #
#   full_join(
#     df_complete_long %>% 
#       filter(!body_norm=="total employment") %>% # remove original "total employment" so that do not double count
#       select(-starts_with("raw_value")) %>%
#       group_by(qpses_scope,measure,tm) %>%
#       summarise(value_final=sum(value_final, na.rm=T))
#   ) %>%
#   mutate(qpses_dept="Total employment", qpses_body=qpses_dept) %>%
#   mutate(mwmi_dept="manual_total", mwmi_body=mwmi_dept) %>%
#   mutate(across(qpses_dept:qpses_body, ~ifelse(is.na(raw_value_QPSES),NA,.)))
# #


# value_final totals to join
test <- df_complete_long %>% 
  filter(!body_norm=="total employment") %>% # remove original "total employment" so that do not double count
  select(-starts_with("raw_value")) %>%
  group_by(qpses_scope,measure,tm) %>%
  summarise(value_final=sum(value_final, na.rm=T))

df_totals_to_join <- df_complete_long %>% 
  filter(body_norm=="total employment") %>% 
  select(-value_final) %>%
  #
  left_join(test) %>%
  mutate(value_final=ifelse(is.na(raw_value_QPSES),value_final,raw_value_QPSES))

#df_complete_long %>% filter(dept_norm=="total employment", measure=="FTE")
df_totals_to_join %>% filter(dept_norm=="total employment", measure=="FTE")
#----------------------------------------#

df_complete2 <- df_complete_long %>%
  filter(!body_norm=="total employment") %>% # remove original "total employment" so that do not duplicate
  bind_rows(df_totals_to_join) %>%
  arrange(tm,dept_norm,body_norm,measure) %>%
  #
  # Create date column for use in dashboard charts later
  mutate(Date=paste0(substr(tm,1,4),"-",substr(tm,5,6),"-01")) %>%
  mutate(Date=as.Date(Date, "%Y-%m-%d")) %>%
  mutate(lastday=days_in_month(Date)) %>%
  group_by(tm) %>%
  mutate(Date=gsub('.{2}$', lastday, Date)) %>%
  mutate(Date=as.Date(Date, "%Y-%m-%d")) %>%
  ungroup() %>%
  select(-lastday)  %>%
  relocate(Date,.after=tm)


#--------------------------------------------------------------------------------------------------------------------------------------#
## Save RDS file to data folder
saveRDS(df_complete2, file="data/output_data/matched_data_qpses_mwmi_V5.RDS")
#
#df_complete2 <- read_rds("data/output_data/matched_data_qpses_mwmi_V5.RDS")
#df_complete2_old <- read_rds("data/output_data/matched_data_qpses_mwmi_V4.RDS")
