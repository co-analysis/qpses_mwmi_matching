## Load Libraries
source("load_libs.R")

## Load cleaned data from QPSES & MWMI
#--------#
join_qpses = readRDS(file="data/output_data/data_match_qpses.RDS")
join_mwmi = readRDS(file="data/output_data/data_match_mwmi.RDS")
#--------#

df_match <- full_join(join_qpses, join_mwmi) %>% 
  #select(tm,t,ends_with("_dept"),org_type,value_type, measure, ends_with("_norm"), ends_with("_value"), qpses_scope) %>%
  select(tm,t,ends_with("_norm"),org_type,value_type, measure,ends_with("_value"), qpses_scope, qpses_dept,qpses_body,mwmi_dept,mwmi_body) %>%
  distinct(.) %>%
  arrange(tm,dept_norm,body_norm) %>%
  #
  group_by(body_norm) %>%
  fill(qpses_scope, .direction="updown") %>%
  ungroup() %>%
  #
  group_by(tm,dept_norm,body_norm) %>%
  fill(mwmi_dept,.direction="updown") %>%
  fill(mwmi_body,.direction="updown") %>%
  ungroup() %>%
  #
  filter(qpses_scope=="y")

#---------------#
# Pull out orginal names from QPSES and MWMI files, to match on later
df_match_OG_names <- df_match %>% select(tm,dept_norm,body_norm,qpses_dept:mwmi_body) %>% distinct(.)
#---------------#

df_match_wide <- df_match %>% 
  filter(qpses_scope=="y") %>%
  #
  select(tm:qpses_scope) %>%
  pivot_longer(ends_with("_value"), names_to="source", values_to="value_body") %>%
  mutate(source=str_to_upper(str_remove(source,"_value"))) %>%
  arrange(tm,dept_norm,body_norm) %>%
  #
  select(tm,ends_with("_norm"),qpses_scope,source,measure,value_body) %>%
  pivot_wider(names_from = tm, values_from = value_body)

#--------------------------------------------------------------------------------------------------------------------------------------#

df_match_with_check <- df_match_wide %>%
  pivot_longer(starts_with("20"), names_to="tm", values_to="value_body") %>%
  relocate(tm,.before=measure) %>%
  relocate(measure,.after=source) %>%
  arrange(tm,dept_norm,body_norm) %>%
  #
  ## Removing MOG rename for DLUHC/MHCLG
  filter(!(grepl("Level",dept_norm) & tm>=202406)) %>%
  filter(!(grepl("Local Gov",dept_norm) & tm<202406)) %>%
  filter(dept_norm!="Communities and Local Government") %>% 
  #
  pivot_wider(names_from = source, names_prefix = "raw_value_", values_from = value_body) %>%
  mutate(dif=raw_value_QPSES/raw_value_MWMI, dif_n=raw_value_QPSES-raw_value_MWMI) %>%
  #
  mutate(matching=ifelse(dif<.95|dif>1.05, "n","y")) %>%
  mutate(matching=ifelse(((dif_n>=0&dif_n<=5) | (dif_n<=0&dif_n>=-5)), "y",matching)) %>%
  group_by(measure,dept_norm,body_norm) %>%
  fill(matching, .direction="down") %>%
  ungroup()

#--------------------------------------------------------------------------------------------------------------------------------------#

df_complete <- df_match_with_check %>%
  group_by(measure,dept_norm) %>%
  mutate(latest_month_with_data=ifelse(!(is.na(raw_value_QPSES) & is.na(raw_value_MWMI)),tm,NA)) %>%
  mutate(latest_month_with_data=max(latest_month_with_data, na.rm=TRUE)) %>%
  #
  mutate(value_available=ifelse(matching=="y" & is.na(raw_value_MWMI), raw_value_QPSES, raw_value_MWMI)) %>%
  mutate(value_available=ifelse(is.na(matching), raw_value_QPSES, value_available)) %>%
  #
  select(-dif,-dif_n,-matching) %>%
  #
  group_by(measure,dept_norm,body_norm) %>%
  mutate(value_available = ifelse(tm==max(df_match_with_check$tm) & is.na(value_available), lag(value_available), value_available)) %>%
  fill(value_available, .direction="down") %>%
  ungroup() %>%
  #
  group_by(measure,dept_norm,body_norm) %>%
  mutate(ref_month_for_body=ifelse(!is.na(raw_value_QPSES)|!is.na(raw_value_MWMI),tm,NA)) %>%
  mutate(ref_month_for_body=max(ref_month_for_body,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(value_imput=ifelse(tm>=ref_month_for_body,value_available,NA)) %>%
  #
  mutate(value_available=ifelse(tm<=latest_month_with_data, value_available, NA)) %>% # Set "_available" to be latest data month and earlier
  mutate(value_imput=ifelse(tm<latest_month_with_data, NA, value_imput)) %>% # Set "_imput" to be latest data month and later
  select(dept_norm:tm,starts_with("raw_"),starts_with("value_")) %>%
  #
  mutate(value_match_QM = ifelse(is.na(raw_value_MWMI),NA,raw_value_QPSES))

#--------------------------------------------------------------------------------------------------------------------------------------#

df_complete_long <- df_complete %>%   
  #
  pivot_longer(contains("value_"), names_to="source", values_to="value") %>%
  mutate(source=gsub("value_","",source)) %>%
  #
  mutate(tm=as.numeric(tm)) %>%
  #
  left_join(df_match_OG_names) %>%
  distinct(.) %>%
  mutate(across(qpses_dept:qpses_body, ~ifelse(source=="raw_QPSES",.,NA))) %>%
  mutate(across(mwmi_dept:mwmi_body, ~ifelse(source=="raw_MWMI",.,NA))) %>%
  select(tm,ends_with("_norm"),qpses_scope,measure,source,value,starts_with("qpses_"),starts_with("mwmi_")) %>%
  #
  filter(!(body_norm=="defence electronics and components agency" & tm>202403))

#----------------------------------------#
df_totals_to_join <- df_complete_long %>% 
  filter(!body_norm=="total employment") %>%
  select(tm:value) %>%
  pivot_wider(names_from = source, values_from = value) %>%
  mutate(available=ifelse(is.na(available),imput,available)) %>%
  #
  #filter(measure=="FTE") %>%
  #filter(dept_norm=="Defence") %>%
  #filter(tm>=202503) %>% print(n=Inf)
  pivot_longer(raw_QPSES:match_QM, names_to="source", values_to="value") %>%
  #
  group_by(tm,qpses_scope,measure,source) %>% 
  summarise(value=sum(value, na.rm=TRUE)) %>%
  mutate(mwmi_dept=ifelse(source=="raw_MWMI","manual_total",NA), mwmi_body=ifelse(source=="raw_MWMI","manual_total",NA)) %>%
  mutate(dept_norm="Total Employment", body_norm="total employment") %>%
  filter(source!="raw_QPSES")
#----------------------------------------#

df_complete2 <- df_complete_long %>%
  filter(!(source!="raw_QPSES" & body_norm=="total employment")) %>%
  bind_rows(df_totals_to_join) %>%
  arrange(tm,dept_norm,body_norm,measure) %>%
  #
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
saveRDS(df_complete2, file="data/output_data/matched_data_qpses_mwmi_V2.RDS")
