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
  ungroup()

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
  pivot_wider(names_from = source, names_prefix = "value_", values_from = value_body) %>%
  mutate(dif=value_QPSES/value_MWMI, dif_n=value_QPSES-value_MWMI) %>%
  #
  mutate(matching=ifelse(dif<.95|dif>1.05, "n","y")) %>%
  mutate(matching=ifelse(((dif_n>=0&dif_n<=5) | (dif_n<=0&dif_n>=-5)), "y",matching)) %>%
  group_by(measure,dept_norm,body_norm) %>%
  fill(matching, .direction="down") %>%
  ungroup()


df_complete <- df_match_with_check %>%
  #
  mutate(value_LA=ifelse(matching=="y" & is.na(value_MWMI), value_QPSES, value_MWMI)) %>%
  mutate(value_LA=ifelse(is.na(matching), value_QPSES, value_LA)) %>%
  #
  group_by(measure,dept_norm,body_norm) %>%
  mutate(value_LA = ifelse(tm==max(df_match_with_check$tm) & is.na(value_LA), lag(value_LA), value_LA)) %>%
  fill(value_LA, .direction="down") %>%
  ungroup() %>%
  #
  select(-dif,-dif_n,-matching) %>%
  #
  group_by(measure,dept_norm,body_norm) %>%
  mutate(known=ifelse(!is.na(value_QPSES)|!is.na(value_MWMI),tm,NA)) %>%
  mutate(known=max(known,na.rm=T)) %>%
  ungroup() %>%
  mutate(value_imput=ifelse(tm>=known,value_LA,NA)) %>%
  mutate(value_LA=ifelse(tm>known,NA,value_LA)) %>%
  select(-known) %>%
  #
  pivot_longer(starts_with("value_"), names_to="source", values_to="value") %>%
  mutate(source=gsub("value_","",source)) %>%
  mutate(source=ifelse(source=="LA","available",source)) %>%
  #
  mutate(tm=as.numeric(tm)) %>%
  #
  left_join(df_match_OG_names) %>%
  distinct(.) %>%
  mutate(across(qpses_dept:qpses_body, ~ifelse(source=="QPSES",.,NA))) %>%
  mutate(across(mwmi_dept:mwmi_body, ~ifelse(source=="MWMI",.,NA))) %>%
  select(tm,ends_with("_norm"),qpses_scope,measure,source,value,starts_with("qpses_"),starts_with("mwmi_"))


df_totals_to_join <- df_complete %>% 
  filter(!body_norm=="total employment") %>% 
  group_by(tm,qpses_scope,measure,source) %>% 
  summarise(value=sum(value,na.rm=T)) %>%
  mutate(mwmi_dept=ifelse(source=="MWMI","manual_total",NA), mwmi_body=ifelse(source=="MWMI","manual_total",NA)) %>%
  mutate(dept_norm="Total Employment", body_norm="total employment") %>%
  filter(source!="QPSES")


df_complete2 <- df_complete %>%
  filter(!(source!="QPSES" & body_norm=="total employment")) %>%
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

#---------------------------------------------------------------------------------#
## Save RDS file to data folder
saveRDS(df_complete2, file="data/output_data/matched_data_qpses_mwmi.RDS")
