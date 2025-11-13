## Load Libraries
source("load_libs.R")

## Load cleaned data from QPSES & MWMI
#--------#
join_qpses = readRDS(file="data/output_data/data_match_qpses.RDS")
join_mwmi = readRDS(file="data/output_data/data_match_mwmi.RDS")
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
df_match_OG_names <- df_match %>% select(tm,dept_norm,body_norm,qpses_dept:mwmi_body) %>% distinct(.)
#---------------#

## Create "wide" dataset with row for each source/measure
## Makes each organisation have full month coverage
df_match_wide <- df_match %>%
  select(tm:qpses_scope) %>%
  pivot_longer(ends_with("_value"), names_to="source", values_to="value_body") %>%
  mutate(source=str_to_upper(str_remove(source,"_value"))) %>%
  arrange(tm,dept_norm,body_norm) %>%
  #
  select(tm,ends_with("_norm"),qpses_scope,source,measure,value_body) %>%
  pivot_wider(names_from = tm, values_from = value_body)


# df_match %>% 
#   select(tm:qpses_scope) %>%
#   pivot_longer(ends_with("_value"), names_to="source", values_to="value_body") %>%
#   mutate(source=str_to_upper(str_remove(source,"_value"))) %>%
#   arrange(tm,dept_norm,body_norm) %>%
#   #
#   select(tm,ends_with("_norm"),qpses_scope,source,measure,value_body) %>%
#   #
#   dplyr::group_by(dept_norm, body_norm, qpses_scope, source, measure, tm) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) 
#   
# df_match %>% filter(tm==202507) %>% filter(grepl("health and soc",body_norm))

#--------------------------------------------------------------------------------------------------------------------------------------#

## Set data to "long" version, and add in figure match check
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
  ## Test if difference between QPSES and MWMI is over 5% or 5 FTE/headcount 
  mutate(matching=ifelse(dif<.95|dif>1.05, "n","y")) %>%
  mutate(matching=ifelse(((dif_n>=0&dif_n<=5) | (dif_n<=0&dif_n>=-5)), "y",matching)) %>%
  group_by(measure,dept_norm,body_norm) %>%
  fill(matching, .direction="down") %>%
  ungroup()

## Look up where organisaitons are not matching
df_match_with_check %>% filter(matching=="n") %>% group_by(dept_norm,body_norm) %>% tally() 
#--------------------------------------------------------------------------------------------------------------------------------------#

## Complete dataset using both QPSES/MWMI figures to "fill in" missing data
df_complete <- df_match_with_check %>%
  #
  # Create column showing the latest month that has data for both QPSES and MWMI
  group_by(measure,dept_norm) %>%
  mutate(latest_month_with_data=ifelse(!(is.na(raw_value_QPSES) & is.na(raw_value_MWMI)),tm,NA)) %>%
  mutate(latest_month_with_data=max(latest_month_with_data, na.rm=TRUE)) %>%
  #
  # Create column with any available data for month in question
  mutate(value_available=ifelse(matching=="y" & is.na(raw_value_MWMI), raw_value_QPSES, raw_value_MWMI)) %>% # If data matching and MWMI is missing, use QPSES value
  mutate(value_available=ifelse(is.na(matching), raw_value_QPSES, value_available)) %>% # If data does NOT match, use QPSES value
  mutate(value_available=ifelse(!is.na(raw_value_QPSES),raw_value_QPSES,value_available)) %>% # If data is available for QPSES, use QPSES value
  #
  select(-dif,-dif_n,-matching) %>%
  #
  # Take the latest "available" data from column created above and roll forward where data is unavailable 
  group_by(measure,dept_norm,body_norm) %>%
  mutate(value_available = ifelse(tm==max(df_match_with_check$tm) & is.na(value_available), lag(value_available), value_available)) %>%
  fill(value_available, .direction="down") %>%
  ungroup() %>%
  #
  # Create column with data for latest month with known data from either QPSES or MWMI
  group_by(measure,dept_norm,body_norm) %>%
  mutate(ref_month_for_body=ifelse(!is.na(raw_value_QPSES)|!is.na(raw_value_MWMI),tm,NA)) %>%
  mutate(ref_month_for_body=max(ref_month_for_body,na.rm=TRUE)) %>%
  ungroup() %>%
  #
  mutate(value_imput=ifelse(tm>=ref_month_for_body,value_available,NA)) %>%
  #
  mutate(value_available=ifelse(tm<=latest_month_with_data, value_available, NA)) %>% # Set "_available" to be latest data month and BEFORE
  mutate(value_imput=ifelse(tm<latest_month_with_data, NA, value_imput)) %>% # Set "_imput" to be latest data month and LATER
  select(dept_norm:tm,starts_with("raw_"),starts_with("value_")) %>%
  #
  mutate(value_match_QM = ifelse(is.na(raw_value_MWMI),NA,raw_value_QPSES)) %>% # column with data for QPSES where MWMI data is available
  mutate(value_final=ifelse(is.na(value_available),value_imput,value_available)) %>% # Final - Complete data for use
  #
  relocate(value_match_QM,.after=raw_value_MWMI)


#--------------------------------------------------------------------------------------------------------------------------------------#

df_complete_long <- df_complete %>%   
  mutate(tm=as.numeric(tm)) %>%
  #
  left_join(df_match_OG_names) %>% # Match on original QPSES/MWMI names from files
  distinct(.) %>%
  #mutate(across(qpses_dept:qpses_body, ~ifelse(source=="raw_QPSES",.,NA))) %>%
  #mutate(across(mwmi_dept:mwmi_body, ~ifelse(source=="raw_MWMI",.,NA))) %>%
  #select(tm,ends_with("_norm"),qpses_scope,measure,source,value,starts_with("qpses_"),starts_with("mwmi_")) %>%
  #
  filter(!(body_norm=="defence electronics and components agency" & tm>202403))

#----------------------------------------#

## Create "total employment" data based on new data columns
df_totals_to_join <- df_complete_long %>% 
  filter(body_norm=="total employment") %>% select(dept_norm:raw_value_QPSES) %>%
  #
  full_join(
    df_complete_long %>% 
      filter(!body_norm=="total employment") %>% # remove original "total employment" so that do not double count
      select(-raw_value_QPSES) %>%
      group_by(qpses_scope,measure,tm) %>%
      summarise(across(raw_value_MWMI:value_final, ~ sum(.,na.rm=TRUE)))
  ) %>%
  mutate(qpses_dept="Total employment", qpses_body=qpses_dept) %>%
  mutate(mwmi_dept="manual_total", mwmi_body=mwmi_dept) %>%
  mutate(across(qpses_dept:qpses_body, ~ifelse(is.na(raw_value_QPSES),NA,.)))
#

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


df_complete_long %>%
  filter(!body_norm=="total employment") %>%
  bind_rows(df_totals_to_join) %>%
  arrange(tm,dept_norm,body_norm,measure) %>%
  #
  #select(-c(qpses_dept:mwmi_body)) %>%
  # Create Year and Month columns from "tm"
  mutate(Year=substr(tm,1,4)) %>%
  mutate(Month=as.character(month(as.numeric(substr(tm,5,6)),label=TRUE, abbr=FALSE))) %>%
  relocate(c(Year,Month),.after=tm)


#--------------------------------------------------------------------------------------------------------------------------------------#
## Save RDS file to data folder
saveRDS(df_complete2, file="data/output_data/matched_data_qpses_mwmi_V5.RDS")
#
# df_complete2 <- read_rds("data/output_data/matched_data_qpses_mwmi_V5.RDS")
# df_complete2_old <- read_rds("data/output_data/matched_data_qpses_mwmi_V4.RDS")
