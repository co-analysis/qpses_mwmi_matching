## Load Libraries
source("load_libs.R")

#--------------------------------------------------------------------------------------------------------------------------------------#
rawdat_mwmi <- readRDS(url("https://github.com/co-analysis/mwmi.govuk.data/blob/main/data/output/cleaned_data_trial.RDS?raw=TRUE", "rb"))
#--------------------------------------------------------------------------------------------------------------------------------------#

mwmi_df <- rawdat_mwmi %>% 
  filter(group=="payroll", sub_group=="total") %>% # ONLY USING PAYROLL - TOTAL
  mutate(org_type=str_squish(str_to_title(org_type))) %>%
  #
  mutate(Quarter=case_when(Month=="March"~1,Month=="June"~2,Month=="September"~3,Month=="December"~4)) %>%
  mutate(Month_n=case_match(as.character(Month), 
                            "January"~"01", "February"~"02", "March"~"03", "April"~"04", "May"~"05", "June"~"06",
                            "July"~"07", "August"~"08", "September"~"09", "October"~"10", "November"~"11", "December"~"12"
  )) %>%
  mutate(t=paste0(Year,Quarter)) %>%
  mutate(tm=paste0(Year,Month_n)) %>%
  mutate(across(c(Year,Quarter:tm), ~as.numeric(.))) %>%
  #
  rename("mwmi_dept"=Department, "mwmi_body"=Body) %>%
  filter(tm>=202403) #%>% # Filter to only 2024 Q1 onwards

#------------------------#
mwmi_to_qpses_name_matcher = read_csv("data/output_data/mwmi_to_qpses_name_matcher.csv")
#------------------------#

mwmi_df %>% filter(grepl("Health and Social Care", mwmi_body)) %>% filter(measure=="fte") %>% arrange(tm) %>% print(n=Inf)
mwmi_join %>% filter(grepl("commun",dept_norm)) %>% filter(measure=="FTE") %>% arrange(tm)


mwmi_join <- mwmi_df %>%
  full_join(mwmi_to_qpses_name_matcher) %>%
  #
  arrange(tm,dept_norm,body_norm) %>%
  select(group,sub_group,Year,Month,tm,mwmi_dept,mwmi_body,org_type, measure,value, dept_norm,body_norm) %>%
  #select(-group,-sub_group) %>%
  #select(-mwmi_dept,-mwmi_body) %>%
  rename("mwmi_value"=value) %>%
  mutate(measure=ifelse(measure=="hc","Headcount","FTE")) %>%
  #
  mutate(dept_norm=case_when(
    grepl("communities",dept_norm) & tm<202406 ~ "levelling up housing and communities",
    grepl("communities",dept_norm) & tm>=202406 ~ "housing communities and local government",
    T~dept_norm)) %>% 
  #
  filter(!(grepl("level",dept_norm) & tm>=202406)) %>%
  filter(!(grepl("local gov",dept_norm) & tm<202406)) %>%
  filter(dept_norm!="communities and local Government") %>% 
  #
  filter(!(grepl("Teaching Regulation Auth", mwmi_body) & tm>=202504)) %>% 
  #
  ## temp filter - 2025/11/11 - remove once returns are corrected
  filter(!(tm==202507 & mwmi_body=="Department of Health and Social Care (excluding agencies)")) %>% 
  #filter(!(tm==202503 & dept_norm=="foreign commonwealth and development office")) %>%
  filter(!(dept_norm=="cabinet office" & body_norm=="uk hydrographic office"))

## Quick check for duplicated bodies
#mwmi_join %>% group_by(dept_norm,body_norm,tm) %>% tally() %>% filter(n!=2)

## Create total FTE/Headcount rows for MWMI
mwmi_total <- mwmi_join %>%
  group_by(group,sub_group,Year,Month,tm,measure) %>%
  summarise(mwmi_value=sum(mwmi_value,na.rm=T)) %>%
  mutate(mwmi_dept="manual_total", mwmi_body="manual_total") %>%
  mutate(dept_norm="total employment", body_norm="total employment") %>%
  ungroup()

mwmi_join2 <- mwmi_join %>% bind_rows(mwmi_total) %>% filter(!mwmi_value==0)

## Save RDS file to data folder
saveRDS(mwmi_join2, file="data/output_data/clean_data_mwmi.RDS")
mwmi_join2 <- read_rds("data/output_data/clean_data_mwmi.RDS")
