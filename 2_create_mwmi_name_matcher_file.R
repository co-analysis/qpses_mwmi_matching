## Load Libraries
source("load_libs.R")

#--------------------------------------------------------------------------------------------------------------------------------------#
rawdat_mwmi <- readRDS(url("https://github.com/co-analysis/mwmi.govuk.data/blob/main/data/output/cleaned_data_trial.RDS?raw=TRUE","rb"))
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
  filter(tm>=202403) # Filter to only 2024 Q1 onwards

#---------------------------------------------------------------------------------#
## MWMI - ENDPBs to keep
keep_endpb = c(
  "advisory conciliation and arbitration service", # DBT
  "office for budget responsibility" , "office of budget responsibility", # HMT(2)
  "institute for apprenticeships and technical education", # DFE
  "health and safety executive" # DWP
)
#---------------------------------------------------------------------------------#

mwmi_with_qpses_naming <- mwmi_df %>%
  select(-Quarter,-t) %>%
  #
  mutate(dept_norm=dept_normalise(mwmi_dept)) %>%
  mutate(body_norm=body_normalise(mwmi_body)) %>%
  #
  #mutate(dept_norm=ifelse(tm<=202406 & grepl("Communities",dept_norm), "Levelling Up Housing and Communities", dept_norm)) %>%
  #mutate(body_norm=ifelse(tm<=202406 & grepl("communities",body_norm), "levelling up housing and communities", body_norm)) %>%
  #
  mutate(dept_norm=ifelse(grepl("Wales Off",dept_norm), "Office of the Secretary of State for Wales", dept_norm)) %>%
  mutate(body_norm=ifelse(grepl("wales off",body_norm), "office of the secretary of state for wales", body_norm)) %>%
  #
  mutate(dept_norm=ifelse(grepl("export",body_norm),"Export Credits Guarantee Department",dept_norm)) %>% 
  mutate(body_norm=ifelse(grepl("export",body_norm),"export credits guarantee department",body_norm)) %>% 
  #
  mutate(dept_norm=ifelse(grepl("land reg",body_norm),"HM Land Registry",dept_norm)) %>% 
  mutate(body_norm=ifelse(grepl("land reg",body_norm),"hm land registry",body_norm)) %>% 
  #
  mutate(body_norm=ifelse(body_norm=="crown prosecution service inspectorate", paste0("hm ",body_norm),body_norm)) %>%
  mutate(body_norm=ifelse(body_norm=="teaching regulation authority","teaching regulation agency",body_norm)) %>% # authority - agency 
  mutate(body_norm=ifelse(body_norm=="valuation office","valuation office agency",body_norm)) %>%
  mutate(body_norm=ifelse(body_norm=="office of budget responsibility",gsub("office of","office for",body_norm),body_norm)) %>% # of - for
  mutate(body_norm=ifelse(body_norm=="office for national statistics", "uk statistics authority", body_norm)) %>%
  #
  mutate(body_norm=ifelse(grepl("queen elizabeth",body_norm),"queen elizabeth ii centre",body_norm)) %>%
  mutate(body_norm=ifelse(grepl("intellectual property",body_norm),"intellectual property office",body_norm)) %>%
  #
  mutate(qpses_scope = ifelse((org_type %in% c("Ministerial Department","Non-Ministerial Department",
                                               "Executive Agency","Crown Non Departmental Public Body") | body_norm %in% keep_endpb),"y","n")) %>%
  filter(!body_norm=="ukri") %>% # CHECK WHY
  arrange(tm,dept_norm,body_norm)


## Save .csv file to data folder
test <- mwmi_with_qpses_naming %>%
  group_by(mwmi_dept,mwmi_body,dept_norm,body_norm) %>%
  tally() %>%
  select(-n)
  
test %>% write.csv(file="output_data/mwmi_to_qpses_name_matcher.csv", row.names=F)
