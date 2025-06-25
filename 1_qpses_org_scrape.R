## Load Libraries
source("load_libs.R")

#--------------------------------------------------------------------------#

qpses_files = list.files("QPSES_data_files", full.names=T)
qpses_files_no_ext = tools::file_path_sans_ext(qpses_files) %>% str_remove("QPSES_data_files/")

qpses_read <- purrr::map_df(
  .x = set_names(qpses_files,qpses_files_no_ext),
  .f = function(s) read_excel(s, sheet="Table 9", col_names = F, skip=2),
  .id = 'file'
)
#
colnames(qpses_read) <- gsub("[...]+","c",colnames(qpses_read))
#--------------------------------------------------------------------------#


## Set column names and tidy
qpses_df <- qpses_read %>%
  select(-c2) %>%
  rename("qpses_body"=c1) %>%
  rename("headcount_cq"=c3, "fte_cq"=c4) %>%
  rename("headcount_pq"=c5, "fte_pq"=c6) %>%
  rename("headcount_change"=c7, "fte_change"=c8) %>%
  #
  filter(!is.na(qpses_body)) %>%
  #
  mutate(qpses_dept=ifelse(is.na(fte_cq),qpses_body,NA)) %>%
  group_by(file) %>%
  fill(qpses_dept, .direction="down") %>%
  ungroup() %>%
  #
  filter(!( is.na(fte_cq) & is.na(fte_pq))) %>%
  relocate(qpses_dept, .before=qpses_body) %>%
  #
  #mutate(file=str_replace(file, " ","_")) %>%
  mutate(t = str_squish(str_remove(file,"QPSES"))) %>%
  mutate(year = str_sub(t,end=4)) %>%
  mutate(quarter = str_sub(t,start=5)) %>%
  mutate(across(t:quarter, ~as.numeric(.))) %>%
  relocate(t:quarter, .after=file) %>%
  #
  mutate(qpses_dept=ifelse(qpses_body=="Total employment",qpses_body,qpses_dept)) %>%
  filter(t>=20241)

#---------------------------------------------------------------#

## Pivot QPSES data to long version
qpses_df2 <- qpses_df %>%
  pivot_longer(headcount_cq:fte_change) %>%
  mutate(measure=ifelse(grepl("headcount",name),"Headcount","FTE")) %>%
  mutate(value_type=case_when(
    grepl("_cq",name) ~ "Initial",
    grepl("_pq",name) ~ "Revised",
    grepl("_change",name) ~ "Change"
  )) %>%
  select(-name) %>%
  relocate(value,.after=value_type) %>%
  mutate(value=as.numeric(value))

#---------------------------------------------------------------#

qpses_initial <- qpses_df2 %>% filter(value_type=="Initial")
#
qpses_revised <- qpses_df2 %>% filter(value_type=="Revised") %>%
  mutate(t=ifelse(quarter==1,t-7,t-1)) %>%
  mutate(year = str_sub(t,end=4)) %>%
  mutate(quarter = str_sub(t,start=5)) %>%
  mutate(across(t:quarter, ~ as.numeric(.)))
#
qpses_change <- qpses_df2 %>% filter(value_type=="Change") 

#---------------------------------------------------------------#

# Normalise body names
body_normalise <- function(x) {
  str_to_lower(x) %>%
    gsub("&","and",.) %>%
    gsub("\\s*\\([^\\)]+\\)", "", .) %>%
    #
    gsub("[^a-z ]","",.) %>%
    gsub("department for ","",.) %>%
    gsub("department of ","",.) %>%
    gsub("ministry of ","",.) %>%
    gsub("former ","",.) %>%
    gsub("^the ","",.) %>%
    gsub("^united kingdom","uk",.) %>%
    #
    str_squish(.)
}

# Normalise department names
dept_normalise <- function(x) {
  str_to_lower(x) %>%
    gsub("&","and",.) %>%
    gsub("\\s*\\([^\\)]+\\)", "", .) %>%
    #
    gsub("[^a-z ]","",.) %>%
    gsub("department for ","",.) %>%
    gsub("department of ","",.) %>%
    gsub("ministry of ","",.) %>%
    gsub("former ","",.) %>%
    #
    str_to_title(.) %>%
    gsub("And ","and ",.) %>%
    gsub("For ","for ",.) %>%
    gsub("Of ","of ",.) %>%
    gsub("In ","in ",.) %>%
    gsub("Hm ","HM ",.) %>%
    gsub("Uk ","UK ",.) %>%
    gsub("^United Kingdom","UK",.) %>%
    gsub("Generals Departments","General's departments",.) %>%
    gsub("Chancellors Other Departments","Chancellor's other departments",.) %>%
    gsub("The Sec","the Sec",.) %>%
    str_squish(.)
}
#---------------------------------------------------------------#

qpses_df3 <- bind_rows(qpses_initial,qpses_revised,qpses_change) %>%
  mutate(dept_norm=dept_normalise(qpses_dept)) %>%
  mutate(body_norm=body_normalise(qpses_body)) %>%
  #
  mutate(dept_norm=ifelse(grepl("Cabinet",dept_norm), "Cabinet Office", dept_norm))


qpses_co_merge <- qpses_df3 %>%
  filter(value_type!="Change") %>%
  filter(grepl("Cabinet",dept_norm)) %>%
  filter(body_norm %in% c("cabinet office","central civil service fast stream","government commercial organisation")) %>%
  #mutate(body_norm=ifelse(body_norm %in% c("cabinet office","central civil service fast stream","government commercial organisation"), "cabinet office",body_norm)) %>%
  group_by(file,t,year,quarter, dept_norm, measure,value_type) %>%
  summarise(value=sum(value,na.rm=T)) %>%
  mutate(body_norm="cabinet office") %>%
  mutate(qpses_dept="Cabinet Office", qpses_body="CO_manual_merge")
#
#qpses_co_merge %>% filter(t==20241) %>% filter(measure=="FTE")

qpses_df3_with_COmerge <- qpses_df3 %>%
  filter(!body_norm %in% c("cabinet office","central civil service fast stream","government commercial organisation")) %>%
  bind_rows(qpses_co_merge)


## Create dataframe to use when matching with QPSES data
join_qpses <- qpses_df3_with_COmerge %>%
  filter(value_type=="Revised" | (t==max(qpses_df3_with_COmerge$t) & value_type=="Initial")) %>%
  rename("qpses_value"=value) %>%
  mutate(t=as.character(t)) %>%
  mutate(tm=case_when(
    endsWith(t,"1") ~ paste0(substr(t,start=1,stop=4),"03"),
    endsWith(t,"2") ~ paste0(substr(t,start=1,stop=4),"06"),
    endsWith(t,"3") ~ paste0(substr(t,start=1,stop=4),"09"),
    endsWith(t,"4") ~ paste0(substr(t,start=1,stop=4),"12"),
  )) %>%
  select(t,tm,qpses_dept:body_norm) %>%
  mutate(across(t:tm, ~ as.numeric(.))) %>%
  mutate(qpses_scope="y") %>%
  filter(tm>=202403)


## Save RDS file to data folder
saveRDS(join_qpses, file="output_data/match_data_qpses.RDS")
