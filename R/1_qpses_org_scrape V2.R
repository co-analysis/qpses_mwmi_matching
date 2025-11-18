## Load Libraries
source("load_libs.R")

#--------------------------------------------------------------------------#

## Read in QPSES files
qpses_files = list.files("data/QPSES_files", full.names=T)
qpses_files_no_ext = tools::file_path_sans_ext(qpses_files) %>% str_remove("data/QPSES_files/")

qpses_read <- purrr::map_df(
  .x = set_names(qpses_files,qpses_files_no_ext),
  .f = function(s) read_excel(s, sheet="Table 9", col_names = F, skip=2),
  .id = 'file'
)
#
colnames(qpses_read) <- gsub("[...]+","c",colnames(qpses_read)) # remove fluff from column names
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

#----------------------------------------#
# Normalise names
name_normalise <- function(x) {
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
#----------------------------------------#

## Pivot QPSES data long
qpses_df_long <- qpses_df %>% 
  pivot_longer(headcount_cq:fte_change) %>%
  mutate(measure=ifelse(grepl("headcount",name),"Headcount","FTE")) %>%
  mutate(value_type=case_when(
    grepl("_cq",name) ~ "initial",
    grepl("_pq",name) ~ "revised",
    grepl("_change",name) ~ "change"
  )) %>% 
  select(-name) %>%
  relocate(value,.after=value_type) %>%
  mutate(value=as.numeric(value)) %>%
  #
  mutate(dept_norm=name_normalise(qpses_dept)) %>%
  mutate(body_norm=name_normalise(qpses_body)) %>%
  #
  mutate(dept_norm=ifelse(grepl("cabinet",dept_norm), "cabinet office", dept_norm)) %>% 
  mutate(qpses_body=gsub('[[:digit:]]+', '', qpses_body)) %>%
  mutate(qpses_body=str_squish(qpses_body))

## Save raw QPSES names for Department and Body
qpses_names_raw <- qpses_df_long %>% select(t:measure,ends_with("norm")) %>% distinct() 

#------------------------------------------------------------------#
qpses_initial <- qpses_df_long %>% filter(value_type=="initial")
#
qpses_revised <- qpses_df_long %>% filter(value_type=="revised") %>%
  # set time columns to reflect "previous" quarter
  mutate(t=ifelse(quarter==1,t-7,t-1)) %>%
  mutate(year = str_sub(t,end=4)) %>%
  mutate(quarter = str_sub(t,start=5)) %>%
  mutate(across(t:quarter, ~ as.numeric(.)))
#------------------------------------------------------------------#

qpses_df2 <- bind_rows(qpses_initial,qpses_revised) %>%
  filter(t>=20241) %>%
  #
  select(t:quarter,ends_with("_norm"),measure:value) %>%
  pivot_wider(names_from=value_type, values_from=value) %>%
  group_by(dept_norm,body_norm,measure) %>%
  mutate(value=ifelse(t==max(t),initial,revised)) %>%
  ungroup() %>%
  #
  filter(value!=0) %>%
  left_join(qpses_names_raw) %>% 
  group_by(dept_norm,body_norm) %>% 
  fill(qpses_dept, .direction = "updown") %>% 
  fill(qpses_body, .direction = "updown") %>% 
  ungroup() %>% 
  rename_with(~paste0("qpses_",.x),initial:value)

#------------------------------------------------------------------#

## Pull out and manually calculate CO figure
qpses_co_merge <- qpses_df2 %>%
  filter(grepl("cabinet",dept_norm)) %>%
  filter(body_norm %in% c("cabinet office", 
                          "central civil service fast stream", 
                          "government commercial organisation")) %>%
  group_by(t,year,quarter, dept_norm, measure) %>%
  summarise(qpses_value=sum(qpses_value,na.rm=T)) %>%
  mutate(body_norm="cabinet office") %>%
  mutate(qpses_dept="Cabinet Office", qpses_body="CO_manual_merge")

#qpses_df2 %>% filter(t==20241) %>% filter(measure=="FTE") %>% filter(body_norm %in% c("cabinet office","central civil service fast stream","government commercial organisation"))
#qpses_co_merge %>% filter(t==20241) %>% filter(measure=="FTE")

qpses_df2_with_CO_merge <- qpses_df2 %>%
  filter(!body_norm %in% c("cabinet office","central civil service fast stream","government commercial organisation")) %>%
  bind_rows(qpses_co_merge)
#
#qpses_df2_with_CO_merge %>% filter(t==20241) %>% filter(measure=="FTE") %>% filter(grepl("cabinet",dept_norm))

#------------------------------------------------------------------#

join_qpses_V2 <- qpses_df2_with_CO_merge %>%
  mutate(t=as.character(t)) %>%
  mutate(tm=case_when(
    endsWith(t,"1") ~ paste0(substr(t,start=1,stop=4),"03"),
    endsWith(t,"2") ~ paste0(substr(t,start=1,stop=4),"06"),
    endsWith(t,"3") ~ paste0(substr(t,start=1,stop=4),"09"),
    endsWith(t,"4") ~ paste0(substr(t,start=1,stop=4),"12"),
  )) %>%
  select(t,tm,dept_norm:qpses_body) %>%
  mutate(across(t:tm, ~ as.numeric(.))) %>%
  mutate(qpses_scope="y") %>%
  filter(tm>=202403) %>% 
  #
  filter(!(grepl("level",dept_norm) & tm>=202406)) %>%
  select(-qpses_initial, -qpses_revised)

#------------------------------------------------------------------#
## Save RDS file to data folder
saveRDS(join_qpses_V2, file="data/output_data/clean_data_qpses.RDS")
#join_qpses_V2 <- read_rds("data/output_data/clean_data_qpses.RDS")
