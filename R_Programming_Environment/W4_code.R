##### Load libraries
libraries_to_load <- (c("dplyr", "tidyr", "readr", "readxl"))
lapply(libraries_to_load, require, character.only = TRUE)


##### Load data
aqs_sites <- read_xlsx("data/aqs_sites.xlsx")
names(aqs_sites) <- gsub(" ", "_", names(aqs_sites))

daily_SPEC <- read_csv("data/daily_SPEC_2014.csv")
names(daily_SPEC) <- gsub(" ", "_", names(daily_SPEC))


##### Questions
q1 <- daily_SPEC %>% 
  filter(`State_Name` == "Wisconsin" & `Parameter_Name` == "Bromine PM2.5 LC") %>% 
  select(`Arithmetic_Mean`) %>% 
  lapply(mean)



q2 <- daily_SPEC %>% 
  select(`Parameter_Name`,`Arithmetic_Mean`) %>% 
  group_by(Parameter_Name) %>% 
  summarize(Average = mean(Arithmetic_Mean, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Average)) %>%
  select(Parameter_Name) %>%
  filter(row_number()==4L)



q3 <- daily_SPEC %>% 
  filter(Parameter_Name == "Sulfate PM2.5 LC") %>%
  group_by(State_Code, County_Code, Site_Num) %>%
  summarize(Average = mean(Arithmetic_Mean, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Average)) %>%
  select(State_Code, County_Code, Site_Num) %>%
  filter(row_number()==1L)



q4 <- daily_SPEC %>% 
  filter(Parameter_Name == "EC PM2.5 LC TOR" & 
           (State_Name == 'California' | State_Name == 'Arizona')) %>%
  group_by(State_Name) %>% summarize(Average = mean(Arithmetic_Mean, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(Average) %>% 
  mutate_all(funs(. - lag(.))) %>%   
  na.omit()



q5 <- daily_SPEC %>% 
  filter(Parameter_Name == "OC PM2.5 LC TOR" & Longitude < -100) %>% 
  summarize(median(Arithmetic_Mean)) 



q6 <- aqs_sites %>%
  filter(Land_Use == 'RESIDENTIAL' & Location_Setting == 'SUBURBAN') %>%
  nrow()



q7 <- inner_join(
  
  daily_SPEC %>% 
    filter(Parameter_Name == "EC PM2.5 LC TOR", Longitude >= -100) %>% 
    select(State_Code, County_Code, Site_Num, Arithmetic_Mean) %>% 
    mutate(State_Code = as.double(State_Code), 
           County_Code = as.double(County_Code), 
           Site_Num = as.double(Site_Num)), 
  
  aqs_sites %>%
    filter(Land_Use == 'RESIDENTIAL' & Location_Setting == 'SUBURBAN') %>%
    select(State_Code, County_Code, Site_Number), 
  
  by = c("State_Code", "County_Code", "Site_Num" = "Site_Number")) %>%
  
  summarize(median(Arithmetic_Mean))



q8 <- inner_join(
  
  daily_SPEC %>% 
    filter(Parameter_Name == "Sulfate PM2.5 LC") %>% 
    select(State_Code, County_Code, Site_Num, Arithmetic_Mean, Date_Local) %>% 
    mutate(State_Code = as.double(State_Code), 
           County_Code = as.double(County_Code), 
           Site_Num = as.double(Site_Num)), 
  
  aqs_sites %>%
    filter(Land_Use == 'COMMERCIAL') %>%
    select(State_Code, County_Code, Site_Number), 
  
  by = c("State_Code", "County_Code", "Site_Num" = "Site_Number")) %>%
  
  mutate(Date_Local = format(Date_Local, '%B')) %>%
  group_by(Date_Local) %>% 
  summarize(Average = mean(Arithmetic_Mean)) %>%
  ungroup() %>%
  arrange(desc(Average)) %>%
  filter(row_number()==1L) %>%
  select(Date_Local)



q9 <- daily_SPEC %>% 
  filter(as.double(State_Code) == 6 &
           as.double(County_Code) == 65 &
           as.double(Site_Num) == 8001 & 
           (Parameter_Name == 'Sulfate PM2.5 LC' |
              Parameter_Name == 'Total Nitrate PM2.5 LC')) %>%
  group_by(Date_Local, Parameter_Name) %>% 
  summarize(average = mean(Arithmetic_Mean)) %>% 
  group_by(Date_Local) %>% 
  summarize(sum = sum(average)) %>% 
  ungroup() %>% filter(sum >10) %>% 
  nrow()



q10 <- daily_SPEC %>% 
  filter(Parameter_Name == 'Sulfate PM2.5 LC' | Parameter_Name == 'Total Nitrate PM2.5 LC') %>%
  select(State_Code, County_Code, Site_Num, Date_Local, Parameter_Name, Arithmetic_Mean) %>%
  group_by(State_Code, County_Code, Site_Num, Date_Local, Parameter_Name) %>%
  summarize(ave = mean(Arithmetic_Mean))%>%
  mutate(ave_Nit = (Parameter_Name == "Total Nitrate PM2.5 LC")*ave,
         ave_Sul = (Parameter_Name == "Sulfate PM2.5 LC")*ave) %>%
  group_by(State_Code, County_Code, Site_Num, Date_Local) %>%
  summarize(ave_Nit = sum(ave_Nit), ave_Sul = sum(ave_Sul)) %>%
  group_by(State_Code, County_Code, Site_Num) %>%
  summarize(cor = cor(ave_Nit, ave_Sul))%>%
  ungroup() %>%
  arrange(desc(cor)) %>%
  filter(row_number()==1L)
