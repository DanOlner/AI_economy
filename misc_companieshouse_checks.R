#various companies house checks and outputs
library(tidyverse)
#library(plotly)
#library(ggrepel)
library(sf)
#library(tmap)
source('functions/helpers.R')
source('functions/ad_hoc_functions.R')

ch <- readRDS('local/data/ch_with_4AIE_measures.rds')



# check top 100 firms by employee size in SY----

#And add in % change over the last year
sy <- ch %>% filter(
  ITL221NM == 'South Yorkshire',
  !is.na(Employees_thisyear),
  Employees_thisyear > 0
  ) %>% 
  arrange(-Employees_thisyear)

sy100 <- sy %>% slice(1:100) %>% 
  mutate(
    percent_changefromprevyear = percent_change(Employees_lastyear,Employees_thisyear) %>% round(2)
  ) %>% 
  select(CompanyName,localauthority_name,postcode,accountcode,Employees_thisyear,Employees_lastyear,percent_changefromprevyear,
         SIC_5DIGIT_CODE:age_of_firm_years)

#Save that list
write_csv(sy100 %>% st_set_geometry(NULL),'data/companieshouse_sy100_July2025.csv')



  