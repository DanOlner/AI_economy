#Ad hoc functions in a different file so we can debug them
library(tidyverse)

addAIIE <- function(df,colname){
  
  colname = enquo(colname)
  
  #Well this is horrible...
  #Hack for c() not being a quosure friendly function in joins
  #https://stackoverflow.com/questions/48449799/join-datasets-using-a-quosure-as-the-by-argument
  # by = set_names(quo_name(colname), 'SICno')
  by = set_names('SICno',quo_name(colname))
  
  #5 digit
  df <- df %>% 
    left_join(
      usuk1col.avs %>% select(SICno,AIIE_5dig = AIIE),
      by = by
      # by = c(colname_string = 'SICno')
      # by = join_by(!colname == SICno)
      # by = c(quo_name(colname) = 'SICno')
    )
  
  #4 digit
  df <- df %>% 
    mutate(Sictext1_sub = str_sub(!!colname,1,4)) %>% 
    left_join(
      usuk1col.avs %>% select(SICno,AIIE_4dig = AIIE),
      by = c('Sictext1_sub' = 'SICno')
    )
  
  #3 digit
  df <- df %>% 
    mutate(Sictext1_sub = str_sub(!!colname,1,3)) %>% 
    left_join(
      usuk1col.avs %>% select(SICno,AIIE_3dig = AIIE),
      by = c('Sictext1_sub' = 'SICno')
    )
  
  #2 digit
  df <- df %>% 
    mutate(Sictext1_sub = str_sub(!!colname,1,2)) %>% 
    left_join(
      usuk1col.avs %>% select(SICno,AIIE_2dig = AIIE),
      by = c('Sictext1_sub' = 'SICno')
    )
  
  df <- df %>% select(-Sictext1_sub)
  
  #case_when will work in order, taking each item first
  #Shouldn't have any remaining NAs but let's see
  df %>% 
    mutate(AIIE = case_when(
      !is.na(AIIE_5dig) ~ AIIE_5dig,
      !is.na(AIIE_4dig) ~ AIIE_4dig,
      !is.na(AIIE_3dig) ~ AIIE_3dig,
      !is.na(AIIE_2dig) ~ AIIE_2dig,
      .default = NA
    )) %>%
    select(-c(AIIE_5dig:AIIE_2dig))
  
}