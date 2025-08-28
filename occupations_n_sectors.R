#Exploring occupation options
library(tidyverse)
library(nomisr)
source('functions/helpers.R')

# CENSUS 2021 OCCUPATIONS----

#Via bulk download
#Workplace counts at MSOA first
#105 categories of SOC2020, so that's 
#3 digit "minor group"
#https://www.hesa.ac.uk/collection/coding-manual-tools/sicsocdata/soc-2020
occ.msoa <- read_csv("local/data/wp016/WP016_msoa.csv")

#Do we have even more cats at LA level? Nope, still 105
occ.la <- read_csv("local/data/wp016/WP016_utla.csv")




# SIC codes----

#Felten et al have 4 digit NAICS codes for industry
#I think those should line up with SIC 2007 4 digit
siclookup <- read_csv('https://github.com/DanOlner/RegionalEconomicTools/raw/refs/heads/gh-pages/data/SIClookup.csv')

#No 4 digit there
#Easiest way to get that is via BRES, weirdly...


#Any random BRES will do....
z <- nomis_get_data(id = "NM_189_1", time = 2023, 
                    geography = '1929380119', 
                    MEASURE = 1,#1 is "Count", 2 is "Industry percent"
                    MEASURES = 20100,#20100 is "value", 20301 is "percent" (which is redundant as "value" of "industry percent" is percent)
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

unique(z$INDUSTRY_TYPE)
unique(z$INDUSTRY_NAME[z$INDUSTRY_TYPE == 'SIC 2007 class (4 digit)'])
unique(z$INDUSTRY_NAME[z$INDUSTRY_TYPE == 'SIC 2007 class (4 digit)'])[1:20]

#DF for 4 digit
fourdig <- z %>% 
  filter(INDUSTRY_TYPE == 'SIC 2007 class (4 digit)') %>% 
  select(INDUSTRY_NAME) %>% 
  distinct() %>% 
  mutate(
    SIC_4DIGIT_CODE = str_sub(INDUSTRY_NAME,1,4),
    SIC_4DIGIT_NAME = str_sub(INDUSTRY_NAME,8,-1),
    SIC_4DIGIT_FULLNAME = INDUSTRY_NAME
    )
  



# MATCHING SIC CODES TO U.S. NAISC 2017----

#So we can use Felten et al's industry classification
#Getting there is a bit of a faff. Let's see what works.
#Note: Felten et al say latest NAICS they use is 2019 but they must mean 2017
#Paper out in 2021, 2022 NAICS revision is in future

#Handy ONS page for sources...
#https://www.ons.gov.uk/aboutus/transparencyandgovernance/freedomofinformationfoi/mapinguksic2007codestonaic

#That's got ISIC rev 4.0 to NAICS 2022
#(ISIC = UN classification)

#Ah, here's ISIC / NAICS 2017
#https://www.census.gov/naics/concordances/ISIC_4_to_2017_NAICS.xlsx

#Can then get from ISIC to NACE (Eurostat), though not at brilliant resolution I don't think
#ONS: "ISIC (International Standard Industrial Classification of All Economic Activities) is the UN (United Nations) global classification, to which the Eurostat NACE (statistical classification of economic activities in the European Community**)** classification aligns exactly at the 2 digit level and at the 3 digit level in some instances."

#UK SIC 2007 matches NACE down to 4 digit, but that may not help us

#Will have to look at each stage and see, huh?

#Let's run through steps we're attempting
#1. NAICS 2017 to ISIC 4.0
#2. ISIC to NACE
#3. NACE to SIC 2007

#Noting that in Felten et al, we have 250 industry categories


#ISIC/NAICS
# url1 <- 'https://www.census.gov/naics/concordances/ISIC_4_to_2017_NAICS.xlsx'
# p1f <- tempfile(fileext=".xlsx")
# download.file(url1, p1f, mode="wb") 

#That's not working so got local copy
isic_naics <- readxl::read_excel(path = "data/lookups/ISIC_4_to_2017_NAICS.xlsx",range = "ISIC 4 to NAICS 17 technical!A1:G1656") 


#ISIC 4 to NACE
#Again, rubbish lack of visible download link so local copy
#https://circabc.europa.eu/ui/group/c1b49c83-24a7-4ff2-951c-621ac0a89fd8/library/f454b1bb-77dc-4fe2-8c1c-679d8086744c?p=1&n=10&sort=modified_DESC
nace_isic <- read_csv("data/lookups/NACE REV2toISICREV4.csv")

#Has letter codes in - remove any rows containing those
nace_isic <- nace_isic %>% 
  filter(
    !grepl('[a-zA-Z]',Source)
  )


#All looks sensible. Now let's just run through the logic again, with added bits
#1. I want to add in the four digit NAICS cats from the Felten et all index
#Those should merge just on the 1st four digits of six from isic_naics above

#We want to then check if too much resolution is lost going from NAICS to ISIC 4
#Hopefully not...

#That would then let us get from ISIC to NACE
#And then 4 digit match to UK codes is built in

#Let's see how the AIEO 4 digit codes look...
aieo_industry <- readxl::read_excel(path = "local/data/AIOE/AIOE_DataAppendix.xlsx", range = "Appendix B!A1:C251")


#Merge into the six digit lookup
#And see what we can see

#Add in four digit NAICS code
isic_naics <- isic_naics %>% 
  mutate(
    NAICS_4digit = str_sub(`2017\r\nNAICS\r\nUS`,1,4)
  )

isic_naics.4digfromAIEO <- isic_naics %>% 
  right_join(
    aieo_industry %>% select(NAICS,IndustryTitleFromAIEO = `Industry Title`) %>% mutate(NAICS = as.character(NAICS)), 
    by = c('NAICS_4digit' = 'NAICS')
  )

reduced <- isic_naics.4digfromAIEO %>% 
  select(`ISIC 4.0`,`ISIC Revision 4.0 Title`,`2017\r\nNAICS\r\nUS`, NAICS_4digit,IndustryTitleFromAIEO)

#Add in ISIC / NACE lookup

#Let's keep only 3 digits for ISIC, see if that works
#Otherwise we've got some nesting going on there...
nace_isic_3dig <- nace_isic %>% filter(
  nchar(Target)  >= 3
)


reduced <- reduced %>% 
  left_join(
    nace_isic_3dig %>% rename(ISIC = Target, NACE = Source),
    by = c("ISIC 4.0" = "ISIC")
  )


#So in theory...
#"NACE to UK SIC (NACE is identical to UK SIC at the 4 digit class level)"
#https://www.ons.gov.uk/aboutus/transparencyandgovernance/freedomofinformationfoi/mapinguksic2007codestonaic

#Confusing patterning in NACE for the match
#2 digit sit in the middle of a four digit match
#E.g. 22 == logging == 0220 in SIC 2007
#3 digit is at the end 
#E.g. 162 == support for agri == 0162 in SIC 2007
#Though that one isn't a perfect match anyway - 016 from 3 digit would be better

#Let's see what we get. This is going to need manual fettling.
reduced <- reduced %>% 
  mutate(NACE_justdigits = gsub('\\.','',NACE))


#Keep most common row from the Industry title....
justUS_EU <- reduced %>%
  select(NAICS_4digit,IndustryTitleFromAIEO,NACE_justdigits) %>%
  add_count(NAICS_4digit, NACE_justdigits, name = "freq") %>%
  group_by(NAICS_4digit) %>%
  filter(freq == max(freq, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-freq) %>% 
  distinct()

  
#Try matches sequentially from SIC2007
justUS_EU_joins <- justUS_EU %>% 
  mutate(NACE_3digit = str_sub(NACE_justdigits,1,3)) %>% 
  left_join(
    fourdig %>% select(SIC_4DIGIT_CODE,SIC_4DIGIT_FULLNAME),
    by  = c('NACE_justdigits' = 'SIC_4DIGIT_CODE')
    ) %>% 
  left_join(
    siclookup %>% select(SIC_3DIGIT_CODE,SIC_3DIGIT_NAME) %>% distinct(),
    by = c('NACE_3digit' = 'SIC_3DIGIT_CODE')
  )
  

#That did pretty well actually! 
#Some gaps, but let's just check a few things

#For ONLY ROWS WHERE WE GOT 3 DIGIT MATCH
#How many are 1 to 1 matches to NAICR here
#How many not?

#Could be in either direction...
chk_sic07dups <- justUS_EU_joins %>% 
  select(NAICS_4digit,IndustryTitleFromAIEO,SIC_3DIGIT_NAME,SIC_4DIGIT_FULLNAME) %>% 
  filter(!is.na(SIC_3DIGIT_NAME)) %>% 
  # distinct(NAICS_4digit, .keep_all = T) %>% 
  group_by(NAICS_4digit) %>% 
  mutate(distinctcount_SIC07_3dig = n_distinct(SIC_3DIGIT_NAME))
  # summarise(distinctcount_SIC07_3dig = n_distinct(SIC_3DIGIT_NAME))


#OK, best I think we can do here:
#Keep unique NAICR, see what matches, manually fix non-matches
chk <- justUS_EU_joins %>% 
  select(NAICS_4digit,IndustryTitleFromAIEO,SIC_3DIGIT_NAME,SIC_4DIGIT_FULLNAME) %>% 
  # distinct(NAICS_4digit, .keep_all = T) %>% #Some are dups, see below...
  distinct(IndustryTitleFromAIEO, .keep_all = T) %>% #This one keeps all that are in the AIOE list
  arrange(NAICS_4digit)
  
#If doing manually... a few rows missing
#Add those back in
# aieo_industry %>% 
#   filter(NAICS %in% chk$NAICS_4digit)

#Ah no, the orig has some duplicates (or codes that include different subcodes)
#E.g.
#3320	Fabricated Metal Product Manufacturing (3323 and 3324 only)
#3320	Fabricated Metal Product Manufacturing (3321, 3322, 3325, 3326, and 3329 only)

#Gonna have to combine those

#Which ones are duplicates?
aieo_industry %>% 
  filter(NAICS %in% 
    (aieo_industry %>% 
      group_by(NAICS) %>% 
      summarise(n = n()) %>% 
      filter(n > 1) %>% 
      select(NAICS) %>% 
      pull())
)


#can work with that / match rest
#But gonna have to fettle manually...
write_csv(chk,'local/data/AIOE_list_forSIC2007link.csv')


#Could do with four digit being in there with the others side by side really...
#Match to 4 digits of the 5 digit code
siclookup2 <- siclookup %>% 
  mutate(SIC_4DIGIT_CODE = str_sub(SIC_5DIGIT_CODE,1,4)) %>% 
  left_join(
    fourdig %>% select(SIC_4DIGIT_CODE,SIC_4DIGIT_NAME = INDUSTRY_NAME),
      # mutate(SIC_3DIGIT_CODE = str_sub(SIC_4DIGIT_NAME,1,4)),
    by = 'SIC_4DIGIT_CODE'
  ) %>% 
  relocate(SIC_4DIGIT_CODE, .before = SIC_5DIGIT_CODE) %>% 
  relocate(SIC_4DIGIT_NAME, .before = SIC_5DIGIT_CODE)


#remind me how many cats in each?
#4 digit not that far off 5...
siclookup2 %>% 
  select(contains("DIGIT_NAME")) %>% 
  select(!contains("GVA")) %>% 
  summarise(across(everything(),n_distinct))

#For this exercise, just need to look at 3 and 4 digit
siclookup2 %>% 
  select(SIC_3DIGIT_NAME,SIC_4DIGIT_NAME) %>% 
  distinct() %>% 
  View



