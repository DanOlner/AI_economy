#Exploring occupation options
library(tidyverse)
library(nomisr)
library(sf)
library(tmap)
source('functions/helpers.R')
source('functions/ad_hoc_functions.R')

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


# MAKE FILE TO THEN EDIT MANUALLY IN SPREADSHEET----


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




# RELOAD MANUAL EDIT FOR CHECKS----

# I am choosing from 2 to 5 digit, picking a single one that’s most applicable. SO: how this will have to work re. Assigning of the AIOE numbers:
# Assign the highest digit levels first THEN try the next one down if no match etc.
# So e.g. “2841 : Manufacture of metal forming machinery” can get its own AIOE number; “281 : Manufacture of general purpose machinery” can go next; “28 : Manufacture of machinery and equipment n.e.c.” would be last and attach to any remaining with that code. Simplez!
# I’ve gone for avoiding many-to-one / US-to-UK and stuck just to one-to-many US-to-UK. So that means some AIOE values will need to be averaged for a particular SIC07 sector.
# Slight change here: I am now repeating US sector rows. Not a problem if one value matches to several UK SICs – it just gets put in each. E.g. doing this with education from primary to secondary.

usuk <- read_csv('data/AIOE_list_forSIC2007link.csv')

#Checks
#There should be ONE sector per row, either 2,3,4 or 5 digit UK SIC
# nacount <- usuk %>%
#   select(contains("SIC")) %>%
#   rowwise() %>%
#   mutate(NAcount = mean(!is.na(c_across(everything())))) %>%
#   ungroup()
# 
# table(nacount$NAcount)

#Oops missed a few...
#Fixed, now only one sector per row, just one between 2 and 5 digit


#Next, let's check how many / which UK sectors are missing from that list
#Easiest way is probably to apply to a copy of the lookup
#Marking only those we've used
#So e.g. if we've used a 2 digit, that will fill all 5 digit.
#Gaps will be visible there

siccheck <- siclookup2 %>% 
  select(contains("DIGIT_NAME")) %>% 
  select(!contains("GVA"))

#reminder, we just checked, there'll be only one of these present per row
siccheck <- siccheck %>% 
  mutate(sectorpresentinAIOE = case_when(
    SIC_2DIGIT_NAME %in% usuk$SIC_2DIGIT_NAME ~ "2digit",
    SIC_3DIGIT_NAME %in% usuk$SIC_3DIGIT_NAME ~ "3digit",
    SIC_4DIGIT_NAME %in% usuk$SIC_4DIGIT_NAME ~ "4digit",
    SIC_5DIGIT_NAME %in% usuk$SIC_5DIGIT_NAME ~ "5digit",
    .default = NA
  )
  )

#OK, on first check we've got... 81.4% match
#Mostly farming missing. Not too bad.
#But some fixing to do...
table(!is.na(siccheck$sectorpresentinAIOE)) %>% prop.table

#Looking at the orig, it doesn't actually contain agri/farming sectors, only processing 
#Except logging, for some reason, that's still there...
#So we'd have to repeat that...

#Which would look like this:
#Note, also removing the final four, which don't apply to any sectors we can use 
#(household work, extra terr etc)
siccheck.minusagri <- siccheck %>% 
  slice(c(35,42:(nrow(siccheck)-4))) %>% 
  mutate(sectorpresentinAIOE = case_when(
    SIC_2DIGIT_NAME %in% usuk$SIC_2DIGIT_NAME ~ "2digit",
    SIC_3DIGIT_NAME %in% usuk$SIC_3DIGIT_NAME ~ "3digit",
    SIC_4DIGIT_NAME %in% usuk$SIC_4DIGIT_NAME ~ "4digit",
    SIC_5DIGIT_NAME %in% usuk$SIC_5DIGIT_NAME ~ "5digit",
    .default = NA
  )
  )

#85.7%, still plenty to fill...
#And done! All UK codes matched against a part of the US codes in the AIOE there
table(!is.na(siccheck.minusagri$sectorpresentinAIOE)) %>% prop.table


#Some other basic checks on the AIIE (AI Industry Exposure) measure itself
#OK, so that's normalised isn't it? Yep, mean 0 SD 1. Coolio.
plot(density(aieo_industry$AIIE))
mean(aieo_industry$AIIE)
sd(aieo_industry$AIIE)

#Will it need renormalising for the UK? Probably.
#Also possible we could use rank instead.


# PROCESS US/UK LINK FILE----

#Add in actual AIIE score
#Match on name because NAICS is repeated...
usuk <- usuk %>% 
  left_join(
    aieo_industry %>% select(-NAICS),
    by = c('IndustryTitleFromAIEO' = 'Industry Title')
  )

#It *probably* makes sense to combine all the SIC levels into one column
#We can still cascade through from 5 to 2, hopefully in a tidier way
#And it'll make other operations much neater
#Having already very carefully checked there's only one entry per row!
usuk1col <- usuk %>% 
  rowwise() %>% 
  mutate(SIC = max(c_across(SIC_2DIGIT_NAME:SIC_5DIGIT_NAME),na.rm = T))

#Pull out SIC code (number of digits gives level)
usuk1col <- usuk1col %>% 
  mutate(
    SICno = str_split(SIC,' : ')[[1]][1],
    SIClevel = nchar(SICno)
  ) %>% 
  select(-c(SIC_2DIGIT_NAME:SIC_5DIGIT_NAME))

#One stage we can do first before use:
#Find average AIIE where there are multiple SIC2007 codes
#Easy enough - just apply average to all after grouping
#Ignoring NAs in SIC2007s
#In theory...

#Can summarise so each SIC2007 multiple becomes just one
#We no longer strictly need the NAICS bits but let's keep...
#Easier to see which NAICS are repeated (though don't need cos AIIE values are unique)
#But note when single NAICS match to multiple SICS
#The resulting "max" will be a bit random
usuk1col.avs <- usuk1col %>% 
  group_by(SIC) %>% 
  summarise(
    AIIE = mean(AIIE),
    NAICS_4digit = max(NAICS_4digit),
    IndustryTitleFromAIEO = max(IndustryTitleFromAIEO),
    Repeat = max(Repeat),
    SICno = max(SICno),
    SIClevel = max(SIClevel)
  ) %>% 
  ungroup()


#What's dist look like now? I have questions about the dist...
#Has hardly changed...
plot(density(usuk1col.avs$AIIE))
mean(usuk1col.avs$AIIE)
sd(usuk1col.avs$AIIE)


# TEST LINK TO CH DATA----

#Get CH data, test on subset
#Keep geodata to attempt plots later (probaby)
ch <- readRDS("../companieshouseopen/local/PROCESSED_accountextracts_n_livelist_geocoded_combined_July2025.rds") 

ch_gm <- ch %>% filter(qg('manchester',ITL221NM))

#Drop those SIC codes, they only apply to the first SIC entry
ch_gm <- ch_gm %>% select(-c(SIC_5DIGIT_CODE:SIC_SECTION_NAME))

#Remind me of SIC code proportions for each...
numsectorsdefined <- ch_gm %>%
  rowwise() %>%
    mutate(NAcount = mean(!is.na(c_across(SICCode.SicText_1:SICCode.SicText_4)))) %>%
    ungroup()

#I found a muuuuch faster way of doing this somewhere.
#Can't remember where, darnit.
table(numsectorsdefined$NAcount) %>% prop.table() * 100

#So thats:
#82.5% with just one code - so 17.4% with more than one code, which is a decent chunk
#10.64% with two
#3.92% with three
#2.86% with four

#Can come back to what else that correlates with e.g. in terms of firm size. which sectors tend to be assoc'd
#For now, just want code that attaches AIIE score to that


#Plan to test:
# For each CH firm, attach an AIIE score.
# Try each SIC level in turn from 5 to 2, use most granular match available.
# (That’s probably going to be easiest to apply e.g. to all using 5 digits, then all using 4 digits etc)
# Repeating for firms using more than 1 SIC code in their description, so there’s one AIIE score per SIC, up to a max of 4.
# Average that score per firm (if only 1, is just that one).

#Might be able to do this with joins...

#First, let's extract just the digits
ch_gm <- ch_gm %>% 
  mutate(across(SICCode.SicText_1:SICCode.SicText_4,~ str_sub(.,1,5),.names = "fivedig_{.col}"))

#Case_when will work in sequence
#Test first then function up and apply to each column
# ch_gm <- ch_gm %>% 
#   mutate(
#     AIIE = case_when(
#       `fivedigSICCode.SicText_1` 
#     )
#   )

#Two stages - run through 5 to 2 digit first
#Then repeat for each of the four SIC columns

#5 digit matches
ch_gm.x <- ch_gm %>% 
  left_join(
    usuk1col.avs %>% select(SICno,AIIE_5dig = AIIE),
    by = c('fivedig_SICCode.SicText_1' = 'SICno')
  )

table(!is.na(ch_gm.x$AIIE_5dig)) %>% prop.table() * 100

#4 digit
ch_gm.x <- ch_gm.x %>% 
  mutate(Sictext1_sub = str_sub(SICCode.SicText_1,1,4)) %>% 
  left_join(
    usuk1col.avs %>% select(SICno,AIIE_4dig = AIIE),
    by = c('Sictext1_sub' = 'SICno')
  )

table(!is.na(ch_gm.x$AIIE_4dig)) %>% prop.table() * 100

#3 digit
ch_gm.x <- ch_gm.x %>% 
  mutate(Sictext1_sub = str_sub(SICCode.SicText_1,1,3)) %>% 
  left_join(
    usuk1col.avs %>% select(SICno,AIIE_3dig = AIIE),
    by = c('Sictext1_sub' = 'SICno')
  )

table(!is.na(ch_gm.x$AIIE_3dig)) %>% prop.table() * 100

#2 digit
ch_gm.x <- ch_gm.x %>% 
  mutate(Sictext1_sub = str_sub(SICCode.SicText_1,1,2)) %>% 
  left_join(
    usuk1col.avs %>% select(SICno,AIIE_2dig = AIIE),
    by = c('Sictext1_sub' = 'SICno')
  )

table(!is.na(ch_gm.x$AIIE_2dig)) %>% prop.table() * 100

ch_gm.x <- ch_gm.x %>% select(-Sictext1_sub)

#So - that behaviour makes sense given the way we're doing this.
#We may get 2 digit match as well as e.g. 4 or 5
#We want to keep the most granular available match ONLY per firm

#case_when will work in order, taking each item first
#Shouldn't have any remaining NAs but let's see
ch_gm.x <- ch_gm.x %>% 
  mutate(AIIE = case_when(
    !is.na(AIIE_5dig) ~ AIIE_5dig,
    !is.na(AIIE_4dig) ~ AIIE_4dig,
    !is.na(AIIE_3dig) ~ AIIE_3dig,
    !is.na(AIIE_2dig) ~ AIIE_2dig,
    .default = NA
  ))

table(!is.na(ch_gm.x$AIIE)) %>% prop.table()

#Any remaining are sectors we can't use anyway I think... Yep
table(ch_gm.x$fivedig_SICCode.SicText_1[is.na(ch_gm.x$AIIE)])

# debugonce(addAIIE)
# x <- addAIIE(
#   ch_gm.x %>% select(-c(AIIE_5dig:AIIE_2dig)),
#   fivedig_SICCode.SicText_2
#   )


#Do from scratch starting at one
ch_gm_aiie <- addAIIE(ch_gm,fivedig_SICCode.SicText_1)
ch_gm_aiie <- addAIIE(ch_gm_aiie %>% rename(AIIE1 = AIIE),fivedig_SICCode.SicText_2)
ch_gm_aiie <- addAIIE(ch_gm_aiie %>% rename(AIIE2 = AIIE),fivedig_SICCode.SicText_3)
ch_gm_aiie <- addAIIE(ch_gm_aiie %>% rename(AIIE3 = AIIE),fivedig_SICCode.SicText_4)
ch_gm_aiie <- ch_gm_aiie %>% rename(AIIE4 = AIIE)


#And then we just take an average...
#Which will mostly just return the value of the first SIC match's AIIE
ch_gm_aiie<- ch_gm_aiie %>% 
  rowwise() %>% 
  mutate(AIIEfinal = mean(c_across( starts_with('AIIE') ) ,na.rm=T)) 

#Keep only those with values
ch_gm_aiie <- ch_gm_aiie %>% 
  filter(!is.nan(AIIEfinal))


#OK. Now we can do something like make a map. Let's just save that though
saveRDS(ch_gm_aiie,'local/data/ch_gm_aiie_draft1.rds')



# EXAMINE AIIE NUMBERS FOR GM----

#Options - can weight by jobs...

#Taken from here
#https://github.com/DanOlner/companieshouseopen/blob/e6dfcd2eef5a2fb142f5fffc05394645daf9b4d4/testcode/initial_datadigging.R#L123C1-L150C113 
sq = st_make_grid(ch_gm_aiie, cellsize = 1000, square = F)

#Turn into sf object so gridsquares can have IDs to group by
sq <- sq %>% st_sf() %>% mutate(id = 1:nrow(.))

# plot(sq)

#Intersection...
overlay <- st_intersection(ch_gm_aiie,sq)


#This no longer needs to be geo, which will speed up
#Can link back to grids once done

#Let's find an average AIIE weighted by employee number in each grid square
section.summary <- overlay %>% 
  st_set_geometry(NULL) %>% 
  filter(Employees_thisyear > 0) %>% #Only firms with employees recorded in latest year
  group_by(id) %>% 
  summarise(
    AIIE_weightedbyemployees = weighted.mean(AIIEfinal,Employees_thisyear),
    totalemployees = sum(Employees_thisyear)
    ) %>% 
  group_by(id) %>%
  filter(sum(totalemployees) >= 10)#keep only gridsquares where total employee count is more than / equal to 100
  ungroup()
  
#Link that back into the grid squares...
#Use right join to drop empties
sq.aiie <- sq %>% 
  right_join(
    section.summary,
    by = 'id'
  )
  
itl2 <- st_read('../RegionalEcons_web/data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp') %>% st_simplify(preserveTopology = T, dTolerance = 100) %>% 
  filter(qg('manchester',ITL221NM))

  
lad <- st_read("~/Dropbox/MapPolygons/UK/2024/Local_Authority_Districts_May_2024_Boundaries_UK_BFC/LAD_MAY_2024_UK_BFC.shp") %>% st_simplify(preserveTopology = T, dTolerance = 100)
  
#https://stackoverflow.com/a/33144808/5023561
#Make different pastel-ish colours
# n <- length(unique(sq.aiie$modal_sector))
# set.seed(101)
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))

tmap_mode('view')

tm_shape(
  sq.aiie
  # sq.aiie %>% mutate(combined_label = paste0(ITL221NM,', ',modal_sector))
) +
  tm_polygons('AIIE_weightedbyemployees', fill.scale = tm_scale_continuous(values = "matplotlib.rd_yl_bu"), 
              id="AIIE_weightedbyemployees", col_alpha = 0, fill_alpha = 0.65) +
  # tm_view(set.view = c(7, 51, 4)) +
  tm_shape(lad) +
  tm_borders(col = 'black', lwd = 1, fill_alpha = 0.3) +
  tm_shape(itl2) +
  tm_borders(col = 'black', lwd = 4) +
  tm_view(set_view = c(-2.2,53.49326048352635,11))#centred on Bradford
# tm_view(set_view = c(-1.598452,52.740283,8))
# tm_view(bbox = "England")
  
  




















