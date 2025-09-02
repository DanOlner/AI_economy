#Exploring occupation links
library(tidyverse)
library(nomisr)
library(sf)
library(tmap)
library(ggridges)
source('functions/helpers.R')
source('functions/ad_hoc_functions.R')

# CENSUS 2021 OCCUPATIONS----

#Via bulk download
#Workplace counts at MSOA first
#105 categories of SOC2020, so that's 
#3 digit "minor group"
#https://www.hesa.ac.uk/collection/coding-manual-tools/sicsocdata/soc-2020
occ.msoa <- read_csv("local/data/wp016/WP016_msoa.csv")

#Some tweaks to MSOA occupation counts census data
occ.msoa <- occ.msoa %>% 
  filter(!qg("does not apply",`Occupation (current) (105 categories) Label`)) %>% 
  mutate(
    occupation_name = str_sub(`Occupation (current) (105 categories) Label`,5,-1),
    occupation_code = str_sub(`Occupation (current) (105 categories) Label`,1,3)
  ) %>% 
  select(-c(`Occupation (current) (105 categories) Code`,`Occupation (current) (105 categories) Label`))



#Do we have even more cats at LA level? Nope, still 105
#Upper tier
occ.la <- read_csv("local/data/wp016/WP016_utla.csv")

#Some tweaks to local authority occupation counts census data
occ.la <- occ.la %>% 
  filter(!qg("does not apply",`Occupation (current) (105 categories) Label`)) %>% 
  mutate(
    occupation_name = str_sub(`Occupation (current) (105 categories) Label`,5,-1),
    occupation_code = str_sub(`Occupation (current) (105 categories) Label`,1,3)
  ) %>% 
  select(-c(`Occupation (current) (105 categories) Code`,`Occupation (current) (105 categories) Label`))

#Lower tier
occ.lla <- read_csv("local/data/wp016/WP016_ltla.csv")

occ.lla <- occ.lla %>% 
  filter(!qg("does not apply",`Occupation (current) (105 categories) Label`)) %>% 
  mutate(
    occupation_name = str_sub(`Occupation (current) (105 categories) Label`,5,-1),
    occupation_code = str_sub(`Occupation (current) (105 categories) Label`,1,3)
  ) %>% 
  select(-c(`Occupation (current) (105 categories) Code`,`Occupation (current) (105 categories) Label`))






# Get lookups----

aieo_occupation <- readxl::read_excel(path = "local/data/AIOE/AIOE_DataAppendix.xlsx", range = "Appendix A!A1:C775") %>% 
  rename(SOC = `SOC Code`, occupation = `Occupation Title`)

#Confirm normalised... tick
mean(aieo_occupation$AIOE)
sd(aieo_occupation$AIOE)

#774
length(unique(aieo_occupation$SOC))

#Get UK / US occupation lookup via NFER
#https://www.nfer.ac.uk/key-topics-expertise/education-to-employment/the-skills-imperative-2035/resources/systematic-mapping-of-soc-2020/ 
#Explainer here (Andy Dickerson on this list): https://www.nfer.ac.uk/media/1tojrw0o/matching_uk_soc2020_to_o_net.pdf
soclookup <- readxl::read_excel(path = "local/data/soc2020_onet2019_systematic_mapping.xls", range = "mapping SOC2020-ONET2019!A1:D1160")

#US codes in UK lookup
#926
length(unique(soclookup$`O*NET-SOC Code`))

#How many without the two trailing digits?
#806
length(unique(str_sub(soclookup$`O*NET-SOC Code`,0,-4)))


#More in the UK/US lookup
#Match, if without trailing two digits?
table(unique(str_sub(soclookup$`O*NET-SOC Code`,1,-4)) %in% unique(aieo_occupation$SOC))

#That's most of em
#Which ones don't match in UK lookup?
unique(str_sub(soclookup$`O*NET-SOC Code`,1,-4))[!unique(str_sub(soclookup$`O*NET-SOC Code`,1,-4)) %in% unique(aieo_occupation$SOC)]

#Which don't match in US SOCs?
unique(aieo_occupation$SOC)[!unique(aieo_occupation$SOC) %in% unique(str_sub(soclookup$`O*NET-SOC Code`,1,-4))]


#We can deal with the remaining ones through bootstrapping based on area spread for other occs


#Have checked against O*NET and some do seem to be missing from the AIOE lookup
#So we may just need to use the ones we can get values for

#Add in 105 3 digit SOC codes via the Census
soc20203dig <- tibble(
  SOC20203dig_code = str_sub(unique(occ.la$`Occupation (current) (105 categories) Label`),1,3),
  SOC20203dig_name = str_sub(unique(occ.la$`Occupation (current) (105 categories) Label`),5,-1)
) %>% 
  slice(2:nrow(.))#"does not apply" row

soclookup <- soclookup %>% 
  mutate(SOC20203dig_code = str_sub(as.character(`SOC2020 Unit Group`),1,3)) %>% 
  left_join(
    soc20203dig,
    by = 'SOC20203dig_code'
  )

#Add in four digit O*NET
soclookup <- soclookup %>% 
  mutate(ONET_4dig = str_sub(`O*NET-SOC Code`,1,-4))


#join what codes we can, see what's not covered
#Include name from both to check match is correct
soclookup <- soclookup %>% 
  left_join(
    aieo_occupation %>% rename(occ_name_fromAIOE = occupation),
    by = c("ONET_4dig" = "SOC")
  )

table(soclookup$Title == soclookup$occ_name_fromAIOE)

#Without NAs
# soclookup %>% select(Title,occ_name_fromAIOE) %>% 
#   filter(!is.na(occ_name_fromAIOE)) %>% View

#Fair chunk without matches, 15%
table(!is.na(soclookup$AIOE)) %>% prop.table() * 100



#Get average AIOE per UK 3 digit SOC code
soc_3dig_AIOE <- soclookup %>% 
  group_by(SOC20203dig_code,SOC20203dig_name) %>% 
  summarise(AIOE = mean(AIOE,na.rm=T)) %>% 
  ungroup()

#The only two that didn't get an AIOE there
#I think we can reasonably deduce from the AIOE
#It's a couple of IT related sectors

#So, taking all relevant sectors from AIEO
#Find average then apply
it.av <- aieo_occupation %>% 
  filter(qg('information',occupation)) %>% 
  filter(!qg('medical',occupation)) %>% 
  summarise(AIOE = mean(AIOE)) %>% 
  select(AIOE) %>% 
  pull

#Apply that...
soc_3dig_AIOE$AIOE[is.nan(soc_3dig_AIOE$AIOE)] <- it.av

#OK, save
saveRDS(soc_3dig_AIOE,'data/UK_SOC_3digit_linkedAIOE.rds')





# Apply to Local Authority Census 2021 occupation data----

#Check match to LA level SOC data... all there, good good
table(unique(occ.la$occupation_code) %in% soc_3dig_AIOE$SOC20203dig_code)


#Add AIOE into the Census data
occ.la.aioe <- occ.la %>% 
  left_join(
    soc_3dig_AIOE %>% select(-SOC20203dig_name),
    by = c('occupation_code' = 'SOC20203dig_code')
  )

#Check per LA means
la.aioe.avs <- occ.la.aioe %>% 
  group_by(`Upper tier local authorities Label`) %>% 
  summarise(
    AIOE_weightedmean = weighted.mean(AIOE,Count),
    sd_AIOE = sqrt(Hmisc::wtd.var(AIOE,Count))
    )


#GM LA match?
#This is run in sector_linkages... all of em, nice
table(gmlas %in% la.aioe.avs$`Upper tier local authorities Label`)

#rerun core cities for this data... this is England + Wales only
#From https://github.com/DanOlner/RegionalEconomicTools/blob/46907c9eb05193fe790579f73ff5f1ff019d90f5/quarto_docs/Bradford_sectorclusters.qmd#L384C1-L411C53
corecities <- getdistinct('sheffield|Belfast|Birmingham|Bristol|Cardiff|Glasgow|Leeds|Liverpool|Manchester|Tyne|Nottingham', unique(la.aioe.avs$`Upper tier local authorities Label`))

#Yes, all in there, need to remove a few...
corecities <- corecities[!grepl(x = corecities, pattern = 'Greater|shire|side', ignore.case = T)]
corecities <- corecities[order(corecities)]

#Different order to CH jobs...
# la.aioe.avs$gm <- ifelse(la.aioe.avs$`Upper tier local authorities Label` %in% gmlas,"GM","other")

la.aioe.avs <- la.aioe.avs %>%
  rename(laname = `Upper tier local authorities Label`) %>% 
  mutate(type = case_when(
    laname %in% gmlas ~ "GM LA",
    laname %in% corecities[!qg('manc',corecities)] ~ "core city (minus manc)",
    .default = "other"
  ),
  is_GM = laname %in% gmlas,
  xmin = AIOE_weightedmean - (sd_AIOE),
  xmax = AIOE_weightedmean + (sd_AIOE)
  )


ggplot(la.aioe.avs %>% filter(type != 'other'), 
       aes(
         x = AIOE_weightedmean, 
         y = fct_reorder(laname,AIOE_weightedmean), 
         colour = type, 
         size = is_GM)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = xmin, xmax = xmax), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('blue','green','black')) +
  scale_color_brewer(palette = 'Dark2') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab(' -- ')



# Repeat for MSOA level----

#Add AIOE into the MSOA data
occ.msoa.aioe <- occ.msoa %>% 
  rename(
    msoacode = `Middle layer Super Output Areas Code`,
    msoalabel = `Middle layer Super Output Areas Label`
    ) %>% 
  left_join(
    soc_3dig_AIOE %>% select(-SOC20203dig_name),
    by = c('occupation_code' = 'SOC20203dig_code')
  )

#Per MSOA weighted means
msoa.aioe.avs <- occ.msoa.aioe %>% 
  group_by(msoacode,msoalabel) %>% 
  summarise(
    AIOE_weightedmean = weighted.mean(AIOE,Count),
    sd_AIOE = sqrt(Hmisc::wtd.var(AIOE,Count))
  ) %>% 
  mutate(
    laname = str_sub(msoalabel,1,-5),
    type = case_when(
      laname %in% gmlas ~ "GM LA",
      laname %in% corecities[!qg('manc',corecities)] ~ "core city (minus manc)",
      .default = "other"
    ),
    is_GM = laname %in% gmlas
  )


#Check MSOA part names contain upper tier local authorities...
# occ.msoa.aioe <- occ.msoa.aioe %>% 
#   mutate(
#     laname = str_sub(msoalabel,1,-5)
#   )
# 
# table(unique(la.aioe.avs$laname) %in% unique(occ.msoa.aioe$laname))
# 
# #Those look like unitaries. We don't need them right now if we're just comparing to core cities
# unique(la.aioe.avs$laname)[!unique(la.aioe.avs$laname) %in% unique(occ.msoa.aioe$laname)]

table(msoa.aioe.avs$type)


#Plot individual MSOA values for each LA
ggplot(msoa.aioe.avs %>% filter(type != 'other'), 
       aes(
         x = AIOE_weightedmean, 
         y = fct_reorder(laname,AIOE_weightedmean), 
         colour = type, 
         size = is_GM)) +
  geom_jitter(height = 0.1) +
  # geom_errorbar(aes(xmin = xmin, xmax = xmax), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('blue','green','black')) +
  scale_color_brewer(palette = 'Dark2') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab(' -- ')







