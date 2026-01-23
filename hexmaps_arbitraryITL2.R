#Hexmaps for different ITL2s
#Code from sector_linkages.R
library(tidyverse)
library(plotly)
library(sf)
library(tmap)
library(patchwork)
source('functions/helpers.R')
source('functions/ad_hoc_functions.R')


# itl2name = 'Greater Manchester'
itl2name = 'South Yorkshire'

#Geographies
itl2 <- st_read('../RegionalEconomicTools/data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp') %>% st_simplify(preserveTopology = T, dTolerance = 100) %>% 
  filter(qg(itl2name,ITL221NM))


lad <- st_read("~/Dropbox/MapPolygons/UK/2024/Local_Authority_Districts_May_2024_Boundaries_UK_BFC/LAD_MAY_2024_UK_BFC.shp") %>% st_simplify(preserveTopology = T, dTolerance = 100)

ch <- readRDS('local/data/ch_with_4AIE_measures.rds')

lad <- lad %>% filter(LAD24NM %in% unique(ch$localauthority_name[ch$ITL221NM==itl2name]))

ch_sub <- ch %>% filter(ITL221NM == itl2name)

sq = st_make_grid(ch_sub, cellsize = 1000, square = F)

#Turn into sf object so gridsquares can have IDs to group by
sq <- sq %>% st_sf() %>% mutate(id = 1:nrow(.))

overlay <- st_intersection(ch_sub,sq)

#This no longer needs to be geo, which will speed up
#Can link back to grids once done

#Let's find an average AIIE weighted by employee number in each grid square
hexsummary <- overlay %>% 
  st_set_geometry(NULL) %>% 
  filter(Employees_thisyear > 0) %>% #Only firms with employees recorded in latest year
  # filter(between(Employees_thisyear,1,3)) %>% #Microfirms
  # filter(between(Employees_thisyear,4,9)) %>% #Microfirms
  # filter(Employees_thisyear > 9) %>%
  group_by(id) %>% 
  summarise(
    AIIE_weightedbyemployees = weighted.mean(AIIEfinal,Employees_thisyear),
    augmentmorethanreplaceprob_weightedbyemployees = weighted.mean(augment_morethan_replace_prob,Employees_thisyear),
    augmentBT_weightedbyemployees = weighted.mean(augment_logability_BT,Employees_thisyear),
    replaceBT_weightedbyemployees = weighted.mean(replace_logability_BT,Employees_thisyear),
    totalemployees = sum(Employees_thisyear),
    totalfirms = n(),
    la = max(localauthority_name)
  ) %>% 
  group_by(id) %>%
  filter(sum(totalemployees) >= 10) %>% #keep only gridsquares where total employee count is more than / equal to 100
  ungroup()


#Link that back into the grid squares...
#Use right join to drop empties
sq.ch <- sq %>% 
  right_join(
    hexsummary,
    by = 'id'
  )

sq.ch <- sq.ch %>% 
  mutate(hovertext = paste0(la,", aug>repl: ", round(augmentmorethanreplaceprob_weightedbyemployees,2)," Firm count: ",totalfirms, ", id: ",id))

# tmap_mode('view')
tmap_mode('plot')

tm_shape(sq.ch %>% rename(aug = augmentBT_weightedbyemployees, repl = replaceBT_weightedbyemployees, AIIE = AIIE_weightedbyemployees, `aug>rep` = augmentmorethanreplaceprob_weightedbyemployees)) +
  tm_polygons(
    fill = tm_vars(c("AIIE", "aug>rep"),
    # fill = tm_vars(c("aug", "repl"),
                   multivariate = TRUE), id="hovertext",
    fill.scale = 
      tm_scale_bivariate(
        scale1 = tm_scale_intervals(style = "fisher", n = 3, labels = c("L", "M", "H")),
        scale2 = tm_scale_intervals(style = "fisher", n = 3, labels = c("L", "M", "H")),
        # values = "stevens.bluered")) +
        values = "bu_br_bivs")) +
  # tm_view(set.view = c(7, 51, 4)) +
  tm_shape(lad) +
  tm_borders(col = 'black', lwd = 1, fill_alpha = 0.3) +
  tm_shape(itl2) +
  tm_borders(col = 'black', lwd = 4) +
  tm_view(set_view = c(-2.2,53.49326048352635,11))#centred on GM
