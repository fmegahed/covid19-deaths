setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if(require(pacman)==FALSE) install.packages("pacman") # check to see if the pacman package is installed; if not install it
if(require(devtools)==FALSE) install.packages("devtools") # check to see if the devtools package is installed; if not install it
if(require(albersusa)==FALSE) devtools::install_github('hrbrmstr/albersusa') #install package if needed

# Install (if needed) and load the following packages
pacman::p_load(tidyverse, magrittr, janitor, dataPreparation, lubridate, skimr, # for data analysis
               COVID19, rvest, # for extracting relevant data
               DT, pander, stargazer, knitr, # for formatting and nicely printed outputs
               scales, RColorBrewer, DataExplorer, tiff, grid,# for plots
               plotly, albersusa, tigris, leaflet, tmap, # for maps
               zoo, fpp2, NbClust, # for TS analysis and clustering
               VIM, nnet, caret, MuMIn, # explanatory modeling
               conflicted) # for managing conflicts in functions with same names

# Handling conflicting function names from packages
conflict_prefer('combine', 'dplyr') # Preferring dplyr::combine over any other package
conflict_prefer('select', "dplyr") # Preferring dplyr::select over any other package
conflict_prefer("summarize", "dplyr") # Preferring dplyr::summarize over any other package
conflict_prefer("filter", "dplyr") # Preferring filter from dplyr
conflict_prefer("dist", "stats") # Preferring dist from stats
conflict_prefer("as.dist", "stats") # Preferring as.dist from stats
conflict_prefer("addLegend", "leaflet")

# Custom Functions
source_url('https://raw.githubusercontent.com/fmegahed/covid19-deaths/master/Markdown/custom_functions.R')

# Setting the seed
set.seed(2021) # to assist with reproducibility


############################ Deaths #######################################################
endDate = '2021-02-27'
endDatePrintV = format(ymd(endDate), format = "%b %d, %Y")

counties = readRDS("../Data/Output/counties.rds")

googleKey = c("Alabama, Washington County", "Arkansas, Washington County", 
              "New Hampshire, Strafford County", "New York, Monroe County",
              "North Carolina, Cherokee County", "Ohio, Huron County",
              "Oklahoma, Greer County", "Tennessee, Campbell County", "Tennessee, Lincoln County")


df = counties %>% 
  select(id, date, key_google_mobility, newDeaths) %>% # selecting minimal amount of cols for visual inspection
  arrange(id, date) %>% # arranged to ensure correct calculations
  mutate(newMA7 = rollmeanr(newDeaths, k = 7, fill = NA), # 7-day mm of new (adjusted) deaths
         maxMA7 = max(newMA7, na.rm = T), # obtaining the max per county to scale data
         scaledNewMA7 = pmax(0, newMA7/maxMA7, na.rm = TRUE) ) %>% # scaling data to a 0-1 scale by county
  select(id, key_google_mobility, date, scaledNewMA7)

pdf(file = '../Figures/sampleScaledDeaths.pdf',
     width = 6.5, height = 3.75, pointsize = 8)
df %>% filter(key_google_mobility %in% googleKey) %>% 
  ggplot(aes(x = date, y = scaledNewMA7, group = id)) +
  geom_line() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  facet_wrap(~ key_google_mobility, scales = 'free_y', ncol = 3) +
  theme(legend.position = 'none') + 
  labs(color = '', x = 'Month', y = 'Scaled Deaths By County') + theme_bw(base_size = 9)
invisible( dev.off() ) # to suppress the unwanted output from dev.off




###########################################################################################
clusterCounties <- readRDS("../Data/Output/clusterCounties.rds")

spaghettiDF <- counties %>% # from the counties
  select(id, date, newDeaths, key_google_mobility) %>% # selecting minimal columns
  left_join(clusterCounties[, c('id', 'cluster_group')], by = 'id') %>% # to get clusters
  arrange(id, date) %>% # arranged to ensure correct calculations
  mutate(newMA7 = rollmeanr(newDeaths, k = 7, fill = NA), # 7-day ma of new (adjusted) deaths
         maxMA7 = max(newMA7, na.rm = T), # obtaining the max per county to scale data
         scaledNewMA7 = pmax(0, newMA7/maxMA7, na.rm = TRUE) ) %>% 
  ungroup() %>% select(date, cluster_group, scaledNewMA7, key_google_mobility) %>% 
  group_by(date, cluster_group)

spaghettiDF$cluster_group %<>% as.factor() 


colorPal <-  brewer.pal(4, "Greys") %>% rev()
names(colorPal) <- levels(spaghettiDF$cluster_group)


pdf(file = '../Figures/spaghettiPlot.pdf', width = 6.5, height = 3.75, pointsize = 8)
spaghettiDF %>%  
  ggplot(aes(x = date, y = scaledNewMA7, color = cluster_group, group = key_google_mobility)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_line(size = 0.25, alpha = 0.15 ) +
  stat_summary(aes(group = 0.5), 
               fun= median,
               geom = "line",
               size = 1.5, col = 'black') + 
  facet_wrap(~ cluster_group, ncol = 1) + theme_bw(base_size = 9) +
  theme(legend.position = 'none') + 
  labs(x = 'Month', y = 'Scaled New Deaths By Cluster By Day',
       caption = paste0('Solid black line represents the median for each cluster | 
       Based on data from March 01, 2020 - ', endDatePrintV) )  +
  scale_color_manual(values = colorPal)
invisible( dev.off() ) # to suppress the unwanted output from dev.off



summaryDf <- spaghettiDF %>% 
  summarise(Median = median(scaledNewMA7, na.rm= TRUE),
            `First Quartile` = quantile(scaledNewMA7, probs = 0.25, na.rm= TRUE),
            `Third Quartile` = quantile(scaledNewMA7, probs = 0.75, na.rm= TRUE)) %>% 
  pivot_longer(cols = c(`First Quartile`, Median, `Third Quartile`),
               names_to = 'Statistic')


pdf(file = '../Figures/summaryPlot.pdf', width = 6.5, height = 3.75, pointsize = 8)
summaryDf %>% 
  ggplot(aes(x = date, y = value, linetype =  Statistic)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_line(size = 0.75) +
  scale_linetype_manual(values = c('dotted', 'solid', 'twodash')) +
  facet_wrap(~ cluster_group, ncol = 1) +
  theme_bw(base_size = 9) +
  theme(legend.position = 'top') + 
  labs(color = '', x = 'Month', y = 'Quartiles of Scaled New Deaths By Cluster By Day')
invisible( dev.off() ) # to suppress the unwanted output from dev.off


#########################################################################################
# Joining the clusterCounties results with the existing county simple features object (cty_sf)
cty_sf <- counties_sf("aeqd") %>% filter(!state %in% c('Alaska', 'Hawaii')) # from albersua

clusterCounties$fips <- str_pad(clusterCounties$key_numeric, width = 5, side = 'left', pad = '0')
clusterCounties %<>% ungroup()
cty_sf %<>% left_join(clusterCounties[, c('fips', 'cluster_group')], by = 'fips') # adding cluster_group to cty_sf

# Creating a static visual for the paper
pdf(file = '../Figures/clusterMap.pdf', width = 6.5, height = 6.5, pointsize = 8)
tm_shape(cty_sf) + 
  tm_polygons('cluster_group', title = 'Cluster #', palette = colorPal, colorNA = "black") +
  tm_style("white") +
  tm_layout(outer.margins = c(0,0,0,0), frame = F, legend.text.size = 1)
invisible( dev.off() ) # to suppress the unwanted output from dev.off



########################################################################
multiClassDF <- readRDS("../Data/Output/multiClassDF.rds") %>% 
  mutate(e_popdensity = log(e_popdensity)) %>% 
  mutate_if(is.character, as.factor) %>% select(-fips)

skim(multiClassDF, where(is.numeric)) %>% kable(format = "latex", booktabs = T) %>%
  kableExtra::kable_styling(font_size = 9)



#############################################################################

counties = covid19(country = 'US', level = 3, start = '2020-03-01', end = endDate,
                   raw = FALSE)

counties %<>% select(id, key_google_mobility, date, deaths) %>% 
  mutate(newDeaths = c(NA, diff(deaths)),
         newMA7 = rollmeanr(newDeaths, k = 7, fill = NA))

us = covid19(country = 'US', level = 1, start = '2020-03-01', end = endDate)
us %<>% select(id, key_google_mobility, date, deaths) %>% 
  mutate(newDeaths = c(NA, diff(deaths)),
         newMA7 = rollmeanr(newDeaths, k = 7, fill = NA))

indices = c('Alabama, Lee County', 'California, Los Angeles County',
            'Illinois, Madison County', 'Kansas, Stevens County', 'New Jersey, Ocean County',
            'New York, New York County', 'Ohio, Butler County', 'Texas, Cameron County') # some counties

# filtering some counties and combining both data frames
df = counties %>% filter(key_google_mobility %in% indices)
df = rbind(us, df)

df$key_google_mobility %<>% recode(US = 'Aggregate for the Entire US')

pdf(file = '../Figures/nonScaledDeaths.pdf',
    width = 6.5, height = 3.75, pointsize = 8)
df %>%
  ggplot(aes(x = date, y = newMA7, group = id)) +
  geom_line() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  facet_wrap(~ key_google_mobility, scales = 'free_y', ncol = 3) +
  theme(legend.position = 'none') + 
  labs(color = '', x = 'Month', y = 'Deaths Due to COVID-19') + theme_bw(base_size = 9)
invisible( dev.off() ) # to suppress the unwanted output from dev.off


######################################################################################################
multiClassDF <- readRDS("J:/My Drive/Miami/Code/GitHub/covid19-deaths/Data/Output/multiClassDF.rds")
multiClassDF$e_popdensity <- log(multiClassDF$e_popdensity)

multiClassDF %<>% select(-c(fips, location)) %>% group_by(cluster_group)


multiClassDF %>% summarise(across(where(is.numeric), 
                                  list(mean = ~ mean(.x, na.rm = TRUE) %>% round(digits = 2),
                                       sd = ~ sd(.x, na.rm = TRUE) %>% round(digits = 2) 
                                       )
                                  ) 
                           ) %>% t()

tabyl(multiClassDF, party, cluster_group) 
tabyl(multiClassDF, party, cluster_group) %>% adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)


tabyl(multiClassDF, region, cluster_group) 
tabyl(multiClassDF, region, cluster_group) %>% adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
