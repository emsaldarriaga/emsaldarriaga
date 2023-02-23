
read = c("rvest","readxl","rlist","htmltab","httr","readr","htmltools","XML")
apis = c("tidyverse","tidycensus","tigris","censusapi","bea.R","idbr")
managedata = c("dplyr","stringr","doBy","collapse","data.table","reshape2")
spatial = c("SpatialEpi","SUMMER","stringi","sp","rgdal","rgeos","spdep","INLA","maptools","maps","geoR","sf","fields")
plot = c("ggplot2","cowplot","viridis","grid","colorspace","biscale","gridExtra","ggthemes","shape","RColorBrewer",
         "plotly","dygraphs","scales","av","extrafont","lubridate")
misc = c("xts","kableExtra","hutils","gganimate")
sapply(c(read,apis,managedata,spatial,plot),require, character.only=T)
rm(read,apis,managedata,spatial,plot)

fatalshoot = readr::read_csv("../Downloads/fatal-police-shootings-data.csv") %>% dplyr::select(-id) %>%
  mutate(Year = year(date))

vars = (tidycensus::load_variables(2018, "acs5", cache = TRUE) %>% .[c(52,53,68, 83,84,99,114,115,130,145,146,161,176,177,192,207,208,223,238,239,254,269,270,285,300,301,316),1]) %>% unlist() %>% as.character()

labs = paste0(rep(c("white","black","nativeam","asian","hawaiian","other","twoplus","nonhispwhite","hispanic"),each=3),
       rep(c("_total","_male","_female"),times=9))


pop_race = NULL
for(i in 2015:2021){ #not available for 2022+
  pop_race = rbind(pop_race, tidycensus::get_acs(geography   = "state",variables   = vars, year= i,geometry = FALSE,
                                                 cache_table = TRUE) %>% mutate(Variable = rep(labs,times=52),Year=i) %>%
                     dplyr::select(-c(variable,moe)) %>% setNames(c("FIPS","State","Estimate","Variable","Year"))
                   )
}

pop_race = spread(setDT(pop_race),key = "Variable",value = "Estimate") %>%  
  left_join(.,tidycensus::fips_codes %>% distinct(state,state_code),by=c("FIPS"="state_code")) %>% .[,c(1,2,31,3,4:30)]



