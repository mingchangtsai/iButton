library(pacman)
p_load(tidyverse,conflicted,readxl,arrow,dygraphs,lubridate,parallel)
conflict_prefer("filter","dplyr")



### GPS files
# https://msmith.de/FITfileR/articles/FITfileR.html
library(FITfileR)

filelist <- list.files("../GPS Watch Data/FIT FILES (RAW)/Leah", full.names = T)

gpsdata <- readFitFile(filelist[1])

gpsdata2 <- records(gpsdata)$record_1 %>% 
  mutate(time_PDT=with_tz(timestamp,"America/Los_Angeles"),
         index=row_number())



dygraph(gpsdata2[,c("index","speed")]) %>% 
  dyRangeSelector()


