file_rename <- function(directory){
  tmpfilelist <- data.frame(orig=list.files(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data",directory,"iButton"), recursive = T,full.names = T)) %>% 
    # filter(grepl("iButton",orig)) %>% 
    mutate(file=str_extract(basename(orig),"^[^.]+")) # get filename with extension
  
  # check if filenames are raw or changed
  if(str_detect(basename(tmpfilelist$orig[1]), "^[0-9A-F]+_")){ #hex version
    tmpfilelist <- tmpfilelist %>% 
      mutate(serial = str_extract(file, "^[A-Za-z0-9]{15}"), # 15 chars at start of filename
             serial = substr(serial,9,nchar(serial)-1),
             metric = ifelse(substr(file,nchar(file),nchar(file))=="1","Temp","RH"),
             date = str_extract(file, "(?<=_)[^_]+(?=_)"),
             date = substr(date,1,6),
             date = as.Date(paste0("20",substr(date,5,6),"-",substr(date,1,2),"-",substr(date,3,4))),
             subject = paste0("P",grp$Participant[i],"T",substr(grp$Trial[i],1,1))
      ) %>% 
      left_join(sensor,by="serial") %>% 
      select(-file,-sticker,-serial) %>% 
      mutate(newfile=paste0(dirname(orig),"/",date,"_",subject,"_",location,"_",metric,".xlsx"))
  } else { #date version
    tmpfilelist <- tmpfilelist %>%
      separate(
        col = file,
        into = c("date","subject","location","metric"),
        sep = "_",
        remove = T
      ) %>% 
      mutate(newfile=orig)
  }
  return(tmpfilelist)
}

