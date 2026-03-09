library(pacman)
p_load(tidyverse,conflicted,readxl,hms,FITfileR,here)
conflict_prefer_all("dplyr", quiet = T)

cat(as.character(Sys.time()),"\n")

# filelist <- here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-AirFlakeBelaySITTINGPilot-May-June2025","Pilot Data","Participant Data.xlsx")
filelist <- here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data","ProjectSperro_Participant Data.xlsx")
mydata <- read_xlsx(filelist,sheet = "master_database") %>% 
  mutate(Time_of_Day=hms::as_hms(Time_of_Day),
         Date=as.Date(Date)) %>% 
  fill(Date:Modality, .direction = "down")
if(file.exists(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data","ProjectSperro_analyzed_summary.csv"))){
  out_summary <- suppressMessages(read_csv(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data","ProjectSperro_analyzed_summary.csv")))
} else {
  out_summary <- data.frame()
}

if(nrow(mydata)!=nrow(out_summary)){
  # source("file_rename.R")
  source(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","iButton","file_rename.R"))
  
  mydata <- mydata %>% 
    filter(row_number()>nrow(out_summary))
  
  sensor <- read_xlsx(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data","iButtons Reference Number.xlsx")) %>% 
    rename(sticker=1,serial=2,location=3)
  
  grp <- mydata %>% 
    distinct(Date,Participant,Trial,Jacket_Code) %>% 
    mutate(directory=ifelse(nchar(Trial)==1,
                            paste0("Participant ",Participant,"/Trial ",Trial),
                            paste0("Participant ",Participant,"/",Trial)))
  
  
  for(i in seq_along(grp$Date)){
    print(grp$directory[i])
    subdata <- mydata %>% 
      filter(Date==grp$Date[i],Participant==grp$Participant[i],Trial==grp$Trial[i]) %>% 
      select(Date,Jacket_Code,Time_of_Day,Duration) %>% 
      mutate(time_start=as.numeric(Time_of_Day),
             time_end=time_start+Duration*60)
    
    if(dir.exists(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data",grp$directory[i],"iButton"))){
      filelist <- file_rename(grp$directory[i])
      
      if(filelist$orig[1]!=filelist$newfile[1]){
        file.rename(filelist$orig,filelist$newfile)
      }
      
      for(j in seq_along(filelist$orig)){
        tmp <- suppressMessages(read_xlsx(filelist$newfile[j]))
        
        if(length(grep("Date",tmp$`DatalogID:`))>0){
          tmp <- read_xlsx(filelist$newfile[j], skip = 24) %>% 
            filter(Date==Date[1]) %>% 
            mutate(Date=as.Date(Date),
                   Time=as_hms(Time),
                   time_s=as.numeric(Time),
                   Value=as.numeric(Value)) %>% 
            filter(Date==subdata$Date[1])
          
          
          out <- subdata %>% 
            rowwise() %>% 
            mutate(value=mean(tmp$Value[tmp$time_s>=(time_start+60) & tmp$time_s <=(time_end-60)]),
                   metric=filelist$metric[j],
                   subject=paste0("P",grp$Participant[i],"T",substr(grp$Trial[i],1,1)),
                   location=filelist$location[j])
          
          ifelse(j==1,allout <- out, allout <- bind_rows(allout,out))
        }
      }
    } else {
      allout <- subdata %>% 
        mutate(subject=paste0("P",grp$Participant[i],"T",substr(grp$Trial[i],1,1)),
               metric=NA,
               location=NA)
    }
    
    ### read HR file
    hrfilelist <- list.files(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data",grp$directory[i]), pattern = "fit",full.names = T)
    
    if(length(hrfilelist)>0){
      hrdata <- readFitFile(hrfilelist) %>% 
        records() 
      
      if(is.null(ncol(hrdata))){
        idx <- which.max(sapply(hrdata, nrow))
        
        # If you expect only one match:
        hrdata <- hrdata[[idx]]    
      }
      
      hrdata <- hrdata %>% 
        select(timestamp,heart_rate) %>% 
        rename(hr=heart_rate) %>% 
        mutate(timestamp=with_tz(timestamp,"America/Los_Angeles"),
               date=as.Date(timestamp), 
               time=as_hms(timestamp),
               time_s=as.numeric(time))
      
      allout <- allout %>% 
        # select(-time_s,-time_end) %>%
        unite(metric,metric,location,sep = "_", na.rm = T) %>%
        mutate(metric=ifelse(metric=="",NA,metric)) %>% 
        bind_rows(subdata %>% 
                    rowwise() %>% 
                    mutate(value=mean(hrdata %>% filter(between(time_s+60,time_end-60,time_end)) %>% pull(hr)),
                           metric="HR",
                           subject=paste0("P",grp$Participant[i],"T",substr(grp$Trial[i],1,1))) %>% 
                    ungroup())
    } else {
      allout <- allout %>% 
        unite(metric,metric,location,sep = "_", na.rm = T) %>% 
        mutate(metric=ifelse(metric=="",NA,metric))
    }
    
    allout2 <- allout %>% 
      select(-time_start, -time_end) %>% 
      filter(!is.na(metric)) %>% 
      pivot_wider(names_from = "metric",
                  values_from = "value")
    
    ifelse(i==1, finalout <- allout2, finalout <- bind_rows(finalout, allout2))    
  }
  
  if(file.exists(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data","ProjectSperro_analyzed_summary.csv"))){
    write_csv(finalout,here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data","ProjectSperro_analyzed_summary.csv"),na="",append = T)
  } else {
    write_csv(finalout,here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","misc","Arcteryx","EXT - R&D Arcteryx Research - Arcteryx-ProjectSperro","Pilot Data","ProjectSperro_analyzed_summary.csv"),na="")
  }
}

# library(cronR)
# cmd1 <- cron_rscript("workflow.R")
# cron_add(cmd1, frequency = "*/2 * * * *", id="iButton", description="run iButton analysis script every 2min for testing")
# cron_add(cmd1, frequency = "0 23 * * *", id="iButton", description="run iButton analysis script every night at 11pm")

