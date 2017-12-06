rm(list=ls())

# Libraries to load
library(readr)
library(dplyr)
library(lubridate)      
library(tidyquant)
library(gtools)
library(tidyverse)
library(forecast)
library(forecastHybrid)
library(forecastxgb)
library(timetk)
library(sweep)
library(prophet)

options(scipen = 999)


# You need to spec. 'risorse'. The geo mapping file is the same for all stores.
geo <- read.csv(paste(risorse,"/geomapping.csv",sep=""), stringsAsFactors = FALSE)

# You need to spec. 'store' and directory 'loc'
  setwd(loc)
  lettura <- read_csv("raw.csv", col_types = cols(Date = col_date(format = "%Y%m%d")))
  
  
  ### Mapping Data & Grouping by Month
  setwd(risorse)
  source("Mapping.R")
  
  ### 1st Model: Hybrid
  source("Hybrid Model.R")
  output1 <- final %>% rename(Hybrid=value)
  
  ### 2nd Model: Prophet with Log Transformation
  invisible(capture.output(source("Prophet Model.R")))
  
  
  
  ### Check to avoid jumps in predictions
  j2_corr <- j2_mean %>% mutate(max_actual=max(Hybrid[key=="actual"]), 
                                delta=max((Hybrid-Prophet)[key=="forecast"], na.rm=TRUE),
                                Sessions = ifelse(delta > max_actual, Prophet, Mean))
  j2_corr$Sessions[j2_corr$key=="actual"] <- j2_corr$Hybrid[j2_corr$key=="actual"]
  
  j2_corr_2 <- j2_corr %>% mutate(delta2=max((Prophet-Hybrid)[key=="forecast"],na.rm=TRUE), 
                                  Sessions2 = ifelse(delta2 > max_actual, Hybrid, Sessions))
  j2_corr_2$Sessions2[j2_corr_2$key=="actual"] <- j2_corr_2$Hybrid[j2_corr_2$key=="actual"]
  
  eps <- j2_corr_2 %>% filter(Sessions != Sessions2)
  output <- j2_corr_2 %>% select(CountryBP, Mezzi,ds, key, Sessions2) %>% rename(Date=ds, Sessions = Sessions2) 
  
  
### Plotting a graph per Country. Each plot is segmented by Channel
source("Plot.R")
  
### Writing the output with actual and forecast values
write.csv(output, paste(store,'_forecast.csv',sep=""), row.names = FALSE)

