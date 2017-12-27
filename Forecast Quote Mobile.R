rm(list=ls())

library(tidyquant)
library(forecast)
library(forecastHybrid)
library(sweep)
library(timetk)
library(prophet)
library(scales)


risorse <- "MyDirectory"
## geo <- read.csv(paste(risorse,"/geomapping.csv",sep=""), stringsAsFactors = FALSE)
forecast <- "OtherDirectory"

store <- "store"


  loc<-paste(forecast,"/", store, sep="")
  setwd(loc)
  
  ## lettura <- read.csv("raw.csv",stringsAsFactors = FALSE)
  ## medium <- read.csv(paste(loc, "/channelmapping.csv",sep=""), stringsAsFactors = FALSE, sep=";")
  ## medium <- medium %>% rename(Medium=mezzi)
  
  # ## Medium Mapping 
  # lettura_mod <- as_tibble(lettura) %>% mutate(Store=store, Date=as.Date(as.character(Date), format = "%Y%m%d"))
  # lettura_map <- left_join(lettura_mod, medium, by=c("Medium"="mediumGA")) %>% select(- Medium) %>%
  #   rename(Device=Device.Category, Medium=Medium.y)
  # lettura_map$Medium[is.na(lettura_map$Medium)] <- "Others"
  # 
  # ## Geo Mapping
  # 
  # db_map <- left_join(lettura_map, geo, by=c("Country" = "CountryGA")) %>% 
  #   select(Store, CountryBP, Date, Device, Medium, Sessions) %>% 
  #   rename (Country=CountryBP)
  # db_map$Country[is.na(db_map$Country)] <- "OT non EU"
  # 
  # ## Grouping and summarising
  # db_map <- db_map %>% group_by(Store, Country, Date, Device, Medium) %>% summarise(Sessions=sum(Sessions))
  # db_map
  # write.csv(db_map, "raw_mappato.csv", row.names=F)
  
  db_map <- read_csv(paste(loc,"/raw_mappato.csv",sep=""), 
                     col_types = cols(Date = col_date(format = "%Y-%m-%d")))
  db_map <- as.tibble(db_map)
  db_map
  
  ### Grouping by Month
  db_month <- db_map %>% group_by(Store, Country, Device, Medium, Month = as_date(as.yearmon(Date))) %>% 
    summarise(Sessions=sum(Sessions))
  
  ## Mapping Device (Tablet + Desktop --> Altro)

  db_dev <- db_month %>% group_by(Store, Country, DISP=ifelse(Device =="mobile","Mobile","Altro"), Medium, Month) %>% 
    summarise(Sessions=sum(Sessions))
  
  ## Salvo le Ultime Quote per Country/Medium
  Quote_temp <- db_dev %>% filter(Month=="2017-08-01") %>% spread(DISP, Sessions) 
  Quote_temp$Altro[is.na(Quote_temp$Altro)] <- 0
  Quote_temp$Mobile[is.na(Quote_temp$Mobile)] <- 0
  Quote <- Quote_temp %>% mutate (Quota=Mobile/(Mobile+Altro)) %>% select(Store, Country, Medium, Quota)
  
  
  ## Saving the last Mobile Quotas at Country/Medium level
  storico_quote <- db_dev %>% spread(DISP,Sessions)
  storico_quote$Altro[is.na(storico_quote$Altro)] <- 0
  storico_quote$Mobile[is.na(storico_quote$Mobile)] <- 0
  storico_quote <- storico_quote %>% mutate(Quota=Mobile/(Mobile+Altro))
  
  
  ## Grouping by Country
  
  db_country <- db_dev %>% group_by(Store, Country, DISP, Month) %>% summarise(Sessions=sum(Sessions))
  db_spread <- spread(db_country, DISP, Sessions)
  db_spread$Altro[is.na(db_spread$Altro)] <- 0 
  db_spread$Mobile[is.na(db_spread$Mobile)] <- 0 
  db_spread
  
  db_quota <- db_spread %>% mutate(Quota=Mobile/(Mobile+Altro))
  db_quota
  
  qt_ctr <- db_quota %>%
    ggplot(aes(x=Month, y=Quota, col=Country)) + 
    geom_line(size=1) +
    facet_wrap(~ Country) +
    theme ( legend.position = "none", axis.text.x=element_text(angle=50, hjust=1)) + 
    ylab("Quota Mobile") + xlab("Mese") + 
    labs(title="Quote per Country - Storico")
  
  
  qt_ctr
  ## Saving this great picture!
  
  # dir.create("Forecast_Mobile")
  setwd(paste(loc, "/Forecast_Mobile", sep=""))
  png("Quote_per_Country.png")
  print(qt_ctr)
  dev.off()
  
  
  ## Making Forecasts
  prep <- db_quota %>% ungroup() %>% select(Country, Month, Quota)
  
  ## Hybrid Model
  
  final=NULL
  calendar <- tibble(Month = seq.Date(from=as.Date(min(db_quota$Month)), to=as.Date("2017-08-01"), "months"))
  
  for(country in unique(prep$Country))   { 
    
      print(country)
      tmp <- prep %>% filter(Country==country) 
      
        cal <- calendar %>% mutate(Country=country)
        
        tmp_calendar <- left_join(cal, tmp, by=c("Country","Month"))
        tmp_calendar$Quota[which(is.na(tmp_calendar$Quota))] <- 0
        
        tmp.ts <- tk_ts(tmp_calendar$Quota, 
                        freq = 12, 
                        start = c(year(min(db_quota$Month)), month(min(db_quota$Month))), 
                        silent = TRUE)
        
        m <- hybridModel(tmp.ts, models = 'ae', verbose = FALSE)
        p <- forecast(m, h = 16)
        
        swp <- sw_sweep(p, timetk_idx = T, rename_index = "Month") %>% 
          mutate(Country = country, Month = as_date(as.yearmon(Month))) %>%
          select(Country, Month, key, value)
        
        final = rbind(final, swp) 
        
      }
    
  final <- final %>% rename(Hybrid=value) 
  final$key[final$key=="forecast"] <- "Hybrid"
  
  # final %>% 
  #   ggplot(aes(x=Month, y=Hybrid, col=key)) +
  #   geom_line(size=1) +
  #   theme(legend.position = "none") +
  #   facet_wrap(~ Country) +
  #   labs (title = "Quote per Country - Hybrid Forecast") +
  #   ylab("Quota Mobile")
    
  
  ## Prophet Model
  
  prep
  
  rename <- prep %>% rename(ds=Month, y=Quota)
  nested <- rename %>% nest(-Country)
  
  ppht <- nested %>% mutate(prophet = map( data, prophet))
  ftr <- ppht %>% mutate( future= map(prophet, make_future_dataframe, periods=16, freq='m'))
  prdct <- ftr %>% mutate( pred = map2(prophet, future, predict))
  
  fnl_ppht <- prdct %>% 
    select(Country, pred) %>% 
    unnest() %>% 
    select(Country, ds, Proph = yhat) %>% rename(Month=ds)
  
  
  ### Joining the models' results
  join_fc <- left_join(final, fnl_ppht %>% filter(Month >= "2017-09-01"), by=c("Country", "Month"))
  
  # join_fc %>% ggplot(aes(x=Month, y=Hybrid, col=key)) +
  #   geom_line(size=1) +
  #   geom_line(aes(x=Month, y=Proph, col="Prophet"), size=1) +
  #   facet_wrap(~ Country) +
  #   ylab("Quota Mobile") +
  #   labs(title="Quote per Country - Forecast")
  
  
  ### Adding mean value between hybrid and prophet forecasts
  fc_mean <- join_fc %>% mutate(Mean=rowMeans(data.frame(Proph, Hybrid), na.rm=TRUE))
  fc_mean$Mean[fc_mean$key=="actual"] <- NA
  
  
  # fc_mean %>%
  #     ggplot(aes(x=Month, y= Hybrid, col=key)) +
  #     geom_line(size=1)+
  #     geom_line(size=1, aes(x=Month, y=Proph, col="Prophet")) +
  #     geom_line(size=1, aes(x=Month, y=Mean, col="Mean")) +
  #     facet_wrap(~ Country) + 
  #     ylab("Quota Mobile") +
  #     labs(title="Quote per Country - Forecast")
  
  
  ##  Consistency validation
  fc_mean %>% filter(Mean >= 1) %>% summarise(Errori=n())
  
  ## Saving and plotting the forecasts for Mobile Quotas at Country Level
  temp <- fc_mean %>% mutate(Quota=ifelse(key=="actual", Hybrid, Mean)) %>% select(-Hybrid, - Mean, - Proph)
  
  qt_ctr_fc <- temp %>% ggplot(aes(x=Month, y=Quota, col=key)) +
    geom_line(size=1) +
    facet_wrap(~ Country) + 
    ylab("Quota Mobile") +
    labs(title="Quote per Country - Forecast") +
    theme(legend.position="none", axis.text.x=element_text(angle=50, hjust=1)) + 
    labs(title="Forecast Quote per Country")
  
  qt_ctr_fc
  png("Forecast_quote_country.png")
  print(qt_ctr_fc)
  dev.off()
  setwd(paste(loc, "/Forecast_Mobile", sep=""))
  temp <- temp %>% mutate(key=ifelse(key=="actual", "Actual", "Forecast"))
  write.csv2(temp, "Forecast_quote_mobile_country.csv", row.names=F, dec=",", sep=";")
  
  ## Computing  MoM rates for Country
  pre_mom <- fc_mean %>% filter(Month >= "2017-08-01") %>% select(Country, Month, Mean)
  last_m <- fc_mean %>% filter(Month=="2017-08-01") %>% select(Country, Month, Hybrid)
  pre_mom_2 <- left_join(pre_mom, last_m, by=c("Country", "Month"))
  pre_mom_2$Mean[pre_mom_2$Month == "2017-08-01"] <- pre_mom_2$Hybrid[pre_mom_2$Month == "2017-08-01"]
  pre_mom_3 <- pre_mom_2 %>% select(-Hybrid)
  
  mom <- pre_mom_3 %>% nest(-Country) %>% 
    mutate(ts = map(data, tk_ts, start=c(2017,8,1), freq=12), Tasso=map(ts, Delt)) %>% 
    select(Country, data, Tasso) %>% unnest() %>% filter(Month!="2017-08-01") %>% select(-Mean)
  
  
  
  ## Computing Quotas for Country and Medium
  db=NULL
  Quote <- Quote %>% ungroup() %>% select(- Store)

  for (paese in unique(Quote$Country)) {
    for (canale in unique(( Quote %>% filter(Country==paese))$Medium )) {
      
      Last <- Quote %>% filter(Country==paese, Medium==canale) %>% select(Quota)
      new_qt <- mom %>% filter(Country==paese) %>% mutate(Medium=canale, Quota=NA) 
      i=1
      new_qt[1, 5] <- Last * (1+new_qt[1, 2])
      
      for (i in seq(2,16)){
        new_qt[i, 5] <- Last * (1 + new_qt[i, 2])
      }
      
      db <- rbind(db, new_qt)
    }
    
  }
  
  
  ## Consistency validation
  summary(db$Quota)
  boxplot(db$Quota)
  
  if (quantile(db$Quota, 0.97) < 1) {
    
    db$Quota[db$Quota >= 1] <- quantile(db$Quota, 0.97)
  } else {
    
    print("error")
  }
  summary(db$Quota)
  
  
  ## Binding history and forecast
  st_qt <- storico_quote %>% ungroup()  %>% select(- Store, -Altro, -Mobile)
  fc_qt <- db %>% select(-Tasso) %>% select(Country, Medium, Month, Quota)
  
  all_qt <- rbind(st_qt, fc_qt) %>% 
    arrange(Country, Medium, Month) %>% 
    mutate(Key=ifelse(Month<="2017-08-01","Actual","Forecast"))
  
  
  
  ## Plotting
  setwd(paste(loc, "/Forecast_Mobile", sep=""))
  for(paese in unique(all_qt$Country)){
    
    png(paste(store,paese,"Mobile.png",sep="_"))
    
    print(all_qt %>% filter(Country==paese) %>% ggplot(aes(x=Month, y=Quota, col=Key)) +
        geom_line(size=1) +
        facet_wrap(~ Medium) + 
        labs(title = paste(store, paese))  +
        xlab("") +
        theme (axis.text.x=element_text(angle=50,hjust=1), legend.position = "none") 

    )
    dev.off()
  }
    
  write.csv2(all_qt, "Mobile_forecast.csv", row.names=F,dec=",", sep=";")