### Forecast Loop
final=NULL
calendar <- tibble(Date = seq.Date(from=as.Date("2014-01-01"), to=as.Date("2017-08-01"), "months"))

for(country in unique(input$CountryBP))   { 
  for (medium in unique((input %>% filter(CountryBP==country))$Mezzi)) {
    
    print(paste(country, medium))
    tmp3 <- input %>% filter(CountryBP==country, Mezzi==medium) 
    
    if (nrow(tmp3) < 4) {
      storico <- tmp3 %>% mutate(Date= as_date(as.yearmon(Date)), key="actual", value=Sessions) %>% select(- Sessions)
      final=rbind(final, storico)
      
    } 
    else {
      
      cal2 <- calendar %>% mutate(CountryBP=country, Mezzi=medium)
      
      tmp_calendar <- left_join(cal2, tmp3, by=c("CountryBP","Mezzi","Date"))
      tmp_calendar$Sessions[which(is.na(tmp_calendar$Sessions))] <- 0
      
      tmp.ts3 <- tk_ts(tmp_calendar$Sessions, freq = 12, start = c(2014, 01), silent = TRUE)
      m3 <- hybridModel(tmp.ts3, models = 'ae', verbose = FALSE)
      p3 <- forecast(m3, h = 16)
      
      swp3 <- sw_sweep(p3, timetk_idx = T, rename_index = "Date") %>% 
        mutate(Mezzi = medium, CountryBP = country, Date=as_date(as.yearmon(Date))) %>% group_by(CountryBP, Mezzi) %>%
        select(CountryBP, Mezzi, Date, key, value)
      final = rbind(final, swp3) 
    
      }
  }
}

final[,5] <- round(final[,5])