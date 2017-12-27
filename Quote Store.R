# Fixed Settings
rm(list=ls())
library(readr)
library(tibble)
library(tidyquant)
forecast <- "MyDirectory"

# Variable Settings
store <- "MyStore"
loc <- paste(forecast,"/", store, sep="")
setwd(loc)


## Reading the previous forecasts at monthly level detailed for Country/Medium.
m_fc <- as.tibble(read_csv(paste(loc, "/", store, "_forecast.csv", sep=""), 
                 col_types = cols(Date = col_date(format = "%Y-%m-%d"))))
m_fc
summary(m_fc)


## Reading the Mobile's quota forecast, monthly at Country Level
mobile <- paste(loc, "/Forecast_Mobile/Forecast_quote_mobile_country.csv", sep="")
fc_quote <- read_delim(mobile, ";", escape_double = FALSE, 
                       col_types = cols(Month = col_date(format = "%Y-%m-%d"),
                                        Quota = col_double()), locale = locale(decimal_mark = ","), 
                       trim_ws = TRUE)
fc_quote
summary(fc_quote)


## Grouping Monthly forecast at Country level and calculating Mobile traffic
m_fc_ctr <- m_fc %>% group_by(CountryBP, Date, Key) %>% summarise(Sessions=sum(Sessions))

temp <- inner_join(m_fc_ctr, fc_quote, by=c("CountryBP"="Country", "Date"="Month", "Key"="key")) %>%
  mutate(Mobile=Sessions*Quota)

mob_traff <- temp %>% ungroup() %>% group_by(Date, Key) %>%
  summarise(Sessions=sum(Sessions), Mobile=sum(Mobile)) %>% mutate(Quote=Mobile/Sessions)

mob_traff

storePlot <- mob_traff %>% ggplot(aes(x=Date, y=Quote, col=Key))+
  geom_line(size=1)+
  labs(title=store)+
  xlab("Month")

storePlot

setwd(paste(loc, "/Forecast_Mobile", sep=""))
png(paste("Quote_",store, ".png", sep=""))
print(storePlot)
dev.off()

write.csv(mob_traff %>% select(Date, Key, Quote), paste(store, "Quote Store.csv", sep="_"), row.names=F)
