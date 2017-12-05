### Geo Mapping
  head(geo)
  lettura_geo <- left_join(lettura, geo, by = c("Country" = "CountryGA"))
  
  # Replacing NA with default 'OT non EU'
  lettura_geo$CountryBP[which(is.na(lettura_geo$CountryBP))] <- 'OT non EU'


### Channel Mapping
  medium <- read.csv(paste(loc, "/channelmapping.csv",sep=""), stringsAsFactors = FALSE, sep=";")
  lettura_geo_medium <- left_join(lettura_geo, medium, by=c("Medium" = "mediumGA")) %>% rename("Mezzi"="mezzi")
  
  # Replacing NA with default 'Others'
  lettura_geo_medium$Mezzi[which(is.na(lettura_geo_medium$Mezzi))] <- 'Others'


### Removing Device detail & Grouping by Month
  input <- lettura_geo_medium %>%
    group_by(CountryBP, Mezzi,Date= as_date(as.yearmon(Date)) ) %>%
    summarise(Sessions=sum(Sessions))
