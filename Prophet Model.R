Mesi <- input %>% summarise(Mesi=n())

### 1) Without Log transf.
  rename_input <- input %>% rename(ds=Date, y=Sessions)
  nest_input <- rename_input %>% nest()
  nest_input_mesi <- left_join(nest_input, Mesi, by=c("CountryBP", "Mezzi"))
  
  prophet_input <- nest_input_mesi %>% filter(Mesi>4) %>% mutate( pro = map( data, prophet))
  future_input <- prophet_input %>% mutate( future= map(pro, make_future_dataframe, periods=16, freq='m'))
  predict_input <- future_input %>% mutate( pred = map2(pro, future, predict))
  
  fc_input <- predict_input %>% 
    select(CountryBP, Mezzi, pred) %>% 
    unnest() %>% 
    select(CountryBP, Mezzi, ds, NoLog= yhat) 


### Join with Hybrid's output
  join_1 <- left_join(output1 %>% rename(ds=Date), 
                      fc_input %>% filter(ds > "2017-08-01"),
                      by=c("CountryBP", "Mezzi", "ds") ) %>% 
    mutate(NoLog=round(NoLog))



### 2) With Log transf.
  log <- rename_input %>% mutate(y=log(y))
  nest_log <- log %>% nest()
  nest_log_mesi <- left_join(nest_log, Mesi, by=c("CountryBP", "Mezzi"))
  prophet_log <- nest_log_mesi %>% filter(Mesi>4) %>% mutate( pro = map( data, prophet))
  
  future_log <- prophet_log %>% mutate( future = map(pro, make_future_dataframe, periods=16, freq='m'))
  predict_log <- future_log %>% mutate( pred = map2(pro, future, predict))
  
  fc_log <- predict_log %>% 
    select(CountryBP, Mezzi, pred) %>% 
    unnest() %>% 
    select(CountryBP, Mezzi, ds, ConLog=yhat)

### Join with previous results
  join_2 <- left_join(join_1, 
                    fc_log %>% filter(ds > "2017-08-01") %>% mutate(ConLog=round(exp(ConLog))),
                    by=c("CountryBP", "Mezzi", "ds")
                    )


### If forecast <0 => Log correction
  j2_min <- join_2 %>% mutate(min= min(NoLog, na.rm=TRUE))
  j2_cor <- j2_min %>% mutate( Prophet = ifelse( min<0 , ConLog , NoLog)) %>% select(-c(min, NoLog, ConLog))


### Combining multiple outputs -> mean
  # and plot
j2_mean <- j2_cor %>% mutate(Mean=round(rowMeans(data.frame(Prophet, Hybrid), na.rm=TRUE)))

plots3=list()
k=0

for (country in unique(j2_mean$CountryBP)) {

  k=k+1
  reducto <- j2_mean %>% filter(CountryBP==country)
  reducto$Mean[reducto$key=="actual"]<-NA

  plots3[[k]] <- reducto %>%
    ggplot(aes(x=ds, y= Hybrid, col=key)) +
    geom_line(size=1)+
    geom_line(size=1, aes(x=ds, y=Prophet, col="Prophet")) +
    geom_line(size=1, aes(x=ds, y=Mean, col="Mean")) +

    facet_wrap(~ Mezzi, ncol=3, scale = "free_y") +
    expand_limits(y = 0) +
    ylab('Sessions') + xlab("Date") +
    labs(title = paste(store,"-",country))

  print(plots3[[k]])
}