setwd(loc)

plots=list()
j=0

for (country in unique(output$CountryBP)) {

  j=j+1
  reducto <- output %>% filter(CountryBP==country)
  
  plots[[j]] <- reducto %>% 
    
    ggplot(aes(x=Date, y= Sessions, col=key)) +
    geom_line(size=1)+
    
    facet_wrap(~ Mezzi, ncol=3, scale = "free_y") +
    labs(title = paste(store,"-",country)) +
    ylab("Sessions")+
    theme(legend.position="none")
    
  png(paste(store," - ",country,".png", sep=""), width = 789, height = 540)
  print(plots[[j]])
  dev.off()
  
}
