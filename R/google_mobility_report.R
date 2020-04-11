library(RCurl)
library(rlist)
library(lattice)
library(latticeExtra)
library(directlabels)
library(RColorBrewer)
library(readr)
library(dplyr)
library(tidyr)
library(wbstats)
library(data.table)
library(prophet)
library(ggplot2)
library(zoo)

library(showtext)
showtext_auto()
library(myriad)
import_myriad_semi()

#library(plotly)

mytheme <- theme_myriad_semi() +
  theme(plot.title = element_text(size = rel(2), face = "bold"),
        plot.subtitle = element_text(size = rel(1.5)),
        plot.caption = element_text(size = rel(0.9)),
        axis.text.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(2)),
        legend.text = element_text(size = rel(2))
  )


theme_covid <- function() { 
  mytheme
}
theme_set(theme_covid())




setwd("D:/OneDrive/data/covid19/data")

datafound <- FALSE

for (data_iter in rev(seq(as.Date("2020-1-22"), Sys.Date(), by = "days"))) {
  
  
  data_csv <- format(as.Date(data_iter, origin="1970-01-01"),"%Y%m%d.csv")
  
  print(paste0("try reading ",data_csv))
  
  current_url_local = paste0("https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_local_area_trends_G20_",data_csv)
  current_url_national = paste0("https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_national_trends_G20_",data_csv)
  
  
  urlExists = url.exists(current_url_local) & url.exists(current_url_national)
  
  if (urlExists) {
    print(" ... FOUND")
    file_local_mobility_trends <- getURL(current_url_local)#"https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_local_area_trends_G20_20200410.csv")
    data_local_mobility_trends      <- read.csv(text = file_local_mobility_trends)
    
    file_national_mobility_trends <- getURL(current_url_national)#"https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_national_trends_G20_20200410.csv")
    data_national_mobility_trends      <- read.csv(text = file_national_mobility_trends)
    datafound <- TRUE
    break
    
  } else {
    print(" ... not found")
  }
}

if(datafound) {

  for (country in c('IT','ES','FR','DE','GB','US')) {
  
  
    pdf(
      file = sprintf("Mobility graphs %s %s.pdf", country, format(as.Date(data_iter, origin="1970-01-01"),"%Y-%m-%d")),
      paper = "a4r",
      width = 14,
      height = 14
    ) # apro il device
    
    d <- data_national_mobility_trends %>% filter(Country==country) %>% pivot_longer(-c("Country","location","category"), names_to = "Xdate", values_to = "deltap") %>% mutate(date = as.Date(Xdate,"X%Y.%m.%d")) %>% select (date,location,category,deltap)
    
    p <- ggplot(data = d, aes(x = date, y = deltap)) + 
      geom_line(color = "#00AFBB", size = 0.5) +
      geom_point(color="steelblue")+
      facet_wrap(~category)+ 
      labs(x = "Days", 
           y = "mobility % change", 
           title = "National mobility report", 
           subtitle = country, 
           caption = "Enrico Papalini @popoloni / Data: Google") + 
      geom_hline(yintercept=0,linetype="dashed",size=0.1, color="black") +
      theme_myriad_semi() +
      theme(plot.title = element_text(size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(0.8)),
            plot.caption = element_text(size = rel(1)),
            # turn off the strip label and tighten the panel spacing
            #strip.text = element_blank(),
            strip.text.x = element_text(size = rel(0.75), face = "bold"),
            panel.spacing.x = unit(-0.05, "lines"),
            panel.spacing.y = unit(0.3, "lines"),
            axis.text.y = element_text(size = rel(0.75)),
            axis.title.x = element_text(size = rel(1)),
            axis.title.y = element_text(size = rel(1)),
            axis.text.x = element_text(size = rel(0.75)),
            legend.text = element_text(size = rel(1)))
    
    print(p)
    
    p <- ggplot(data = d, aes(x = date, y = deltap, color=category)) + 
      geom_line(size = 0.5) +
      geom_point()+ 
      labs(x = "Days", 
           y = "mobility % change", 
           title = "National mobility report", 
           subtitle = country, 
           caption = "Enrico Papalini @popoloni / Data: Google") + 
      geom_hline(yintercept=0,linetype="dashed",size=0.1, color="black") +
      theme_myriad_semi() +
      theme(plot.title = element_text(size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(0.8)),
            plot.caption = element_text(size = rel(1)),
            # turn off the strip label and tighten the panel spacing
            #strip.text = element_blank(),
            strip.text.x = element_text(size = rel(0.75), face = "bold"),
            panel.spacing.x = unit(-0.05, "lines"),
            panel.spacing.y = unit(0.3, "lines"),
            axis.text.y = element_text(size = rel(0.75)),
            axis.title.x = element_text(size = rel(1)),
            axis.title.y = element_text(size = rel(1)),
            axis.text.x = element_text(size = rel(0.75)),
            legend.text = element_text(size = rel(1)))  
    print(p)
    
    
    if (!country=='GB'){
    dl <- data_local_mobility_trends%>% filter(Country==country) %>% pivot_longer(-c("Country","location","category"), names_to = "Xdate", values_to = "deltap") %>% mutate(date = as.Date(Xdate,"X%Y.%m.%d")) %>% select (date,location,category,deltap)
    
    p <- ggplot(data = dl, aes(x = date, y = deltap, color=location)) + 
      geom_line(size = 0.1) +
      geom_point(size = 0.5)+
      facet_wrap(~category)+ 
      labs(x = "Days", 
           y = "mobility % change", 
           title = "National mobility report", 
           subtitle = country, 
           caption = "Enrico Papalini @popoloni / Data: Google") + 
      geom_hline(yintercept=0,linetype="dashed",size=0.1, color="black") +
      theme_myriad_semi() +
      theme(plot.title = element_text(size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(0.8)),
            plot.caption = element_text(size = rel(1)),
            # turn off the strip label and tighten the panel spacing
            #strip.text = element_blank(),
            strip.text.x = element_text(size = rel(0.75), face = "bold"),
            panel.spacing.x = unit(-0.05, "lines"),
            panel.spacing.y = unit(0.3, "lines"),
            axis.text.y = element_text(size = rel(0.75)),
            axis.title.x = element_text(size = rel(1)),
            axis.title.y = element_text(size = rel(1)),
            axis.text.x = element_text(size = rel(0.75)),
            legend.text = element_text(size = rel(1)))  
    print(p)
    
    p <- ggplot(data = dl, aes(x = date, y = deltap, color=category)) + 
      geom_line(size = 0.1) +
      geom_point(size = 0.5)+
      facet_wrap(~location)+ 
      labs(x = "Days", 
           y = "mobility % change", 
           title = "National mobility report", 
           subtitle = country, 
           caption = "Enrico Papalini @popoloni / Data: Google") + 
      geom_hline(yintercept=0,linetype="dashed",size=0.1, color="black") +
      theme_myriad_semi() +
      theme(plot.title = element_text(size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(0.8)),
            plot.caption = element_text(size = rel(1)),
            # turn off the strip label and tighten the panel spacing
            #strip.text = element_blank(),
            strip.text.x = element_text(size = rel(0.75), face = "bold"),
            panel.spacing.x = unit(-0.05, "lines"),
            panel.spacing.y = unit(0.3, "lines"),
            axis.text.y = element_text(size = rel(0.75)),
            axis.title.x = element_text(size = rel(1)),
            axis.title.y = element_text(size = rel(1)),
            axis.text.x = element_text(size = rel(0.75)),
            legend.text = element_text(size = rel(1)))  
    print(p)
    }
    
    dev.off() # lo chiudo
  }
}


#t <- data_national_mobility_trends %>% filter(Country=='IT') %>% ungroup() %>% pivot_longer(-c("Country","location","category"), names_to = "Xdate", values_to = "deltap") %>% mutate(date = as.Date(Xdate,"X%Y.%m.%d")) %>% select (date,location,category,deltap)

