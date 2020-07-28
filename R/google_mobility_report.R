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
library(viridisLite)
library(ggrepel)
library(directlabels)
#library(gghighlight)

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

datafound_google <- FALSE
datafound_apple  <- FALSE
date_google <- as.Date("2020-1-22")
data_apple <- as.Date("2020-1-22")



current_url_google="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
urlExists_google = url.exists(current_url_google)

#data_google_mobility_trends<-read.table(current_url_google, header = TRUE, sep = ",",stringsAsFactors = FALSE) 

if (urlExists_google & !datafound_google) {
  print(" ... FOUND GOOGLE")
  #date_google <- data_iter
  
  file_google_mobility_trends <- getURL(current_url_google)#"https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_local_area_trends_G20_20200410.csv")
  data_google_mobility_trends      <- read.csv(text = file_google_mobility_trends, stringsAsFactors=FALSE)
  date_google <- as.Date(max(data_google_mobility_trends$date,na.rm=TRUE))
  datafound_google <- TRUE
  
  data_national_mobility_trends <- data_google_mobility_trends %>% filter(sub_region_1=='')
  data_local_mobility_trends <- data_google_mobility_trends %>% filter(!sub_region_1=='' &sub_region_2=='')
  
  
} else {
  if(!datafound_google)
    print(" ... Google not found yet")
}


for (data_iter in rev(seq(as.Date("2020-1-22"), Sys.Date(), by = "days"))) {
  
  
  #data_csv_google <- format(as.Date(data_iter, origin="1970-01-01"),"%Y%m%d.csv")
  data_csv_apple <- format(as.Date(data_iter, origin="1970-01-01"),"-%Y-%m-%d.csv")
  number_apple <- as.numeric(format(as.Date(data_iter, origin="1970-01-01"),"%d"))-17
  
  #print(paste0("try reading ",data_csv_apple))
  
  
  #current_url_local = paste0("https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_local_area_trends_G20_",data_csv_google)
  #current_url_national = paste0("https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_national_trends_G20_",data_csv_google)
  #current_url_apple = paste0("https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev7/v1/en-us/applemobilitytrends",data_csv_apple)
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2005HotfixDev13/v1/en-us/applemobilitytrends-2020-04-13.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev6/v1/en-us/applemobilitytrends-2020-04-15.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev7/v1/en-us/applemobilitytrends-2020-04-16.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev9/v1/en-us/applemobilitytrends-2020-04-18.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev16/v1/en-us/applemobilitytrends-2020-04-24.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev20/v2/en-us/applemobilitytrends-2020-04-27.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2007HotfixDev42/v2/en-us/applemobilitytrends-2020-04-28.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2007HotfixDev53/v2/en-us/applemobilitytrends-2020-05-08.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2007HotfixDev58/v2/en-us/applemobilitytrends-2020-05-12.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2008HotfixDev26/v2/en-us/applemobilitytrends-2020-05-13.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2008HotfixDev35/v3/en-us/applemobilitytrends-2020-05-18.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2008HotfixDev40/v3/en-us/applemobilitytrends-2020-05-22.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2009HotfixDev11/v3/en-us/applemobilitytrends-2020-05-27.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2009HotfixDev16/v3/en-us/applemobilitytrends-2020-06-01.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2009HotfixDev23/v3/en-us/applemobilitytrends-2020-06-06.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2009HotfixDev27/v3/en-us/applemobilitytrends-2020-06-08.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2010HotfixDev14/v3/en-us/applemobilitytrends-2020-06-10.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2010HotfixDev24/v3/en-us/applemobilitytrends-2020-06-19.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2011HotfixDev14/v3/en-us/applemobilitytrends-2020-07-04.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2012HotfixDev14/v3/en-us/applemobilitytrends-2020-07-15.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2012HotfixDev18/v3/en-us/applemobilitytrends-2020-07-18.csv
  #https://covid19-static.cdn-apple.com/covid19-mobility-data/2013HotfixDev8/v3/en-us/applemobilitytrends-2020-07-25.csv
  
  current_url_apple = sprintf("https://covid19-static.cdn-apple.com/covid19-mobility-data/2013HotfixDev%s/v3/en-us/applemobilitytrends%s",number_apple,data_csv_apple)
  
  #if(!datafound_google) {
  #  urlExists_google = url.exists(current_url_local) & url.exists(current_url_national)
  #}
  
  print(current_url_apple)
  
  mydata<-NULL
  
  if(!datafound_apple){
    tryCatch(
      {
        mydata<-read.table(current_url_apple, header = TRUE, sep = ",",stringsAsFactors = FALSE, fill=TRUE, blank.lines.skip=TRUE,encoding="UTF-8",skipNul=TRUE) 
      },
      error=function(cond) {
        message(sprintf("error %s:",current_url_apple))
      },
      #warning=function(cond) {
      #  message(sprintf("warning:",current_url_apple))
      #},
      finally={
      }
    )   
  }
  # if (urlExists_google & !datafound_google) {
  #   print(" ... FOUND GOOGLE")
  #   date_google <- data_iter
  #   file_local_mobility_trends <- getURL(current_url_local)#"https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_local_area_trends_G20_20200410.csv")
  #   data_local_mobility_trends      <- read.csv(text = file_local_mobility_trends)
  #   
  #   file_national_mobility_trends <- getURL(current_url_national)#"https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_national_trends_G20_20200410.csv")
  #   data_national_mobility_trends      <- read.csv(text = file_national_mobility_trends)
  #   datafound_google <- TRUE
  # } else {
  #   if(!datafound_google)
  #     print(" ... Google not found yet")
  # }
  
  if (!is.null(mydata) & !datafound_apple) {
    print(" ... FOUND APPLE")
    date_apple <- as.Date(data_iter, origin="1970-01-01")
    #file_apple_mobility_trends <- getURL(current_url_apple)#"https://covid19-static.cdn-apple.com/covid19-mobility-data/2005HotfixDev13/v1/en-us/applemobilitytrends-2020-04-13.csv")
    data_apple_mobility_trends      <- mydata
    datafound_apple <- TRUE
  } else {
    if(!datafound_apple)
      sprintf("%s %s",current_url_apple," ... Apple not found yet")
  } 
  
  if(datafound_apple) {#if(datafound_google & datafound_apple) {
    break
  }
  
}


if(datafound_google & datafound_apple) {
  
  write.csv(data_national_mobility_trends,sprintf("google_national_mobility_trends_df_%s.csv", format(as.Date(date_google, origin="1970-01-01"),"%Y-%m-%d")), row.names = FALSE)
  write.csv(data_local_mobility_trends,sprintf("google_local_mobility_trends_df_%s.csv", format(as.Date(date_google, origin="1970-01-01"),"%Y-%m-%d")), row.names = FALSE)
  write.csv(data_apple_mobility_trends,sprintf("apple_mobility_trends_df_%s.csv", format(as.Date(date_apple, origin="1970-01-01"),"%Y-%m-%d")), row.names = FALSE)

  
  #data_apple_mobility_trends$geo_type = data_apple_mobility_trends$X.U.FEFF.geo_type  
  #data_apple_mobility_trends <- data_apple_mobility_trends %>% select (-c('X.U.FEFF.geo_type','alternative_name','X2020.01.13')) %>% drop_na()  
  str(data_apple_mobility_trends)
  
  da <- data_apple_mobility_trends %>% select (-c('alternative_name','sub.region','country','X2020.01.13'))  %>% filter(geo_type=='country/region') %>% pivot_longer(-c("geo_type","region","transportation_type"), names_to = "Xdate", values_to = "deltap") %>% mutate(date = as.Date(Xdate,"X%Y.%m.%d")) %>% mutate(location=region, category=transportation_type, deltap=deltap-100) %>% select (date,location,category,deltap)

  dt <- data_apple_mobility_trends %>% select (-c('alternative_name','sub.region','country','X2020.01.13')) %>% filter(geo_type=='city') %>% pivot_longer(-c("geo_type","region","transportation_type"), names_to = "Xdate", values_to = "deltap") %>% mutate(date = as.Date(Xdate,"X%Y.%m.%d")) %>% mutate(location=region, category=transportation_type, deltap=deltap-100) %>% select (date,location,category,deltap)
  

  
  dg <- data_google_mobility_trends %>% select (-c("iso_3166_2_code", "census_fips_code")) %>% filter(sub_region_1=='') %>% select(-c("sub_region_1","sub_region_2","country_region_code")) %>% pivot_longer(-c("country_region","date"), names_to = "mytype", values_to = "deltap") %>% mutate(location=country_region, category=gsub("_percent_change_from_baseline","",mytype),date = as.Date(date,"%Y-%m-%d")) %>% select (date,location,category,deltap)
  
  #dgl <- data_google_mobility_trends %>% filter(!sub_region_1=='' &sub_region_2=='') %>% select(-c("sub_region_2","country_region_code")) %>% pivot_longer(-c("country_region","sub_region_1","date"), names_to = "mytype", values_to = "deltap") %>% mutate(country=country_region, location=sub_region_1, category=gsub("_percent_change_from_baseline","",mytype),date = as.Date(date,"%Y-%m-%d")) %>% select (date,country,location,category,deltap)
  
  da <- da %>%  mutate (location = case_when(location == 'Korea, Republic of' ~ "South Korea", 
                                             location == 'Taiwan, Province of China'~ "Taiwan", 
                                             location == 'Gambia, The'~ "Gambia", 
                                             location == 'United States'~ "US", 
                                             location == 'Viet Nam'~ "Vietnam",
                                             location == "Côte d'Ivoire"~ "Ivory Coast",
                                             location == 'Russian Federation'~ "Russia",
                                             location == 'United Kingdom'~ "UK",
                                             location == 'Iran, Islamic Republic of'~ "Iran",
                                             location == 'Macedonia, the former Yugoslav Republic of'~ "North Macedonia",
                                             location == 'Palestine, State of'~ "Palestine",
                                             location == 'Syrian Arab Republic'~ "Syria",
                                             location == "Moldova, Republic of"~ "Moldova",
                                             location == "Brunei Darussalam"~ "Brunei",
                                             location == "Bolivia, Plurinational State of" ~ "Bolivia",
                                             location == "Saint Barthélemy"~ "Saint Barthelemy",
                                             location == "Moldova, Republic of"~ "Moldova",
                                             location == "Saint Martin (French part)"~ "Saint Martin",
                                             location == "Holy See (Vatican City State)"~ "Vatican",
                                             location == "Congo, the Democratic Republic of the"~ "Congo (Kinshasa)",
                                             location == "Congo" ~ "Congo (Brazzaville)",
                                             location == "Venezuela, Bolivarian Republic of" ~ "Venezuela",
                                             location == "Tanzania, United Republic of" ~ "Tanzania",
                                             location == "Lao People's Democratic Republic" ~ "Laos",
                                             location == "Timor-Leste" ~ "East Timor",
                                             location == "Cabo Verde" ~ "Cape Verde",
                                             TRUE  ~ location))
  
  dg<- dg %>%  mutate (location = case_when(location == 'Korea, Republic of' ~ "South Korea", 
                                            location == 'Taiwan, Province of China'~ "Taiwan", 
                                            location == 'Gambia, The'~ "Gambia", 
                                            location == 'United States'~ "US", 
                                            location == 'Viet Nam'~ "Vietnam",
                                            location == "Côte d'Ivoire"~ "Ivory Coast",
                                            location == 'Russian Federation'~ "Russia",
                                            location == 'United Kingdom'~ "UK",
                                            location == 'Iran, Islamic Republic of'~ "Iran",
                                            location == 'Macedonia, the former Yugoslav Republic of'~ "North Macedonia",
                                            location == 'Palestine, State of'~ "Palestine",
                                            location == 'Syrian Arab Republic'~ "Syria",
                                            location == "Moldova, Republic of"~ "Moldova",
                                            location == "Brunei Darussalam"~ "Brunei",
                                            location == "Bolivia, Plurinational State of" ~ "Bolivia",
                                            location == "Saint Barthélemy"~ "Saint Barthelemy",
                                            location == "Moldova, Republic of"~ "Moldova",
                                            location == "Saint Martin (French part)"~ "Saint Martin",
                                            location == "Holy See (Vatican City State)"~ "Vatican",
                                            location == "Congo, the Democratic Republic of the"~ "Congo (Kinshasa)",
                                            location == "Congo" ~ "Congo (Brazzaville)",
                                            location == "Venezuela, Bolivarian Republic of" ~ "Venezuela",
                                            location == "Tanzania, United Republic of" ~ "Tanzania",
                                            location == "Lao People's Democratic Republic" ~ "Laos",
                                            location == "Timor-Leste" ~ "East Timor",
                                            location == "Cabo Verde" ~ "Cape Verde",
                                            TRUE  ~ location))
  
  dc <- da %>% dplyr::union(dg) %>% arrange(date,location,category)
   
  label_date <- min(max(da$date),max(dg$date),max(dt$date))
  
  locations <-  dc %>% distinct(location) %>% arrange(location) #%>% filter(location %in% c('Italy','Spain','France','Germany','UK','US'))
  towns     <- dt %>% distinct(location) %>% arrange(location)
  
  countrylist = c('Italy','Spain','France','Germany','UK','US')
  
  for (country in countrylist) {
    
    if (dim(dc  %>% filter(location==country))[1] > 0){
    pdf(
      file = sprintf("Mobility graphs %s %s.pdf", country, format(as.Date(max(date_apple,date_google), origin="1970-01-01"),"%Y-%m-%d")),
      paper = "a4r",
      width = 14,
      height = 14
    ) # apro il device
    
    
    d <- dc  %>% filter(location==country)
    
    p <- d %>% 
      ggplot(mapping=aes(x = date, y = deltap))+ 
      geom_line(color = "#00AFBB", size = 0.5) +
      geom_point(color="steelblue") + 
      guides(color = FALSE) +
      facet_wrap(~category)+ 
      labs(x = "Days", 
           y = "mobility % change", 
           title = "National mobility report", 
           subtitle = country, 
           caption = "Enrico Papalini @popoloni / Data: Apple & Google") + 
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
    
    
    
    p <- d %>%
      ggplot(mapping=aes(x = date, y = deltap, color=category)) + 
      geom_line(size = 0.5) +
      geom_point() +
      guides(color = FALSE) + 
      labs(x = "Days", 
           y = "mobility % change", 
           title = "National mobility report", 
           subtitle = country, 
           caption = "Enrico Papalini @popoloni / Data: Apple & Google") + 
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
            legend.text = element_text(size = rel(1))) + scale_colour_viridis_d() +
      geom_dl(aes(label = category), method = list("last.points", cex = 0.8, rot=30))
    print(p)
    
    d <- dc  %>% filter( location %in% countrylist) %>% 
      mutate(mycolor = ifelse(location==country,"blue","lightgrey"), 
             mylabel = ifelse(location==country,country,""))
    
    p <- d %>%
      ggplot(mapping=aes(x = date, y = deltap, color=mycolor)) + 
      geom_line(size = 0.5) +
      #geom_point() +
      guides(color = FALSE) + 
      facet_wrap(~category)+
      labs(x = "Days", 
           y = "mobility % change", 
           title = "National mobility report", 
           subtitle = sprintf("%s vs others",country), 
           caption = "Enrico Papalini @popoloni / Data: Apple & Google") + 
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
            legend.text = element_text(size = rel(1))) + 
      geom_dl(aes(label = mylabel), method = list("last.points", cex = 0.8, rot=30)) 
      
    print(p)
    
    dev.off() # lo chiudo
    }
  }
  
  townlist = c('Milan','Rome','Madrid','Paris','Berlin','London')
  
  for (town in townlist) {
    
    if (dim(dt  %>% filter(location==town))[1] > 0) {
    pdf(
      file = sprintf("Mobility graphs %s %s.pdf", town, format(as.Date(max(date_apple,date_google), origin="1970-01-01"),"%Y-%m-%d")),
      paper = "a4r",
      width = 14,
      height = 14
    ) # apro il device
    
    
    
    d <- dt  %>% filter(location==town)
    
    p <- d %>% 
      ggplot(mapping=aes(x = date, y = deltap))+ 
      geom_line(color = "#00AFBB", size = 0.5) +
      geom_point(color="steelblue") + 
      guides(color = FALSE) +
      facet_wrap(~category)+ 
      labs(x = "Days", 
           y = "mobility % change", 
           title = "Town mobility report", 
           subtitle = town, 
           caption = "Enrico Papalini @popoloni / Data: Apple") + 
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
    
    
    
    p <- d %>%
      ggplot(mapping=aes(x = date, y = deltap, color=category)) + 
      geom_line(size = 0.5) +
      geom_point() +
      guides(color = FALSE) + 
      labs(x = "Days", 
           y = "mobility % change", 
           title = "Town mobility report", 
           subtitle = town, 
           caption = "Enrico Papalini @popoloni / Data: Apple") + 
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
            legend.text = element_text(size = rel(1))) + scale_colour_viridis_d() +
      geom_dl(aes(label = category), method = list("last.points", cex = 0.8, rot=30))
    print(p)
    
    d <- dt  %>% filter( location %in% townlist) %>% 
      mutate(mycolor = ifelse(location==town,"blue","lightgrey"), 
             mylabel = ifelse(location==town,town,""))
    
    p <- d %>%
      ggplot(mapping=aes(x = date, y = deltap, color=mycolor)) + 
      geom_line(size = 0.5) +
      #geom_point() +
      guides(color = FALSE) + 
      facet_wrap(~category)+
      labs(x = "Days", 
           y = "mobility % change", 
           title = "Town mobility report", 
           subtitle = sprintf("%s vs others",town), 
           caption = "Enrico Papalini @popoloni / Data: Apple") + 
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
            legend.text = element_text(size = rel(1))) + 
      geom_dl(aes(label = mylabel), method = list("last.points", cex = 0.8, rot=30)) 
    
    print(p)
    
    dev.off() # lo chiudo
    }
  }
}




# if(FALSE & datafound_google) {
# 
#   
#   for (country in c('Italy','Spain','France','Germany','UK','US')) {
#     
#     
#     pdf(
#       file = sprintf("Mobility graphs %s %s.pdf", country, format(as.Date(date_google, origin="1970-01-01"),"%Y-%m-%d")),
#       paper = "a4r",
#       width = 14,
#       height = 14
#     ) # apro il device
#     
#     d <- dg %>% filter(location==country)
#     
#     p <- ggplot(data = d, aes(x = date, y = deltap)) + 
#       geom_line(color = "#00AFBB", size = 0.5) +
#       geom_point(color="steelblue")+
#       facet_wrap(~category)+ 
#       labs(x = "Days", 
#            y = "mobility % change", 
#            title = "National mobility report", 
#            subtitle = country, 
#            caption = "Enrico Papalini @popoloni / Data: Google") + 
#       geom_hline(yintercept=0,linetype="dashed",size=0.1, color="black") +
#       theme_myriad_semi() +
#       theme(plot.title = element_text(size = rel(1), face = "bold"),
#             plot.subtitle = element_text(size = rel(0.8)),
#             plot.caption = element_text(size = rel(1)),
#             # turn off the strip label and tighten the panel spacing
#             #strip.text = element_blank(),
#             strip.text.x = element_text(size = rel(0.75), face = "bold"),
#             panel.spacing.x = unit(-0.05, "lines"),
#             panel.spacing.y = unit(0.3, "lines"),
#             axis.text.y = element_text(size = rel(0.75)),
#             axis.title.x = element_text(size = rel(1)),
#             axis.title.y = element_text(size = rel(1)),
#             axis.text.x = element_text(size = rel(0.75)),
#             legend.text = element_text(size = rel(1)))
#     
#     print(p)
#     
#     p <- ggplot(data = d, aes(x = date, y = deltap, color=category)) + 
#       geom_line(size = 0.5) +
#       geom_point()+ 
#       labs(x = "Days", 
#            y = "mobility % change", 
#            title = "National mobility report", 
#            subtitle = country, 
#            caption = "Enrico Papalini @popoloni / Data: Google") + 
#       geom_hline(yintercept=0,linetype="dashed",size=0.1, color="black") +
#       theme_myriad_semi() +
#       theme(plot.title = element_text(size = rel(1), face = "bold"),
#             plot.subtitle = element_text(size = rel(0.8)),
#             plot.caption = element_text(size = rel(1)),
#             # turn off the strip label and tighten the panel spacing
#             #strip.text = element_blank(),
#             strip.text.x = element_text(size = rel(0.75), face = "bold"),
#             panel.spacing.x = unit(-0.05, "lines"),
#             panel.spacing.y = unit(0.3, "lines"),
#             axis.text.y = element_text(size = rel(0.75)),
#             axis.title.x = element_text(size = rel(1)),
#             axis.title.y = element_text(size = rel(1)),
#             axis.text.x = element_text(size = rel(0.75)),
#             legend.text = element_text(size = rel(1)))  
#     print(p)
#     
#     
#     if (FALSE){
#       dl <- dgl %>% filter(country==country) 
#       
#       p <- ggplot(data = dl, aes(x = date, y = deltap, color=location)) + 
#         geom_line(size = 0.1) +
#         geom_point(size = 0.5)+
#         facet_wrap(~category)+ 
#         labs(x = "Days", 
#              y = "mobility % change", 
#              title = "National mobility report", 
#              subtitle = country, 
#              caption = "Enrico Papalini @popoloni / Data: Google") + 
#         geom_hline(yintercept=0,linetype="dashed",size=0.1, color="black") +
#         theme_myriad_semi() +
#         theme(plot.title = element_text(size = rel(1), face = "bold"),
#               plot.subtitle = element_text(size = rel(0.8)),
#               plot.caption = element_text(size = rel(1)),
#               # turn off the strip label and tighten the panel spacing
#               #strip.text = element_blank(),
#               strip.text.x = element_text(size = rel(0.75), face = "bold"),
#               panel.spacing.x = unit(-0.05, "lines"),
#               panel.spacing.y = unit(0.3, "lines"),
#               axis.text.y = element_text(size = rel(0.75)),
#               axis.title.x = element_text(size = rel(1)),
#               axis.title.y = element_text(size = rel(1)),
#               axis.text.x = element_text(size = rel(0.75)),
#               legend.text = element_text(size = rel(1)))  
#       print(p)
#       
#       p <- ggplot(data = dl, aes(x = date, y = deltap, color=category)) + 
#         geom_line(size = 0.1) +
#         geom_point(size = 0.5)+
#         facet_wrap(~location)+ 
#         labs(x = "Days", 
#              y = "mobility % change", 
#              title = "National mobility report", 
#              subtitle = country, 
#              caption = "Enrico Papalini @popoloni / Data: Google") + 
#         geom_hline(yintercept=0,linetype="dashed",size=0.1, color="black") +
#         theme_myriad_semi() +
#         theme(plot.title = element_text(size = rel(1), face = "bold"),
#               plot.subtitle = element_text(size = rel(0.8)),
#               plot.caption = element_text(size = rel(1)),
#               # turn off the strip label and tighten the panel spacing
#               #strip.text = element_blank(),
#               strip.text.x = element_text(size = rel(0.75), face = "bold"),
#               panel.spacing.x = unit(-0.05, "lines"),
#               panel.spacing.y = unit(0.3, "lines"),
#               axis.text.y = element_text(size = rel(0.75)),
#               axis.title.x = element_text(size = rel(1)),
#               axis.title.y = element_text(size = rel(1)),
#               axis.text.x = element_text(size = rel(0.75)),
#               legend.text = element_text(size = rel(1)))  
#       print(p)
#     }
#     
#     dev.off() # lo chiudo
#   }
# }


#t <- data_national_mobility_trends %>% filter(Country=='IT') %>% ungroup() %>% pivot_longer(-c("Country","location","category"), names_to = "Xdate", values_to = "deltap") %>% mutate(date = as.Date(Xdate,"X%Y.%m.%d")) %>% select (date,location,category,deltap)


#l <- dt %>% distinct(location)


