library(RCurl)
library(rlist)
library(lattice)
library(directlabels)
library(RColorBrewer)
library(readr)
library(dplyr)
library(tidyr)
library(wbstats)
library(data.table)

setwd("D:/OneDrive/data/covid19/data")

pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2019)


iso3_cnames <- read_csv("countries_iso3.csv")
iso2_to_iso3 <- read_csv("iso2_to_iso3.csv")

cname_table <- left_join(iso3_cnames, iso2_to_iso3)

eu <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
        "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
        "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")

europe <- c("ALB", "AND", "AUT", "BLR", "BEL", "BIH", "BGR", "HRV", "CYP", "CZE",
            "DNK", "EST", "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "HUN", "ISL",
            "IRL", "ITA", "LVA", "LIE", "LTU", "LUX", "MKD", "MLT", "MDA", "MCO",
            "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SMR", "SRB", "SVK", "SVN",
            "ESP", "SWE", "CHE", "UKR", "GBR", "VAT", "RSB", "IMN", "MNE")

north_america <- c("AIA", "ATG", "ABW", "BHS", "BRB", "BLZ", "BMU", "VGB", "CAN", "CYM",
                   "CRI", "CUB", "CUW", "DMA", "DOM", "SLV", "GRL", "GRD", "GLP", "GTM",
                   "HTI", "HND", "JAM", "MTQ", "MEX", "SPM", "MSR", "ANT", "KNA", "NIC",
                   "PAN", "PRI", "KNA", "LCA", "SPM", "VCT", "TTO", "TCA", "VIR", "USA",
                   "SXM")

south_america <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "FLK", "GUF", "GUY", "PRY",
                   "PER", "SUR", "URY", "VEN")


africa <- c("DZA", "AGO", "SHN", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF",
            "TCD", "COM", "COG", "DJI", "EGY", "GNQ", "ERI", "ETH", "GAB", "GMB",
            "GHA", "GNB", "GIN", "CIV", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI",
            "MLI", "MRT", "MUS", "MYT", "MAR", "MOZ", "NAM", "NER", "NGA", "STP",
            "REU", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SHN", "SDN",
            "SWZ", "TZA", "TGO", "TUN", "UGA", "COD", "ZMB", "TZA", "ZWE", "SSD",
            "COD")

asia <- c("AFG", "ARM", "AZE", "BHR", "BGD", "BTN", "BRN", "KHM", "CHN", "CXR",
          "CCK", "IOT", "GEO", "HKG", "IND", "IDN", "IRN", "IRQ", "ISR", "JPN",
          "JOR", "KAZ", "PRK", "KOR", "KWT", "KGZ", "LAO", "LBN", "MAC", "MYS",
          "MDV", "MNG", "MMR", "NPL", "OMN", "PAK", "PHL", "QAT", "SAU", "SGP",
          "LKA", "SYR", "TWN", "TJK", "THA", "TUR", "TKM", "ARE", "UZB", "VNM",
          "YEM", "PSE")

oceania <- c("ASM", "AUS", "NZL", "COK", "FJI", "PYF", "GUM", "KIR", "MNP", "MHL",
             "FSM", "UMI", "NRU", "NCL", "NZL", "NIU", "NFK", "PLW", "PNG", "MNP",
             "SLB", "TKL", "TON", "TUV", "VUT", "UMI", "WLF", "WSM", "TLS")


cname_table <- cname_table %>%
  mutate (country = case_when(cname == 'Korea, Republic of' ~ "South Korea", 
                              cname == 'Taiwan, Province of China'~ "Taiwan", 
                              cname == 'Gambia, The'~ "Gambia", 
                              cname == 'United States'~ "US", 
                              cname == 'Viet Nam'~ "Vietnam",
                              cname == "Côte d'Ivoire"~ "Ivory Coast",
                              cname == 'Russian Federation'~ "Russia",
                              cname == 'United Kingdom'~ "UK",
                              cname == 'Iran, Islamic Republic of'~ "Iran",
                              cname == 'Macedonia, the former Yugoslav Republic of'~ "North Macedonia",
                              cname == 'Palestine, State of'~ "Palestine",
                              cname == 'Syrian Arab Republic'~ "Syria",
                              cname == "Moldova, Republic of"~ "Moldova",
                              cname == "Brunei Darussalam"~ "Brunei",
                              cname == "Bolivia, Plurinational State of" ~ "Bolivia",
                              cname == "Saint Barthélemy"~ "Saint Barthelemy",
                              cname == "Moldova, Republic of"~ "Moldova",
                              cname == "Saint Martin (French part)"~ "Saint Martin",
                              cname == "Holy See (Vatican City State)"~ "Vatican",
                              cname == "Congo, the Democratic Republic of the"~ "Congo (Kinshasa)",
                              cname == "Congo" ~ "Congo (Brazzaville)",
                              cname == "Venezuela, Bolivarian Republic of" ~ "Venezuela",
                              cname == "Tanzania, United Republic of" ~ "Tanzania",
                              cname == "Lao People's Democratic Republic" ~ "Laos",
                              cname == "Timor-Leste" ~ "East Timor",
                              cname == "Cabo Verde" ~ "Cape Verde",
                              TRUE  ~ cname))




lw <- list()
li <- list()

data_csv <- ""
data_file<-NULL

for (data_iter in seq(as.Date("2020-1-22"), Sys.Date(), by = "days")) {
  
  data_csv <- format(as.Date(data_iter, origin="1970-01-01"),"%m-%d-%Y.csv")
  
  print(paste0("reading ",data_csv))
  
  current_url = paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",data_csv)
  
  urlExists = url.exists(current_url)
  
  # Regular HTTP
  if(!urlExists)
    break
  
  data_file <- getURL(current_url)
  
  data_daily <- read.csv(text = data_file,stringsAsFactors=FALSE)
  data_daily[is.na(data_daily)] <- 0
  data_daily[is.null(data_daily)] <- "-"
  data_daily$date <- as.Date(data_iter, origin="1970-01-01")
  
  names(data_daily)[names(data_daily) == "Province_State"] <- "region"
  names(data_daily)[names(data_daily) == "Province.State"] <- "region"
  names(data_daily)[names(data_daily) == "X.U.FEFF.Province.State"] <- "region"
  names(data_daily)[names(data_daily) == "Country_Region"] <- "country"
  names(data_daily)[names(data_daily) == "Country.Region"] <- "country"
  names(data_daily)[names(data_daily) == "Last_Update"] <- "last_update"
  names(data_daily)[names(data_daily) == "Last.Update"] <- "last_update"
  names(data_daily)[names(data_daily) == "Confirmed"] <- "confirmed"
  names(data_daily)[names(data_daily) == "Deaths"] <- "death"
  names(data_daily)[names(data_daily) == "Recovered"] <- "recovered"
  
  data_daily <- data_daily %>%
    mutate (region= case_when(country=="Diamond Princess"|country=="Cruise Ship"~ "Ship", TRUE  ~ region)) %>%
    mutate (country = case_when(country == 'Mainland China' ~ "China", 
                                country == 'Bahamas, The'~ "Bahamas", 
                                country == 'Gambia, The'~ "Gambia", 
                                country == 'Korea, South'|country == 'Republic of Korea'~ "South Korea", 
                                country == 'occupied Palestinian territory'~ "Palestine",
                                country == 'United Kingdom'~ "UK",
                                country == 'Iran (Islamic Republic of)'~ "Iran",
                                country == 'St. Martin'~ "Saint Martin",
                                country == 'Czechia'~ "Czech Republic",
                                country == 'Republic of Ireland'~ "Ireland",
                                country == 'Russian Federation'~ "Russia",
                                country == 'Republic of Moldova'~ "Moldova",
                                country == 'Republic of the Congo'~ "Congo (Brazzaville)",
                                country == 'Viet Nam'~ "Vietnam",
                                country == "Cote d'Ivoire"~ "Ivory Coast",
                                country == "Taiwan*"~ "Taiwan",
                                country == " Azerbaijan" ~ "Azerbaijan",
                                country == "The Bahamas" ~ "Bahamas",
                                country == "The Gambia" ~ "Gambia",
                                country == "West Bank and Gaza"~ "Palestine",
                                country == "Holy See"~ "Vatican",
                                country == "Holy See (Vatican City State)"~ "Vatican",
                                country == "Vatican City"~ "Vatican",
                                country == "North Ireland"~ "UK",
                                country == "Kosovo"~ "Serbia",
                                TRUE  ~ country)) %>%
    mutate (country = case_when(
      region %in% c(
        "Anhui",
        "Beijing",
        "Chongqing",
        "Fujian",
        "Gansu",
        "Guangdong",
        "Guangxi",
        "Guizhou",
        "Hainan",
        "Hebei",
        "Heilongjiang",
        "Henan",
        "Hong Kong",
        "Hubei",
        "Hunan",
        "Inner Mongolia",
        "Jiangsu",
        "Jiangxi",
        "Jilin",
        "Liaoning",
        "Macau",
        "Ningxia",
        "Qinghai",
        "Shaanxi",
        "Shandong",
        "Shanghai",
        "Shanxi",
        "Sichuan",
        "Tianjin",
        "Tibet",
        "Xinjiang",
        "Yunnan",
        "Zhejiang"
      ) ~ "China",
      TRUE  ~ country
    )) %>%
    mutate (country = case_when(region == 'Taiwan' ~ "Taiwan", TRUE  ~ country))
  
  
  
  data_daily <- data_daily %>% 
    group_by(date, country, region) %>% 
    summarise(confirmed = sum(confirmed, na.rm = TRUE), death = sum(death, na.rm = TRUE), recovered = sum(recovered, na.rm = TRUE)) %>% 
    ungroup()
  
  li <- list.append(li,subset(data_daily, (country %in% c("Italy"))))
  
  lw <- list.append(lw,subset(data_daily, !(country %in% c("Italy"))))
  
}


print(paste0("reading ","dpc-covid19-ita-regioni.csv"))


#data_file_population <- getURL("http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv")
data_file_ita <- getURL("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
data_ita      <- read.csv(text = data_file_ita)
data_ita$date <- as.Date(substr(data_ita$data,1,10))

names(data_ita)[names(data_ita) == "denominazione_regione"] <- "region"
names(data_ita)[names(data_ita) == "stato"] <- "country"
names(data_ita)[names(data_ita) == "totale_casi"] <- "confirmed"
names(data_ita)[names(data_ita) == "deceduti"] <- "death"
names(data_ita)[names(data_ita) == "dimessi_guariti"] <- "recovered"

data_ita$country <- "Italy"

data_ita <- data_ita[!duplicated(data_ita, by = c("date", "country")), ]

data_ita <- data_ita[, c("date", "country", "region","confirmed", "death", "recovered")]

data_ita_w <- rbindlist(li, fill = TRUE)

lw <- list.append(lw,subset(data_ita_w,date < min(data_ita$date)))

lw <- list.append(lw,data_ita)

covid19_raw_df <- rbindlist(lw, fill = TRUE)

covid19_raw_df <- covid19_raw_df %>%
  arrange(date,country,region)

# ISTAT 2018
pop_data_ita <- data.frame(covid19_raw_df %>% filter(country=="Italy") %>% select(region) %>% distinct() %>% arrange(region)%>%pull(region),
                           c(60483973,
                             1315196,
                             567118,
                             1956687,
                             5826860,
                             4452629,
                             1216853,
                             5896693,
                             1556981,
                             10036258,
                             1531753,
                             308493,
                             527750,
                             539898,
                             4375865,
                             4048242,
                             1648176,
                             5026989,
                             3736968,
                             884640,
                             126202,
                             4903722))
names(pop_data_ita) <- c("region","population")
pop_data_ita$country <- "Italy"

# add population
covid19_raw_df <- covid19_raw_df %>% 
  left_join(select(cname_table,country,iso3), by = c("country")) %>% 
  left_join(select(pop_data,iso3c,value), by = c("iso3"="iso3c")) %>% rename(population=value)

covid19_raw_df <- covid19_raw_df %>%   
  mutate(population = case_when(country=="Italy"~ NA_real_, TRUE  ~ population)) 

covid19_raw_df <- covid19_raw_df %>% 
  full_join(select(pop_data_ita,country,region,population), by = c("region", "country"))%>% 
  group_by(date,country,region,confirmed,death,recovered,iso3) %>%
  mutate(population=case_when(country=="Italy"~ population.y, TRUE  ~ population.x)) %>% select (-c("population.x","population.y"))



covid19_country_df <-  covid19_raw_df %>% filter(region != "Ship") %>%
  group_by(date, country) %>%
  summarise(confirmed = sum(confirmed, na.rm = TRUE), death = sum(death, na.rm = TRUE), recovered = sum(recovered, na.rm = TRUE), population = max(population, na.rm = TRUE)) %>%
  mutate(active = confirmed - death-recovered) %>%
  ungroup() %>% mutate (population = case_when(country=="Italy"~ 60483973, TRUE  ~ population)) %>%
  arrange(date,country)

covid19_italy_df <-subset(covid19_country_df, country %in% "Italy" & confirmed > 1)



write.csv(covid19_raw_df,sprintf("covid19_raw_df_%s.csv", Sys.Date()), row.names = FALSE)
write.csv(covid19_country_df,sprintf("covid19_country_df_%s.csv", Sys.Date()), row.names = FALSE)
write.csv(covid19_italy_df,sprintf("covid19_italy_df_%s.csv", Sys.Date()), row.names = FALSE)
write.csv(covid19_country_df %>% distinct(country) %>% arrange(country),sprintf("countries_%s.csv", Sys.Date()), row.names = FALSE)


##
for (useBIG in c(FALSE,TRUE)) {
  
  if (!useBIG) {
    ntop=12
    nx=4
    ny=3
    BIGlabel=""
    excludelist = c("China","South Korea","Japan") 
    cases_minimum = 50
    
  } else {
    ntop=24
    nx=4
    ny=3
    BIGlabel="BIG "
    excludelist = c()
    cases_minimum = 100
    
  }
  
  
  pdf(file=sprintf("COVID-19 graphs %s%s.pdf",BIGlabel, Sys.Date()),paper="a4r",width=14, height=14) # apro il device
  
  ## some grphics based on
  ## https://www.bnosac.be/index.php/blog
  
  
  lattice.theme <- trellis.par.get()
  lattice.options(default.theme = standard.theme(color = FALSE))
  
  
  trellis.par.set(strip.background = list(col = "grey"))
  qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  
  for (useWorld in c(TRUE,FALSE)) {
    
    if (useWorld){
      
      
      country_subset <- sort(covid19_country_df%>% filter(date==max(covid19_country_df$date)-1&!(country %in% excludelist)) %>% arrange (desc(confirmed)) %>%top_n(ntop,confirmed) %>% pull(country)) #sort(c("France", "Spain", "Germany", "Italy","UK","US","China")) 
      datx <- subset(covid19_country_df, country %in% country_subset)
      
      if (!useBIG) {
        col <- brewer.pal(length(country_subset), "Paired")
      } else {
        col <-col_vector[1:length(country_subset)]
      }
      mytitle <- "WORLD"
    } else {
      temp <- covid19_raw_df%>% filter(date==max(covid19_country_df$date)-1 & country=="Italy") %>% arrange (desc(confirmed)) %>% ungroup()
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      region_subset <- sort(temp %>% top_n(ntop,confirmed) %>% pull(region)) 
      datx <- subset(covid19_raw_df, region %in% region_subset) %>% ungroup() %>% mutate(country=region)
      #names(datx)[names(datx) == "country"] <- "state"
      #names(datx)[names(datx) == "region"] <- "country"
      datx <- datx %>% 
        mutate(active = confirmed - death-recovered) %>%
        arrange(date,country)
      country_subset <- region_subset #sort(levels(region_subset)[as.integer(region_subset)])
      i <- sapply(datx, is.factor)
      datx[i] <- lapply(datx[i], as.character)
      if (!useBIG) {
        col <- brewer.pal(length(country_subset), "Set3")
      } else {
        col <-col_vector[1:length(country_subset)]
      }
      mytitle <- "ITALY"
    }
    
    datx[is.na(datx)] <- 0
    
    daty <- datx
    #daty <- subset(datx, !(country %in% c("China")))
    #col <- sample(color,length(country_subset))
    
    
    xyp <- xyplot(confirmed ~ date | country, data = daty,type = c("o"),
                  scales = list(y = list(relation = "free", rot = 0), x = list(rot = 45, format = "%Y-%m-%d")), 
                  layout = c(nx, ny), main = sprintf("%s - Confirmed cases of COVID-19\n(last date in this graph is %s)", mytitle, max(daty$date)),grid = TRUE,
                  panel = function(x,y,...) { 
                    panel.xyplot(x, y, col=col[panel.number()], pch=20, ...)
                  })
    print(xyp)
    
    xyp <- xyplot(log10(confirmed) ~ date | country, data = daty, type = c("o"), 
                  scales = list(y = list(relation = "free", rot = 0), x = list(rot = 45, format = "%Y-%m-%d")), 
                  layout = c(nx, ny), main = sprintf("%s - Log 10 Confirmed cases of COVID-19\n(last date in this graph is %s)", mytitle, max(daty$date)),grid = TRUE,
                  panel = function(x,y,...) { 
                    panel.xyplot(x, y, col=col[panel.number()], pch=20, ...)
                  })
    print(xyp)
    
    xyp <- xyplot(log10(active) ~ date | country, data = daty, type = c("o"), 
                  scales = list(y = list(relation = "free", rot = 0), x = list(rot = 45, format = "%Y-%m-%d")), 
                  layout = c(nx, ny), main = sprintf("%s - Log 10 Active cases of COVID-19\n(last date in this graph is %s)", mytitle, max(daty$date)),grid = TRUE,
                  panel = function(x,y,...) { 
                    panel.xyplot(x, y, col=col[panel.number()], pch=20, ...)
                  })
    print(xyp)
    
    
    datx <- datx[order(datx$date, datx$country, decreasing = TRUE), ]
    datx<-setDT(datx, keep.rownames=TRUE, key=NULL, check.names=FALSE)

    
    datx <- datx[, days_since_case_onset := (as.integer(date) - as.integer(min(date[confirmed > cases_minimum]))), by = list(country)]
    datx <- datx[, newly_confirmed       := as.integer(confirmed - shift(confirmed, n = 1, type = "lead")),        by = list(country)]
    
    datx <- datx[, days_since_case_ondeath := (as.integer(date) - as.integer(min(date[death > cases_minimum]))), by = list(country)]
    datx <- datx[, newly_dead       := as.integer(death - shift(death, n = 1, type = "lead")),        by = list(country)]
    
    datx <- datx[, days_since_case_onsafe := (as.integer(date) - as.integer(min(date[recovered > cases_minimum]))), by = list(country)]
    datx <- datx[, newly_recovered       := as.integer(recovered - shift(recovered, n = 1, type = "lead")),        by = list(country)]
    
    datx <- datx[, days_since_case_on := (as.integer(date) - as.integer(min(date[active > cases_minimum]))), by = list(country)]
    datx <- datx[, newly_active       := as.integer(active - shift(active, n = 1, type = "lead")),        by = list(country)]
    
    mykey <- list(title="Countries",
                  space="right",
                  text=list(country_subset),
                  points=list(pch=c(1:length(country_subset)), col=col),
                  lines=list(col=col))
    
    mypanel <- function(x, y, ...) {
      panel.xyplot(x, y, ...)
      panel.curve(log10(cases_minimum*2**(x/1)), from=0,  to=150, add=TRUE, type="l", col = "gray",lty=5) # raddoppia ogni giorno
      panel.curve(log10(cases_minimum*2**(x/2)), from=0,  to=150, add=TRUE, type="l", col = "gray",lty=5)
      panel.curve(log10(cases_minimum*2**(x/3)), from=0,  to=150, add=TRUE, type="l", col = "gray",lty=5)
      panel.curve(log10(cases_minimum*2**(x/5)), from=0,  to=150, add=TRUE, type="l", col = "gray",lty=5)
      panel.curve(log10(cases_minimum*2**(x/7)), from=0,  to=150, add=TRUE, type="l", col = "gray",lty=5)
      panel.curve(log10(cases_minimum*2**(x/10)),from=0,  to=150, add=TRUE, type="l", col = "gray",lty=5)
    }
    
    
    # totally confirmed cases vs days
    xyp <- xyplot(confirmed ~ date | paste(mytitle,"Confirmed cases of COVID-19",sep=" - "), groups = country,
                  data = subset(datx, date > as.Date("2020-03-01")), 
                  xlab = "Date", ylab = "log10 of number of new COVID-19 cases",
                  scales = list(x = list(rot = 45, format = "%Y-%m-%d", at = seq(as.Date("2020-03-01"), Sys.Date(), by = "2 days"))), 
                  key = mykey, 
                  type = "o", pch=1:length(country_subset),col=col,lwd = 2,grid = TRUE) 
    print(xyp)
    
    # log(10) of newly confirmed cases vs days
    xyp <- xyplot(newly_confirmed ~ date | paste(mytitle,"Newly confirmed cases of COVID-19",sep=" - "), groups = country,
                  data = subset(datx, date > as.Date("2020-03-01")), 
                  xlab = "Date", ylab = "number of new COVID-19 cases", 
                  yscale.components = yscale.components.log10ticks,
                  scales = list(y = list(log = 10),x = list(rot = 45, format = "%Y-%m-%d", at = seq(as.Date("2020-03-01"), Sys.Date(), by = "2 days"))), 
                  key = mykey, 
                  type = c("p", "smooth"), #type = "o", 
                  pch=1:length(country_subset),col=col,lwd = 2,grid = TRUE) 
    print(xyp)
    
    
    
    test <- datx %>%
      pivot_longer(
        cols = c("confirmed","active","recovered","death"),
        names_to = "cases",
        values_drop_na = TRUE
      )
    
    # log(10) of totally confirmed cases vs days
    xyp <- xyplot(value ~ date | factor(cases,levels=c("recovered","death","confirmed","active")), groups = country,
                  data = subset(test, date > as.Date("2020-03-01")), 
                  xlab = "Date", ylab = "number of cases",
                  yscale.components = yscale.components.log10ticks,
                  scales = list(y = list(log = 10),x = list(rot = 45, format = "%Y-%m-%d", at = seq(as.Date("2020-03-01"), Sys.Date(), by = "2 days"))), 
                  key = mykey, 
                  type = "o", pch=1:length(country_subset),col=col,lwd = 2,grid = TRUE) 
    print(xyp)
    
    
    # log(10) of confirmed cases vs days since reach 100 cases
    xyp<-xyplot(confirmed ~ days_since_case_onset |  paste0(mytitle," - ","confirmed cases of COVID-19 since onset of sick person nr ",cases_minimum), 
                groups = country,
                data = subset(datx, days_since_case_onset >= 0 ), 
                scales = list(y = list(log = 10),x = list(at = seq(0,max(datx$days_since_case_onset), by =5))),
                xlab = paste0("Days since COVID-19 onset - confirmed case ",cases_minimum), ylab = "number of confirmed cases",
                #key = mykey,
                yscale.components = yscale.components.log10ticks,
                type = "o", pch=1:length(country_subset),col=col,lwd = 2,
                grid = TRUE,
                panel = mypanel)
    print(direct.label(xyp,"smart.grid"))
    
    # log(10) of death cases vs days since reach 100 cases
    xyp<-xyplot(death ~ days_since_case_ondeath |  paste0(mytitle," - ","dead cases of COVID-19 since dead of sick person nr ",cases_minimum), 
                groups = country,
                data = subset(datx, days_since_case_ondeath >= 0 ), 
                xlab = paste0("Days since nr deaths was ",cases_minimum), ylab = "number of deaths",
                scales = list(y = list(log = 10)),yscale.components = yscale.components.log10ticks,
                #key = mykey,
                type = "o", pch=1:length(country_subset),col=col,lwd = 2,
                grid = TRUE,
                panel = mypanel) 
    print(direct.label(xyp,"smart.grid"))
    
    # log(10) of recovered cases vs days since reach 100 cases
    xyp<-xyplot(recovered ~ days_since_case_onsafe |  paste0(mytitle," - ","recovered cases of COVID-19 since recovered of sick person nr ",cases_minimum), 
                groups = country,
                data = subset(datx, days_since_case_onsafe >= 0 ), 
                xlab = paste0("Days since nr recovered case was ",cases_minimum), ylab = "number of recovered",
                scales = list(y = list(log = 10)),yscale.components = yscale.components.log10ticks,
                #key = mykey,
                type = "o", pch=1:length(country_subset),col=col,lwd = 2,
                grid = TRUE,
                panel = mypanel) 
    print(direct.label(xyp,"smart.grid"))
    
    # log(10) of active cases vs days since reach 100 cases
    xyp<-xyplot(active ~ days_since_case_on |  paste0(mytitle," - ","active cases of COVID-19 since nr active was ",cases_minimum), 
                groups = country,
                data = subset(datx, days_since_case_on >= 0 ), 
                xlab = paste0("Days since nr active case was ",cases_minimum), ylab = "number of active cases",
                scales = list(y = list(log = 10)),yscale.components = yscale.components.log10ticks,
                key = mykey,
                type = "o", pch=1:length(country_subset),col=col,lwd = 2,
                grid = TRUE,
                panel = mypanel)
    print(direct.label(xyp,"smart.grid"))
    
    
  }
  
  dev.off() # lo chiudo
}




### START ITALY PROJECTIONS

library(prophet)
library(ggplot2)

pdf(file=sprintf("COVID-19 prophet %s.pdf", Sys.Date()),paper="a4r",width=14, height=14) # apro il device

datx <- covid19_italy_df

test <- datx %>%
  pivot_longer(
    cols = c("confirmed","active","recovered","death"),
    names_to = "cases",
    values_drop_na = TRUE
  )

for (casetype in c("confirmed","active","recovered","death")) {
  
  df <- test %>% filter(cases==casetype & value>100) %>% mutate(ds=date,y=log(value)) %>% select (ds,y) %>% arrange(ds)
  #df <- covid19_italy_df %>% filter(active>0) %>% mutate(ds=date,y=log10(active)) %>% select (ds,y) %>% arrange(ds)
  m <- prophet(df,growth = "linear",yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE,fit=FALSE,
               changepoint_range=0.98,
               changepoint.prior.scale = 0.05,
               interval.width = 0.95)
  m <-fit.prophet(m, df)
  future <- make_future_dataframe(m, periods= 5)
  forecast <- predict(m, future)
  p <- plot(m, forecast) + add_changepoints_to_plot(m) + ggtitle(casetype)
  print(p)
}





###
if(TRUE){
  
  library(deSolve)
  
  Infected <- covid19_italy_df %>% filter(date > as.Date("2020-02-27")) %>% pull(confirmed)
  Day <- 1:(length(Infected))
  N <- covid19_italy_df %>% filter (date==max(covid19_country_df$date)-1) %>% pull(max(population)) # population 
  country <- covid19_italy_df %>% filter (date==max(covid19_country_df$date)-1) %>% pull(country)
  
  old <- par(mfrow = c(1, 2))
  plot(Day, Infected, type ="b")
  plot(Day, Infected, log = "y")
  abline(lm(log10(Infected) ~ Day),col="red")
  title(sprintf("Total infections COVID-19 %s",country), outer = TRUE, line = -2)
  
  SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
      dS <- -beta/N * I * S
      dI <- beta/N * I * S - gamma * I
      dR <- gamma * I
      list(c(dS, dI, dR))
    })
  }
  
  init <- c(S = N-Infected[1], I = Infected[1], R = 0)
  RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[ , 3]
    sum((Infected - fit)^2)
  }
  
  Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
  Opt$message
  ## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
  
  Opt_par <- setNames(Opt$par, c("beta", "gamma"))
  Opt_par
  ##      beta     gamma 
  ## 0.6427585 0.3572415
  
  t <- 1:120 # time in days
  fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
  col <- 1:3 # colour
  
  matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
  matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
  ## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
  ## omitted from logarithmic plot
  
  points(Day, Infected)
  legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
  title(sprintf("SIR model COVID-19  %s",country), outer = TRUE, line = -2)
  
  
  par(old)
  
  R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
  R0
  ##       R0 
  ## 1.799227
  
  fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic
  ##          I
  ## 54 9765121
  
  max_infected <- max(fit$I)
  max_infected / 5 # severe cases
  ## [1] 1953024
  
  max_infected * 0.06 # cases with need for intensive care
  ## [1] 585907.3
  
  # https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
  max_infected * 0.007 # deaths with supposed 0.7% fatality rate
  ## [1] 68355.85
  
}

###

if(TRUE){
  
  ### https://github.com/lilywang1988/eSIR
  
  #library(devtools)
  #install_github("lilywang1988/eSIR")
  
  library(eSIR) 
  
  set.seed(20192020)
  StartDay <-  min(covid19_italy_df$date)
  NI_complete <- covid19_italy_df %>% filter(date >= StartDay) %>% pull(confirmed) #infected
  RI_complete <- covid19_italy_df %>% filter(date >= StartDay) %>% mutate(R=death+recovered) %>% pull(R) #recovered (including death)
  N <- covid19_italy_df %>% filter (date==max(covid19_country_df$date)-1) %>% pull(max(population)) # population 
  death_in_R <- covid19_italy_df %>% filter(date==max(covid19_country_df$date)-1) %>% mutate(DR=death/(death+recovered)) %>% pull(DR) #death rate
  beta0 <- Opt_par["beta"]
  gamma0 <- Opt_par["gamma"]
  
  Day <- 1:(length(NI_complete))
  
  EndDay <- StartDay+tail(Day, n=1)-1
  
  country <- covid19_italy_df %>% filter (date==max(covid19_country_df$date)-1) %>% pull(country)
  
  
  R <- RI_complete/N
  Y <- NI_complete/N- R 
  
  
  
  # ### without pi(t), the standard state-space SIR model without intervention
  # res.nopi <- tvt.eSIR(Y,R,begin_str=format(StartDay,"%m/%d/%Y"),death_in_R = death_in_R, beta0 = beta0, gamma0=gamma0,T_fin=150,
  #                      casename=sprintf("%s_nopi",country),save_files = F,save_plot_data = F,
  #                      M=5e3,nburnin = 2e3)
  # print(res.nopi$plot_infection)
  # print(res.nopi$plot_removed)
  
  ### a SIR model with a time-varying transmission rate - Step function of pi(t)
  change_time <- c("02/21/2020","03/08/2020","03/10/2020","03/21/2020")
  pi0<- c(1.0,0.9,0.4,0.2,0.1)
  res.step <-tvt.eSIR(Y,R,begin_str=format(StartDay,"%m/%d/%Y"),death_in_R = death_in_R, beta0 = beta0, gamma0=gamma0,T_fin=160,
                      pi0=pi0,change_time=change_time,dic=T,casename=sprintf("%s_step",country),
                      save_files = F, save_mcmc=F,save_plot_data = F,add_death =T,
                      M=5e3,nburnin = 2e3)
  print(res.step$plot_infection)
  print(res.step$plot_removed)
  
  
  # ### SIR with time-varying quarantine, which follows a Dirac Delta function rho(t)
  # change_time <- c("02/21/2020","03/08/2020","03/10/2020","03/21/2020")
  # phi0<- c(0.05,0.25,0.4,0.2)
  # res.q <- qh.eSIR (Y,R,begin_str=format(StartDay,"%m/%d/%Y"),death_in_R = death_in_R, beta0 = beta0, gamma0=gamma0,T_fin=150,
  #                   phi0=phi0,change_time=change_time,casename=sprintf("%s_q",country),
  #                   save_files = F,save_mcmc = F,save_plot_data = F,
  #                   M=5e3,nburnin = 2e3)
  # 
  # print(res.q$plot_infection)
  # print(res.q$plot_removed)
  
  
  change_time <- c("02/21/2020","03/08/2020","03/10/2020","03/21/2020","04/14/2020","05/08/2020","06/03/2020")
  pi0<- c(1.0,0.9,0.4,0.2,0.1,0.4,0.6,1.0)
  res.step_reopen <-tvt.eSIR(Y,R,begin_str=format(StartDay,"%m/%d/%Y"),death_in_R = death_in_R, beta0 = beta0, gamma0=gamma0,T_fin=160,
                      pi0=pi0,change_time=change_time,dic=T,casename=sprintf("%sstep_reopen",country),
                      save_files = F, save_mcmc=F,save_plot_data = F,add_death =T,
                      M=5e3,nburnin = 2e3)
  print(res.step_reopen$plot_infection)
  print(res.step_reopen$plot_removed)
  
  
  ### 
}


### end italy 
dev.off() # lo chiudo
#






