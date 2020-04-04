setwd("D:/OneDrive/data/covid19/data")

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(socviz)
library(ggrepel)
library(paletteer)


library(showtext)
showtext_auto()
library(myriad)
import_myriad_semi()



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


#https://kieranhealy.org/blog/archives/2020/03/21/covid-19-tracking/
#https://kieranhealy.org/blog/archives/2020/03/27/a-covid-small-multiple/



here::here("data")

## Download today's excel file, saving it to data/ and reading it in
get_ecdc_data <- function(url = "https://www.ecdc.europa.eu/sites/default/files/documents/https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx",
                          fname = "COVID-19-geographic-distribution-worldwide-", 
                          date = lubridate::today(), 
                          ext = "xlsx", 
                          dest = "data") {
  
  #target <-  paste0(url, fname, date, ".", ext)
  target <- url
  message("target: ", target)
  
  destination <- fs::path(here::here("data"), paste0(fname, date), ext = ext)
  message("saving to: ", destination)
  
  tf <- tempfile(fileext = ext)
  curl::curl_download(target, tf)
  fs::file_copy(tf, destination, overwrite = TRUE)
  
  switch(ext, 
         xls = janitor::clean_names(readxl::read_xls(tf)),
         xlsx = janitor::clean_names(readxl::read_xlsx(tf))
  )
}                

coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- dplyr::union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

iso3_cnames <- read_csv("data/countries_iso3.csv")
iso2_to_iso3 <- read_csv("data/iso2_to_iso3.csv")

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


##############################################################################################


covid_raw <- get_ecdc_data(url = "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx",
                           ext = "xlsx")
covid_raw

covid <- covid_raw %>%
  mutate(date = lubridate::ymd(date_rep),
         iso2 = geo_id)

## merge in the iso country names
covid <- left_join(covid, cname_table)

# covid
# 
# 
# ## Looks like a missing data code
# covid %>% 
#   filter(cases == -9)
# 
# anti_join(covid, cname_table) %>%
#   select(geo_id, countries_and_territories, iso2, iso3, cname) %>%
#   distinct()
# 
# 
cname_xwalk <- read_csv("data/ecdc_to_iso2_xwalk.csv",
                         na = "")
# 
# cname_xwalk

covid <- coalesce_join(covid, cname_xwalk, 
                       by = "geo_id", join = dplyr::left_join)

## Take a look again
# anti_join(covid, cname_table) %>%
#   select(geo_id, countries_and_territories, iso2, iso3, cname) %>%
#   distinct()

cov_curve_deaths <- covid %>%
  select(date, cname, iso3, cases, deaths) %>%
  drop_na(iso3) %>%
  group_by(iso3) %>%
  arrange(date) %>%
  mutate(cu_cases = cumsum(cases), 
         cu_deaths = cumsum(deaths)) %>%
  filter(cu_deaths > 9) %>%
  mutate(days_elapsed = date - min(date),
         end_label = ifelse(date == max(date), cname, NA))

cov_curve_cases <- covid %>%
  select(date, cname, iso3, cases, deaths) %>%
  drop_na(iso3) %>%
  group_by(iso3) %>%
  arrange(date) %>%
  mutate(cu_cases = cumsum(cases), 
         cu_deaths = cumsum(deaths)) %>%
  filter(cu_cases > 24) %>%
  mutate(days_elapsed = date - min(date),
         end_label = ifelse(date == max(date), cname, NA))


# cov_curve

focus_cn <- c("CAN", "GBR", "USA", "DEU", "ITA",
              "FRA", "ESP", "TUR", "CHE")

cov_curve_deaths_filtered <- cov_curve_deaths %>% filter(iso3 %in% focus_cn)

# min_x<- log10(10) #min(cov_curve_deaths_filtered$cu_deaths)
# max_x<- as.numeric(max(cov_curve_deaths_filtered$days_elapsed))
# max_y<- max(cov_curve_deaths_filtered$cu_deaths)
# 
# f1 <- function(x) {(x/1)+min_x}
# f2 <- function(x) {(x/2)+min_x}
# f3 <- function(x) {(x/3)+min_x}
# f5 <- function(x) {(x/5)+min_x}
# f7 <- function(x) {(x/7)+min_x}
# f10 <- function(x) {(x/10)+min_x}
# 
# xlim1<-c(0,min(max_x,1*log2(max_y)))
# xlim2<-c(0,min(max_x,2*log2(max_y)))
# xlim3<-c(0,min(max_x,3*log2(max_y)))
# xlim5<-c(0,min(max_x,5*log2(max_y)))
# xlim7<-c(0,min(max_x,7*log2(max_y)))
# xlim10<-c(0,min(max_x,10*log2(max_y)))

g1 <- stat_function(fun=f1, linetype="dashed", aes(colour="double each day"), xlim=xlim1)
g2 <- stat_function(fun=f2, linetype="dashed", aes(colour="double each 2 days"), xlim=xlim2)
g3 <- stat_function(fun=f3, linetype="dashed", aes(colour="double each 3 days"), xlim=xlim3)
g5 <- stat_function(fun=f5, linetype="dashed", aes(colour="double each 5 days"), xlim=xlim5)
g7 <- stat_function(fun=f7, linetype="dashed", aes(colour="double each week"), xlim=xlim7)
g10 <- stat_function(fun=f10, linetype="dashed", aes(colour="double each ten days"), xlim=xlim10)


plot_deaths<-cov_curve_deaths_filtered %>% ## focus on just a few countries, defined above
  mutate(end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran", 
                            `Korea, Republic of` = "South Korea", 
                            `United Kingdom` = "UK")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_deaths, 
                       color = cname, label = end_label, 
                       group = cname)) + 
  geom_line(size = 0.8) + 
  geom_text_repel(nudge_x = 1.1,
                  nudge_y = 0.1, 
                  segment.color = NA) + 
  guides(color = FALSE) + 
  scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                     breaks = 10^seq(1, 5),
                     trans = "log10") + 
  labs(x = "Days Since 10th Confirmed Death", 
       y = "Cumulative Number of Deaths (log scale)", 
       title = "Cumulative Deaths from COVID-19, Selected Countries", 
       subtitle = paste("Data as of", format(max(cov_curve_deaths$date), "%A, %B %e, %Y")), 
       caption = "Kieran Healy @kjhealy / Data: ECDC") + 
  mytheme #+g1+g2+g3+g5+g7+g10

cov_curve_cases_filtered <- cov_curve_cases %>% filter(iso3 %in% focus_cn)

# min_x<- log10(25) #min(cov_curve_cases_filtered$cu_cases)
# max_x<- as.numeric(max(cov_curve_cases_filtered$days_elapsed))
# max_y<- max(cov_curve_cases_filtered$cu_cases)
# 
# f1 <- function(x) {(x/1)+min_x}
# f2 <- function(x) {(x/2)+min_x}
# f3 <- function(x) {(x/3)+min_x}
# f5 <- function(x) {(x/5)+min_x}
# f7 <- function(x) {(x/7)+min_x}
# f10 <- function(x) {(x/10)+min_x}
# 
# xlim1<-c(0,min(max_x,1*log2(max_y)))
# xlim2<-c(0,min(max_x,2*log2(max_y)))
# xlim3<-c(0,min(max_x,3*log2(max_y)))
# xlim5<-c(0,min(max_x,5*log2(max_y)))
# xlim7<-c(0,min(max_x,7*log2(max_y)))
# xlim10<-c(0,min(max_x,10*log2(max_y)))

g1 <- stat_function(fun=f1, linetype="dashed", aes(colour="double each day"), xlim=xlim1)
g2 <- stat_function(fun=f2, linetype="dashed", aes(colour="double each 2 days"), xlim=xlim2)
g3 <- stat_function(fun=f3, linetype="dashed", aes(colour="double each 3 days"), xlim=xlim3)
g5 <- stat_function(fun=f5, linetype="dashed", aes(colour="double each 5 days"), xlim=xlim5)
g7 <- stat_function(fun=f7, linetype="dashed", aes(colour="double each week"), xlim=xlim7)
g10 <- stat_function(fun=f10, linetype="dashed", aes(colour="double each ten days"), xlim=xlim10)

plot_cases<-cov_curve_cases_filtered %>% ## focus on just a few countries, defined above
  mutate(end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran", 
                            `Korea, Republic of` = "South Korea", 
                            `United Kingdom` = "UK")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_cases, 
                       color = cname, label = end_label, 
                       group = cname)) + 
  geom_line(size = 0.8) + 
  geom_text_repel(nudge_x = 1.1,
                  nudge_y = 0.1, 
                  segment.color = NA) + 
  guides(color = FALSE) + 
  scale_color_manual(values = prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                     breaks = 10^seq(1, 5),
                     trans = "log10") + 
  labs(x = "Days Since 25th Confirmed Cases", 
       y = "Cumulative Number of Cases (log scale)", 
       title = "Cumulative Cases from COVID-19, Selected Countries", 
       subtitle = paste("Data as of", format(max(cov_curve_cases$date), "%A, %B %e, %Y")), 
       caption = "Kieran Healy @kjhealy / Data: ECDC") + 
  mytheme #+g1+g2+g3+g5+g7+g10



cov_case_curve <- covid %>%
  select(date, cname, iso3, cases, deaths) %>%
  drop_na(iso3) %>%
  group_by(iso3) %>%
  arrange(date) %>%
  mutate(cu_cases = cumsum(cases), 
         cu_deaths = cumsum(deaths)) %>%
  filter(cu_cases > 99) %>%
  mutate(days_elapsed = date - min(date),
         end_label = ifelse(date == max(date), cname, NA),
         end_label = recode(end_label, `United States` = "USA",
                            `Iran, Islamic Republic of` = "Iran", 
                            `Korea, Republic of` = "South Korea", 
                            `United Kingdom` = "UK"),
         cname = recode(cname, `United States` = "USA",
                        `Iran, Islamic Republic of` = "Iran", 
                        `Korea, Republic of` = "South Korea", 
                        `United Kingdom` = "UK"))

## Top 50 countries by >> 100 cases, let's say. 
top_50 <- cov_case_curve %>%
  group_by(cname) %>%
  filter(cu_cases == max(cu_cases)) %>%
  ungroup() %>%
  top_n(50, cu_cases) %>%
  select(iso3, cname, cu_cases) %>%
  mutate(days_elapsed = 1, 
         cu_cases = max(cov_case_curve$cu_cases) - 1e4) 


cov_case_curve_bg <- cov_case_curve %>% 
  select(-cname) %>%
  filter(iso3 %in% top_50$iso3) 

cov_case_curve_endpoints <- cov_case_curve %>% 
  filter(iso3 %in% top_50$iso3) %>%
  group_by(iso3) %>%
  filter(cu_cases == max(cu_cases)) %>%
  select(cname, iso3, days_elapsed, cu_cases) %>%
  ungroup()


cov_case_sm <- cov_case_curve  %>%
  filter(iso3 %in% top_50$iso3) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_cases)) + 
  # The line traces for every country, in every panel
  geom_line(data = cov_case_curve_bg, 
            aes(group = iso3),
            size = 0.15, color = "gray80") + 
  # The line trace in red, for the country in any given panel
  geom_line(color = "firebrick",
            lineend = "round") + 
  # The point at the end. Bonus trick: some points can have fills!
  geom_point(data = cov_case_curve_endpoints, 
             size = 1.1, 
             shape = 21, 
             color = "firebrick",
             fill = "firebrick2"
  ) + 
  # The country label inside the panel, in lieu of the strip label
  geom_text(data = top_50, 
            mapping = aes(label = cname), 
            vjust = "inward", 
            hjust = "inward",
            fontface = "bold", 
            color = "firebrick", 
            size = 2.1) + 
  # Log transform and friendly labels
  scale_y_log10(labels = scales::label_number_si()) + 
  # Facet by country, order from high to low
  facet_wrap(~ reorder(cname, -cu_cases), ncol = 5) + 
  labs(x = "Days Since 100th Confirmed Case", 
       y = "Cumulative Number of Cases (log10 scale)", 
       title = "Cumulative Number of Reported Cases of COVID-19: Top 50 Countries", 
       subtitle = paste("Data as of", format(max(cov_case_curve$date), "%A, %B %e, %Y")), 
       caption = "Kieran Healy @kjhealy / Data: https://www.ecdc.europa.eu/") + 
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(0.7)),
        plot.caption = element_text(size = rel(1)),
        # turn off the strip label and tighten the panel spacing
        strip.text = element_blank(),
        panel.spacing.x = unit(-0.05, "lines"),
        panel.spacing.y = unit(0.3, "lines"),
        axis.text.y = element_text(size = rel(0.5)),
        axis.title.x = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1)),
        axis.text.x = element_text(size = rel(0.5)),
        legend.text = element_text(size = rel(1)))

pdf(file=sprintf("COVID-19 graphs ggp2 %s.pdf",Sys.Date()),paper="a4r",width=14, height=14) # apro il device
print(plot_cases)
print(plot_deaths)
print(cov_case_sm)
dev.off()
