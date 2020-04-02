setwd("D:/OneDrive/data/covid19/data")

saveoutput <- 1   # 1=yes; 0=no
# threshold for model selection  
threshmod <- 0.05
lookforpeaks <- 1

#install.packages("devtools")
library(maps)
library(devtools)
install_github("dkahle/ggmap")

#install.packages("rjson")
library("rjson")
library("tidyverse")
#install.packages("stringr")
library("stringr")
#install.packages("ggmap")
library(ggmap)
library(readxl)

# Create empty map and save (DONE ONCE!)
# mapItalyEmpty <- get_map(location = "Italy", zoom = 6, maptype = "toner-lite",
#               source = "stamen", crop = TRUE, color = c("bw"))
# mapItalyEmpty <- ggmap(mapItalyEmpty)
# save(mapItalyEmpty, file = "MapItalyEmpty.RData")

delta <- 10

json_file <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province.json"
json_data <- fromJSON(file=json_file)

# Provinces' population counts from Istat
provpop <- data.frame(read_excel("province-ELAB-MB-only-totals.xls"))
# We add columns with latitude, longitude, and labels
provpop$lat <- rep(NA,length(provpop$Totale))
provpop$long <- rep(NA,length(provpop$Totale))
provpop$rog <- rep(0,length(provpop$Totale))
provpop$label <- rep(0,length(provpop$Totale))
provpop$rate <- rep(0,length(provpop$Totale))
provpop$enddate <-NA


# HARD CODING ONE RECORD FROM SICILY (IT SAYS "NULL")
json_data[[2150]]$totale_casi <- NA

n <- length(json_data)
print(n)
covid <- list(date=rep(NA,n),nation=rep(NA,n), region_code = rep(NA,n), region = rep(NA,n), province_code = rep(NA,n),
province = rep(NA,n), province_ltrs = rep(NA,n), lat = rep(NA,n), long = rep(NA,n),
cases = rep(NA,n))

for(i in 1: n)
{
#print(i)
  covid$date[i] <- json_data[[i]]$data
  covid$nation[i] <- json_data[[i]]$stato
  covid$region_code[i] <- json_data[[i]]$codice_regione
  covid$region[i] <- json_data[[i]]$denominazione_regione
  covid$province_code[i] <- json_data[[i]]$codice_provincia
  covid$province[i] <- json_data[[i]]$denominazione_provincia
  covid$province_ltrs[i] <- json_data[[i]]$sigla_provincia
  covid$lat[i] <- json_data[[i]]$lat
  covid$long[i] <- json_data[[i]]$long
  covid$cases[i] <- json_data[[i]]$totale_casi
}

print("Dates:")
table(covid$date)
print(paste("Total number of days =",length(table(covid$date))))
times <- 1:length(table(covid$date))

print("Provinces:")
table(covid$province)
provs <- unique(covid$province[covid$province != "In fase di definizione/aggiornamento"])
provs

for(i in 1:length(provs))
cat(paste(provs[i], "\ \ \ ",provpop$Totale[provpop$Provincia==provs[i]],"\n"))

print("Province letter codes:")
table(covid$province_ltrs)
provltrs <- unique(covid$province_ltrs)

# Remove time from dates
covid$date <- str_sub(covid$date,1,10)
table(covid$date)
datetoday <- max(covid$date)
print(paste("Most recent date is",datetoday))

if(saveoutput==1)
{
pdf(paste("rplot",datetoday,".pdf",sep=""),width=8,height=6) 
sink(paste("output",datetoday,".txt",sep=""))
}
# Note that province "In fase di definizione/aggiornamento" has lat=long=0 and unique
# province_code and province_ltrs = ""
par(mfrow=c(2,3))

for(j in 1:length(provs))
{
selprovince <- provs[j]
cat("\n")
print(selprovince)
datessel <- as.Date(covid$date[covid$province==selprovince], "%Y-%m-%d")
#print(datessel)
casessel <- as.numeric(covid$cases[covid$province==selprovince])
#print(casessel)
Totsel <- provpop$Totale[provpop$Provincia==selprovince]
print(Totsel)
provpop$lat[provpop$Provincia==selprovince] <- covid$lat[covid$province==selprovince][1]
provpop$long[provpop$Provincia==selprovince] <- covid$long[covid$province==selprovince][1]
#plot(times, casessel, xlab="Days since 23feb2020", ylab="Cases",main=paste(selprovince,datessel[length(datessel)]), xlim=c(1,length(times)+delta+60), ylim=c(0,1.4*max(casessel)))

# First derivative
trendrel <- function(xval,modsel)
{
if(modsel == 3)
{
#predxval <- predict(fit, newdata = list(times=xval,times2=xval^2,times3=xval^3,Npop=Totsel), type="response")
#res <- (predxval/Totsel)*(betahat[2:4]%*%c(1,2*xval,3*xval^2))
res <- (betahat[2:4]%*%c(1,2*xval,3*xval^2))
}
if(modsel ==2)
{
#predxval <- predict(fit, newdata = list(times=xval,times2=xval^2,Npop=Totsel), type="response")
#res <- (predxval/Totsel)*(betahat[2]+2*betahat[3]*xval)
res <- (betahat[2]+2*betahat[3]*xval)
}
if(modsel ==1)
{
#  predxval <- predict(fit, newdata = list(times=xval,Npop=Totsel), type="response")
#  res <- (predxval/Totsel)*betahat[2]
  res <- betahat[2]
}
return(res)
}

# Second derivative
curvrel <- function(xval,modsel)
{
if(modsel==3)
{
#predxval <- predict(fit, newdata = list(times=xval,times2=xval^2,times3=xval^3,Npop=Totsel), type="response")
#res <- (predxval/Totsel)*((2*betahat[3]+6*betahat[4]*xval)+(betahat[2]+2*betahat[3]*xval+3*betahat[4]*xval^2)^2)
res <- ((2*betahat[3]+6*betahat[4]*xval)+(betahat[2]+2*betahat[3]*xval+3*betahat[4]*xval^2)^2)
}
if(modsel==2)  
{
#predxval <- predict(fit, newdata = list(times=xval,times2=xval^2,Npop=Totsel), type="response")
#res <- (predxval/Totsel)*(2*betahat[3]+(betahat[2]+2*betahat[3]*xval)^2)
res <- (2*betahat[3]+(betahat[2]+2*betahat[3]*xval)^2)
}
if(modsel==1)  
{
# predxval <- predict(fit, newdata = list(times=xval,Npop=Totsel), type="response")
# res <- (predxval/Totsel)*(betahat[2]^2)
  res <- (betahat[2]^2)
}
return(res)
}
print(max(casessel)/Totsel)
if((max(casessel)/Totsel) < 0.00005)
{
text(9,max(casessel)+0.5,"Prevalence too low!")
provpop$label[provpop$Provincia==selprovince] <- 0
provpop$rog[provpop$Provincia==selprovince] <- NA
}  else 
{
times2 <- times^2
times3 <- times^3
fit1 <- glm(casessel~times+offset(rep(Totsel,length(times))), family=poisson(link=log))
res1 <- coef(summary.lm(fit1))
betahat1 <- res1[,1]
print(fit1)
print(res1)
fit2 <- glm(casessel~times+times2+offset(rep(Totsel,length(times))), family=poisson(link=log))
res2 <- coef(summary.lm(fit2))
betahat2 <- res2[,1]
print(fit2)
print(res2)
fit3 <- glm(casessel~times+times2+times3+offset(rep(Totsel,length(times))), family=poisson(link=log))
res3 <- coef(summary.lm(fit3))
betahat3 <- res3[,1]
print(fit3)
print(res3)
if(fit1$converged == FALSE && fit2$converged == FALSE && fit3$converged == FALSE)  text(7, max(casessel), "No convergence")
if(fit1$converged == FALSE && fit2$converged == FALSE && fit3$converged == TRUE) 
{
fit <- fit3
}

if(fit1$converged == FALSE && fit2$converged == TRUE && fit3$converged == FALSE) 
{
fit <- fit2
}

if(fit1$converged == FALSE && fit2$converged == TRUE && fit3$converged == TRUE) 
{
# Select best model
  if(res3[4,4] < threshmod)
  {
  modsel <- 3
  fit <- fit3
  } else
  {
  modsel <- 2
  fit <- fit2
  }
}

if(fit1$converged == TRUE && fit2$converged == FALSE && fit3$converged == FALSE)
{
modsel <- 1
fit <- fit1
}

if(fit1$converged == TRUE && fit2$converged == FALSE && fit3$converged == TRUE) 
{
# Select best model by LR test
lrstat <- fit1$deviance - fit3$deviance
pval <- 1-pchisq(lrstat,1)
if(pval < threshmod)
{
modsel <- 3
fit <- fit3
} else
{
modsel <- 1
fit <- fit1
}
}

if(fit1$converged == TRUE && fit2$converged == TRUE && fit3$converged == FALSE)
{
  # Select best model
  if(res2[3,4] < threshmod)
  {
  modsel <- 2
  fit <- fit2
  } else
  {
  modsel <- 1
  fit <- fit1
  }
}

if(fit1$converged == TRUE && fit2$converged == TRUE && fit3$converged == TRUE) 
{
  # Select best model
  if(res3[4,4] < threshmod)
  {
    modsel <- 3
    fit <- fit3
  } else
    if(res2[3,4] < threshmod)
    {
      modsel <- 2
      fit <- fit2
    } else
    {
      modsel <- 1
      fit <- fit1
    } 
}
    
#text(6,1.15*max(casessel), paste("Degree =",modsel))
#print(paste("Degree =",modsel))
res <- coef(summary.lm(fit))
#print(res)
betahat <- res[,1]
#print(betahat)
today <- length(times)
predtoday <- fitted.values(fit)[today]
timesplot <- (1:((length(times)+delta)*50))/50  # longer prediction
timesplot2 <- (1:((length(times)+delta+60)*50))/50     # shorter prediction


if(modsel == 1)
{
  linespred <- predict(fit, newdata = list(times=timesplot2,Npop=Totsel), type = "response")
  
}
if(modsel == 2)
{
  linespred <- predict(fit, newdata = list(times=timesplot2,times2=timesplot2^2,Npop=Totsel), type = "response")
  
}
if(modsel ==3)
{
  linespred <- predict(fit, newdata = list(times=timesplot2,times2=timesplot2^2,times3=timesplot2^3,Npop=Totsel), type = "response")
  
}

###
enddate <- 'to be calculated'

test <- linespred
test[test < 10] <- 0
res <-floor(with(rle(test), {
  ok <- values == 0 & lengths > 3
  ends <- cumsum(lengths)
  starts <- ends - lengths + 1
  data.frame(starts, ends)[ok, ]
})/50)

if(dim(res)[1]>1) {enddate <- datessel[1]+res[2,1]} else {enddate <- 'unknown'}

plot(times, casessel, xlab=sprintf("End date %s",enddate), ylab="Cases",main=paste(selprovince,datessel[length(datessel)]), xlim=c(1,length(times)+delta+60), ylim=c(0,1.4*max(casessel)))


###


if(modsel == 1)
{
  
  lines(timesplot2,linespred, col="red")
}
if(modsel == 2)
{
  
  lines(timesplot2,linespred, col="red")
}
if(modsel ==3)
{
  
  lines(timesplot2,linespred, col="red")
}



# Check to make sure that these are identical - OK!
#fitted.values(fit)
#predict(fit, newdata = list(times=1:today,times2=(1:today)^2,times3=(1:today)^3,Npop=Totsel), type = "response")

trendtoday <- trendrel(today,modsel)  # These use model-based denominators!
curvtoday <- curvrel(today,modsel)    # These use model-based denominators!
#rateofgrowth <- trendtoday/(casessel[today]/Totsel)  # Obsolete
rateofgrowth <- trendtoday
#text(6,0.85*max(casessel),paste("Rate",round(rateofgrowth,digit=3)))
if(rateofgrowth > 0.1 & rateofgrowth <= 0.2) provpop$label[provpop$Provincia==selprovince] <- 1
if(rateofgrowth > 0.2 & rateofgrowth <= 0.3) provpop$label[provpop$Provincia==selprovince] <- 2
if(rateofgrowth > 0.3 & rateofgrowth <= 0.4) provpop$label[provpop$Provincia==selprovince] <- 3
if(rateofgrowth > 0.4 ) provpop$label[provpop$Provincia==selprovince] <- 4
provpop$rate[provpop$Provincia==selprovince] <- casessel[today]/Totsel
provpop$rog[provpop$Provincia==selprovince] <- rateofgrowth
provpop$enddate[provpop$Provincia==selprovince] <- enddate
# PEAK ("FLAT") POINTS
if(lookforpeaks ==1)
{
if(modsel == 1)
{
#text(5, max(casessel), "No flats")
print("No flats")
}
if(modsel == 2)
{
peak <- -betahat[2]/(2*betahat[3])
#text(5, max(casessel), paste("Flat at:",round(peak,digit=1)))
print(paste("INTERPRET WITH CAUTION: Flat at",round(peak,digit=1)))
}
if(modsel ==3)
{
if((4*betahat[3]^2-12*betahat[2]*betahat[4]) >0)
{
peak1 <- (-2*betahat[3]-sqrt(4*betahat[3]^2-12*betahat[2]*betahat[4]))/(6*betahat[4])
peak2 <- (-2*betahat[3]+sqrt(4*betahat[3]^2-12*betahat[2]*betahat[4]))/(6*betahat[4])
#text(8, max(casessel), paste("Flat at:",round(peak1,digit=1),",",round(peak2,digit=1)))
print(paste("INTERPRET WITH CAUTION: Flat at",round(peak1,digit=1),",",round(peak2,digit=1)))
}
else
{
#text(5, max(casessel), "No flats")
print("No flats")
}
}
}
}  # This is the end of all calculations for provinces with non-trivial prevalence
}


cat("\n")
names(provpop)[c(1,2,3)] <- c("Code","Province","Pop")
print.data.frame(provpop[,c(1,2,3,6,7,9)])

#Plot Italy
sel <- 1*(covid$province != "In fase di definizione/aggiornamento")
par(mfrow=c(1,1))

# MAP!
x <- covid$long[sel==1]
y <- covid$lat[sel==1]
colrog <- rep(NA,length(provpop[,1]))
labrog <- rep(NA,length(provpop[,1]))
for(i in 1:length(provpop[,1]))
{
  colrog[i] <- "chartreuse2"
  labrog[i] <- "<=0.1 or N/A"
  if(provpop[i,7] == 1) {colrog[i] <- "yellow1"; labrog[i] <- "(0.1,0.2]"}
  if(provpop[i,7] == 2) {colrog[i] <- "deepskyblue2"; labrog[i] <- "(0.2,0.3]"}
  if(provpop[i,7] == 3) {colrog[i] <- "darkorange2"; labrog[i] <- "(0.3,0.4]"}
  if(provpop[i,7] == 4) {colrog[i] <- "red"; labrog[i] <- ">0.4"}
}

#print(colrog)
  
# MAP FROM R (no servers)
# Prepare the empty plot (pch=16 is small filled circle, 19 larger filled circle)
# map("Italy", fill = FALSE)
# points(x, y, pch=1, col=3, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), xaxt="n", yaxt="n", bty="n",xlab="",ylab="",type="n")
# # Overplot with approriate colors (16 is smaller circle, 19 larger circle)
# points(provpop[,5],provpop[,4],pch=rep(1,length(colrog)),col=colrog,cex=sqrt(provpop[,8]*5000))      
# }
# #title(main="Provinces")
# legend("topright", bty="n", c("<=0.1 or N/A","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]",">0.4"), cex=.7,pch=rep(1,5), col=c("chartreuse2","yellow1","deepskyblue2","darkorange2","red"), title= "Rate of growth")
# #####

# NICER MAP
labrogf <- factor(labrog, levels = c("<=0.1 or N/A","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]",">0.4"))

# MAP FROM GOOGLE (do not use)
# Deselect and then select "Geocoding API" from https://console.developers.google.com/google/maps-apis/api-list?project=psychic-cursor-271123 
# if this does not work!
# For google map, you have to give the center of the window you are looking at.
# Possibility for the map type argument: terrain / satellite / roadmap / hybrid
# get the map info
# Plot it
# ggmap(map) + 
#   theme_void() + 
#   ggtitle("terrain") + 
#   theme(
#     plot.title = element_text(colour = "darkorange2"), 
#     panel.border = element_rect(colour = "grey", fill=NA, size=2)
#   )
# SIZE IS FOR DIAMETER (SAME AS ABOVE)
# mapg <- get_googlemap(source="google","Rome, Italy", zoom = 6, maptype = "terrain", color="bw")
# mapobj <- ggmap(mapg)
# mapfin <- mapobj + geom_point(data=data.frame(lon=provpop[,5], lat=provpop[,4]),
# aes(x=lon, y=lat, colour=labrogf), shape = 21, size=sqrt(provpop[,8]*20000), alpha=3,stroke=1) +
#  scale_colour_manual(name="", values =c("<=0.1 or N/A"="chartreuse2","(0.1,0.2]"="yellow1", "(0.2,0.3]"="deepskyblue2", "(0.3,0.4]"="darkorange2", ">0.4"="red")) +
#  ggtitle("Relative change in reported positive cases", subtitle = format(as.Date(datetoday),'%B %d, %Y'))
# mapfin

# USING STAMEN MAPS
MapItalyEmpty <- load(file = "MapItalyEmpty.RData")

mapfin <- mapItalyEmpty + 
  geom_point(data=data.frame(lon=provpop[,5], lat=provpop[,4]),
             aes(x=lon, y=lat, colour=labrogf), shape = 21, size=sqrt(provpop[,8]*20000), alpha=3,stroke=1) +
  scale_colour_manual(name="", values =c("<=0.1 or N/A"="chartreuse2","(0.1,0.2]"="yellow1", "(0.2,0.3]"="deepskyblue2", "(0.3,0.4]"="darkorange2", ">0.4"="red")) +
  ggtitle("Relative change in reported positive cases", subtitle = format(as.Date(datetoday),'%B %d, %Y'))

mapfin


if(saveoutput ==1)
{
sink()
dev.off()
}

if(saveoutput ==1)
pdf(paste("Map-COVID-19-",datetoday,".pdf",sep=""),paper="a4r") 
if(saveoutput ==1)
mapfin
if(saveoutput ==1)
dev.off()

if(saveoutput ==1)
pdf(paste("Map-COVID-19.pdf",sep=""),paper="a4r") 
if(saveoutput ==1)
mapfin
if(saveoutput ==1)
dev.off()

