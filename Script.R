#Seminarska naloga: Umetna inteligenca
#Simon Šegatin (63160294), Anže Košir (63160174)
#Ljubljana, december 2017
# -------------------------------------------------------------------------------------------------- r setup

setwd("GitHub/vreme_seminarska/");
Sys.setlocale("LC_TIME", "Slovenian_Slovenia.1250");
rm(list = ls());

library(CORElearn);
library(rpart);
library(zoo);
library(ipred);
library(kknn);

source("wrapper.R");

# -------------------------------------------------------------------------------------------------- r setup / functions

getSeason <- function(DATES) {
    WS <- as.Date("12-21", format = "%m-%d") # Zimski solsticij
    SE <- as.Date("3-21",  format = "%m-%d") # Spomladansko enakonoèje
    SS <- as.Date("6-21",  format = "%m-%d") # Poletni solsticij
    FE <- as.Date("9-23",  format = "%m-%d") # Jesensko enakonoèje

    d <- as.Date(format(DATES, "%m-%d"), format="%m-%d")

    ifelse (d >= WS | d < SE, "Zima",
      ifelse (d >= SE & d < SS, "Pomlad",
        ifelse (d >= SS & d < FE, "Poletje", "Jesen")))
}

getO3concentration <- function(o3) {
     ifelse (o3 >= 0 & o3 < 60, "NIZKA",
      ifelse (o3 >= 60 & o3 < 120, "SREDNJA",
        ifelse (o3 >= 120 & o3 < 180, "VISOKA", "EKSTREMNA")))
}

getPM10concentration <- function(pm10) {
     ifelse (pm10 <= 35.0, "NIZKA", "VISOKA")
}

# -------------------------------------------------------------------------------------------------- functions / data setup

data = read.table("podatkiSem1.txt", header = TRUE, sep = ",");

#Remove attribute Glob_sevanje_min as it's always 0
data$Glob_sevanje_min <- NULL;

#Change date to Date class
data$Datum = as.Date(data$Datum);

#Add attribute Weekday
data$Weekday = factor(weekdays(data$Datum), levels=c("ponedeljek", "torek", "sreda", "èetrtek", "petek", "sobota", "nedelja"), ordered=TRUE);

#Add attribute Month
data$Month = factor(months(data$Datum), levels=c("januar", "februar", "marec", "april", "maj", "junij", "julij", "avgust", "september", "oktober", "november", "december"), ordered=TRUE);

#Add attribute Year
data$Year = as.numeric(format(data$Datum, "%Y"));

#Add attribute Season
data$Season = factor(getSeason(data$Datum), levels=c("Zima", "Pomlad", "Poletje", "Jesen"));

#Add attribute O3Class 
data$O3Class = factor(getO3concentration(data$O3), levels=c("NIZKA", "SREDNJA", "VISOKA", "EKSTREMNA"));

#Add attribute PM10Class 
data$PM10Class = factor(getPM10concentration(data$PM10), levels=c("NIZKA", "VISOKA"));

data$Year_mon = as.yearmon(data$Datum)

#----------------------------------------------------------------------------------- data setup / visualization

par(mfrow=c(1, 2))

#Min, mean, max O3 by year
plot(1, ylim=c(0,250), yaxs="i", xlim=range(data$Year), xaxs="i", axes=FALSE, xlab="", ylab="Koncentracija O3", main="Koncentracija O3 po letih")
mtext("Min (zelena), Mean (modra), Max (rdeèa)")
box()
axis(1, at=c(2013:2016))
axis(2, at=seq(0, 250, by=50))
lines(aggregate(O3 ~ Year, data = data, max), col="red")
lines(aggregate(O3 ~ Year, data = data, mean), col="blue")
lines(aggregate(O3 ~ Year, data = data, min), col="green")

#Min, mean, max PM10 by year
plot(1, ylim=c(0,150), yaxs="i", xlim=range(data$Year), xaxs="i", axes=FALSE, xlab="", ylab="Koncentracija PM10", main="Koncentracija PM10 po letih")
mtext("Min (zelena), Mean (modra), Max (rdeèa)")
box()
axis(1, at=c(2013:2016))
axis(2, at=seq(0, 150, by=25))
lines(aggregate(PM10 ~ Year, data = data, max), col="red")
lines(aggregate(PM10 ~ Year, data = data, mean), col="blue")
lines(aggregate(PM10 ~ Year, data = data, min), col="green")

#Glob_sevanje_mean mean by month
plot(aggregate(Glob_sevanje_mean ~ Year_mon, data = data[data$Postaja=="Ljubljana",], mean), type="l", col="red", ylab="Raven globalnega sevanja", xlab="", main="Povpreèna raven globalnega sevanja po mesecih")
lines(aggregate(Glob_sevanje_mean ~ Year_mon, data = data[data$Postaja=="Koper",], mean), col="blue")
mtext("Rdeèa: Ljubljana, modra: Koper")

#O3 and PM10 by month in 2016
par(mfrow=c(1, 2))
plot(aggregate(O3 ~ as.numeric(format(Datum, "%m")), data = data[which(data$Year==2013 & data$Postaja=="Ljubljana"),], mean), axes=F, type="l", col="red", ylab="Raven koncentracije O3", xlab="", main="Povpreèna raven koncentracije O3 v letu 2016")
axis(1, at=1:12, labels=c("jan", "feb", "mar", "apr", "maj", "jun", "jul", "avg", "sep", "okt", "nov", "dec"))
axis(2, at=seq(0, 150, by=25))
box()
plot(aggregate(PM10 ~ as.numeric(format(Datum, "%m")), data = data[which(data$Year==2013 & data$Postaja=="Ljubljana"),], mean), axes=F, type="l", col="blue", ylab="Raven koncentracije PM10", xlab="", main="Povpreèna raven koncentracije PM10 v letu 2016")
axis(1, at=1:12, labels=c("jan", "feb", "mar", "apr", "maj", "jun", "jul", "avg", "sep", "okt", "nov", "dec"))
axis(2, at=seq(0, 60, by=5))
box();

par(mfrow=c(1, 2))
hist(data$O3, xlab="Koncentracija O3", main="Distribucija koncentracije O3")
box()
hist(data$PM10, xlab="Koncentracija PM10", main="Distribucija koncentracije PM10")
box()

#----------------------------------------------------------------------------------- visualization / attribute importance

data$Datum <- NULL;

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[which(data$Year > 2014 & data$Year <= 2015), ];
validacijskamnozica = data[data$Year > 2015,];

sort(attrEval(O3Class ~ ., ucnamnozica, "InfGain"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "Gini"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "GainRatio"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "MDL"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "Relief"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "ReliefFexpRank"), decreasing = TRUE)

sort(attrEval(PM10Class ~ ., ucnamnozica, "InfGain"), decreasing = TRUE)
sort(attrEval(PM10Class ~ ., ucnamnozica, "Gini"), decreasing = TRUE)
sort(attrEval(PM10Class ~ ., ucnamnozica, "GainRatio"), decreasing = TRUE)
sort(attrEval(PM10Class ~ ., ucnamnozica, "MDL"), decreasing = TRUE)
sort(attrEval(PM10Class ~ ., ucnamnozica, "Relief"), decreasing = TRUE)
sort(attrEval(PM10Class ~ ., ucnamnozica, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(PM10Class ~ ., ucnamnozica, "ReliefFexpRank"), decreasing = TRUE)

wrapper(ucnamnozica, className="O3Class", classModel="rf", folds=10)
wrapper(ucnamnozica, className="PM10Class", classModel="tree", folds=10)

#----------------------------------------------------------------------------------- attribute importance / classification

#O3Class

#rpart decision tree
model <- rpart(O3Class ~ Glob_sevanje_max + Temperatura_lokacija_max + Temperatura_Krvavec_mean + Pritisk_min + Sunki_vetra_max + Pritisk_max + Hitrost_vetra_max + Vlaga_max + Padavine_mean + Temperatura_Krvavec_min + Padavine_sum + Pritisk_mean, data = ucnamnozica);

#CORElearn decision tree
model <- CoreModel(O3Class ~ Glob_sevanje_max + Temperatura_lokacija_max + Temperatura_Krvavec_mean + Pritisk_min + Sunki_vetra_max + Pritisk_max + Hitrost_vetra_max + Vlaga_max + Padavine_mean + Temperatura_Krvavec_min + Padavine_sum + Pritisk_mean, data = ucnamnozica, model="tree");

#CORElearn bayes
model <- CoreModel(O3Class ~ Glob_sevanje_max + Pritisk_max + Month + Postaja + PM10Class + Year + Sunki_vetra_mean + Temperatura_Krvavec_min + Vlaga_min, data=ucnamnozica, model="bayes");

#CORElearn knn
for(k in 1:100) {
	model <- CoreModel(O3Class ~ Month + Temperatura_lokacija_max + Vlaga_max + Sunki_vetra_max + Pritisk_max + Sunki_vetra_min + Padavine_mean + Glob_sevanje_mean + Weekday + Temperatura_Krvavec_min + Sunki_vetra_mean + Temperatura_Krvavec_mean + Hitrost_vetra_max + Vlaga_min + Padavine_sum + PM10Class + Temperatura_Krvavec_max + Glob_sevanje_max + Season, data = ucnamnozica, model="knn", kInNN=k);
	observed <- testnamnozica$O3Class;
	predicted <- predict(model, testnamnozica, type = "class");
	t <- table(observed, predicted);
	print(k);
	print(sum(diag(t)) / sum(t));
}

observed <- testnamnozica$O3Class;
predicted <- predict(model, testnamnozica, type = "class");
t <- table(observed, predicted);
sum(diag(t)) / sum(t);

observed <- validacijskamnozica$O3Class;
predicted <- predict(model, validacijskamnozica, type = "class");
t <- table(observed, predicted);
sum(diag(t)) / sum(t);

#PM10Class
#rpart decision tree
model <- rpart(PM10Class ~ Temperatura_lokacija_max + Hitrost_vetra_min + O3Class + Month + Year + Padavine_sum + Temperatura_lokacija_mean + Postaja + Sunki_vetra_min + Pritisk_max + Temperatura_Krvavec_mean + Pritisk_min + Sunki_vetra_mean + Padavine_mean, data = ucnamnozica);

#CORElearn decision tree
model <- CoreModel(PM10Class ~ Temperatura_lokacija_max + Hitrost_vetra_min + O3Class + Month + Year + Padavine_sum + Temperatura_lokacija_mean + Postaja + Sunki_vetra_min + Pritisk_max + Temperatura_Krvavec_mean + Pritisk_min + Sunki_vetra_mean + Padavine_mean, data = ucnamnozica, model="tree");

#CORElearn bayes
model <- CoreModel(PM10Class ~ Month + Postaja + Sunki_vetra_max + Padavine_sum + Vlaga_max + Padavine_mean + Year + Pritisk_max, data=ucnamnozica, model="bayes");

#CORElearn knn
for(k in 1:100) {
	model <- CoreModel(PM10Class~ Temperatura_lokacija_max + Temperatura_Krvavec_max + Sunki_vetra_min + Hitrost_vetra_min + Temperatura_lokacija_min + Temperatura_Krvavec_mean + Glob_sevanje_mean + Postaja + Padavine_mean + Glob_sevanje_max + Padavine_sum + Temperatura_lokacija_mean + Temperatura_Krvavec_min + Hitrost_vetra_mean + Hitrost_vetra_max + O3Class + Pritisk_mean + Year + Month + Vlaga_min + Vlaga_max + Pritisk_min + Sunki_vetra_max + Sunki_vetra_mean, data = ucnamnozica, model="knn", kInNN=k);
	observed <- testnamnozica$PM10Class;
	predicted <- predict(model, testnamnozica, type = "class");
	t <- table(observed, predicted);
	print(k);
	print(sum(diag(t)) / sum(t));
}

observed <- testnamnozica$PM10Class;
predicted <- predict(model, testnamnozica, type = "class");
t <- table(observed, predicted);
sum(diag(t)) / sum(t);

observed <- validacijskamnozica$PM10Class;
predicted <- predict(model, validacijskamnozica, type = "class");
t <- table(observed, predicted);
sum(diag(t)) / sum(t);


#----------------------------------------------------------------------------------- classification / regression

#regression evaluation equations

mae <- function(observed, predicted)
{
	mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
	sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
	mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val) 
{  
	sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}

#attribute setup

#O3 rpart

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[data$Year > 2014 & data$Year <= 2015, ];
validacijskamnozica = data[data$Year > 2015,];

observed = testnamnozica$O3;
observed2 = validacijskamnozica$O3;

ucnamnozica$PM10 <- NULL;
testnamnozica$O3 <- NULL;
testnamnozica$PM10 <- NULL;
validacijskamnozica$O3 <- NULL;
validacijskamnozica$PM10 <- NULL;

model <- rpart(ucnamnozica$O3 ~ Month + Temperatura_lokacija_max + Vlaga_max + Sunki_vetra_max + Pritisk_max + Sunki_vetra_min + Padavine_mean + Glob_sevanje_mean + Weekday + Temperatura_Krvavec_min + Sunki_vetra_mean + Temperatura_Krvavec_mean + Hitrost_vetra_max + Vlaga_min + Padavine_sum + Temperatura_Krvavec_max + Glob_sevanje_max + Season, ucnamnozica, minsplit = 100, cp = 0.001);

predicted <- predict(model, testnamnozica);
mae(observed, predicted);
rmae(observed, predicted, mean(observed));
mse(observed, predicted);
rmse(observed, predicted, mean(observed));

predicted <- predict(model, validacijskamnozica);
mae(observed2, predicted);
rmae(observed2, predicted, mean(observed2));
mse(observed2, predicted);
rmse(observed2, predicted, mean(observed2));

#PM10 rpart

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[data$Year > 2014 & data$Year <= 2015, ];
validacijskamnozica = data[data$Year > 2015,];

observed = testnamnozica$PM10;
observed2 = validacijskamnozica$PM10;

ucnamnozica$O3 <- NULL;
testnamnozica$O3 <- NULL;
testnamnozica$PM10 <- NULL;
validacijskamnozica$O3 <- NULL;
validacijskamnozica$PM10 <- NULL;


model <- rpart(ucnamnozica$PM10 ~ Temperatura_lokacija_max + Hitrost_vetra_min + Month + Year + Padavine_sum + Temperatura_lokacija_mean + Postaja + Sunki_vetra_min + Pritisk_max + Temperatura_Krvavec_mean + Pritisk_min + Sunki_vetra_mean + Padavine_mean, ucnamnozica, minsplit = 50, cp = 0.001);
predicted <- predict(model, testnamnozica);
mae(observed, predicted);
rmae(observed, predicted, mean(observed));
mse(observed, predicted);
rmse(observed, predicted, mean(observed));

predicted <- predict(model, validacijskamnozica);
mae(observed2, predicted);
rmae(observed2, predicted, mean(observed2));
mse(observed2, predicted);
rmse(observed2, predicted, mean(observed2));

#O3 CORElearn tree

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[data$Year > 2014 & data$Year <= 2015, ];
validacijskamnozica = data[data$Year > 2015,];

observed = testnamnozica$O3;
observed2 = validacijskamnozica$O3;

ucnamnozica$PM10 <- NULL;
testnamnozica$O3 <- NULL;
testnamnozica$PM10 <- NULL;
validacijskamnozica$O3 <- NULL;
validacijskamnozica$PM10 <- NULL;

model <- CoreModel(O3 ~ Month + Temperatura_lokacija_max + Vlaga_max + Sunki_vetra_max + Pritisk_max + Sunki_vetra_min + Padavine_mean + Glob_sevanje_mean + Weekday + Temperatura_Krvavec_min + Sunki_vetra_mean + Temperatura_Krvavec_mean + Hitrost_vetra_max + Vlaga_min + Padavine_sum + Temperatura_Krvavec_max + Glob_sevanje_max + Season, data=ucnamnozica, model="regTree", modelTypeReg = 3)

predicted <- predict(model, testnamnozica);
mae(observed, predicted);
rmae(observed, predicted, mean(observed));
mse(observed, predicted);
rmse(observed, predicted, mean(observed));

predicted <- predict(model, validacijskamnozica);
mae(observed2, predicted);
rmae(observed2, predicted, mean(observed2));
mse(observed2, predicted);
rmse(observed2, predicted, mean(observed2));

#PM10 CORElearn tree

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[data$Year > 2014 & data$Year <= 2015, ];
validacijskamnozica = data[data$Year > 2015,];

observed = testnamnozica$PM10;
observed2 = validacijskamnozica$PM10;

ucnamnozica$O3 <- NULL;
testnamnozica$O3 <- NULL;
testnamnozica$PM10 <- NULL;
validacijskamnozica$O3 <- NULL;
validacijskamnozica$PM10 <- NULL;

model <- CoreModel(PM10 ~ Temperatura_lokacija_max + Hitrost_vetra_min + Month + Year + Padavine_sum + Temperatura_lokacija_mean + Postaja + Sunki_vetra_min + Pritisk_max + Temperatura_Krvavec_mean + Pritisk_min + Sunki_vetra_mean + Padavine_mean, data=ucnamnozica, model="regTree", modelTypeReg = 3)

predicted <- predict(model, testnamnozica);
mae(observed, predicted);
rmae(observed, predicted, mean(observed));
mse(observed, predicted);
rmse(observed, predicted, mean(observed));

predicted <- predict(model, validacijskamnozica);
mae(observed2, predicted);
rmae(observed2, predicted, mean(observed2));
mse(observed2, predicted);
rmse(observed2, predicted, mean(observed2));

#O3 knn

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[data$Year > 2014 & data$Year <= 2015, ];
validacijskamnozica = data[data$Year > 2015,];

observed = testnamnozica$O3;
observed2 = validacijskamnozica$O3;

ucnamnozica$PM10 <- NULL;
#testnamnozica$O3 <- NULL;
#testnamnozica$PM10 <- NULL;
#validacijskamnozica$O3 <- NULL;
#validacijskamnozica$PM10 <- NULL;

model <- kknn(O3 ~ Month + Temperatura_lokacija_max + Vlaga_max + Sunki_vetra_max + Pritisk_max + Sunki_vetra_min + Padavine_mean + Glob_sevanje_mean + Weekday + Temperatura_Krvavec_min + Sunki_vetra_mean + Temperatura_Krvavec_mean + Hitrost_vetra_max + Vlaga_min + Padavine_sum + Temperatura_Krvavec_max + Glob_sevanje_max + Season, ucnamnozica, testnamnozica, k=70);

predicted <- fitted(model);

mae(observed, predicted);
rmae(observed, predicted, mean(ucnamnozica$O3));
mse(observed, predicted);
rmse(observed, predicted, mean(ucnamnozica$O3));

model <- kknn(O3 ~ Month + Temperatura_lokacija_max + Vlaga_max + Sunki_vetra_max + Pritisk_max + Sunki_vetra_min + Padavine_mean + Glob_sevanje_mean + Weekday + Temperatura_Krvavec_min + Sunki_vetra_mean + Temperatura_Krvavec_mean + Hitrost_vetra_max + Vlaga_min + Padavine_sum + Temperatura_Krvavec_max + Glob_sevanje_max + Season, ucnamnozica, validacijskamnozica, k=70);

predicted <- fitted(model);

mae(observed2, predicted);
rmae(observed2, predicted, mean(validacijskamnozica$O3));
mse(observed2, predicted);
rmse(observed2, predicted, mean(validacijskamnozica$O3));

#PM10 knn

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[data$Year > 2014 & data$Year <= 2015, ];
validacijskamnozica = data[data$Year > 2015,];

observed = testnamnozica$PM10;
observed2 = validacijskamnozica$PM10;

ucnamnozica$O3 <- NULL;
#testnamnozica$O3 <- NULL;
#testnamnozica$PM10 <- NULL;
#validacijskamnozica$O3 <- NULL;
#validacijskamnozica$PM10 <- NULL;

model <- kknn(PM10 ~ Temperatura_lokacija_max + Hitrost_vetra_min + Month + Year + Padavine_sum + Temperatura_lokacija_mean + Postaja + Sunki_vetra_min + Pritisk_max + Temperatura_Krvavec_mean + Pritisk_min + Sunki_vetra_mean + Padavine_mean, ucnamnozica, testnamnozica, k=100);

predicted <- fitted(model);

mae(observed, predicted);
rmae(observed, predicted, mean(ucnamnozica$PM10));
mse(observed, predicted);
rmse(observed, predicted, mean(ucnamnozica$PM10));

model <- kknn(PM10 ~ Temperatura_lokacija_max + Hitrost_vetra_min + Month + Year + Padavine_sum + Temperatura_lokacija_mean + Postaja + Sunki_vetra_min + Pritisk_max + Temperatura_Krvavec_mean + Pritisk_min + Sunki_vetra_mean + Padavine_mean, ucnamnozica, validacijskamnozica, k=100);

predicted <- fitted(model);

mae(observed2, predicted);
rmae(observed2, predicted, mean(validacijskamnozica$PM10));
mse(observed2, predicted);
rmse(observed2, predicted, mean(validacijskamnozica$PM10));

#----------------------------------------------------------------------------------- classification / regression
