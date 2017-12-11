setwd("GitHub/vreme_seminarska/");
rm(list = ls());
library(CORElearn);
library(rpart);
library(ipred);
source("wrapper.R");

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

# -------------------------------------------------------------------------------------------------- Funkcije / data setup

data = read.table("podatkiSem1.txt", header = TRUE, sep = ",");

#Remove attribute Glob_sevanje_min as it's always 0
data$Glob_sevanje_min <- NULL;

#Change date to Date class
data$Datum = as.Date(data$Datum);

#Add attribute Weekday
data$Weekday = factor(weekdays(data$Datum), levels=c("ponedeljek", "torek", "sreda", "èetrtek", "petek", "sobota", "nedelja"), ordered=TRUE);

#Add attribute Month
data$Month = factor(months(data$Datum), levels=c("januar", "februar", "marec", "april", "maj", "junij", "julij", "avgust", "september", "october", "november", "december"), ordered=TRUE);

#Add attribute Year
data$Year = as.numeric(format(data$Datum, "%Y"));

#Add attribute Season
data$Season = factor(getSeason(data$Datum), levels=c("Zima", "Pomlad", "Poletje", "Jesen"));

#Add attribute O3Class 
data$O3Class = factor(getO3concentration(data$O3), levels=c("NIZKA", "SREDNJA", "VISOKA", "EKSTREMNA"));

#Add attribute PM10Class 
data$PM10Class = factor(getPM10concentration(data$PM10), levels=c("NIZKA", "VISOKA"));

data$Datum <- NULL;

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[data$Year > 2014 && data$Year <= 2015, ];
validacijskamnozica = data[data$Year > 2015,];

testnamnozica$O3 <- NULL;
testnamnozica$PM10 <- NULL;


#----------------------------------------------------------------------------------- data setup / ocenjevanje atributov

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


#----------------------------------------------------------------------------------- ocenjevanje atributov / predikcija

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

#CORElearn random forest
model <- CoreModel(O3Class ~ ., data = ucnamnozica, model="rf");

observed <- testnamnozica$O3Class;
predicted <- predict(model, testnamnozica, type = "class");
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

#CORElearn random forest
model <- CoreModel(PM10Class~ ., data = ucnamnozica, model="rf");

observed <- testnamnozica$PM10Class;
predicted <- predict(model, testnamnozica, type = "class");
t <- table(observed, predicted);
sum(diag(t)) / sum(t);


#----------------------------------------------------------------------------------- predikcija / ostalo

#nrow(ucnamnozica);
#table(ucnamnozica$O3Class );

#nrow(testnamnozica);
#table(testnamnozica$O3Class );


#ucnamnozica$Year = NULL
#testnamnozica$Year = NULL


#plot(dt);
#text(dt, pretty = 0);

#----------------------------------------------------------------------regresija

rm(list = ls());
library(CORElearn);
library(rpart);
library(ipred);
source("wrapper.R");

#------------------------------------------------ regression evaluation equations
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

#----------------------------------------------------------attribute setup
data = read.table("podatkiSem1.txt", header = TRUE, sep = ",");


#Remove attribute Glob_sevanje_min as it's always 0
data$Glob_sevanje_min <- NULL;

#Change date to Date class
data$Datum = as.Date(data$Datum);

#Add attribute Weekday
data$Weekday = factor(weekdays(data$Datum), levels=c("ponedeljek", "torek", "sreda", "èetrtek", "petek", "sobota", "nedelja"), ordered=TRUE);

#Add attribute Month
data$Month = factor(months(data$Datum), levels=c("januar", "februar", "marec", "april", "maj", "junij", "julij", "avgust", "september", "october", "november", "december"), ordered=TRUE);

#Add attribute Year
data$Year = as.numeric(format(data$Datum, "%Y"));

#Add attribute Season
data$Season = factor(getSeason(data$Datum), levels=c("Zima", "Pomlad", "Poletje", "Jesen"));

data$Datum <- NULL;

#---------------------------------------------------------------------------------O3

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[data$Year > 2014 && data$Year <= 2015, ];
validacijskamnozica = data[data$Year > 2015,];

observed = testnamnozica$O3;
observed2 = validacijskamnozica$O3;

ucnamnozica$PM10 <- NULL;
testnamnozica$O3 <- NULL;
testnamnozica$PM10 <- NULL;
validacjskamnozica$O3 <- NULL;
validacijskamnozica$PM10 <- NULL;

model <- rpart(ucnamnozica$O3 ~ Month + Temperatura_lokacija_max + Vlaga_max + Sunki_vetra_max + Pritisk_max + Sunki_vetra_min + Padavine_mean + Glob_sevanje_mean + Weekday + Temperatura_Krvavec_min + Sunki_vetra_mean + Temperatura_Krvavec_mean + Hitrost_vetra_max + Vlaga_min + Padavine_sum + Temperatura_Krvavec_max + Glob_sevanje_max + Season, ucnamnozica, minsplit = 50, cp = 0.01);
predicted <- predict(model, testnamnozica);
mae(observed, predicted);
rmae(observed, predicted, mean(testnamnozica$O3));
mse(observed, predicted);
rmse(observed, predicted, mean(testnamnozica$O3));

#----------------------------------------------------------------------------------PM10

ucnamnozica = data[data$Year <= 2014,];
testnamnozica = data[data$Year > 2014 && data$Year <= 2015, ];
validacijskamnozica = data[data$Year > 2015,];

observed = testnamnozica$PM10;
observed2 = validacijskamnozica$PM10;

ucnamnozica$O3 <- NULL;
testnamnozica$O3 <- NULL;
testnamnozica$PM10 <- NULL;
validacjskamnozica$O3 <- NULL;
validacijskamnozica$PM10 <- NULL;


model <- rpart(ucnamnozica$PM10 ~ Temperatura_lokacija_max + Hitrost_vetra_min + Month + Padavine_sum + Temperatura_lokacija_mean + Postaja + Sunki_vetra_min + Pritisk_max + Temperatura_Krvavec_mean + Pritisk_min + Sunki_vetra_mean + Padavine_mean, ucnamnozica, minsplit = 100, cp = 0.001);
predicted <- predict(model, testnamnozica);
mae(observed, predicted);
rmae(observed, predicted, mean(testnamnozica$PM10));
mse(observed, predicted);
rmse(observed, predicted, mean(testnamnozica$PM10));


plot(model);text(model, pretty = 0);

rpart.control()

