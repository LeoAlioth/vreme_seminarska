setwd("GitHub/vreme_seminarska/");
rm(list = ls());
library(CORElearn);
library(rpart);
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

#Add attribute Month
data$Month = factor(months(data$Datum), levels=c("januar", "februar", "marec", "april", "maj", "junij", "julij", "avgust", "september", "october", "november", "december"));

#Add attribute Year
data$Year = as.numeric(format(data$Datum, "%Y"));

#Add attribute Season
data$Season = factor(getSeason(data$Datum), levels=c("Zima", "Pomlad", "Poletje", "Jesen"));

#Add attribute O3Class 
data$O3Class = factor(getO3concentration(data$O3), levels=c("NIZKA", "SREDNJA", "VISOKA", "EKSTREMNA"));

#Add attribute PM10Class 
data$PM10Class = factor(getPM10concentration(data$PM10), levels=c("NIZKA", "VISOKA"));

data$Datum <- NULL;
data$O3 <- NULL;
data$PM10 <- NULL;

ucnamnozica = data[data$Year <= 2015,];
testnamnozica = data[data$Year > 2015,];

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

wrapper(ucnamnozica, className="O3Class", classModel="tree", folds=10)
wrapper(ucnamnozica, className="PM10Class", classModel="tree", folds=10)


#----------------------------------------------------------------------------------- ocenjevanje atributov / predikcija

#O3Class

#rpart decision tree
model <- rpart(O3Class ~ ., data = ucnamnozica);

#CORElearn decision tree
model <- CoreModel(O3Class ~ ., data = ucnamnozica, model="tree");

#CORElearn bayes
model <- CoreModel(O3Class ~ ., model="bayes");

#CORElearn knn
for(k in 1:100) {
	model <- CoreModel(O3Class ~ ., data = ucnamnozica, model="knn", kInNN=k);
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
model <- rpart(PM10Class ~ ., data = ucnamnozica);

#CORElearn decision tree
model <- CoreModel(PM10Class ~ ., data = ucnamnozica, model="tree");

#CORElearn bayes
model <- CoreModel(PM10Class~ ., model="bayes");

#CORElearn knn
for(k in 1:100) {
	model <- CoreModel(PM10Class~ ., data = ucnamnozica, model="knn", kInNN=k);
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

