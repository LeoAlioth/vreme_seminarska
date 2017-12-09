rm(list = ls())

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

getO3concentration <- function(d) {
     ifelse (d >= 0 & d < 60, "NIZKA",
      ifelse (d >= 60 & d < 120, "SREDNJA",
        ifelse (d >= 120 & d < 180, "VISOKA", "EKSTREMNA")))
}

# -------------------------------------------------------------------------------------------------- Funkcije / data setup

data = read.table("podatkiSem1.txt", header = TRUE, sep = ",");

#Remove attribute Glob_sevanje_min as it's always 0
data$Glob_sevanje_min <- NULL;

#Change date to Date class
data$Datum = as.Date(data$Datum);

#Add attribute Weekday
data$Weekday = factor(weekdays(data$Datum), levels=c("ponedeljek", "torek", "sreda", "èetrtek", "petek", "sobota", "nedelja"));

#Add attribute Season
data$Season = factor(getSeason(data$Datum), levels=c("Zima", "Pomlad", "Poletje", "Jesen"));

#Add attribute O3Level
data$O3Class = factor(getO3concentration(data$O3), levels=c("NIZKA", "SREDNJA", "VISOKA", "EKSTREMNA"));

data$O3 <- NULL;

#ucnamnozica = data[format(data$Datum, "%Y") <= "2014",];
#testnamnozica = data[format(data$Datum, "%Y") > "2014",];

data$Year = as.numeric(format(data$Datum, "%Y"));
data$Datum <- NULL;
ucnamnozica = data[data$Year <= "2014",];
testnamnozica = data[data$Year > "2014",];

library("CORElearn");
library(rpart);

#----------------------------------------------------------------------------------- data setup / ocenjevanje atributov

sort(attrEval(O3Class ~ ., ucnamnozica, "InfGain"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "Gini"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "GainRatio"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "MDL"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "Relief"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(O3Class ~ ., ucnamnozica, "ReliefFexpRank"), decreasing = TRUE)

source("wrapper.R");
wrapper(ucnamnozica, className="O3Class", classModel="tree", folds=10)


#----------------------------------------------------------------------------------- ocenjevanje atributov / predikcija

#rpart decision tree
model <- rpart(O3Class ~ ., data = ucnamnozica);

#CORElearn decision tree
model <- CoreModel(O3Class ~ ., data = ucnamnozica, model="tree");

#CORElearn bayes
model <- CoreModel(O3Class ~ ., model="bayes");

#CORElearn knn
k <- 25;
model <- CoreModel(O3Class ~ ., data = ucnamnozica, model="knn", kInNN=k);

#CORElearn random forest
model <- CoreModel(O3Class ~ ., data = ucnamnozica, model="rf");

observed <- testnamnozica$O3Class;
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

