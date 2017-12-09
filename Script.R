
rm(list = ls())

getSeason <- function(DATES) {
    WS <- as.Date("12-21", format = "%m-%d") # Winter Solstice
    SE <- as.Date("3-21",  format = "%m-%d") # Spring Equinox
    SS <- as.Date("6-21",  format = "%m-%d") # Summer Solstice
    FE <- as.Date("9-23",  format = "%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    d <- as.Date(DATES, format="%m-%d")

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Autumn")))
}

getO3concentration <- function(d) {
     ifelse (d >= 0 & d < 60, "Nizka",
      ifelse (d >= 60 & d < 120, "Srednja",
        ifelse (d >= 120 & d < 180, "Visoka", "Ekstremna")))
}

# -------------------------------------------------------------------------------------------------- Funkcije / data setup

data = read.table("podatkiSem1.txt", header = TRUE, sep = ",");
data$Datum = as.Date(data$Datum);
data$Weekday = weekdays(data$Datum);
data$Year = format(data$Datum, "%Y");
data$Datum = format(data$Datum, "%m-%d");
data$O3Level = getO3concentration(data$O3);
data$Season = getSeason(data$Datum);


data$O3 = NULL;
data$Glob_sevanje_min = NULL;
data$Datum = NULL;

ucnamnozica = data[data$Year <= "2014",];
testnamnozica = data[data$Year > "2014",];
library("CORElearn");

#----------------------------------------------------------------------------------- data setup / ocenjevanje atributov

sort(attrEval(O3Level ~ ., ucnamnozica, "InfGain"), decreasing = TRUE)




#----------------------------------------------------------------------------------- ocenjevanje atributov / predikcija


dt <- rpart(O3Level ~ Season + Postaja + Weekday, data = ucnamnozica)


#----------------------------------------------------------------------------------- predikcija / ostalo
nrow(ucnamnozica);
table(ucnamnozica$O3Level);

nrow(testnamnozica);
table(testnamnozica$O3Level);

library(rpart);
ucnamnozica$Year = NULL
testnamnozica$Year = NULL


dt <- rpart(O3Level ~ Season + Postaja + Weekday, data = ucnamnozica)

plot(dt);
text(dt, pretty = 0);
observed <- testnamnozica$O3Level
observed

predicted <- predict(dt, testnamnozica, type = "class")
predicted

t <- table(observed, predicted)
t

sum(diag(t)) / sum(t)
ucnamnozica$Glob_sevanje_min = NULL
