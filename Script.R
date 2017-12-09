data = read.table("podatkiSem1.txt", header = TRUE, sep = ",");
data$Datum = as.Date(data$Datum);
ucnamnozica = data[data$Datum <= as.Date("2014-12-31")];
testnamnozica = data[data$Datum > as.Date("2014-12-31")];