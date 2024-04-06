#wczytanie danych i bibliotek
library("data.table")
library("dplyr")
library("ggplot2")
library("plyr")


airports <- read.csv("~/RStudio/PDU/PD4/airports.csv")
carriers <- read.csv("~/RStudio/PDU/PD4/carriers.csv")
plane_data <- read.csv("~/RStudio/PDU/PD4/plane-data.csv")
for (year in 1987:2008) {
  file_name <- paste0("~/RStudio/PDU/PD4/", year)
  file_name <- paste0(file_name, ".csv")
  var_name <- paste0("y", year)
  assign (var_name, read.csv(file_name))
} 
years <- list(y1987,y1988,y1989,y1990,y1991,y1992,y1993,y1994,y1995,y1996,y1997,y1998,y1999,y2000,y2001,y2001,y2002,y2003,y2004,y2005,y2006,y2007,y2008)
weekday_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")









