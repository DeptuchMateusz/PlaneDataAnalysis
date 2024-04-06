#ile % opoznionych samolotow wg dnia tygodnia
Data <- y2006%>%
  group_by(DayOfWeek)%>%
  summarise(NumberOfFlights = n(), NumberOfDelayedFlights = sum(ArrDelay>0, na.rm = TRUE))
Data <- cbind(Data, Data[,3]/Data[,2]*100)
colnames(Data)[4] <- c("PercentageOfDelayedFlights")

ggplot(Data, aes(x = factor(weekday_order[DayOfWeek], level = weekday_order), y = PercentageOfDelayedFlights)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Percentage of delayed flights by Day of Week in 2006", x = "Day of Week", y = "Percentage of delayed flights (%)")




years <- list(y1987,y1988,y1989,y1990,y1991,y1992,y1993,y1994,y1995,y1996,y1997,y1998,y1999,y2000,y2001,y2001,y2002,y2003,y2004,y2005,y2006,y2007,y2008)
Data<- data.frame(matrix(c(1:7,0,0,0,0,0,0,0,0,0,0,0,0,0,0),ncol = 3))

colnames(Data) <- c("DayOfWeek", "NumberOfFlights", "NumberOfDelayedFlights")

for (year in years) {
  year <- year%>%
    group_by(DayOfWeek)%>%
    summarise(NumberOfFlights = n(), NumberOfDelayedFlights = sum(ArrDelay>0, na.rm = TRUE))
  
  Data <- merge(Data, year, by = "DayOfWeek")
  Data$NumberOfFlights <- Data$NumberOfFlights.x + Data$NumberOfFlights.y
  Data$NumberOfDelayedFlights <- Data$NumberOfDelayedFlights.x + Data$NumberOfDelayedFlights.y
  Data <- Data[,c("DayOfWeek", "NumberOfFlights", "NumberOfDelayedFlights")]
  
}
Data <- cbind(Data, Data[3]/Data[2]*100)
colnames(Data)[4] <- c("PercentageOfDelayedFlights")
ggplot(Data, aes(x = factor(weekday_order[DayOfWeek], level = weekday_order), y = PercentageOfDelayedFlights)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Percentage of Delayed Flights by Day of Week", x = "Day of Week", y = "Percentage of delayed flights (%)")

