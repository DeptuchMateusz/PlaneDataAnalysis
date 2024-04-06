
#opoznienie wylotu w zaleznosci od lotniska
Data <- data.frame()
for (year in years) {
  year1 <- year[,c("Year", "Origin", "DepDelay")]%>%
    na.omit()%>%
    group_by(Origin)%>%
    summarise(TotalDelay = sum(DepDelay), NumberOfFlights = n())
  Data <- rbind(Data, year1)
}
Data <- Data%>%
  group_by(Origin)%>%
  summarise(TotalDelay = sum(TotalDelay), TotalNumberOfFlights = sum(NumberOfFlights))
Data <- (cbind(Data, Data[,2]/Data[,3]))[,c(1,4)]
colnames(Data) <-c( "iata", "AverageDelay")

rownames(Data) <- NULL

Data <- Data%>%
  left_join(airports , by = "iata")

PlotData <- head(Data, 10)
ggplot(PlotData, aes(x = factor(airport,level = airport), y = AverageDelay)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(x = "Airport", y = "Average Delay (min)") +
  ggtitle("Average Delay by Airport (top 10)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

PlotData <- tail(Data, 10)
ggplot(PlotData, aes(x = factor(airport,level = airport), y = AverageDelay)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(x = "Airport", y = "Average Delay (min)") +
  ggtitle("Average Delay by Airport (bottom 10)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(Data, aes(x = factor(airport,level = airport), y = AverageDelay))+
  geom_point(color = "purple", size = 0.8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  labs(x = "Airports", y = "Average Delay") + 
  ggtitle("Average Delay by Airport")

Data <- Data[order(Data$AverageDelay, decreasing  = TRUE),]
ggplot(Data, aes(x = factor(airport,level = airport), y = AverageDelay))+
  geom_point(color = "purple", size = 0.8)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  labs(x = "Airports", y = "Average Delay") + 
  ggtitle("Average Delay by Airport")




        