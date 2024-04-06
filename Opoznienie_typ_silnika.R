#opoznienia wg typu silnika
colnames(plane_data)[1] <- "TailNum"
engine_data <- plane_data[,c("TailNum", "engine_type")]


Data <- data.frame()
for (year in years) {
  year1 <- year[,c("TailNum","ArrDelay")]%>%
    left_join(engine_data, by = "TailNum")
  year1 <- year1%>%
    na.omit()%>%
    group_by(engine_type)%>%
    summarise(TotalDelay = sum(ArrDelay), NumberOfPlanes = n())
  year1 <- year1[!(year1$engine_type %in% c("None", "")),]
  Data <- rbind(Data, year1)
}
Data<- Data%>%
  group_by(engine_type)%>%
  summarise(TotalDelay = sum(TotalDelay), NumberOfPlanes = sum(NumberOfPlanes))
Data<-cbind(Data, Data[,"TotalDelay"]/Data[,"NumberOfPlanes"])
colnames(Data)[4] <- "AverageDelay"
Data <- arrange(Data, desc(AverageDelay))


ggplot(Data, aes(x = factor(engine_type, level = engine_type), y = AverageDelay)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(x = "Engine Type", y = "Average Delay (min)") +
  ggtitle("Average Delay by Engine Type") 

Data <- y2007%>%
  left_join(engine_data, by = "TailNum")
Data <- Data[,c("TailNum", "engine_type", "ArrDelay")]
Data<- Data%>%
  na.omit()%>%
  group_by(engine_type)%>%
  summarise(AverageDelay = mean(ArrDelay))%>%
  arrange(desc(AverageDelay))
Data <- Data[!(Data$engine_type %in% c("None", "")),]

ggplot(Data, aes(x = factor(engine_type, level = engine_type), y = AverageDelay)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(x = "Engine Type", y = "Average Delay (min)") +
  ggtitle("Average Delay by Engine Type in 2007") 



