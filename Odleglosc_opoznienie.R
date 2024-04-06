#czy odleglosc wplywa na opoznienie

Data1987 <- y1987[, c("Distance", "ArrDelay")]
rm(y1987)
Data1988 <- y1988[, c("Distance", "ArrDelay")]
rm(y1988)
Data1989 <- y1989[, c("Distance", "ArrDelay")]
rm(y1989)
Data1990 <- y1990[, c("Distance", "ArrDelay")]
rm(y1990)
Data1991 <- y1991[, c("Distance", "ArrDelay")]
rm(y1991)
Data1992 <- y1992[, c("Distance", "ArrDelay")]
rm(y1992)
Data1993 <- y1993[, c("Distance", "ArrDelay")]
rm(y1993)
Data1994 <- y1994[, c("Distance", "ArrDelay")]
rm(y1994)
Data1995 <- y1995[, c("Distance", "ArrDelay")]
rm(y1995)
Data1996 <- y1996[, c("Distance", "ArrDelay")]
rm(y1996)
Data1997 <- y1997[, c("Distance", "ArrDelay")]
rm(y1997)
Data1998 <- y1998[, c("Distance", "ArrDelay")]
rm(y1998)
Data1999 <- y1999[, c("Distance", "ArrDelay")]
rm(y1999)
Data2000 <- y2000[, c("Distance", "ArrDelay")]
rm(y2000)
Data2001 <- y2001[, c("Distance", "ArrDelay")]
rm(y2001)
Data2002 <- y2002[, c("Distance", "ArrDelay")]
rm(y2002)
Data2003 <- y2003[, c("Distance", "ArrDelay")]
rm(y2003)
Data2004 <- y2004[, c("Distance", "ArrDelay")]
rm(y2004)
Data2005 <- y2005[, c("Distance", "ArrDelay")]
rm(y2005)
Data2006 <- y2006[, c("Distance", "ArrDelay")]
rm(y2006)
Data2007 <- y2007[, c("Distance", "ArrDelay")]
rm(y2007)
Data2008 <- y2008[, c("Distance", "ArrDelay")]
rm(y2008)


Data <- rbind(Data1987, Data1988, Data1989, Data1990, Data1991, Data1992, Data1993, Data1994, Data1995, Data1996,
              Data1997, Data1998, Data1999, Data2000, Data2001, Data2002, Data2003, Data2004,
              Data2005, Data2006, Data2007, Data2008)
Data[,1]<-round_any(Data[,1], 10)

unloadNamespace('plyr')

PlotData <- Data %>%
  group_by(Distance)%>%
  na.omit()%>%
  summarise(AverageDelay = mean(ArrDelay))%>%
  na.omit()
PlotData<-PlotData[-1,]


ggplot(PlotData, aes(x = Distance, y = AverageDelay)) +
  geom_point() +
  labs(title = "Average Delay by Distance", x = "Distance (miles)", y = "Average Delay (min)")

library('plyr')
PlotData <- Data1993
PlotData[,1] <- round_any(PlotData[,1],10)
unloadNamespace('plyr')
PlotData <- PlotData%>%
  group_by(Distance)%>%
  na.omit()%>%
  summarise(AverageDelay = mean(ArrDelay))%>%
  na.omit()

ggplot(PlotData, aes(x = Distance, y = AverageDelay)) +
  geom_point() +
  labs(title = "Average Delay by Distance in 1993", x = "Distance (miles)", y = "Average Delay (min)")
