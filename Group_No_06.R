
mydata<-read.csv("D:/data sci/water_potability.csv",header=TRUE,sep=",")
str(mydata)

medianValueOfPH<-median(mydata$ph,na.rm = TRUE)
mydata$ph[is.na(mydata$ph)]<-medianValueOfPH
medianValueOfSulfate<-median(mydata$Sulfate,na.rm = TRUE)
mydata$Sulfate[is.na(mydata$Sulfate)]<-medianValueOfSulfate
medianValueOfTrihalomethanes<-median(mydata$Trihalomethanes,na.rm = TRUE)
mydata$Trihalomethanes[is.na(mydata$Trihalomethanes)]<-medianValueOfTrihalomethanes

mydata$Potability<-as.factor(mydata$Potability)
mydata$Potability<-factor(mydata$Potability,levels = c(1,0),labels = c("YES","NO"))
hist(mydata$ph, main="HISTOGRAM OF PH", xlab="PH", ylab="FREQUENCY", col="skyblue", border="black")
hist(mydata$Hardness, main="HISTOGRAM OF HARDNESS", xlab="HARDNESS", ylab="FREQUENCY", col="skyblue", border="black")
hist(mydata$Solids, main="HISTOGRAM OF SOLIDS", xlab="SOLIDS", ylab="FREQUENCY", col="skyblue", border="black")
hist(mydata$Chloramines, main="HISTOGRAM OF CHLORAMINES", xlab="CHLORAMINES", ylab="FREQUENCY", col="skyblue", border="black")
hist(mydata$Sulfate, main="HISTOGRAM OF SULFATE", xlab="SULFATE", ylab="FREQUENCY", col="skyblue", border="black")
hist(mydata$Conductivity, main="HISTOGRAM OF CONDUCTIVITY", xlab="CONDUCTIVITY", ylab="FREQUENCY", col="skyblue", border="black")
hist(mydata$Organic_carbon, main="HISTOGRAM OF ORGANIC CARBON", xlab="ORGANIC CARBON", ylab="FREQUENCY", col="skyblue", border="black")
hist(mydata$Trihalomethanes, main="HISTOGRAM OF TRIHALOMETHANES", xlab="TRIHALOMETHANES", ylab="FREQUENCY", col="skyblue", border="black")
hist(mydata$Turbidity, main="HISTOGRAM OF TURBIDITY", xlab="TURBIDITY", ylab="FREQUENCY", col="skyblue", border="black")
barplot(table(mydata$Potability), main="BAR PLOT OF POTABILITY", xlab="POTABILITY", ylab="FREQUENCY", col="skyblue")
pie(table(mydata$Potability), main="PIE CHART OF POTABILITY", col=rainbow(length(unique(mydata$Potability))))
library(ggplot2)



boxplot(mydata$ph, main = "BOXPLOT OF PH", xlab = "PH")

boxplot(mydata$Hardness, main = "BOXPLOT OF HARDNESS", xlab = "HARDNESS")
boxplot(mydata$Solids, main = "BOXPLOT OF SOLIDS", xlab = "SOLIDS")
boxplot(mydata$Chloramines, main = "BOXPLOT OF CHLORAMINES", xlab = "CHLORAMINES")
boxplot(mydata$Sulfate, main = "BOXPLOT OF SULFATE", xlab = "SULFATE")
boxplot(mydata$Conductivity, main = "BOXPLOT OF CONDUCTIVITY", xlab = "CONDUCTIVITY")
boxplot(mydata$Organic_carbon, main = "BOXPLOT OF ORGANIC CARBON", xlab = "ORGANIC CARBON")
boxplot(mydata$Trihalomethanes, main = "BOXPLOT OF TRIHALOMETHANES", xlab = "TRIHALOMETHANES")
boxplot(mydata$Turbidity, main = "BOXPLOT OF TURBIDITY", xlab = "TURBIDITY")


densityOfPh <- density(mydata$ph)
plot(densityOfPh, main="DENSITY PLOT OF PH", xlab="PH", ylab="DENSITY", col="darkred")

densityOfHardness<-density(mydata$Hardness)
plot(densityOfHardness, main="DENSITY PLOT OF HARDNESS", xlab="HARDNESS", ylab="DENSITY", col="darkred")

densityOfSolids<-density(mydata$Solids)
plot(densityOfSolids, main="DENSITY PLOT OF SOLIDS", xlab="SOLIDS", ylab="DENSITY", col="darkred")

densityOfChloramin<-density(mydata$Chloramines)
plot(densityOfChloramin, main="DENSITY PLOT OF CHLORAMINES", xlab="CHLORAMINES", ylab="DENSITY", col="darkred")

densityOfSulfate<-density(mydata$Sulfate)
plot(densityOfSulfate, main="DENSITY PLOT OF SULFATE", xlab="SULFATE", ylab="DENSITY", col="darkred")

densityOfConductivity<-density(mydata$Conductivity)
plot(densityOfConductivity, main="DENSITY PLOT OF CONDUCTIVITY", xlab="CONDUCTIVITY", ylab="DENSITY", col="darkred")


densityOfOrganic<-density(mydata$Organic_carbon)
plot(densityOfOrganic, main="DENSITY PLOT OF ORGANIC CARBON", xlab="ORGANIC CARBON", ylab="DENSITY", col="darkred")

densityOfTrihalomethanes<-density(mydata$Trihalomethanes)
plot(densityOfTrihalomethanes, main="DENSITY PLOT OF TRIHALOMETHANES", xlab="TRIHALOMETHANES", ylab="DENSITY", col="darkred")

densityOfTurbidity<-density(mydata$Turbidity)
plot(densityOfTurbidity, main="DENSITY PLOT OF TURBIDITY", xlab="TURBIDITY", ylab="DENSITY", col="darkred")



ggplot(mydata, aes(x = ph, y = Solids)) +
  geom_point() +
  labs(x = "PH", y = "SOLIDS", title = "SCATTER PLOT PH VS SOLIDS")

ggplot(mydata, aes(x = ph, y = Solids)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "PH", y = "SOLIDS", title = "WITH REGRESSION LINE")


ggplot(mydata, aes(x = Hardness, y = Solids)) +
  geom_point() +
  labs(x = "HARDNESS", y = "SOLIDS", title = "SCATTER PLOT OF HARDNESS VS SOLIDS")

ggplot(mydata, aes(x = Hardness, y = Solids)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "HARDNESS", y = "SOLIDS", title = "WITH REGRESSION LINE")

ggplot(mydata, aes(x = Sulfate, y = Organic_carbon)) +
  geom_point() +
  labs(x = "SULFATE", y = "ORGANIC CARBON", title = "SCATTER PLOT OF SULFATE VS ORGANIC CARBON")

ggplot(mydata, aes(x = Sulfate, y = Organic_carbon)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "SULFATE", y = "ORGANIC CARBON", title = "WITH REGRESSION LINE")



  ggplot(mydata, aes(x = Chloramines, y = Trihalomethanes)) +
  geom_point() +
  labs(x = "CHLORAMINES", y = "TRIHALOMETHANES", title = "SCATTER PLOT OF CHLORAMINES VS TRIHALOMETHANES")


  ggplot(mydata, aes(x = Chloramines, y = Trihalomethanes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "CHLORAMINES", y = "TRIHALOMETHANES", title = "WITH REGRESSION LINE")




ggplot(mydata, aes(x = ph, y = Conductivity)) +
  geom_point() +
  labs(x = "PH", y = "CONDUCTIVITY", title = "SCATTER PLOT OF PH VS CONDUCTIVITY")

 ggplot(mydata, aes(x = ph, y = Conductivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "PH", y = "CONDUCTIVITY", title = "WITH REGRESSION LINE")



ggplot(mydata, aes(x = Solids, y = Chloramines)) +
  geom_point() +
  labs(x = "SOLIDS", y = "Chloramines", title = "SCATTER PLOT OF SOLIDS VS Chloramines")

ggplot(mydata, aes(x = Solids, y = Chloramines)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "SOLIDS", y = "CHLORAMINES", title = "WITH REGRESSION LINE")


ggplot(mydata, aes(x = Solids, y = Turbidity)) +
  geom_point() +
  labs(x = "SOLIDS", y = "TURBIDITY", title = "SCATTER PLOT OF SOLIDS VS TURBIDITY")

ggplot(mydata, aes(x = Solids, y = Turbidity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "SOLIDS", y = "TURBIDITY", title = "WITH REGRESSION LINE")



ggplot(mydata, aes(x = Solids, y = Conductivity)) +
  geom_point() +
  labs(x = "SOLIDS", y = "CONDUCTIVITY", title = "SCATTER PLOT OF SOLIDS VS CONDUCTIVITY")

ggplot(mydata, aes(x = Solids, y = Conductivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "SOLIDS", y = "CONDUCTIVITY", title = "WITH REGRESSION LINE")




ggplot(mydata, aes(x = Solids, y = Organic_carbon)) +
  geom_point() +
  labs(x = "SOLIDS", y = "ORGANIC CARBON", title = "SCATTER PLOT OF SOLIDS VS ORGANIC CARBON")

ggplot(mydata, aes(x = Solids, y = Organic_carbon)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "SOLIDS", y = "ORGANIC CARBON", title = "WITH REGRESSION LINE")




ggplot(mydata, aes(x = Conductivity, y = Turbidity)) +
  geom_point() +
  labs(x = "CONDUCTIVITY", y = "Turbidity", title = "SCATTER PLOT OF CONDUCTIVITY VS Turbidity")

ggplot(mydata, aes(x = Conductivity, y =Turbidity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "CONDUCTIVITY", y = "TURBIDITY", title = "WITH REGRESSION LINE")




ggplot(mydata, aes(x = Chloramines, y = Turbidity)) +
  geom_point() +
  labs(x = "CHLORAMINES", y = "TURBIDITY", title = "SCATTER PLOT OF CHLORAMINES VS TURBIDITY")

ggplot(mydata, aes(x = Chloramines, y =Turbidity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "CHLORAMINES", y = "TURBIDITY", title = "WITH REGRESSION LINE")




ggplot(mydata, aes(x = ph, y = Hardness)) +
  geom_point() +
  labs(x = "PH", y = "HARDNESS", title = "SCATTER PLOT OF PH VS HARDNESS")

ggplot(mydata, aes(x = ph, y = Hardness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "PH", y = "HARDNESS", title = "WITH REGRESSION LINE")


library(reshape2)
numeric_data <- mydata[, sapply(mydata, is.numeric)]
correlation_matrix <- cor(numeric_data)

melted_correlation <- melt(correlation_matrix)


 ggplot(melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "orange", high = "red") +
  labs(title = "Heatmap of Correlation Matrix")



ggplot(melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black") +
  scale_fill_gradient(low = "orange", high = "red") +
  labs(title = "HEATMAP WITH CORRELATION VALUES")


numeric_data <- mydata[, c("ph", "Hardness", "Solids", "Chloramines", "Sulfate", "Conductivity", "Organic_carbon", "Trihalomethanes", "Turbidity")]
pairs(numeric_data)





dp <- ggplot(mydata, aes(x=Potability, y=ph, fill=Potability)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="PLOT OF PH BY POTABILITY",x="POTABILITY", y = "PH")
dp + theme_classic()



dp <- ggplot(mydata, aes(x=Potability, y=Hardness, fill=Potability)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="PLOT OF HARDNESS BY POTABILITY",x="POTABILITY", y = "HARDNESS")
dp + theme_classic()



dp <- ggplot(mydata, aes(x=Potability, y=Solids, fill=Potability)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="PLOT OF SOLIDS BY POTABILITY",x="POTABILITY", y = "SOLIDS")
dp + theme_classic()



dp <- ggplot(mydata, aes(x=Potability, y=Chloramines, fill=Potability)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="PLOT OF CHLORAMINES BY POTABILITY",x="POTABILITY", y = "CHLORAMINES")
dp + theme_classic()



dp <- ggplot(mydata, aes(x=Potability, y=Conductivity, fill=Potability)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="PLOT OF CONDUCTIVITY BY POTABILITY",x="POTABILITY", y = "CONDUCTIVITY")
dp + theme_classic()



dp <- ggplot(mydata, aes(x=Potability, y=Sulfate, fill=Potability)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="PLOT OF SULFATE BY POTABILITY",x="POTABILITY", y = "SULFATE")
dp + theme_classic()



dp <- ggplot(mydata, aes(x=Potability, y=Organic_carbon, fill=Potability)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="PLOT OF ORGANIC CARBON BY POTABILITY",x="POTABILITY", y = "ORGANIC CARBON")
dp + theme_classic()

dp <- ggplot(mydata, aes(x=Potability, y=Trihalomethanes, fill=Potability)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="PLOT OF TRIHALOMETHANES BY POTABILITY",x="POTABILITY", y = "TRIHALOMETHANES")
dp + theme_classic()


