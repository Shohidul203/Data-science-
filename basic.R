install.packages("tibble")

library(tibble)

id<-c(101, 102, 103)
age<-c(31, 33, 36)
course<-c("c++" ,"r", "java")
stdata<- data.frame(id, cg,course)

#add columns
semester<-c(8,9,10)
stdata<-add_column(stdata, semester, .after =2)
stdata

#add column
stdata<- cbind(stdata, name= c("sojib", "md.", "shohidul"))

#delete ccolumn
stdata <- stdata[-c(5)]



#list

g<- "my first list"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", " two", "three")
mylist <- list(title = g, ages =h, j, k)
mylist





#take input

var1 = readline(prompt = "hey give your name : ")
var2 = readline(prompt = "hey give your age : " )
var2 = as.integer(var2)
print(var1)
print(var2)

#take input with scan

x= scan()

print(x)






#edit function

mydata <- data.frame(age = numeric(0), gender= character(0), weight= numeric(0))
mydata <- edit(mydata)
mydata






#import excel(csv) file

#mydataframe<- read.csv(file, header =logical_value, sep="delimiter")

myexceldata <- read.csv("C:/datasci/employees.csv", header=TRUE , sep= ",")

myexceldata

#show attribute name
names(myexceldata)



myexdata<- cbind(myexceldata, gender= c())

#delete data
myexdata <- myexdata[-c(9)]
myexdata

myexdata<- cbind(myexceldata, gender= c("male", "female"))
myexdata

#level korte annotating datasets use korte hy (char to numeric)

myexdata$gender <- factor(myexdata$gender,levels=c("male", "female"), labels=c(1,2))


#show imported datatype
str(myexdata)


#show descriptive statistics value
summary(myexdata)

#standard deviation

s<- myexdata$SALARY
sd(s)

# standard deviation for multiple column


install.packages("dplyr")
library(dplyr)

myexdata%>% summarise_if(is.numeric, sd)

#add data in missing value
myexdata[] = lapply(myexdata, sub, pattern = " ", replacement = "10", fixed = TRUE)

myexdata

#count missing values
sum(is.na(myexdata$SALARY))

#for all attribute

colSums(is.na(myexdata))


#add data in missing value for specific column
which(is.na(myexdata&SALAR;))

myexdata


#remove missing values
myexdata<-na.omit()

myexdata