install.packages("rvest")
   
library(rvest)
install.packages("dplyr")
library(dplyr)
productTitle<-list()
previousPrice<-list()
currentPrice<-list()
remainingProduct<-list()

linkCollect<-read_html("https://evaly.com.bd/")
product<-linkCollect %>% html_nodes(".card-daydeal") %>% html_text()
product
productTitle<-linkCollect %>% html_nodes(".title") %>% html_text()
productTitle
previousPrice<- linkCollect %>% html_nodes(".price-1") %>% html_text()
previousPrice
currentPrice<- linkCollect %>% html_nodes(".price-2") %>% html_text()
currentPrice
remainingProduct<-linkCollect %>% html_nodes(".text-\\[10px\\]") %>% html_text()
remainingProduct
max_length <- max(length(productTitle), length(previousPrice), length(currentPrice), length(remainingProduct))

productTitle <- c(productTitle, rep(NA, max_length - length(productTitle)))
previousPrice <- c(previousPrice, rep(NA, max_length - length(previousPrice)))
currentPrice <- c(currentPrice, rep(NA, max_length - length(currentPrice)))
remainingProduct <- c(remainingProduct, rep(NA, max_length - length(remainingProduct)))
finalInformation<-data.frame(productTitle,previousPrice,currentPrice,remainingProduct)
finalInformation
write.csv(finalInformation, file = "D:/data sci/webfinalproduct.csv", fileEncoding = "UTF-8")
