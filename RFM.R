install.packages("xlsx")
install.packages("NbClust")
install.packages("factoextra")

library(openxlsx)
library(factoextra)
library(dplyr)

CustomerRFM <- read.xlsx("online_retail_II.xlsx","Year 2009-2010")

# tarih numeric olduğundan char a çevir
CustomerRFM$InvoiceDate.new <- format(as.POSIXct((CustomerRFM$InvoiceDate) * 86400, origin = "1999-12-30", tz = "GMT"), "%d-%m-%y %H:%M")
#sonra Date e çevir
CustomerRFM$InvoiceDate.new <- dmy_hm(CustomerRFM$InvoiceDate.new)
View(CustomerRFM)


CustomerRFM <- CustomerRFM %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         Price = replace(Price, Price<=0, NA))


CustomerRFM <- na.omit(CustomerRFM)
summary(CustomerRFM)


CustomerRFM <- CustomerRFM %>% 
  mutate(Invoice=as.factor(Invoice), StockCode=as.factor(StockCode), 
         Customer.ID=as.factor(Customer.ID), 
         Country=as.factor(Country))


CustomerRFM2 <- CustomerRFM %>% 
  mutate(total_dolar = Quantity*Price)


baslangic = as.Date(max(CustomerRFM2$InvoiceDate.new), "%d-%m-%y")

bitis = as.Date("01-11-2010 00:00", "%d-%m-%Y")

recency = bitis - baslangic
recency



CustomerRFM2 <- CustomerRFM2 %>% 
  group_by(Customer.ID) %>% 
  summarise(recency=as.Date("01-01-2012 00:00", "%d-%m-%Y") - as.Date(max(InvoiceDate.new), "%d-%m-%y"),
            frequency=n_distinct(Invoice), monitery= sum(total_dolar)/n_distinct(Invoice)) 


summary(CustomerRFM2)
