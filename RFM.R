install.packages("xlsx")
install.packages("NbClust")
install.packages("factoextra")

library(openxlsx)
library(factoextra)
library(dplyr)

CustomerRFM <- read.xlsx("online_retail_II.xlsx","Year 2009-2010")

# convert date to char
CustomerRFM$InvoiceDate.new <- format(as.POSIXct((CustomerRFM$InvoiceDate) * 86400, origin = "1999-12-30", tz = "GMT"), "%d-%m-%y %H:%M")
#convert date to POSIX
CustomerRFM$InvoiceDate.new <- dmy_hm(CustomerRFM$InvoiceDate.new)
View(CustomerRFM)


CustomerRFM <- CustomerRFM %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         Price = replace(Price, Price<=0, NA))


CustomerRFM <- na.omit(CustomerRFM)
summary(CustomerRFM)


CustomerRFM <- CustomerRFM %>% 
  mutate(Invoice=as.factor(Invoice), StockCode=as.factor(StockCode),
         Country=as.factor(Country))


CustomerRFM2 <- CustomerRFM %>% 
  mutate(total_dolar = Quantity*Price)


baslangic = as.Date(max(CustomerRFM2$InvoiceDate.new), "%d-%m-%y")

bitis = as.Date("01-12-2010 23:59", "%d-%m-%Y")

recency = bitis - baslangic
recency


CustomerRFM3 <- CustomerRFM2 %>% 
  group_by(Customer.ID) %>% 
  summarise(recency=as.numeric((as.Date("10-12-2010 00:00", "%d-%m-%Y") - as.Date(max(InvoiceDate.new), "%d-%m-%y"))),
            frequency=n_distinct(Invoice), monitery= sum(total_dolar)/n_distinct(Invoice)) 

View(CustomerRFM3)
str(CustomerRFM3)
summary(CustomerRFM3)


# scaling uzing z-score standardisation
CustomerRFM3.scaled <- scale(CustomerRFM3)
View(CustomerRFM3.scaled)

# k-means clustering
set.seed(1234)
cust_clustering <- kmeans(CustomerRFM3.scaled, 5)

cust_clustering$size

cust_cluster <- cust_clustering$cluster
table(cust_cluster)

#with hclust function compute pairwise distance matrices 
dist.res <- dist(CustomerRFM3.scaled, method ="euclidean")
cust_clustering <- hclust(dist.res, method ="ward.D")
plot(cust_clustering, labels = FALSE, hang = -1)

# highlight clusters on the dendrogram based on cut-off distance h is the distance height
rect.hclust(cust_clustering, h = 300)

# or more detailed
# add a rectangle around 3 groups, border means show each segment using a different colour
rect.hclust(cust_clustering, k = 7, border = 2:9)

cust_cluster <-cutree(cust_clustering, k=7)
table(cust_cluster)

# show the statistical results

#show the cluster of each customer
cust_clusters <- data.frame(CustomerRFM3$Customer.ID, cust_cluster)
View(cust_clusters)

cust_features.statistics <- aggregate(CustomerRFM3[c(-1)], by = list(cluster = cust_cluster), mean)
cust_features.statistics$N <- as.numeric(table(cust_cluster))
View(cust_features.statistics)
















