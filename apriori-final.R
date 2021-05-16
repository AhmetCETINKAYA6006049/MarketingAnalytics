install.packages("arules")
install.packages("psych")
install.packages("xlsx")
install.packages("openxlsx")
install.packages("dplyr")

library(arules)
library(psych)
library(dplyr)
library(openxlsx)



dataset <- read.xlsx("online_retail_II.xlsx","Year 2009-2010")

# convert date to char
dataset$InvoiceDate.new <- format(as.POSIXct((dataset$InvoiceDate) * 86400, origin = "1999-12-30", tz = "GMT"), "%d-%m-%y %H:%M")
#convert date to POSIX
dataset$InvoiceDate.new <- dmy_hm(dataset$InvoiceDate.new)
View(dataset)
str(dataset)

#dataset to be used
#sıfırdan küçük değer içeren ve satış olmayan satırları(düzeltme, vergi iade vs)
#satış dışı stockcode kalemlerini çıkar (ADJUST,ADJUST2,BANK CHARGES,C2,D, MANUAL,POST,TEST001,TEST002)
# dplyr paketinde mutate fonksiyonu kullanarak

dataset2 <- dataset %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         Price = replace(Price, Price<=0, NA),
         Invoice = replace(Invoice, Invoice < "489434" | Invoice > "538171", NA),
         StockCode = replace(StockCode, 
                             StockCode == "ADJUST" |
                             StockCode == "ADJUST2"|
                             StockCode == "BANK CHARGES"|
                             StockCode == "C2"|
                             StockCode == "D"|
                             StockCode == "M"|
                             StockCode == "POST"|
                             StockCode == "TEST001" |
                             StockCode == "TEST002",NA))

#na.omit() ile NA satırları çıkar
dataset2 <- na.omit(dataset2)
summary(dataset2)
View(dataset2)

#tekil ürün sayısı
unique_product_count <- length(unique(dataset2$StockCode))
unique_product_count


#hangi ürün toplamda ne kadar satış yapıldı?
df_product_occurrence <- dataset2%>% 
                          count(Description)

View(df_product_occurrence)


#apriori başlangıç

# sadece invoice ve description gerekli olduğundan bu satırları iceren veriseti tanımla
dataset2_apr1 <- dataset2[c(1,3)]
View(dataset2_apr1)

# her bir invoicedeki ürünlerin descriptionlarını ayır
# ve her bir invoicedeki descriptionları tek satırda virgülle ayırarak invoice bazında düzenle
dataset2_apr2 <- dataset2_apr1 %>%
  group_by(Invoice)%>% 
  arrange(Description) %>%
  summarise(products_purchased  = paste(Description, collapse =","))

View(dataset2_apr2)


# aprioride bunu transaction olarak almak için dataseti csv dosyasına dönüştür ve
# read.transaction() ile aprori için uygun formata çevir.
dataset2_apr3 <- dataset2_apr2[2]
View(dataset2_apr3)
# write.csv() yaptığımızda column names-header lar ürün olarak geliyor, 
# bunu engellemek için write.table() fonk kullanılır.
write.table(dataset2_apr3,"retail_csv_apriori.csv", sep =",", col.names = FALSE, row.names =FALSE)

products <- read.transactions("retail_csv_apriori.csv", sep =",", format = "basket")

summary(products)


# ilk 3 transaction ı göster 
inspect(products[1:3])

# to view the support level for the first 3 alphabetical items in the retail sales data:
itemFrequency(products[,1:3])

# itemFrequencyPlot() function allows us to produce a bar chart depicting the proportion of transactions.
itemFrequencyPlot(products, support = 0.001)

# to show the top N products purchased
itemFrequencyPlot(products, topN = 10)

# use the apriori with default support = 0.1 and confidence = 0.8
rules <- apriori(products)
summary(rules)

# We didnt get any results we must define a lowe suport level and lower confidence
# minlen shows the mininum length of together bought items (frequent itemset)

productsRules  <- apriori(products, parameter = list( support = 0.0002, confidence = 0.80, minlen =2))
summary(productsRules)

#lift is important, higher is more common purchased
inspect(productsRules[1:10])

# lift is important, coverage is  the proportion of :  lefthand side itemset / total transactions
inspect(sort(productsRules, by = "lift")[1:10])

# rules of a product is gathered by subset() function
hangingChickGreenRules <- subset(productsRules, items %in% "HANGING CHICK  GREEN")

inspect(hangingChickGreenRules)



















