# Makale okumada İstanbul ve çevresinde hizmet veren bir perakende firması için 
# kümeleme, birliktelik kuralları, pazar sepet analizi ve segmentasyon konusunu ele alan
# Retail Store Segmentation for Target Marketing (Bilgiç et al.) makalesini okumuştum.
# Bu makaledeki bilgileri ele almak için veristeri araştırması yaptım.
# University of California at Irvine Makine Öğrenmesi Deposu'ndan 
# London SOuth Bank University Data Science Bölümünden Dr. Chen in yüklemiş olduğu 
# online retail verisetini seçtim.
# İlgili araştırmacılar RFM uygulamaları gerçekleştirmişler.
# Ben de R ile apriori il birliktelik kuralları örnek uygulaması yapacağım.


install.packages("arules")
install.packages("psych")
install.packages("xlsx")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("lubridate")

library(arules)
library(psych)
library(dplyr)
library(openxlsx)
library(lubridate)


dataset <- read.xlsx("online_retail_II.xlsx","Year 2009-2010")
View(dataset)

#Tarih Sütununu Date formatında göstermek için
# önce POSIX e çevir (Date tipi char yap)
dataset$InvoiceDate.new <- format(as.POSIXct((dataset$InvoiceDate) * 86400, origin = "1999-12-30", tz = "GMT"), "%d-%m-%y %H:%M")

View(dataset)
str(dataset)
# sonra Date e çevir -> lubridate library altında dmy_hm() fonksiyonu
dataset$InvoiceDate.new <- dmy_hm(dataset$InvoiceDate.new)

View(dataset)
str(dataset)

#istatistiksel bilgiler
summary(dataset)


# psych library den describe()'da
# trimmed mean,aşırı değerler düşüldükten sonraki ortalamadır.
# yanında * olan özellikler faktördür. sayısal değildir.
describe(dataset)

#datasetinden kullanmayacağımız değerli çıkar
#sıfırdan küçük değer içeren ve satış olmayan satırları(düzeltme, vergi, iade vs)
# dplyr paketinde mutate fonksiyonu kullanarak
#satış dışı stockcode kalemlerini çıkar (ADJUST,ADJUST2,BANK CHARGES,C2,D, MANUAL,POST,TEST001,TEST002)


temizlenmis_dataset <- dataset %>% 
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
View(temizlenmis_dataset)

#na.omit() ile NA satırları çıkar
temizlenmis_dataset <- na.omit(temizlenmis_dataset)
View(temizlenmis_dataset)

summary(temizlenmis_dataset)


# kaç tane fatura var ( invoice )
invoice_count <- length(unique(temizlenmis_dataset$Invoice))
invoice_count

#tekil ürün sayısı
# 4434 adet tekil ürün olduğu için support(x) = pr(x)/tüm ürünler çok düşük çıkacaktır.
tekil_ürün_sayısı <- length(unique(temizlenmis_dataset$Description))
tekil_ürün_sayısı


#hangi ürün toplamda faturalarda kaç kez gözüktü?
df_ürün_gözükme <- temizlenmis_dataset%>% 
  count(Description)

View(df_ürün_gözükme)


#apriori başlangıç

# sadece invoice ve description(ürün stok adı)  gerekli olduğundan bu sütunları iceren veriseti tanımla
dataset_apriori <- temizlenmis_dataset[c(1,3)]
View(dataset_apriori)

# her bir faturadaki ürünleri tek satırda noktalı virgülle ayırarak fatura bazında düzenle
dataset_apriori2 <- dataset_apriori %>%
  group_by(Invoice)%>% 
  arrange(Description) %>%
  summarise(satin_alinan_urunler  = paste(Description, collapse =","))

View(dataset_apriori2)


# aprioride bunu transaction olarak almak için dataseti csv dosyasına dönüştür ve
# read.transaction() ile apriori için uygun formata çevir.
dataset_transaction <- dataset_apriori2[2]
View(dataset_transaction)

#Data Frame formatındaki listemizi kaydedelim
# write.csv() yaptığımızda column names-header lar birinci ürün olarak geliyor, 
# bunu engellemek için write.table() fonk kullanılır.
write.table(dataset_transaction,"retail_apriori.csv", sep =",", col.names = FALSE, row.names =FALSE)

# apriori için transactionları oku
products <- read.transactions("retail_apriori.csv", sep =",")

summary(products)

# ilk 3 transaction ı göster 
inspect(products[1:3])

# ilk üç ürünün support seviyesini göster
itemFrequency(products[,1:3])

# itemFrequencyPlot() function allows us to produce a bar chart depicting the proportion of transactions.
# %0.1 support seviyesinde rünlerin sıklıklarını göster
itemFrequencyPlot(products, support = 0.001)

# Ençok satın alınan 10 ürünü göster 
itemFrequencyPlot(products, topN = 10)

# varsayılan olarak support = 0.1 ve confidence = 0.8 için apriori uygula
rules <- apriori(products)
summary(rules)

# varssayılan  support-confidence için kural döndürmedi.
# daha düşük oranlar için tekrar deneyelim.

productsRules  <- apriori(products, parameter = list( support = 0.0002, confidence = 0.80, target = "rules"))
summary(productsRules)


# support(X => Y) X ve Y ürünlerini içerentransaction(işlem ) oranının toplam işlenlere oranıdır. support= Pr(X U Y) 
# confidence X => Y)X ve Y'yi içeren işlemlerin X i içeren işlemlere oranıdır.                    confidence= support(X U Y) / support(X)
# coverage sol taraftaki itemsetlerin toplam işlemlere oranıdır                                   coverage = lhs / total transactions(18966)
# lift k-itemset in support'unun, içindeki ürünlerin tekil supportlarının çarpımına oranıdır.     lift =support(X n Y) /[support(X)*support(Y)]
#inspect ile ilk 20 birliktelik kuralını görster
inspect(productsRules[1:20])

#lift i en yüksek 10 kuralı göster.
inspect(sort(productsRules, by = "lift")[1:10])

# bir ürünün kural seti arules libraryden subset() fonksiyonu ile gösterilir.
hangingChickCreamRules <- subset(productsRules, items %in% "HANGING CHICK CREAM")

inspect(hangingChickCreamRules)

# Teşekkürler