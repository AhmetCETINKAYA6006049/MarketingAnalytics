subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
subject_name

temperature <- c(94, 98.6, 101.4)
temperature


flu_status <- c(FALSE, FALSE, TRUE)
flu_status

typeof(temperature)
typeof(flu_status)
typeof(subject_name)

temperature[2]
temperature[2:3]
temperature[-2]

temperature[c(TRUE, TRUE, FALSE)]

gender <- factor(c("MALE", "FEMALE", "MALE"))
gender

blood <- factor(c("0","AB", "A"),
                levels = c("A", "B", "AB", "0"))
blood[1:2]

symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),
                   levels = c("MILD", "MODERATE", "SEVERE"), 
                   ordered = TRUE)
symptoms > "MILD"

subject_name[1]
temperature[1]


subject1 <- list( fullname = subject_name[1],
                  temperature = temperature[1],
                  flu_status = flu_status[1],
                  gender = gender[1],
                  blood = blood[1],
                  symptoms = symptoms[1])

subject1[2]

subject1[[2]]

subject1$temperature

#dataframes

pt_data <- data.frame(subject_name, temperature, 
                      flu_status, gender, blood, symptoms, 
                      stringsAsFactors = FALSE)

str(pt_data)

pt_data$subject_name

pt_data$subject_name[2]

pt_data(c("temperature", "flu_status"))

pt_data[1,2]

#1. ve 3. satırın 2. ve 4.sütunu yazdır
pt_data[c(1,3),c(2,4)]

#1.sütunu yazdır
pt_data[,1]

#1. satırı yazdır
pt_data[1, ]

#belirli sütunları ve satırları çıkar
pt_data[-2, c(-1,-3,-5,-6)]

#enviromenttaki data ve valueları göster
ls()

#başlıkları olduğu için row.nmes =FALSE
write.csv(pt_data, file ="pt_data.csv", row.names = FALSE)

ds2 <- read.csv("pt_data.csv", stringsAsFactors =FALSE)
ds2

insurance <- read.csv("insurance.csv", stringsAsFactors = FALSE)
str(insurance)
View(insurance)

summary(insurance$age)

summary(insurance[c("age","bmi")])
range(insurance$age)
var(insurance$charges)
sd(insurance$charges)

#quantile
IQR(insurance$charges)
quantile(insurance$charges)
quantile(insurance$charges, prob = c(0.01, 0.99))

boxplot(insurance$charges, main = "Boxplot of Insurance Charges", ylab = "Charge")

hist(insurance$charges, main = "Histogram of Insurance Charges", xlab = "Charge")

table(insurance$sex)
prop.table(table(insurance$sex))

sex_table <- table(insurance$sex)
sex_pct <- prop.table(table(insurance$sex)) * 100
round(sex_pct, digits = 1)

round(prop.table(table(insurance$sex)) * 100, digits = 1)

plot(x = insurance$age, y = insurance$charges,
     main = "Scatterplot of Age vs. Charges",
     xlab = "age",
     ylab = "charge")










