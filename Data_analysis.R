# Biblioteki
library(EnvStats) # do statystyk opisowych
library(plyr) # do count
library(tidyverse) # do %>%
library(ggstatsplot) # do outlierow
library(zoo)    # na.approx()
library(car)    # vif()
library(lmtest) # gqtest()
library(randtests) # dwtest()
library(aod)      # wald.test()
library(strucchange) # sctest()
library(MASS) #stepAIC()

# Pobranie danych
data<-read.csv2("C:/Users/FUJITSU/Desktop/Dokumenty/Studia/IiE 1 MGR - Metody ekonometryczne/Projekt/IiE20192020dataset7.csv")

# Usuniecie NA y
data<-data[!is.na(data$Y), ]

# Podzial danych
set.seed(297936)
rand <- sample(c(1:997), 249)

dataset<-data[-rand,]
dataset_test<-data[rand,]

# Statystyki opisowe
basic_stats <- matrix(NA,7,length(dataset[1,])-1)
row.names(basic_stats) <-c ("Mean", "Median", "Standard deviation", "Kurtosis","Skewness","Coefficient of variation", "Missings")
colnames(basic_stats) <- colnames(dataset[1:(length(dataset[1,])-1)])

for(i in 1:(length(dataset[1,])-1)) {
  basic_stats[1,i]<-mean(dataset[,i], na.rm = TRUE)
  basic_stats[2,i]<-median(dataset[,i], na.rm = TRUE)
  basic_stats[3,i]<-sd(dataset[,i], na.rm = TRUE)
  basic_stats[4,i]<-kurtosis(dataset[,i], na.rm = TRUE)
  basic_stats[5,i]<-skewness(dataset[,i], na.rm = TRUE)
  basic_stats[6,i]<-cv(dataset[,i], na.rm = TRUE)
  basic_stats[7,i]<-sum(is.na(dataset[,i]))
}
round(basic_stats,2)

cat_variable_stats<- dataset  %>% group_by(X7) %>% count()
cat_variable_stats

# Wykresy pudeĹ‚kowe
boxplot(dataset$Y, main = "Boxplot of Y")
par(mfrow = c(2,3))
boxplot(dataset$X1, main = "Boxplot of X1")
boxplot(dataset$X2, main = "Boxplot of X2")
boxplot(dataset$X3, main = "Boxplot of X3")
boxplot(dataset$X4, main = "Boxplot of X4")
boxplot(dataset$X5, main = "Boxplot of X5")
boxplot(dataset$X6, main = "Boxplot of X6")

# Histogramy
par(mfrow = c(1,1))
hist(dataset$Y, density = 20, breaks = 20, prob = TRUE, xlab = "y", ylim = c(0, 0.01), main = "Histogram of Y")
curve(dnorm(x, mean = mean(dataset$Y), sd = sqrt(var(dataset$Y))), col = "blue", lwd = 2, add = TRUE, yaxt = "n")
par(mfrow = c(2,3))
hist(dataset$X1, density = 20, breaks = 20, prob = TRUE, xlab = "x1", ylim = c(0, 0.01), main = "Histogram of X1")
curve(dnorm(x, mean = basic_stats[1,2], sd = basic_stats[3,2]), col = "blue", lwd = 2, add = TRUE, yaxt = "n")
hist(dataset$X2, density = 20, breaks = 20, prob = TRUE, xlab = "x2", ylim = c(0, 0.01), main = "Histogram of X2")
curve(dnorm(x, mean = basic_stats[1,3], sd = basic_stats[3,3]), col = "blue", lwd = 2, add = TRUE, yaxt = "n")
hist(dataset$X3, density = 20, breaks = 20, prob = TRUE, xlab = "x3", ylim = c(0, 0.01), main = "Histogram of X3")
curve(dnorm(x, mean = basic_stats[1,4], sd = basic_stats[3,4]), col = "blue", lwd = 2, add = TRUE, yaxt = "n")
hist(dataset$X4, density = 20, breaks = 20, prob = TRUE, xlab = "x4", ylim = c(0, 0.01), main = "Histogram of X4")
curve(dnorm(x, mean = basic_stats[1,5], sd = basic_stats[3,5]), col = "blue", lwd = 2, add = TRUE, yaxt = "n")
hist(dataset$X5, density = 20, breaks = 20, prob = TRUE, xlab = "x5", ylim = c(0, 0.01), main = "Histogram of X5")
curve(dnorm(x, mean = basic_stats[1,6], sd = basic_stats[3,6]), col = "blue", lwd = 2, add = TRUE, yaxt = "n")
hist(dataset$X6, density = 20, breaks = 20, prob = TRUE, xlab = "x6", ylim = c(0, 0.01), main = "Histogram of X6")
curve(dnorm(x, mean = basic_stats[1,7], sd = basic_stats[3,7]), col = "blue", lwd = 2, add = TRUE, yaxt = "n")

# Wykresy zaleznosci
par(mfrow=c(3,2))
for(i in 2:(length(dataset[1,])-1)){
plot<-ggplot(dataset, aes(x=dataset[,i],y=dataset[,1]))
plot<-plot+geom_point()+labs(title = paste("Dependency graph between ", colnames(dataset)[i], " and ", colnames(dataset)[1], sep=""),  x = colnames(dataset)[i],y=colnames(dataset)[1])+geom_smooth(method = "lm", se=F, color = "red")
print(plot)
}

plot<-ggplot(dataset, aes(x=dataset[,8],y=dataset[,1], fill=dataset[,8]))+geom_boxplot()+labs(title = "",  x = colnames(dataset)[8],y=colnames(dataset)[1])+labs(title = paste("Dependency graph between ", colnames(dataset)[8], " and ", colnames(dataset)[1], sep=""),  x = colnames(dataset)[8],y=colnames(dataset)[1]) +theme(legend.position = "none") 
print(plot)

# Transformacja wartosci NA jako mediane
for(i in 1:(length(dataset[1,])-1)){
  for(j in 1:length(dataset[,1])){
    if(is.na(dataset[j,i]==TRUE)){
      dataset[j,i]<-basic_stats[2,i]
    }
  }
}

# Dopasowanie obserwacji zmiennej kategorycznej za pomoca k-srednich
X7_kmeans<-kmeans(dataset[,1:7],centers = 3)
cat_check<-X7_kmeans$cluster[which(is.na(dataset[,8]))]
cat_check

dataset[472,8]<-"C"
dataset[169,8]<-"A"

# Transformacja kategorycznej zmiennej
dataset<-cbind(dataset,NA,NA,NA)
dataset_test<-cbind(dataset_test,NA,NA,NA)

colnames(dataset)[9]<-"A"
colnames(dataset)[10]<-"B"
colnames(dataset)[11]<-"C"

colnames(dataset_test)[9]<-"A"
colnames(dataset_test)[10]<-"B"
colnames(dataset_test)[11]<-"C"

for(i in 1:nrow(dataset)) {
  if(dataset[i,8] == "A") {
    dataset$A[i] <- 1
    dataset$B[i]<-0
    dataset$C[i]<-0
  }
  else if(dataset[i,8] == "B") {
    dataset$A[i] <- 0
    dataset$B[i] <- 1
    dataset$C[i]<-0
  }
  else {
    dataset$A[i] <- 0
    dataset$B[i] <- 0
    dataset$C[i]<-1
  }
}

dataset_test<-dataset_test[!is.na(dataset_test$X7), ]

for(i in 1:length(dataset_test[,1])) {
  if(dataset_test[i,8] == "A") {
    dataset_test$A[i]<- 1
    dataset_test$B[i]<-0
    dataset_test$C[i]<-0
  } else if (dataset_test[i,8] == "B") {
    dataset_test$A[i] <- 0
    dataset_test$B[i] <- 1
    dataset_test$C[i]<-0
  } else{
    dataset_test$A[i] <- 0
    dataset_test$B[i] <- 0
    dataset_test$C[i]<-1
  }
}

drops<-"X7"
dataset<-dataset[,!(names(dataset) %in% drops)]  
dataset_test<-dataset_test[,!(names(dataset_test) %in% drops)]

# Usuwanie outlierow
outliers_X1<-boxplot(dataset$X1, plot = FALSE)$out
outliers_X2<-boxplot(dataset$X2, plot = FALSE)$out
outliers_X3<-boxplot(dataset$X3, plot = FALSE)$out
outliers_X4<-boxplot(dataset$X4, plot = FALSE)$out
outliers_X5<-boxplot(dataset$X5, plot = FALSE)$out
outliers_X6<-boxplot(dataset$X6, plot = FALSE)$out

dataset<-dataset[-which(dataset$X1 %in% outliers_X1),]
dataset<-dataset[-which(dataset$X2 %in% outliers_X2),]
dataset<-dataset[-which(dataset$X3 %in% outliers_X3),]
dataset<-dataset[-which(dataset$X4 %in% outliers_X4),]
dataset<-dataset[-which(dataset$X5 %in% outliers_X5),]
dataset<-dataset[-which(dataset$X6 %in% outliers_X6),]

# Korelacja AB
drops<-c("A","B","C")
dataset_AB<-dataset[,!(names(dataset) %in% drops[3])]
cor.plot(dataset_AB, main = "Correlation plot with A and B")
dataset_AC<-dataset[,!(names(dataset) %in% drops[2])]
cor.plot(dataset_AC, main = "Correlation plot with A and C")
dataset_BC<-dataset[,!(names(dataset) %in% drops[1])]
cor.plot(dataset_BC, main = "Correlation plot with B and C")

# Budowa modeli
model_AB <- lm(Y~.,dataset_AB)
model_AC <- lm(Y~.,dataset_AC)
model_BC <- lm(Y~.,dataset_BC)

summary(model_AB)
summary(model_AC)
summary(model_BC)

# Metoda krokowo wsteczna
stepAIC(model_AB)
stepAIC(model_AC)
stepAIC(model_BC)

# Metoda Hellwiga
hellwig <- function( y, x, method="pearson")                               
{
  requireNamespace("utils")
  x <- as.data.frame(x)
  xnames <- colnames(x)
  cm <- stats::cor(x, method=method) # correlation matrix among indeps
  cd <- stats::cor(x, y, method=method) # correlations with dependent
  # list of combination vectors
  k <- sapply( seq(1, length(x)), function(i)
    utils::combn(length(x), i, simplify=FALSE) )
  k <- do.call("c", k)
  # function calculating individual capacities
  hfun <- function(v)
  {
    sapply(v, function(i) cd[i]^2 / sum(abs(cm[v,i])) )
  }
  h <- sapply(k, hfun)
  hellwigs <- data.frame( k = sapply( k, function(x){paste(xnames[x], collapse = "-")}),
                          h = sapply(h, sum),
                          stringsAsFactors=FALSE)
  hellwigs <- arrange(hellwigs,desc(h))
  return(hellwigs)
  
}
h_AB <- hellwig(dataset$Y, dataset_AB[2:9])
h_AC <- hellwig(dataset$Y, dataset_AC[2:9])
h_BC <- hellwig(dataset$Y, dataset_BC[2:9])
h_AB
h_AC
h_BC

# Model z tylko istotnymi zmiennymi
dataset_AC <- dataset_AC[,c(1,2,5,8,9)]

model_AC <- lm(Y~.,dataset_AC)

summary(model_AC)

shapiro.test(model_AC$residuals)

# Koincydencja
coincidence <- data.frame(cor(dataset_AC)[-1,1], model_AC$coefficients[2:5], ifelse(sign(cor(dataset_AC)[-1,1]) == sign(model_AC$coefficients[2:5]), "TRUE", "FALSE"))
colnames(coincidence) <- c("Correlation with Y", "Model coefficients", "Coincidence test")
coincidence

# Rkwadrat
r2 <- summary(model_AC)$r.squared
r2

# Test wspolliniowosci
cor.plot(dataset_AC)
vif(model_AC)

# Test homoskedastycznosci
gqtest(model_AC)

# Test autokorelacji reszt
dwtest(model_AC)

#Losowosci probki i liniowosci modelu - test Walda-Wolfowitza
runs.test(model_AC$residuals)

#Stabilnosci parametrow - test Chowa 
sctest(model_AC, data = dataset_AC, type = "Chow")

#Stabilnosci postaci funkcyjnej - test RESET
resettest(model_AC, power = c(2,3), type = "regressor")

#Prognoza EX POST dla obserwacji z danych testowych
drops<-"B"
dataset_test<-dataset_test[,!(names(dataset_test) %in% drops)]

ex_post <- predict(model_AC, dataset_test)
ex_post
differences <- dataset_test$Y - ex_post
differences

#Sredni blad predykcji EX POST
differences_mean <- mean(differences, na.rm = TRUE)
differences_mean

#Sredni bezwzgledny blad predykcji EX POST
differences_abs_mean <- mean(abs(differences), na.rm = TRUE)
differences_abs_mean

#Sredniokwadratowy blad predykcji EX POST
differences_squared_mean <- sqrt(mean(differences^2, na.rm = TRUE))
differences_squared_mean

#Sredni bezwzgledny blad procentowy predykcji EX POST 
differences_abs_mean_perc <- mean((abs(differences/dataset_test$Y)), na.rm = TRUE)*100
differences_abs_mean_perc
