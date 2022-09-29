#Mean,median and mode of ungrouped data

data <- c(85,65,55,42,69,55,85,92,55,77)

mean(data)

median(data)

getmode <- function(data){
  uniq <- unique(data)
  uniq[which.max(tabulate(match(data,uniq)))]
}
res <- getmode(data)
print(res)

#Mean,median and mode of grouped data

#Standard deviation,variance and scatter plot

data <- c(85,65,55,42,69,55,85,92,55,77)

x<-sd(data)
print(x)
y<-var(data)
print(y)
plot(x,y,main="scatter plot")
plot(data)

#Mean,median and mode of grouped data with class Intervals

#Regression ,visualize the data using scatter plot

x<- c(172,176,154,165,152,142)
y<- c(64,56,45,69,49,57)

res<-lm(y~x)
print(res)

print(summary(res))

plot(x,y)

#6
data<-c(13,15,16,19,20,21,22,22,25,25,25,25,30,33,33,35,35,35,36,40,45,46,52,70)
boxplot(data)
summary(data)
IQR(data)

#Karl pearson's coefficient

x <- c(100,102,104,107,105,112,103,99)
y <- c(15,12,13,11,12,12,19,26)

sum_of_x <- sum(x)
sum_of_y <- sum(y)
sum_of_x_sq <- sum(x^2)
sum_of_y_sq <- sum(y^2)
sum_of_xy <- sum(x*y)
n <- length(x)

c_c <- (n*sum_of_xy -(sum_of_x*sum_of_y))/sqrt((n*sum_of_x_sq-(sum_of_x^2))*(sum_of_y_sq-(sum_of_y^2)))
print(c_c)

plot(x,y)

#chi-square test

x <-c(146,78,48,98,54,22,42,92,20,10,10,20)
res<-chisq.test(x)
print(res)

#10

Temp<-c(102,101,100,99,98)
pulse<-c(100,90,80,70,60)

cov(Temp,pulse,method = "kendall")

#11
#1
x<-c()

