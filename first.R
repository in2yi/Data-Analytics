#Part 1
EPI_data <- read.csv("epi2024results06022024.csv")
View(EPI_data)
attach(EPI_data)
NAs <- is.na(EPI.new)
EPI.new.noNAs <- EPI.new[!NAs]
summary(EPI.new) 
fivenum(EPI.new,na.rm=TRUE) 
stem(EPI.new) 
hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) 
rug(EPI.new)

#part 2

boxplot(EPI.new, APO.new) #boxplot
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.new) 
qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)       
qqplot(rt(250, df = 5),EPI.new, xlab = "Q-q plot for t dsn")
qqline(EPI.new)


#2a variable one

EPI_data <- read.csv("epi2024results06022024.csv")
View(APO_data)
attach(APO_data)
NAs <- is.na(APO.new)
APO.new.noNAs <- APO.new[!NAs]
summary(APO.new) 
fivenum(APO.new,na.rm=TRUE) 
stem(APO.new) 
hist(APO.new) 
hist(APO.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(APO.new,na.rm=TRUE,bw=1.)) 
rug(APO.new)

boxplot(APO.new, APO.new) #boxplot
plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE) 
qqnorm(APO.new) 
qqline(APO.new) 
qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn")
qqline(APO.new)       
qqplot(rt(250, df = 5),APO.new, xlab = "Q-q plot for t dsn")
qqline(APO.new)











