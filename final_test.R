library("AER")
library("ggplot2")
library("caret")
library("erer")
library("lmtest")
library("sandwich")
library("forecast")
library("dplyr")
library("vcd")
library("broom")

# 15-17
data("mtcars")
model <- lm(data=mtcars, mpg~hp+wt+am)
bptest(model, data=mtcars, varformula = ~ poly(hp, 2) + poly(wt, 2))
gqtest(model, order.by = ~hp, data=mtcars, fraction = 0.3)
vcovHC(model, type="HC3")

# 21-22
t <- read.csv("titanic3.csv")
t <- mutate(t,sex=as.factor(sex),pclass=as.factor(pclass),survived=as.factor(survived))
probit_mod <- glm(data=t, survived~sex+age+I(age^2)+pclass+sibsp,
                family=binomial(link="probit"),x=TRUE)
vcov(probit_mod)
maBina(probit_mod)
#mosaic(data=t,~sex+pclass+survived,shade=TRUE)
#qplot(data=t,x=age,y=..count..,fill=survived,geom="density",position="stack")

# 25-27
set.seed(12)
y<-arima.sim(model = list (ar = c(0.1, 0.6), ma = -0.3), n=100)
x1<-rnorm(100, 15, 5)
x2<-runif(100, 45, 50)
model2 <- lm(y~x1+x2)
coeftest(model2, vcov.=vcovHAC(model2))
bgtest(model2, order=3)
y_aug <- augment(model2, as.data.frame(y))
qplot(data=y_aug,lag(.resid),.resid)

# 31-32
set.seed(123)
y<-arima.sim(model = list(ar = c(0.5, 0.1), ma = c(0.3,0.2)), n = 100)
AIC(Arima(y, order=c(0,1,2)))
model_arima <- Arima(y, order=c(3,0,3), fixed=c(0,NA,NA,0,NA,NA,NA))
summary(model_arima)
AIC(model_arima)

# 35-37
data("CollegeDistance")
set.seed(42)
in_train <- createDataPartition(y = CollegeDistance$wage, p = 0.9, list=FALSE)
CD_train <- CollegeDistance[in_train,]
CD_test <- CollegeDistance[-in_train,]
coeftest(lm(data=CD_train, wage~gender+ethnicity+unemp+education+region))
model3<- ivreg(data=CD_train,
               wage~gender+ethnicity+unemp+education+region | gender+ethnicity+unemp+region+distance)
coeftest(model3, vcov = vcovHC)
prediction <- predict(model3, CD_test)
round(prediction[1],digit=3)