### Code : 신재생에너지(서비스업) 매출액, 종사자수 추정
### Writer : Donghyun Kim
### Date : 2020.11.16

## 1. Set Working Directory
dr <- "C:/"
foldname <- paste0(dr, "Users/mazy4/Desktop")
setwd(foldname)

## 2. Reading File
if(!require(readxl)) {
  install.packages("readxl")
}
library(readxl)

mydata <- read_xlsx("2019년 신재생에너지 산업통계 서비스업 조사완료_201114(분석결과).xlsx", sheet = 2)
mydata <- mydata[, c(2, 3, 5, 6, 8, 9, 10, 11)]

## 3. 변수명 재설정
names(mydata)[1] <- "사업체명"
names(mydata)[2] <- "KSIC"
names(mydata)[3] <- "신재생에너지_업종"
names(mydata)[4] <- "에너지원"
names(mydata)[5] <- "총_종사자수"
names(mydata)[6] <- "신재생에너지종사자"
names(mydata)[7] <- "총_매출액"
names(mydata)[8] <- "신재생에너지매출액"

## 4. 종사자 규모별 분류
dat1 <- subset(mydata, mydata$총_종사자수 < 5) # 5인 미만
dat2 <- subset(mydata, mydata$총_종사자수 >= 5 & mydata$총_종사자수 < 10) # 10인 미만
dat3 <- subset(mydata, mydata$총_종사자수 >= 10 & mydata$총_종사자수 < 50) # 50인 미만
dat4 <- subset(mydata, mydata$총_종사자수 >= 50) # 50인 이상

# 데이터 개수
nrow(dat1) # 5인 미만
nrow(dat2) # 10인 미만
nrow(dat3) # 50인 미만
nrow(dat4) # 50인 이상

## 5. 종사자수 dot plot
if(!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

ggplot(dat1, aes(x = 신재생에너지종사자)) + geom_dotplot(method = "histodot", binwidth = 0.15) +
  ggtitle("5인 미만 사업체 종사자수 Dot Plot")
ggplot(dat2, aes(x = 신재생에너지종사자)) + geom_dotplot(method = "histodot", binwidth = 0.2) +
  ggtitle("10인 미만 사업체 종사자수 Dot Plot")
ggplot(dat3, aes(x = 신재생에너지종사자)) + geom_dotplot(method = "histodot", binwidth = 0.55) +
  ggtitle("50인 미만 사업체 종사자수 Dot Plot")
ggplot(dat4, aes(x = 신재생에너지종사자)) + geom_dotplot() +
  ggtitle("50인 이상 사업체 종사자수 Dot Plot")

## 6. 매출액 dot plot
ggplot(dat1, aes(x = 신재생에너지매출액)) + geom_dotplot(method = "histodot", binwidth = 80) +
  ggtitle("5인 미만 사업체 매출액 Dot Plot")
ggplot(dat2, aes(x = 신재생에너지매출액)) + geom_dotplot(method = "histodot", binwidth = 600) +
  ggtitle("10인 미만 사업체 매출액 Dot Plot")
ggplot(dat3, aes(x = 신재생에너지매출액)) + geom_dotplot(method = "histodot", binwidth = 800) +
  ggtitle("50인 미만 사업체 매출액 Dot Plot")
ggplot(dat4, aes(x = 신재생에너지매출액)) + geom_dotplot() +
  ggtitle("50인 이상 사업체 매출액 Dot Plot")

## 7. 종사자수, 매출액 scatter plot
ggplot(dat1, aes(x = 신재생에너지종사자, y = 신재생에너지매출액)) + geom_point(size = 2, colour = "red") +
  ggtitle("5인 미만 사업체 종사자수 대비 매출액 Scatter Plot")
ggplot(dat2, aes(x = 신재생에너지종사자, y = 신재생에너지매출액)) + geom_point(size = 2, colour = "dark green") +
  ggtitle("10인 미만 사업체 종사자수 대비 매출액 Scatter Plot")
ggplot(dat3, aes(x = 신재생에너지종사자, y = 신재생에너지매출액)) + geom_point(size = 2, colour = "blue") +
  ggtitle("50인 미만 사업체 종사자수 대비 매출액 Scatter Plot")
ggplot(dat4, aes(x = 신재생에너지종사자, y = 신재생에너지매출액)) + geom_point(size = 2, colour = "hot pink") +
  ggtitle("50인 이상 사업체 종사자수 대비 매출액 Scatter Plot")

## 8. 종사자 규모 구분X Scatter Plot
ggplot(mydata, aes(x = 신재생에너지종사자, y = 신재생에너지매출액)) + geom_point(size = 2, colour = "black") +
  ggtitle("표본 사업체 종사자수 대비 매출액 Scatter Plot")

## 9. 종사자 규모별 표본 매출액 기초통계량 작성
basicst <- function(input) {
  a <- mean(input)
  s <- sd(input)
  minimum <- quantile(input)[1]
  q1 <- quantile(input)[2]
  q2 <- quantile(input)[3]
  q3 <- quantile(input)[4]
  maximum <- quantile(input)[5]
  return(list(평균 = a, 표준편차 = s, 사분위수 = c(minimum, q1, q2, q3, maximum)))
}

basicst(dat1$신재생에너지매출액) # 5인 미만
basicst(dat2$신재생에너지매출액) # 10인 미만
basicst(dat3$신재생에너지매출액) # 50인 미만
basicst(dat4$신재생에너지매출액) # 50인 이상

## 10. 종사자 규모별 표본 종사자수 기초통계량 작성
basicst(dat1$신재생에너지종사자) # 5인 미만
basicst(dat2$신재생에너지종사자) # 10인 미만
basicst(dat3$신재생에너지종사자) # 50인 미만
basicst(dat4$신재생에너지종사자) # 50인 이상

#################################################################################################

## Simple Regression Analysis ##

## 1. 전체 자료를 통한 Regression
# (1) Fitting
fit1 <- lm(신재생에너지매출액 ~ 신재생에너지종사자, data = mydata)
fit1_coef <- fit1$coefficients

# (2) Result
summary(fit1)

# (3) Residuals
X1 <- cbind(1, mydata$신재생에너지종사자)
mydata$잔차 <- mydata$신재생에너지매출액 - (X1 %*% fit1_coef)
mydata$abs_잔차 <- abs(mydata$잔차)

# (3) Linear Model and Data Plot
if(!require(gridExtra)) {
  install.packages("gridExtra")
}
library(gridExtra)

# (3-1) Linear Model
a <- ggplot(mydata, aes(x = 신재생에너지종사자, y = 신재생에너지매출액)) + geom_point() + theme_bw() +
  geom_abline(intercept = fit1_coef[1], slope = fit1_coef[2]) + ggtitle("적합선 그래프")

# (3-2) Residuals Plot
b <- ggplot(mydata, aes(x = 신재생에너지종사자, y = 잔차)) + geom_point() + theme_bw() +
  geom_abline(intercept = 0, slope = 0) + ggtitle("잔차 그림")

# (3-3) Absolute Residuals Plot
c <- ggplot(mydata, aes(x = 신재생에너지종사자, y = abs_잔차)) + geom_point() + theme_bw() +
  geom_abline(intercept = 0, slope = 0) + ggtitle("절대값 잔차 그림")

# (3-4) Total Graphing
grid.arrange(a, b, c, nrow = 3, ncol = 1)

###########################################################

## 2. 일부 자료 제거를 통한 Regression
# (1) Filtering
mydata_cor <- subset(mydata, mydata$신재생에너지매출액 <= 5000) # 매출액 5000 이하 데이터만

# (1-1) Check Scatter Plot
ggplot(mydata_cor, aes(x = 신재생에너지종사자, y = 신재생에너지매출액)) + geom_point(size = 2, colour = "black") +
  ggtitle("표본 사업체 종사자수 대비 매출액 Scatter Plot")

# (2) Fitting
fit2 <- lm(신재생에너지매출액 ~ 신재생에너지종사자, data = mydata_cor)
fit2_coef <- fit2$coefficients

# (3) Result
summary(fit2)

# (4) Residuals
X1 <- cbind(1, mydata_cor$신재생에너지종사자)
mydata_cor$잔차 <- mydata_cor$신재생에너지매출액 - (X1 %*% fit2_coef)
mydata_cor$abs_잔차 <- abs(mydata_cor$잔차)

# (4) Linear Model and Data Plot
# (4-1) Linear Model
a <- ggplot(mydata_cor, aes(x = 신재생에너지종사자, y = 신재생에너지매출액)) + geom_point() + theme_bw() +
  geom_abline(intercept = fit2_coef[1], slope = fit2_coef[2]) + ggtitle("적합선 그래프")

# (4-2) Residuals Plot
b <- ggplot(mydata_cor, aes(x = 신재생에너지종사자, y = 잔차)) + geom_point() + theme_bw() +
  geom_abline(intercept = 0, slope = 0) + ggtitle("잔차 그림")

# (4-3) Absolute Residuals Plot
c <- ggplot(mydata_cor, aes(x = 신재생에너지종사자, y = abs_잔차)) + geom_point() + theme_bw() +
  geom_abline(intercept = 0, slope = 0) + ggtitle("절대값 잔차 그림")

# (4-4) Total Graphing
grid.arrange(a, b, c, nrow = 3, ncol = 1)

#################################################################################################

## Simple Regression Analysis without Intercept ##

## 1. 전체 자료를 통한 Regression
fit1 <- lm(신재생에너지매출액 ~ 0 + 신재생에너지종사자, data = mydata)
summary(fit1)

## 2. 일부 자료 제거를 통한 Regression
fit2 <- lm(신재생에너지매출액 ~ 0 + 신재생에너지종사자, data = mydata_cor)
summary(fit2)
