rm(list=ls(all=TRUE))
cat("\014")

# 6대 광역시 
# 6대 광역시 데이터 불러오기
data1 <- read.csv("C:/Rstudy/data1.csv")

library(ggplot2)

# 6대 광역시의 주택매매가격지수 변화 (2013.01~2022.09)
ggplot(data1, aes(x = time, y = house_price)) + geom_line(group = 1)

# 6대 광역시의 한국은행 기준금리 변화 (2013.01~2022.09)
ggplot(data1, aes(x = time, y = rate)) + geom_line(group = 1) 

# 6대 광역시의 주택가격전망 CSI의 변화 (2013.01~2022.09)
ggplot(data1, aes(x = time, y = house_price_CSI)) + geom_line(group = 1) 

# 6대 광역시의 원/달러 환율 변화 (2013.01~2022.09)
ggplot(data1, aes(x = time, y = exchange_rate)) + geom_line(group = 1) 

# 6대 광역시의 주택시장 소비심리지수 변화 (2013.01~2022.09)
ggplot(data1, aes(x = time, y = house_CSI)) + geom_line(group = 1) 

# 6대 광역시의 지가변동률 변화 (2013.01~2022.09)
ggplot(data1, aes(x = time, y = land_rate)) + geom_line(group = 1) 

# 6대 광역시의 선행종합지수의 변화 (2013.01~2022.09)
ggplot(data1, aes(x = time, y = CLI)) + geom_line(group = 1) 

# 데이터 분석 
# 다중회귀분석
ols1 <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI, data = data1)

# 다중공선성 검사
library(car)
vif(ols1)

library(lmtest)
library(sandwich)
library(stargazer)

# 다중회귀분석 결과
summary(ols1)

result1 <- coeftest(ols1, vcov = vcovHC) # 이분산 고려

result1

# 종속변수와 유의한 독립변수들 간의 단순회귀분석 
# 주택매매가격지수 - 한국은행 기준금리 
# 단순회귀분석 결과
ols1_1 <- lm(house_price~rate, data = data1)

summary(ols1_1)

# 선형그래프
ggplot_ols1_1 <- ggplot(ols1_1, aes(x=rate, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("한국은행 기준금리") + ylab("주택매매가격지수")+ ggtitle("6대 광역시의 한국은행 기준금리에 따른 주택매매가격지수의 변화(2013.01~2022.09)")
theme(plot.title = element_text(hjust = 0.5, size = 13),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size=13),
      axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols1_1

# 선형그래프 + 제곱항 그래프 
ols1_1_1 <- lm(house_price~rate + I((rate)^2), data = data1)

plot(house_price~rate, data = data1)
x = sort(data1$rate)
b1 = ols1_1$coef
b2 = ols1_1_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 주택매매가격지수 - 주택시장 소비심리지수
# 단순회귀분석 결과
ols1_2 <- lm(house_price~house_CSI, data = data1)

summary(ols1_2)

# 선형 그래프 
ggplot_ols1_2 <- ggplot(ols1_2, aes(x=house_CSI, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("주택시장 소비심리지수") + ylab("주택매매가격지수")+ ggtitle("6대 광역시의 주택매매가격지수에 따른 주택매매가격지수의 변화(2013.01~2022.09)") +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols1_2

# 선형 그래프 + 제곱항 그래프 
ols1_2_1 <- lm(house_price~house_CSI + I((house_CSI)^2), data = data1)

plot(house_price~house_CSI, data = data1)
x = sort(data1$house_CSI)
b1 = ols1_2$coef
b2 = ols1_2_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 주택매매가격지수 - 선행종합지수
# 단순회귀분석 결과
ols1_3 <- lm(house_price ~ CLI, data = data1)

summary(ols1_3)

# 선형 그래프
ggplot_ols1_3 <- ggplot(ols1_3, aes(x=CLI, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("선행종합지수") + ylab("주택매매가격지수")+ ggtitle("6대 광역시의 선행종합지수에 따른 주택매매가격지수의 변화(2013.01~2022.09)") +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols1_3

# 선형 그래프 + 제곱항 그래프 
ols1_3_1 <- lm(house_price~CLI + I((CLI)^2), data = data1)

plot(house_price~CLI, data = data1)
x = sort(data1$CLI)
b1 = ols1_3$coef
b2 = ols1_3_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 6대 광역시 변수들의 신뢰구간
confint(ols1, "rate", level = 0.95, df = ols1$df.residual)
confint(ols1, "house_price_CSI", level = 0.95, df = ols1$df.residual)
confint(ols1, "exchange_rate", level = 0.95, df = ols1$df.residual)
confint(ols1, "house_CSI", level = 0.95, df = ols1$df.residual)
confint(ols1, "land_rate", level = 0.95, df = ols1$df.residual)
confint(ols1, "CLI", level = 0.95, df = ols1$df.residual)

# 6대 광역시 다중 제약 (F검정)
ftest_1 <- linearHypothesis(ols1, c("rate=0", "house_price_CSI=0", "exchange_rate=0", "house_CSI=0","land_rate=0", "CLI=0"))

ftest_1

# 6대 광역시 ANOVA 분석
aov1 <- aov(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI, data = data1)

aov1

summary(aov1)

anova(ols1)

# ---------------------------------------------------------------------------------------------------------------------------------------------

# 서울 
# 서울 데이터 불러오기
data2 <- read.csv("C:/Rstudy/data2.csv")

# 서울의 주택매매가격지수 변화 (2013.01~2022.09)
ggplot(data2, aes(x = time, y = house_price)) + geom_line(group = 1)

# 서울의 한국은행 기준금리 변화 (2013.01~2022.09)
ggplot(data2, aes(x = time, y = rate)) + geom_line(group = 1) 

# 서울의 주택가격전망 CSI의 변화 (2013.01~2022.09)
ggplot(data2, aes(x = time, y = house_price_CSI)) + geom_line(group = 1) 

# 서울의 원/달러 환율 변화 (2013.01~2022.09)
ggplot(data2, aes(x = time, y = exchange_rate)) + geom_line(group = 1) 

# 서울의 주택시장 소비심리지수 변화 (2013.01~2022.09)
ggplot(data2, aes(x = time, y = house_CSI)) + geom_line(group = 1) 

# 서울의 지가변동률 변화 (2013.01~2022.09)
ggplot(data2, aes(x = time, y = land_rate)) + geom_line(group = 1) 

#서울의 선행종합지수 변화 (2013.01~2022.09)
ggplot(data2, aes(x = time, y = CLI)) + geom_line(group = 1) 

# 데이터 분석
# 다중회귀분석 
ols2 <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI, data = data2)

# 다중공선성 검사
vif(ols2)

# 다중회귀분석 결과 
summary(ols2)

result2 <- coeftest(ols2, vcov = vcovHC) # 이분산 고려

result2

# 종속변수와 유의한 독립변수들 간의 단순회귀분석 
# 주택매매가격지수 - 한국은행 기준금리 
# 단순회귀분석 결과
ols2_1 <- lm(house_price~rate, data = data2)

summary(ols2_1)

# 선형 그래프
ggplot_ols2_1 <- ggplot(ols2_1, aes(x=rate, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("한국은행 기준금리") + ylab("주택매매가격지수")+ ggtitle("서울의 한국은행 기준금리에 따른 주택매매가격지수의 변화(2013.01~2022.09)")+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols2_1

# 선형 그래프 + 제곱항 그래프 
ols2_1_1 <- lm(house_price~rate + I((rate)^2), data = data2)

plot(house_price~rate, data = data2)
x = sort(data2$rate)
b1 = ols2_1$coef
b2 = ols2_1_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 주택매매가격지수 - 주택가격전망 CSI
# 단순회귀분석 결과
ols2_2 <- lm(house_price~house_price_CSI, data = data2)

summary(ols2_2)

# 선형 그래프 
ggplot_ols2_2 <- ggplot(ols2_2, aes(x=house_price_CSI, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("주택가격전망 CSI") + ylab("주택매매가격지수")+ ggtitle("주택가격전망 CSI에 따른 주택매매가격지수의 변화(2013.01~2022.09)")+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols2_2

# 선형 그래프 + 제곱항 그래프 
ols2_2_1 <- lm(house_price~house_price_CSI + I((house_price_CSI)^2), data = data2)

plot(house_price~house_price_CSI, data = data2)
x = sort(data2$house_price_CSI)
b1 = ols2_2$coef
b2 = ols2_2_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 주택매매가격지수 - 주택시장 소비심리지수
# 단순회귀분석 결과
ols2_3 <- lm(house_price~house_CSI, data = data2)

summary(ols2_3)

# 선형 그래프
ggplot_ols2_3 <- ggplot(ols2_3, aes(x=house_CSI, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("주택시장 소비심리지수") + ylab("주택매매가격지수")+ ggtitle("주택시장 소비심리지수에 따른 주택매매가격지수의 변화(2013.01~2022.09)")+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols2_3

# 선형 그래프 + 제곱항 그래프 
ols2_3 <- lm(house_price~house_CSI, data = data2)

ols2_3_1 <- lm(house_price~house_CSI + I((house_CSI)^2), data = data2)
plot(house_price~house_CSI, data = data2)
x = sort(data2$house_CSI)
b1 = ols2_3$coef
b2 = ols2_3_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 주택매매가격지수 - 선행종합지수 
# 단순회귀분석 결과
ols2_4 <- lm(house_price ~ CLI, data = data2)

summary(ols2_4)

# 선형 그래프
ggplot_ols2_4 <- ggplot(ols2_4, aes(x=CLI, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("선행종합지수") + ylab("주택매매가격지수")+ ggtitle("선행종합지수에 따른 주택매매가격지수의 변화(2013.01~2022.09)")+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols2_4

# 선형 그래프 + 제곱항 그래프
ols2_4_1 <- lm(house_price~CLI + I((CLI)^2), data = data2)

plot(house_price~CLI, data = data2)
x = sort(data2$CLI)
b1 = ols2_4$coef
b2 = ols2_4_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 서울 변수들의 신뢰구간
confint(ols2, "rate", level = 0.95, df = ols1$df.residual)
confint(ols2, "house_price_CSI", level = 0.95, df = ols1$df.residual)
confint(ols2, "exchange_rate", level = 0.95, df = ols1$df.residual)
confint(ols2, "house_CSI", level = 0.95, df = ols1$df.residual)
confint(ols2, "land_rate", level = 0.95, df = ols1$df.residual)
confint(ols2, "CLI", level = 0.95, df = ols1$df.residual)

# 서울 다중 제약 (F검정)
ftest_2 <- linearHypothesis(ols2, c("rate=0", "house_price_CSI=0", "exchange_rate=0", "house_CSI=0","land_rate=0", "CLI=0"))

ftest_2

# 서울 ANOVA 분석
aov2 <- aov(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI, data = data2)

aov2

summary(aov2)

anova(ols2)

# ---------------------------------------------------------------------------------------------------------------------------------------------

# 경기도
# 경기도 데이터 불러오기 
data3 <- read.csv("C:/Rstudy/data3.csv")

# 경기도의 주택매매가격지수 변화 (2013.01~2022.09)
ggplot(data3, aes(x = time, y = house_price)) + geom_line(group = 1)

# 경기도의 한국은행 기준금리 변화 (2013.01~2022.09)
ggplot(data3, aes(x = time, y = rate)) + geom_line(group = 1) 

# 경기도의 주택가격전망 CSI 변화 (2013.01~2022.09)
ggplot(data3, aes(x = time, y = house_price_CSI)) + geom_line(group = 1) 

# 경기도의 원/달러 환율 변화 (2013.01~2022.09)
ggplot(data3, aes(x = time, y = exchange_rate)) + geom_line(group = 1) 

# 경기도의 주택시장 소비심리지수 변화 (2013.01~2022.09)
ggplot(data3, aes(x = time, y = house_CSI)) + geom_line(group = 1) 

# 경기도의 지가변동률 변화 (2013.01~2022.09)
ggplot(data3, aes(x = time, y = land_rate)) + geom_line(group = 1) 

# 경기도의 선행종합지수 변화 (2013.01~2022.09)
ggplot(data3, aes(x = time, y = CLI)) + geom_line(group = 1) 

# 데이터 분석
# 다중회귀분석 
ols3 <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI, data = data3)

# 다중공선성 검사
vif(ols3)

# 다중회귀분석 결과
summary(ols3)

result3 <- coeftest(ols3, vcov = vcovHC) # 이분산 고려

result3

# 종속변수와 유의한 독립변수들 간의 단순회귀분석 
# 주택매매가격지수 - 한국은행 기준금리 
# 단순회귀분석 결과
ols3_1 <- lm(house_price ~ rate, data = data3)

summary(ols3_1)

# 선형 그래프 
ggplot_ols3_1 <- ggplot(ols3_1, aes(x=rate, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("한국은행 기준금리") + ylab("주택매매가격지수")+ ggtitle("경기도의 한국은행 기준금리에 따른 주택매매가격지수의 변화(2013.01~2022.09)")+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols3_1

# 선형 그래프 + 제곱항 그래프 
ols3_1_1 <- lm(house_price~rate + I((rate)^2), data = data3)

plot(house_price~rate, data = data3)
x = sort(data3$rate)
b1 = ols3_1$coef
b2 = ols3_1_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 주택매매가격지수 - 지가변동률 
# 단순회귀분석 결과
ols3_2 <- lm(house_price ~ land_rate, data = data3)

summary(ols3_2)

# 선형 그래프
ggplot_ols3_2 <- ggplot(ols3_2, aes(x=land_rate, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("지가변동률") + ylab("주택매매가격지수")+ ggtitle("경기도의 지가변동률에 따른 주택매매가격지수의 변화(2013.01~2022.09)")+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols3_2

# 선형 그래프 + 제곱항 그래프 
ols3_2_1 <- lm(house_price~land_rate + I((land_rate)^2), data = data3)

plot(house_price~land_rate, data = data3)
x = sort(data3$land_rate)
b1 = ols3_2$coef
b2 = ols3_2_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 주택매매가격지수 - 선행종합지수 
# 단순회귀분석 결과
ols3_3 <- lm(house_price ~ CLI, data = data3)

summary(ols3_3)

# 선형 그래프
ggplot_ols3_3 <- ggplot(ols3_3, aes(x=CLI, y=house_price)) +
  geom_point(size = 1, color = "lightblue3") +
  geom_smooth(method = "lm", se=FALSE, color="magenta")+
  xlab("선행종합지수") + ylab("주택매매가격지수")+ ggtitle("경기도의 선행종합지수에 따른 주택매매가격지수의 변화(2013.01~2022.09)")+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.line = element_line(colour = "grey92", size = 1, linetype = "solid"))

ggplot_ols3_3

# 선형 그래프 + 제곱항 그래프
ols3_3_1 <- lm(house_price~CLI + I((CLI)^2), data = data3)

plot(house_price~CLI, data = data3)
x = sort(data3$CLI)
b1 = ols3_3$coef
b2 = ols3_3_1$coef
lines(x, b2[1] + b2[2]*x + b2[3]*x^2, col = "red")
lines(x, b1[1] + b1[2]*x, lty = 2, col = "blue")
legend("topright", c("quadratic", "linear"), lty = c(1,2), col = c("red","blue"))

# 경기도 변수들의 신뢰구간
confint(ols3, "rate", level = 0.95, df = ols1$df.residual)
confint(ols3, "house_price_CSI", level = 0.95, df = ols1$df.residual)
confint(ols3, "exchange_rate", level = 0.95, df = ols1$df.residual)
confint(ols3, "house_CSI", level = 0.95, df = ols1$df.residual)
confint(ols3, "land_rate", level = 0.95, df = ols1$df.residual)
confint(ols3, "CLI", level = 0.95, df = ols1$df.residual)

# 경기도 다중 제약 (F검정)
ftest_3 <- linearHypothesis(ols3, c("rate=0", "house_price_CSI=0", "exchange_rate=0", "house_CSI=0","land_rate=0", "CLI=0"))

ftest_3

# 경기도 ANOVA 분석
aov3 <- aov(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI, data = data3)

aov3

summary(aov3)

anova(ols3)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# 더미변수 추가 후의 데이터 분석
# 6대 광역시 
# 데이터 불러오기 
data1_dm <- read.csv("C:/Rstudy/data1_dm.csv")

# 다중회귀분석 
# 금리 상승 (상승: 1, 그외: 0)
ols1_up <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI + up, data = data1_dm)

summary(ols1_up)

result1_up <- coeftest(ols1_up, vcov = vcovHC)

result1_up
# 금리 하락 (하락: 1, 그외: 0)
ols1_down <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI + down, data = data1_dm)

summary(ols1_down)

result1_down <- coeftest(ols1_down, vcov = vcovHC)

result1_down

# 금리 유지 (유지: 1, 그외: 0)
ols1_maintain <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI + maintain, data = data1_dm)

summary(ols1_maintain)

result1_maintain <- coeftest(ols1_maintain, vcov = vcovHC)

result1_maintain

# ---------------------------------------------------------------------------------------------------------------------------------------------

# 서울
# 데이터 불러오기
data2_dm <- read.csv("C:/Rstudy/data2_dm.csv")

# 금리 상승 (상승: 1, 그외: 0)
ols2_up <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI + up, data = data2_dm)

summary(ols2_up)

result2_up <- coeftest(ols2_up, vcov = vcovHC)

result2_up

# 금리 하락 (하락: 1, 그외: 0)
ols2_down <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI + down, data = data2_dm)

summary(ols2_down)

result2_down <- coeftest(ols2_down, vcov = vcovHC)

result2_down

# 금리 유지 (유지: 1, 그외: 0)
ols2_maintain <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI + maintain, data = data2_dm)

summary(ols2_maintain)

result2_maintain <- coeftest(ols2_maintain, vcov = vcovHC)

result2_maintain

# ---------------------------------------------------------------------------------------------------------------------------------------------

# 경기도
# 데이터 불러오기 
data3_dm <- read.csv("C:/Rstudy/data3_dm.csv")

# 금리 상승 (상승: 1, 그외: 0)
ols3_up <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI + up, data = data3_dm)

summary(ols3_up)

result3_up <- coeftest(ols3_up, vcov = vcovHC)

result3_up

# 금리 하락 (하락: 1, 그외: 0)
ols3_down <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI + down, data = data3_dm)

summary(ols3_down)

result3_down <- coeftest(ols3_down, vcov = vcovHC)

result3_down

# 금리 유지 (유지: 1, 그외: 0)
ols3_maintain <- lm(house_price ~ rate + house_price_CSI + exchange_rate  + house_CSI + land_rate +  CLI + maintain, data = data3_dm)

summary(ols3_maintain)

result3_maintain <- coeftest(ols3_maintain, vcov = vcovHC)

result3_maintain