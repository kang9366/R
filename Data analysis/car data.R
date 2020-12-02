data <- read.csv("./data/car_data.csv", fileEncoding = "CP949")

#NA 제거
dim(data)
data <- data[complete.cases(data), ]

#NA로 이루어진 '가솔린'과 '디젤'열을 기존의 data와 연결시켜 temp라는 새로운 열을 만든다.
가솔린 <- NA
디젤 <- NA
temp <- cbind(data,가솔린, 디젤)
head(temp)

#새로 생성한 가솔린 열에 연료의 종류가 가솔린 인 행은 1을, 그렇지 않은 행은 0을 넣는다.
temp$가솔린[temp$연료 == "가솔린"] <- 1 
temp$가솔린[is.na(temp$가솔린)] <- 0 
temp$디젤[temp$연료 == "디젤"] <- 1 
temp$디젤[is.na(temp$디젤)] <- 0 

#0과 1을 factor로 처리
temp$가솔린 <- as.factor(temp$가솔린)
temp$디젤 <- as.factor(temp$디젤)

colnames(temp)
temp <- temp[,-8]

#나머지 범주형 데이터를 factor처리
temp$년식 <- as.factor(temp$년식) 
temp$LPG <- as.factor(temp$LPG)
temp$회사명 <- as.factor(temp$회사명)
temp$종류 <- as.factor(temp$종류)
temp$하이브리드 <- as.factor(temp$하이브리드)
temp$변속기 <- as.factor(temp$변속기)

#전체데이터로 회귀모형 생성
fit <- lm(가격~., data = temp)
summary(fit)

#유의하지 않은 변수가 많아보이므로 forward, backward, stepwise 세 가지 방법의 변수 선택 방법으로 변수를 선택
#1.forward
fit.con <- lm(가격~1,data = temp)
fit.forward <- step(fit.con,scope=list(lower=fit.con,upper=fit),direction = "forward")

#forward로 선택된 최종 변수는 마력, 회사명, 종류, 배기량, 하이브리드, 중량, 변속기, 년식, 연비, 디젤, 토크이며 수정된 결정계수는 0.8441
summary(fit.forward)

#2. backward
fit.backward <- step(fit, scope = list(lower = fit.con, upper = fit), 
                     direction = "backward")

#backward로 선택된 최종 변수는 회사명, 년식, 종류, 연비, 토크, 하이브리드, 배기량, 중량, 변속기, 디젤이며 수정된 결정계수는 0.8439
summary(fit.backward)

#3. stepwise
fit.both <- step(fit.con, scope = list(lower = fit.con, upper = fit), direction = "both")

#stepwise로 선택된 최종 변수는 회사명, 종류, 배기량, 하이브리드, 중량, 변속기, 년식, 연비, 디젤, 토크이며 수정된 결정계수는 0.8439
summary(fit.both)

#forward 변수선택이 수정된 결정계수가 가장 높으므로 fit.forward 모형을 선택하겠습니다.
fit <- lm(가격 ~ 마력 + 회사명 + 종류 + 배기량 + 하이브리드 + 
              중량 + 변속기 + 년식 + 연비 + 디젤 + 토크, data = temp)

#만든 모형이 회귀분석의 가정을 잘 따르는지 회귀진단
library("car")
library("psych")

pairs.panels(temp[names(temp)])

vif(fit)
fit1 <- lm(가격 ~ 마력 + 회사명 + 종류 + 배기량 + 하이브리드 + 중량 + 변속기 + 년식 + 연비 + 디젤, data = temp)
vif(fit1)

fit2 <- lm(가격 ~ 마력 + 회사명 + 종류 + 배기량 + 하이브리드 + 변속기 + 년식 + 연비 + 디젤, data = temp)
vif(fit2)

fit3 <- lm(가격 ~ 마력 + 회사명 + 종류  + 하이브리드 + 변속기 + 년식 + 연비 + 디젤, data = temp)
vif(fit3)

summary(fit3) 

par(mfrow=c(2,2))
plot(fit3)

#눈에 띄는 이상치를 처리
fit <- lm(가격 ~ 마력 + 회사명 + 종류  + 하이브리드 + 변속기 + 년식 + 연비 + 디젤, data = temp[-c(3,4,10),])
summary(fit)

par(mfrow=c(2,2))
plot(fit)

#회귀모형으로 가격을 예측하고 실제 값과 비교
pre <- predict(fit, newdata = temp)
pre <- as.data.frame(pre)
head(pre)

#하한과 상한이 존재하는 예측구간
pre <- predict(fit, newdata = temp, interval = "predict")
pre <- as.data.frame(pre)
head(pre)

#잘 예측 되었는지 비교하기 위해 실제 값 추가
pre <- cbind(pre, temp$가격)
head(pre)

#실제 값이 포함되면 T, 그렇지 않으면 F를 가지는 tf열을 추가
tf <- NA
pre <- cbind(pre, tf)

pre$tf[pre$`temp$가격`>= pre$lwr & pre$`temp$가격` <= pre$upr] <- T
pre$tf[is.na(pre$tf)] <- F

head(pre)

#예측에 성공하는 비율은 약 0.94로 매우 높은 편이라고 할 수 있다.
sum(pre$tf=="TRUE")/dim(pre)[1] 

