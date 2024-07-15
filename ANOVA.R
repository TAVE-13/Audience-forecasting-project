library(dplyr)
library(readxl)

data <- read_excel("Documents/encoded_data_encoded.xlsx")
View(data)
glimpse(data)

# 범주형 변수들을 factor로 변환
data$원작유무 <- as.factor(data$원작유무)
data$시리즈 <- as.factor(data$시리즈)
data$상영등급 <- as.factor(data$상영등급)
data$장르 <- as.factor(data$장르)

# 이원 배치법 ANOVA 모델 생성 (상호작용 효과 포함)
model <- aov(관객수 ~ 원작유무 * 시리즈, data = data)
model <- aov(관객수 ~ 상영등급 * 장르, data = data)
model <- aov(관객수 ~ 장르 * 원작유무 * 시리즈, data = data)
model <- aov(관객수 ~ 장르 * 시리즈, data = data)
summary(model)


#
glimpse(data)
result <- lm(관객수 ~ `총 언급수` *원작유무 * 시리즈, data = data) 
result <- lm(관객수 ~ `감독 평점`*`평론가 평점`, data = data) 
result <- lm(관객수 ~ 조회수 * `배우 언급수`, data = data) 
summary(result)


#보류
result <- lm(관객수 ~ , data = data) 
summary(model)
anova(result)


# 모델 요약 출력
summary(model)


