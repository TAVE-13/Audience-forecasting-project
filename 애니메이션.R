library(dplyr)
library(readxl)


ani <- read_excel("boxoffice_animation.xlsx")
print(class(ani$관객수))


#시리즈 여부가 관객수에 영향을 미치는가
result1 <- t.test(관객수 ~ 시리즈, data = ani)
result1
#p-value = 0.5568 귀무가설 기각x
#시리즈 여부 간 평균차이 없음


#원작의 유무가 관객수에 영향을 미치는가
result2<- t.test(관객수 ~ 원작유무, data = ani)
result2
#p-value = 0.03642 귀무가설 기각
#원작이 없는 경우에 관객수가 더 많음


#SNS언급량과 관객수 간 상관관계
correlation_test <- cor.test(ani$'총 언급수', ani$관객수, method = "pearson")
correlation_test
#상관계수 0.3895


#예고편 조회수와 관객수 간 상관관계
correlation_test1 <- cor.test(ani$조회수, ani$관객수, method = "pearson")
correlation_test1
#상관계수 0.4852


install.packages("ggplot2")
library(ggplot2)


par(mfrow = c(1,2))

#시리즈 여부에 따른 평균 관객수 barplot
mean_audience_series <- aggregate(관객수 ~ 시리즈, data = ani, FUN = mean)
mean_audience<-mean(ani$관객수)

barplot(mean_audience_series$관객수, 
        names.arg = c("시리즈=0", "시리즈=1"),
        main = "시리즈 여부에 따른 평균 관객수",
        xlab = "시리즈",
        ylab = "평균 관객수",
        col = c("skyblue", "lightgreen"))

text(x = 0.35, y = max(mean_audience_series)*0.1, 
     labels = paste(603227.2),pos = 4)

text(x = 1.5, y = max(mean_audience_series)*0.1, 
     labels = paste(722508.3),pos = 4)


#원작 유무에 따른 평균 관객수 barplot
mean_audience_original <- aggregate(관객수 ~ 원작유무, data = ani, FUN = mean)

mean_audience<-mean(ani$관객수)


barplot(mean_audience_original$관객수, 
        names.arg = c("원작유무=0", "원작유무=1"),
        main = "원작 유무에 따른 평균 관객수",
        xlab = "원작",
        ylab = "평균 관객수",
        col = c("skyblue", "lightgreen"))

text(x = 0.3, y = max(mean_audience_series)*0.1, 
     labels = paste(1046811.2),pos = 4)

text(x = 1.5, y = max(mean_audience_series)*0.1, 
     labels = paste(471781.8),pos = 4)
