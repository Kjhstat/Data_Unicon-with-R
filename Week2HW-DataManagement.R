## 1. cow_data에서 is_edible 이라는 새로운 열을 추가하고
##    나이(age)가 50(개월)이상이면서
##    등급(grade)이 "3" 또는 "등외"이라면 "폐기용", 아니면 "식용"을 작성해주세요 


setwd("C:/Users/KOO/Downloads")

#fread 함수를 사용하기 위한 data.table library
library(data.table)
cow <- as.data.frame(fread("./cow_data.csv", encoding = "UTF-8"))
head(cow)
str(cow)


#너무 많아서 노트북 힘들어함... observation 10000개로 줄임.
cow <- cow[1:10000, ]
str(cow)
View(cow)

#is_edible 열 추가. 임의의 값 대입해서 일단 만들어 줘야함.
cow$is_edible <- 0

# i를 1부터 10000까지 반복시켜서 모든 행이 action 될 수 있도록 for문 사용  
# if 문을 통해 나이가 50개월 이상, 등급이 '3'이나 '등외'이면 -> is_edible을 '폐기용', 아니면 '식용'으로 만들어주기

for (i in 1:nrow(cow)){
  if (cow$age[i] >= 50 & (cow$grade[i] %in% c("3", "등외")))(cow$is_edible[i] <- "폐기용")
  else (cow$is_edible[i] <- "식용")
}



## 2. cowNA_data에서 결측치가 발생한 곳에 평균 값을 집어넣은 새로운 열을 만드세요
cowNA <- as.data.frame(fread("./cowNA_data.csv", encoding = "UTF-8"))
head(cowNA)
str(cowNA)

#먼저 어떤 열에 결측치가 몇개 있는지 보기.
#is.na() -> NA가 있으면 TRUE 돌려줌. TRUE = 1, FALSE = 0
#colsums() 열의 합.
colSums(is.na(cowNA)) 
# weight에 185개 있는것 발견.


#weight결측값에 weight평균 대입.
#sapply이용.. 일반적으로 여러변수에 결측값 발생할 수 있기 때문...

cowNA <- as.data.frame(sapply(cowNA, function(x) ifelse(is.na(x), mean(x, na.rm = T), x)))

colSums(is.na(cowNA))



  
## 3. co 데이터에서 taste 이라는 새로운 열을 추가하고
##    grade가 3,2,1 순서대로 새로운 taste 열을 만들어
##    이곳에 등급에 따라"normal","good","best" 을 작성해주세요 
co <- as.data.frame(fread("./co.csv", encoding = "UTF-8"))
head(co)
str(co)

co$taste <- 0 
for ( i in 1:nrow(co)){
  if(co$grade[i] == "3")(co$taste[i] = "normal")
  else if(co$grade[i] == "2")(co$taste[i] = "good")
  else if(co$grade[i] == "1")(co$taste[i] = "best")
}

View(co)



  
## 4. co 데이터에서 결측치 행을 지운 새로운 데이터 co1을 만들고
##    결측치가 없어진 co1의 price와 grade의 관계를 구하세요(ex : 회귀,corr 등)
co1 <- na.omit(co)

ggplot(co1, aes(x = grade, y =price, group = grade, fill = as.factor(grade)))  + geom_boxplot()




## 5. co1 데이터에서 패키지 ggplot2를 이용하여
##    가격과 등급의 산점도(크기 0.5의 파란색 point)를 그리세요
  
ggplot(co1, aes(x = price, y = grade)) + geom_point(size = 0.5, col = "blue")





## 5-1(회귀 분석을 수강하신 분만)
##    co1 데이터에서 ggplot2과gcookbook패키지를 이용하여
##    가격과 등급의 산점도와 회귀식 모델선을 추가하여 그리세요(99% 확률)
  
ggplot(co1, aes(x = grade, y = price)) + geom_point() + stat_smooth(method=lm, level = 0.99)






## 6. co1 데이터의 region name 에서 앞글자와 두번째 글자를 따와
##    city라는 새로운 열에 넣으세요 (ex : 영남,경북 등) (hint : substring)

View(co1)
substr(co1$`region name`[1], 1, 2)
co1$city <- 0

for (i in 1 : nrow(co1)){
  co1$city[i] <- substr(co1$`region name`[i], 1, 2)
}






## 6-1 #6번을 이용하여 각 도별 평균 가격과 등급을 구하세요
str(co1)
co2<-co1 %>% 
  group_by(city) %>% 
  summarise(average_price = mean(price), average_grade = mean(grade))





## 7. co1 데이터에서 price를 cut function을 사용하여
##    3개의 범주로 나누고 '저가' ,'중가' ,'고가'로 표시된
##    groups라는 새로운 열을 만드세요 (범주기준 : [6000 ~9000),[9000~12000),[12000~15200) )

x <- c(1,2,3,4,5,6,7,8,9,10)
xx <-cut(x, breaks = c(0,3,6,9,12), right = F, labels = c("a","b","c","d"))


co1$groups <- 0
co1$groups <- cut(co1$price, labels = c("저가", "중가", "고가"), breaks = c(6000,9000,12000,15200), right =F)



