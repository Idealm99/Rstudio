install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)



ti <- read.csv("C:/Users/skybl/Downloads/Titanic.csv")  
ti %>% group_by(Sex)

ggplot(ti, aes(x=Sex, y=Survived))+
  geom_bar(stat = 'identity')

# 여성의 생존율이 더 높다.

ti %>% group_by(Pclass)
ggplot(ti, aes(x=Pclass, y=Survived, fill=Pclass ))+
  geom_bar(stat = 'identity')

# 1,3 등급의 표를 가진 사람들의 생존한 사람이 더 많다 

survival_rate <- ti %>%
  group_by(Pclass) %>%
  summarise(SurvivalRate = mean(Survived, na.rm = TRUE))

ggplot(survival_rate, aes(x=Pclass, y=SurvivalRate, fill=Pclass ))+
  geom_bar(stat = 'identity')

# 하지만 생존율을 보면 1,2,3 순서대로 높다 이 그래프로 보았을 때 
#등급이 높은 좌석을 구매한 사람이 먼저 구조 배를 탔다고 생각할 수 있다.

SibSp_Parch <- ti %>%
  mutate(FamilySize = SibSp + Parch)

SibSp_Parch <- SibSp_Parch %>%
  select(-SibSp, -Parch)

ggplot(SibSp_Parch, aes(x=FamilySize, y=Survived, fill=FamilySize ))+
  geom_bar(stat = 'identity')
