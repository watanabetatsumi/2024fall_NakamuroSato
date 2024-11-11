# 依存関係 --------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(gtsummary)
library(fixest)

# スコア予測
B1model <- glm(subsatanceExp ~ NYS + age_gap + familySize, data = alldf)
summary(B1model)

# B1model <- glm(u18_subsatanceExp ~ NYS + age_gap + familySize, data = u18_alldf)
# summary(B1model)

B1fixmodel <- glm(
  subsatanceExp ~ NYS + age_gap + familySize
  + Isfemale + IsBlack + IsHispanic + IsUrban
  , data = alldf
)
summary(B1fixmodel)

B1Twofixmodel <- feglm(
  subsatanceExp ~ NYS + age_gap
  | motherID + year
  , data = alldf
)
summary(B1Twofixmodel)
