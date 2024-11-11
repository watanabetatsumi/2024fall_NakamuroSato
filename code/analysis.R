# 依存関係 --------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(gtsummary)
library(fixest)

fitmodel <- feols(u18_subsatanceExp ~ birthOrder + | childID + motherID + year ,data = alldf)

# スコア予測
B1_model <- glm(subsatanceExp ~ NYS + age_gap + familySize,data=alldf)
summary(B1_model)

B1_fix_model <- glm(subsatanceExp ~ NYS + age_gap + familySize + Isfemale + IsBlack + IsHispanic + IsUrban,data=alldf)
summary(B1_fix_model)

ps_fit <- feols(u18_subsatanceExp ~ NYS + age_gap + familySize | motherID ,data=alldf)
summary(ps_fit)

ps_fit <- feols(u18_subsatanceExp ~ NYS + age_gap + familySize+ Isfemale + IsBlack + IsHispanic | motherID ,data=alldf)
summary(ps_fit)
