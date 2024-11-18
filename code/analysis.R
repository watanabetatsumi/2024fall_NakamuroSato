# 依存関係 --------------------------------------------------------------------
# install.packages("margins")
library(tidyverse)
library(modelsummary)
library(gtsummary)
library(fixest)
library(broom)
library(margins)

# フィルタリング -------------------------------------------------------------------

clean_alldf <- alldf %>% filter(
  # Is1th == 1 | Is2th == 1 | Is3th == 1 | Is4th == 1,
  # 2子以上いる家庭に限定
  N_siblings > 1,
  # NYG < 5,
  # N_siblings < 5,
  # age_gap < 15,
  # age < 25
)

fclean_alldf <- makeStatic_df(clean_alldf)

# hist_f(fclean_alldf,"出生順位")
# hist_f(fclean_alldf,"弟妹の数")
# hist_f(fclean_alldf,"下との年齢差")
# hist(
#   fclean_alldf$未成年の弟妹の数,
#   breaks = seq(0, 8, 1),
#   main = "未成年の弟妹の数の分布",
#   xlab = "未成年の弟妹の数",
#   ylab = "人数"
# )
# # hist_f(fclean_alldf,"年齢")

hist(alldf$AGAP_ij)
hist(alldf$AGAP_ijt)
hist(alldf$NYG_ij)
hist(alldf$NYG_ijt)
hist(alldf$Age)
hist(alldf$BirthYear)
hist(alldf$Year)
hist(alldf$MainTransfer)
hist(alldf$BirthOrder)
hist(alldf$IsLiveTogether)
hist(alldf$IsTransfered)
hist(alldf$IsTransfer_over50)
hist(alldf$U18_SubstanceExp)

# 基本統計量 -------------------------------------------------------------------


# fclean_alldf <- na.omit(fclean_alldf)
summary <- datasummary(All(fclean_alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = fclean_alldf,
                       na.rm = TRUE,
                       fmt = 3,
)
summary

shinto()


# 線形回帰モデル -------------------------------------------------------------------


B1model_lm <- feols(U18_SubstanceExp ~ NYG_ijt + AGAP_ijt
                    + Isfemale
                    + IsUrban
                    # + EnjoyRisk
                    + N_siblings
                    | MotherID 
                    + BirthYear
                    , data = clean_alldf
)
summary(B1model_lm)
etable(B1model_lm)

B1model_lm <- feols(U18_SubstanceExp ~ factor(NYG_ijt) + AGAP_ijt
                    + Isfemale
                    + IsUrban
                    # + EnjoyRisk
                    + N_siblings
                    | MotherID 
                    + BirthYear
                    , data = clean_alldf
)
summary(B1model_lm)
etable(B1model_lm)

B1model_lm_marijuana <- feols(IsGraduate ~ NYG_ijt + AGAP_ijt
                              + Isfemale
                              + IsUrban
                              # + EnjoyRisk
                              + N_siblings
                              | MotherID 
                              + BirthYear
                              , data = clean_alldf
)
summary(B1model_lm_marijuana)
etable(B1model_lm_marijuana)

B1model_lm_marijuana <- feols(U18_mariExp ~ NYG_ijt + AGAP_ijt
                              + Isfemale
                              + IsUrban
                              # + EnjoyRisk
                              + N_siblings
                              | MotherID 
                              + BirthYear
                              , data = clean_alldf
)
summary(B1model_lm_marijuana)
etable(B1model_lm_marijuana)

B1model_lm_alcohol <- feols(U18_alcExp ~ NYG_ijt + AGAP_ijt
                            + Isfemale
                            + N_siblings
                            | MotherID + BirthYear 
                            , data = clean_alldf
)
summary(B1model_lm_alcohol)
etable(B1model_lm_alcohol)

B1model_lm_tabaco <- feols(u18_tabcExp ~ U18NYG + AGAP
                    + Isfemale + IsUrban
                    | motherID
                    , data = clean_alldf
)
summary(B1model_lm_tabaco)
etable(B1model_lm_tabaco)


# ロジット回帰モデル ---------------------------------------------------------------


B1model_glm <- feglm(u18_substanceExp ~ U18NYG + AGAP
                     + Isfemale
                     + N_siblings
                     | motherID 
                     + birthYear
                     , family = binomial(link = logit)
                     , data = clean_alldf
                     )
summary(B1model_glm)
exp(coef(B1model_glm))

B1model_glm <- feglm(IsGraduate ~ U18NYG + AGAP
                     + Isfemale
                     + N_siblings
                     | motherID 
                     + birthYear
                     , family = binomial(link = logit)
                     , data = clean_alldf
)
summary(B1model_glm)
exp(coef(B1model_glm))

B1model_glm <- feglm(u18_substanceExp ~ U18NYG + age_gap + familySize
                     + Isfemale + IsUrban + FamilyIncome 
                     | motherID
                     , family = binomial(link = logit)
                     , data = clean_alldf
)
summary(B1model_glm)
exp(coef(B1model_glm))

B1model_glm_marijuana <- feglm(u18_mariExp ~ U18NYG + age_gap + familySize
                     + Isfemale + IsUrban + FamilyIncome 
                     | motherID
                     , family = binomial(link = logit)
                     , data = clean_alldf
)
summary(B1model_glm_marijuana)
exp(coef(B1model_glm_marijuana))

B1model_glm_alcohol <- feglm(u18_alcExp ~ U18NYG + age_gap + familySize
                               + Isfemale + IsUrban + FamilyIncome 
                               | motherID
                               , family = binomial(link = logit)
                               , data = clean_alldf
)
summary(B1model_glm_alcohol)
exp(coef(B1model_glm_alcohol))

B1model_glm_tabaco <- feglm(u18_tabcExp ~ U18NYG + age_gap + N_siblings
                             + Isfemale + IsUrban + FamilyIncome 
                             | motherID
                             , family = binomial(link = logit)
                             , data = clean_alldf
)
summary(B1model_glm_tabaco)
exp(coef(B1model_glm_tabaco))

# tidy(BB1model_glm,coef.int = TRUE, exponentiate = TRUE)



# 予測スコア算出 -----------------------------------------------------------------

#predictを使って確率を計算

predictSubUse <- predict(B1model_lm, type = "response", newdata = clean_alldf)
predictMarijuana_Use <- predict(B1model_lm_marijuana, type = "response", newdata = clean_alldf)
predictAlcohol_Use <- predict(B1model_lm_alcohol, type = "response", newdata = clean_alldf)
predictTabaco_Use <- predict(B1model_lm_tabaco, type = "response", newdata = clean_alldf)

# predictSubUse <- predict(B1model_glm, type = "response", newdata = clean_alldf)
# predictMarijuana_Use <- predict(B1model_glm_marijuana, type = "response", newdata = clean_alldf)
# predictAlcohol_Use <- predict(B1model_glm_alcohol, type = "response", newdata = clean_alldf)
# predictTabaco_Use <- predict(B1model_glm_tabaco, type = "response", newdata = clean_alldf)

#データフレームに傾向スコアを格納
clean_alldf1 <-clean_alldf %>%
  mutate(
    exSubUse = predictSubUse,
    exSubUse_marijuana = predictMarijuana_Use,
    exSubUse_alcohol = predictAlcohol_Use,
    exSubUse_tabaco = predictTabaco_Use
    ) 
# %>% 
#   mutate(
#     exSubUse = exSubUse - min(exSubUse,na.rm = TRUE),
#     exSubUse_marijuana = exSubUse_marijuana - min(exSubUse_marijuana,na.rm = TRUE),
#     exSubUse_alcohol = exSubUse_alcohol - min(exSubUse_alcohol,na.rm = TRUE),
#     exSubUse_tabaco = exSubUse_tabaco - min(exSubUse_tabaco,na.rm = TRUE),
#   )

hist(clean_alldf1$u18_substanceExp,breaks = seq(0, 1, by = 0.01), main = "Distribution")
hist(clean_alldf1$exSubUse,breaks = seq(-1, 2, by = 0.1), main = "Distribution")


ggplot(clean_alldf1, aes(x = U18NYG, y = u18_substanceExp)) +
  geom_smooth(method = "lm",
              # method.args = list(family = binomial(link = logit)),
              color = "blue", se = FALSE) +
  labs(x = "弟妹の数", y = "未成年使用ダミー") +
  theme_bw() +
  ylim(0, 1)

ggplot(clean_alldf1, aes(x = U18NYG, y = IsEnjoyRisk)) +
  geom_smooth(method = "lm",
              # method.args = list(family = binomial(link = logit)),
              color = "blue", se = FALSE) +
  labs(x = "弟妹の数", y = "リスク行動") +
  theme_bw() +
  ylim(0, 1)


ggplot(clean_alldf1, aes(x = U18NYG, y = exSubUse)) +
  geom_point() +
  geom_smooth(method = "lm",
              # method.args = list(family = binomial(link = logit)),
              color = "blue", se = TRUE) +
  labs(x = "弟妹の数", y = "未成年使用ダミー") +
  theme_bw() +
  ylim(0, 1)

# 基本統計量2 -------------------------------------------------------------------

length(clean_alldf1$exSubUse)
f_alldf <- clean_alldf1 %>% rename(
  "子供ID" = childID,
  "母親ID" = motherID,
  "調査開始年度" = firstSurveyYear,
  "調査年度" = year,
  "教育年数" = educ,
  "出生年" = birthYear,
  "未成年ダミー" = IsU18,
  "年齢" = age,
  "移転(Y)" = Transfer,
  "移転over50%ダミー(Y)" = IsTransfer_over50,
  "女ダミー"= Isfemale,
  "母親の年齢" = motherAge,
  "黒人ダミー" = IsBlack,
  "ヒスパニックダミー" = IsHispanic,
  "都市居住ダミー" = IsUrban,
  "出生順位" = birthOrder,
  "第1子ダミー" = Is1th,
  "第2子ダミー" = Is2th,
  "第3子ダミー" = Is3th,
  "第4子ダミー" = Is4th,
  "兄弟の数" = N_siblings,
  "弟妹の数" = NYG,
  "18以下の弟妹の数" = U18NYG,
  "下との年齢差" = age_gap,
  "家族サイズ" = familySize,
  "未成年物質使用経験(予測)" = exSubUse,
  "未成年飲酒ダミー(予測)" = exSubUse_alcohol,
  "未成年大麻使用ダミー(予測)" = exSubUse_marijuana,
  "未成年喫煙ダミー(予測)" = exSubUse_tabaco,
  "物質使用経験(総合)" = substanceExp,
  "飲酒経験" = AlcoholExp,
  "大麻経験" = MarijuanaExp,
  "喫煙経験" = TabacoExp,
  "乱用経験" = IsAbuse,
  "リスク_好みダミー" = IsEnjoyRisk
) %>% select(
  "子供ID",
  "母親ID",
  "出生年",
  "年齢",
  "調査開始年度",
  "調査年度",
  # "教育年数",
  "女ダミー",
  "母親の年齢" ,
  "未成年ダミー",
  "移転(Y)",
  "移転over50%ダミー(Y)",
  "出生順位",
  "第1子ダミー",
  "第2子ダミー",
  "第3子ダミー",
  "第4子ダミー",
  # "兄弟の数",
  "弟妹の数",
  "18以下の弟妹の数",
  "下との年齢差",
  "未成年物質使用経験(予測)",
  "未成年飲酒ダミー(予測)",
  "未成年大麻使用ダミー(予測)",
  "未成年喫煙ダミー(予測)",
  # "物質使用経験(総合)",
  # "飲酒経験",
  # "大麻経験",
  # "喫煙経験",
  # "乱用経験",
  "家族サイズ",
  # "黒人ダミー",
  # "ヒスパニックダミー",
  "都市居住ダミー",
  # "リスク_好みダミー"
)

length(clean_alldf1$exSubUse)
length(f_alldf$"未成年物質使用経験(予測)")
sum(is.na(f_alldf$"未成年物質使用経験(予測)"))

f_alldf <- f_alldf %>% filter(
  17 < `年齢` & `年齢` < 30
)
summary <- datasummary(All(f_alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = f_alldf,
                       na.rm = TRUE,
                       fmt = 3,
)

summary
str(f_alldf)

hist_f(f_alldf,"年齢")
hist_f(f_alldf,"出生順位")
hist_f(f_alldf,"弟妹の数")


clean_alldf2 <- clean_alldf1 %>% filter(
  18 < age & age < 23
)


# 推計（メイン） -----------------------------------------------------------------

model <- feols(IsTransfer_over50 ~ exSubUse + U18NYG + AGAP
               + U18NYG:exSubUse + exSubUse:AGAP
               + motherAge + IsCollegeStudent
               | childID
               , data = clean_alldf2
)
summary(model)
etable(model)

# + IscollegeStudent
model <- feols(IsTransfer_over50 ~ exSubUse + U18NYG + AGAP 
               + U18NYG:exSubUse + exSubUse:AGAP
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge + IsCollegeStudent
               | motherID + age
               , data = clean_alldf2
)
summary(model)
etable(model)

# + IsEnjoyRisk
model <- feols(IsTransfer_over50 ~ exSubUse + U18NYG + age_gap 
               + U18NYG:exSubUse + exSubUse:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + IsEnjoyRisk + motherAge
               | motherID + age
               , data = clean_alldf2
)
summary(model)
etable(model)


# 各種アウトカム
model <- feols(IsTransfer_over50 ~ exSubUse_marijuana + U18NYG + age_gap 
               + U18NYG:exSubUse_marijuana + exSubUse_marijuana:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age
               , data = clean_alldf2
)
summary(model)
etable(model)

model <- feols(IsTransfer_over50 ~ exSubUse_alcohol + U18NYG + age_gap 
               + U18NYG:exSubUse_alcohol + exSubUse_alcohol:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age
               , data = clean_alldf2
)
summary(model)
etable(model)

model <- feols(IsTransfer_over50 ~ exSubUse_tabaco + U18NYG + age_gap 
               + U18NYG:exSubUse_tabaco + exSubUse_tabaco:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age
               , data = clean_alldf2
)
summary(model)
etable(model)

model <- feols(IsTransfer_over50 ~ exSubUse + U18NYG + age_gap 
               + i(U18NYG, exSubUse, ref = 0) + exSubUse:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age + year
               , data = clean_alldf2
)
summary(model)
etable(model)

# sdで報告するか or P値で報告するか
model <- feols(IsTransfer_over50 ~ exSubUse + U18NYG + age_gap 
               + exSubUse:Is2th + exSubUse:Is3th + exSubUse:Is4th + exSubUse:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age + year
               , data = clean_alldf2
)
summary(model)
etable(model)

model_marijuana <- feols(IsTransfer_over50 ~ exSubUse_marijuana + U18NYG + age_gap 
               + i(U18NYG, exSubUse_marijuana, ref = 0) + exSubUse_marijuana:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age + year
               , data = clean_alldf2
)
summary(model_marijuana)
etable(model_marijuana)

model_alcohol <- feols(IsTransfer_over50 ~ exSubUse_alcohol + U18NYG + age_gap 
               + i(U18NYG, exSubUse_alcohol, ref = 0) + exSubUse_alcohol:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age + year
               , data = clean_alldf2
)
summary(model_alcohol)
etable(model_alcohol)

model_alcohol <- feols(IsTransfer_over50 ~ exSubUse_alcohol + U18NYG + age_gap 
               + exSubUse_alcohol:Is2th + exSubUse_alcohol:Is3th + exSubUse_alcohol:Is4th + exSubUse_alcohol:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age + year
               , data = clean_alldf2
)
summary(model)
etable(model)

model_tabaco <- feols(IsTransfer_over50 ~ exSubUse_tabaco + U18NYG + age_gap 
               + i(U18NYG, exSubUse_tabaco, ref = 0) + exSubUse_tabaco:age_gap
               + + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age + year
               , data = clean_alldf2
)
summary(model_tabaco)
etable(model_tabaco)


# バイアス確認用

model <- feols(IsTransfer_over50 ~ u18_substanceExp + U18NYG + age_gap 
               + U18NYG:u18_substanceExp + u18_substanceExp:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID + age + year
               , data = clean_alldf2
)
summary(model)
etable(model)

model <- feols(IsTransfer_over50 ~ u18_substanceExp + U18NYG + age_gap 
               + i(U18NYG, u18_substanceExp, ref = 0) + u18_substanceExp:age_gap
               + familySize + IsUrban + FamilyIncome
               + Isfemale + motherAge
               | motherID  + age + year
               , data = clean_alldf2
)
summary(model)
etable(model)

ggplot(clean_alldf2, aes(x = age, y = IsTransfer_over50)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = binomial(link = logit)),
              color = "blue", se = FALSE) +
  labs(x = "年齢", y = "仕送り") +
  theme_bw() +
  ylim(0, 1)

ggplot(clean_alldf2, aes(x = birthOrder, y = IsTransfer_over50)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = binomial(link = logit)),
              color = "blue", se = FALSE) +
  labs(x = "出生順位", y = "仕送り") +
  theme_bw() +
  ylim(0, 1)


# model <- feols(IsTransfer_over50 ~ exSubUse + Is2th + Is3th + Is4th + age_gap 
#                + Is2th:exSubUse + Is3th:exSubUse + Is4th:exSubUse + exSubUse:age_gap
#                + familySize + IsUrban 
#                + Isfemale
#                | motherID + age
#               , data = clean_alldf2
# )
# summary(model)
# etable(model)
# 
# model <- feols(IsTransfer_over50 ~ exSubUse + Is2th + Is3th + Is4th + age_gap 
#                + U18NYG:exSubUse + exSubUse:age_gap
#                + familySize + IsUrban
#                + Isfemale
#                | motherID + age
#                , data = clean_alldf2
# )
# summary(model)
# etable(model)
# 

