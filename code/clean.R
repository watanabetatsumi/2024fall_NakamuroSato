# ctrl + shift + h で　作業ファイル開ける
# ctrl + shift + c コメントアウト
# ctrl + shift + r ラベル
# ctrl + shift + s sourceコマンドと同じ
# ctrl + shift + j ctrl + f と同じ
# ctrl + shift + o ラベル先に移動
# ctrl + shift + d 行をコピー


# 依存関係 --------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(gtsummary)
source("./code/function.R")

# データのインポート ---------------------------------------------------------------
source("C:/Users/watan/2024fall_NakamuroSato/data/NLSYData_r.R")
dfNaive <- new_data

dfNaive1 <- renameData(dfNaive)
motherEducdf <- makeMotherEducdf(dfNaive1)
familySizedf <- makeFamilySizedf(dfNaive1)
PIATScoredf <- makePIATscoredf(dfNaive1)
TroubleScoredf <- makeTroubleScoredf(dfNaive1)
Transferdf <- makeTransferdf(dfNaive1)
AlcoholExpdf <- makeVariabledf(dfNaive1,"YASR-5A","Alcoholever",2000,2020)
MarijuanaExpdf <- makeVariabledf(dfNaive1,"YASR-24A","MarijuanaExp",1998,2020)
TabacoExpdf <- makeVariabledf(dfNaive1,"YASR-19A","TabacoExp",1998,2020)
Under18df <- makeVariabledf(dfNaive1,"YASR-4BA","IsU18",2002,2020)
Gradedf <- makeVariabledf(dfNaive1,"Q4-2","educ",1994,2020)
IsGraduatedf <- makeVariabledf(dfNaive1,"Q4-28","IsGraduate",1994,2020)
EnjoyRiskdf <- makeVariabledf(dfNaive1,"Q16-5I-D","EnjoyRisk",1994,2020)
Urbandf <- makeVariabledf(dfNaive1,"URBAN-RURAL","Urban",1994,2020)
AbuseCasedf <- makeVariabledf(dfNaive1,"YASR-71A~000025","AbuseCase",2016,2020)
FamilyIncomedf <- makeVariabledf(dfNaive1,"Q15-141-TOP","FamilyIncome",2002,2014)

# データ整形 -------------------------------------------------------------------

# データの結合
staderd_df <- changeValue(dfNaive1)
masterdf <- left_join(Transferdf, staderd_df,
                      by = c('childID', 'motherID', 'firstSurveyYear'))
covar_df <- reduce(list(
                        motherEducdf,
                        familySizedf,
                        PIATScoredf,
                        TroubleScoredf,
                        AlcoholExpdf,
                        MarijuanaExpdf,
                        TabacoExpdf,
                        Under18df,
                        Gradedf,
                        IsGraduatedf,
                        EnjoyRiskdf,
                        Urbandf,
                        AbuseCasedf,
                        FamilyIncomedf
                      ), full_join,
                      by = c('childID', 'motherID', 'year', 'firstSurveyYear'))
raw_df <- left_join(masterdf,covar_df,by = c('childID', 'motherID','year','firstSurveyYear')) %>% mutate(
    age = year - birthYear
  )

alldf <- raw_df

# 変数の作成

alldf <- alldf %>% group_by(motherID,year) %>%
  mutate(
    N_siblings = max(birthOrder, na.rm = TRUE),
    NYS = N_siblings - birthOrder,
    age_gap = birthYear - lag(birthYear, order_by = birthOrder)
  ) %>%
  ungroup()

alldf <- alldf %>% mutate(
  IsU18 = case_when(
    # これは正しそう
    age > 18 ~ 0,
    TRUE ~ 1
  ),
  IsAbuse = case_when(
    # これは正しそう
    is.na(AbuseCase) ~ NA_real_,
    AbuseCase == 1 ~ 1,
    AbuseCase == 0 ~ 0,
  ),
  AlcoholExp = case_when(
    # これは正しそう
    is.na(Alcoholever) ~ NA_real_,
    Alcoholever == 1 ~ 1,
    Alcoholever == 95 ~ 0,
  ),
  u18_Abuse = case_when(
    # これは正しそう
    IsAbuse == 1 & IsU18 == 1 ~ 1,
    is.na(IsAbuse) | is.na(IsU18) ~ NA_real_,
    any(c(IsAbuse, IsU18) %in% 0) ~ 0
  ),
  o18_Abuse = case_when(
    # これは正しそう
    IsAbuse == 1 & IsU18 == 0 ~ 1,
    is.na(IsAbuse) | is.na(IsU18) ~ NA_real_,
    TRUE ~ 0
  ),
  u18_mariExp = case_when(
    # これは正しそう
    MarijuanaExp == 1 & IsU18 == 1 ~ 1,
    is.na(MarijuanaExp) | is.na(IsU18) ~ NA_real_,
    any(c(MarijuanaExp, IsU18) %in% 0) ~ 0
  ),
  o18_mariExp = case_when(
    # これは正しそう
    MarijuanaExp == 1 & IsU18 == 0 ~ 1,
    is.na(MarijuanaExp) | is.na(IsU18) ~ NA_real_,
    TRUE~ 0
  ),
  u18_alcExp = case_when(
    # これは正しそう
    AlcoholExp == 1 & IsU18 == 1 ~ 1,
    is.na(AlcoholExp) | is.na(IsU18) ~ NA_real_,
    any(c(AlcoholExp, IsU18) %in% 0) ~ 0
  ),
  o18_alcExp = case_when(
    # これは正しそう
    AlcoholExp == 1 & IsU18 == 0 ~ 1,
    is.na(AlcoholExp) | is.na(IsU18) ~ NA_real_,
    TRUE ~ 0
  ),
  u18_tabcExp = case_when(
    # これは正しそう
    TabacoExp == 1 & IsU18 == 1 ~ 1,
    is.na(TabacoExp) | is.na(IsU18) ~ NA_real_,
    any(c(TabacoExp, IsU18) %in% 0) ~ 0
  ),
  o18_tabcExp = case_when(
    # これは正しそう
    TabacoExp == 1 & IsU18 == 0 ~ 1,
    is.na(TabacoExp) | is.na(IsU18) ~ NA_real_,
    TRUE ~ 0
  ),
  u18_subsatanceExp = case_when(
    # これは正しそう
    is.na(u18_alcExp) & is.na(u18_mariExp) & is.na(u18_tabcExp) & is.na(u18_Abuse) ~ NA_real_,
    u18_alcExp == 1 | u18_mariExp == 1 | u18_tabcExp == 1 | u18_Abuse == 1 ~ 1,
    TRUE ~ 0
  ),
  subsatanceExp = case_when(
    # これは正しそう
    is.na(AlcoholExp) & is.na(MarijuanaExp) & is.na(TabacoExp) & is.na(IsAbuse) ~ NA_real_,
    AlcoholExp == 1 | MarijuanaExp == 1 | TabacoExp == 1 | IsAbuse == 1 ~ 1,
    TRUE ~ 0
  ),
  IsBlack = case_when(
    race != "BLACK" & !is.na(race) ~ 0,
    race == "BLACK" ~ 1,
  ),
  IsHispanic = case_when(
    race != "HISPANIC" & !is.na(race) ~ 0,
    race == "HISPANIC" ~ 1,
  ),
  Is2th = case_when(
    birthOrder != 2 & !is.na(birthOrder) ~ 0,
    birthOrder == 2 ~ 1,
  ),
  Is3th = case_when(
    birthOrder != 3 & !is.na(birthOrder) ~ 0,
    birthOrder == 3 ~ 1,
  ),
  Is4th = case_when(
    birthOrder != 4 & !is.na(birthOrder) ~ 0,
    birthOrder == 4 ~ 1,
  ),
  IsTransfer_over50 = case_when(
    Transfer <= 2 ~ 0,
    Transfer > 2 ~ 1,
  ),
  IsEnjoyRisk = case_when(
    EnjoyRisk <= 2 ~ 0,
    EnjoyRisk > 2 ~ 1,
    TRUE ~ NA
  ),
  educ = case_when(
    educ == 0 | educ == 95 ~ NA,
    !is.na(educ) ~ educ
  ),
  IsCollegeStudent = case_when(
    educ <= 12 ~ 0,
    educ > 12 ~ 1,
  ),
  motherEduc = case_when(
    motherEduc == 0 | educ == 95 ~ NA,
    !is.na(motherEduc) ~ motherEduc
  ),
  IsUrban = case_when(
    Urban == 0 ~ 0,
    Urban == 1 ~ 1,
    Urban == 2 ~ NA,
  )
)

summary <- datasummary(All(alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = alldf,
                       na.rm = TRUE,
                       fmt = 3,
)
summary

f_alldf <- alldf %>% rename(
  "子供ID" = childID,
  "母親ID" = motherID,
  "調査開始年度" = firstSurveyYear,
  "調査年度" = year,
  "出生年" = birthYear,
  "年齢" = age,
  "移転(Y)" = Transfer,
  "移転over50%ダミー(Y)" = IsTransfer_over50,
  "女ダミー"= Isfemale,
  "黒人ダミー" = IsBlack,
  "ヒスパニックダミー" = IsHispanic,
  "都市住みダミー" = IsUrban,
  "出生順位" = birthOrder,
  "第2子ダミー" = Is2th,
  "第3子ダミー" = Is3th,
  "第4子ダミー" = Is4th,
  "兄弟の数" = N_siblings,
  "弟妹の数" = NYS,
  "上との年齢差" = age_gap,
  "教育年数(母)" = motherEduc,
  "家族サイズ" = familySize,
  "数学スコア" = PIATmath_v,
  "読解力スコア" = PIATrecog_v,
  "読解把握スコア" = PIATcompreh_v,
  "非行スコア" = TroubleScore,
  "語彙力" = PIATvocab_v,
  "物質使用経験(総合)" = subsatanceExp,
  "飲酒経験" = AlcoholExp,
  "大麻経験" = MarijuanaExp,
  "喫煙経験" = TabacoExp,
  "乱用経験" = IsAbuse,
  "18歳以下ダミー" = IsU18,
  "教育年数" = educ,
  "高校卒業ダミー" = IsGraduate,
  "大学生ダミー" = IsCollegeStudent,
  "リスク_好みダミー" = IsEnjoyRisk,
  "未成年使用ダミー(総合)" = u18_subsatanceExp,
  "未成年飲酒ダミー" = u18_alcExp,
  "未成年大麻使用ダミー" = u18_mariExp,
  "未成年喫煙ダミー" = u18_tabcExp,
  "未成年乱用ダミー" = u18_Abuse
) %>% select(
    "子供ID",
    "母親ID",
    "出生年",
    "年齢",
    "教育年数",
    "調査開始年度",
    "調査年度",
    "女ダミー",
    "移転(Y)",
    "移転over50%ダミー(Y)",
    "出生順位",
    "第2子ダミー",
    "第3子ダミー",
    "第4子ダミー",
    "兄弟の数",
    "弟妹の数",
    "上との年齢差",
    "未成年使用ダミー(総合)",
    "未成年飲酒ダミー",
    "未成年大麻使用ダミー",
    "未成年喫煙ダミー",
    "未成年乱用ダミー",
    "物質使用経験(総合)",
    "飲酒経験",
    "大麻経験",
    "喫煙経験",
    "乱用経験",
    "教育年数(母)",
    "家族サイズ",
    "18歳以下ダミー",
    "高校卒業ダミー",
    "大学生ダミー",
    "黒人ダミー",
    "ヒスパニックダミー",
    "リスク_好みダミー",
    "非行スコア"
  )


# 基本統計量 -------------------------------------------------------------------

summary <- datasummary(All(f_alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = f_alldf,
                       na.rm = TRUE,
                       fmt = 3,
)
summary


hist(
  f_alldf$出生順位,
  breaks = seq(0,12,1),
  xlab = "出生順位",
  ylab = "人数",
  )

# 第一段階の回帰モデルでリスク行動を算出 -----------------------------------------------------------

source("./code/analysis.R")

#predictを使って確率を計算
predictSubUse <- predict(B1fixmodel, newdata = alldf)

#データフレームに傾向スコアを格納
alldf <- alldf %>%
  mutate(exSubUse = predictSubUse + 0.012)

hist(alldf$exSubUse, main = "Distribution")

# 基本統計量2 -------------------------------------------------------------------

alldf <- alldf %>% filter(age > 18)

f_alldf <- alldf %>% rename(
  "子供ID" = childID,
  "母親ID" = motherID,
  "調査開始年度" = firstSurveyYear,
  "調査年度" = year,
  "出生年" = birthYear,
  "年齢" = age,
  "移転(Y)" = Transfer,
  "移転over50%ダミー(Y)" = IsTransfer_over50,
  "女ダミー"= Isfemale,
  "黒人ダミー" = IsBlack,
  "ヒスパニックダミー" = IsHispanic,
  "都市居住ダミー" = IsUrban,
  "出生順位" = birthOrder,
  "第2子ダミー" = Is2th,
  "第3子ダミー" = Is3th,
  "第4子ダミー" = Is4th,
  "兄弟の数" = N_siblings,
  "弟妹の数" = NYS,
  "上との年齢差" = age_gap,
  "家族サイズ" = familySize,
  "物質使用経験(予測)" = exSubUse,
  "物質使用経験(総合)" = subsatanceExp,
  "飲酒経験" = AlcoholExp,
  "大麻経験" = MarijuanaExp,
  "喫煙経験" = TabacoExp,
  "乱用経験" = IsAbuse,
) %>% select(
    "子供ID",
    "母親ID",
    "出生年",
    "年齢",
    "調査開始年度",
    "調査年度",
    "女ダミー",
    "移転(Y)",
    "移転over50%ダミー(Y)",
    "出生順位",
    "第2子ダミー",
    "第3子ダミー",
    "第4子ダミー",
    "兄弟の数",
    "弟妹の数",
    "上との年齢差",
    "物質使用経験(予測)",
    "物質使用経験(総合)",
    "飲酒経験",
    "大麻経験",
    "喫煙経験",
    "乱用経験",
    "家族サイズ",
    "黒人ダミー",
    "ヒスパニックダミー",
    "都市居住ダミー"
  )


summary <- datasummary(All(f_alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = f_alldf,
                       na.rm = TRUE,
                       fmt = 3,
)
summary

# 記述統計（プロット） --------------------------------------------------------------
Ttest(f_alldf,"T検定")

