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

# データのインポート ---------------------------------------------------------------
source("C:/Users/watan/2024fall_NakamuroSato/data/tatsumi-data20241116.R")
setwd("C:/Users/watan/2024fall_NakamuroSato/")
source("./code/function.R")

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
Gradedf <- makeVariabledf(dfNaive1,"Q4-2","educ",1994,2020)
IsGraduatedf <- makeVariabledf(dfNaive1,"Q4-28","IsGraduate",1994,2020)
EnjoyRiskdf <- makeVariabledf(dfNaive1,"Q16-5I-D","EnjoyRisk",1994,2020)
Urbandf <- makeVariabledf(dfNaive1,"URBAN-RURAL","Urban",1994,2020)
AbuseCasedf <- makeVariabledf(dfNaive1,"YASR-71A~000025","AbuseCase",2016,2020)
FamilyIncomedf <- makeVariabledf(dfNaive1,"Q15-141-TOP","FamilyIncome",2002,2014)

# Transferdf1 <- makeVariabledf(dfNaive1,"Q15-74B")
# Transferdf2 <- makeVariabledf(dfNaive1,"Q15-74D","spec_Transfer",2006,2020)
# Under18df <- makeVariabledf(dfNaive1,"YASR-4BA","IsU18",2002,2020)



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
    Flag = ifelse(n() == N_siblings,1,0),
    # 下の兄弟の数
    NYS = N_siblings - birthOrder,
    # その年の18未満の最大age
    year_max = max(ifelse(age < 18 , age, NA), na.rm = TRUE),
    year_max = ifelse(is.infinite(year_max),NA,year_max),
    # (18歳未満の)次の兄弟との年齢差
    lag_gap = case_when(
      is.na(year_max) ~ NA,
      TRUE ~ age - year_max,
    ),
    # 例: "条件カラム"が条件値に等しい場合の"カラム名"の最大値を取得
    AGAP = case_when(
      lag_gap <= 0 ~ lag(birthYear, order_by = age) - birthYear,
      lag_gap > 0 ~ lag_gap,
      is.na(lag_gap) ~ NA
    ),
    AGAP = ifelse(is.infinite(AGAP),NA,AGAP),
    age_gap = lag(birthYear, order_by = NYS) - birthYear,
    age_gap = ifelse(is.na(age_gap), 0, age_gap),
    IsLastChild = case_when(
      age_gap == 0 & N_siblings > 1 ~ 1,
      N_siblings > 1 ~ 0,
      age_gap == 0 & N_siblings == 1 ~ 0
    ),
    IsSinglsibling = case_when(
      N_siblings == 1 ~ 1,
      TRUE ~ 0
    ),
    FamilyIncome = mean(FamilyIncome, na.rm = TRUE),
    familySize = mean(familySize, na.rm = TRUE),
    EnjoyRisk = mean(EnjoyRisk, na.rm = TRUE)
  ) %>% arrange(NYS) %>% mutate(
    U18NYS = cumsum(age < 18),
    U18NYS = case_when(
      age < 18 ~ U18NYS - 1,
      TRUE ~ U18NYS
    )
  ) %>% 
  ungroup() %>% group_by(motherID,year,birthYear) %>%
  mutate(
    # 双子を除去
    Isdouble = n()
  ) %>% 
  filter(
    Isdouble == 1
  ) %>% ungroup()

alldf <- alldf %>% mutate(
  IsAbuse = case_when(
    # これは正しそう
    is.na(AbuseCase) ~ NA,
    AbuseCase == 1 ~ 1,
    AbuseCase == 0 ~ 0,
  ),
  AlcoholExp = case_when(
    # これは正しそう
    is.na(Alcoholever) ~ NA,
    Alcoholever == 1 ~ 1,
    Alcoholever == 95 ~ 0,
  )
) %>% group_by(motherID,childID) %>% arrange(year) %>%
  mutate(
    IsAbuse = case_when(
      IsAbuse == 1 ~ 1,
      cumsum(IsAbuse == 1) > 0 ~ 1,
      IsAbuse == 0 ~ 0,
    ),
    AlcoholExp = case_when(
      AlcoholExp == 1 ~ 1,
      cumsum(AlcoholExp == 1) > 0 ~ 1,
      AlcoholExp == 0 ~ 0,
    ),
    MarijuanaExp = case_when(
      MarijuanaExp == 1 ~ 1,
      cumsum(MarijuanaExp == 1) > 0 ~ 1,
      MarijuanaExp == 0 ~ 0,
    ),
    TabacoExp = case_when(
      TabacoExp == 1 ~ 1,
      cumsum(TabacoExp == 1) > 0 ~ 1,
      TabacoExp == 0 ~ 0,
    )
  ) %>% ungroup()


alldf <- alldf %>% mutate(
  IsU18 = case_when(
    # これは正しそう
    age > 17 ~ 0,
    TRUE ~ 1
  ),
  u18_Abuse = case_when(
    # これは正しそう
    IsAbuse == 1 & IsU18 == 1 ~ 1,
    is.na(IsAbuse)  ~ NA,
    TRUE ~ 0
  ),
  o18_Abuse = case_when(
    # これは正しそう
    IsAbuse == 1 & IsU18 == 0 ~ 1,
    is.na(IsAbuse)  ~ NA,
    TRUE ~ 0
  ),
  u18_mariExp = case_when(
    # これは正しそう
    MarijuanaExp == 1 & IsU18 == 1 ~ 1,
    is.na(MarijuanaExp)  ~ NA,
    TRUE ~ 0
  ),
  o18_mariExp = case_when(
    # これは正しそう
    MarijuanaExp == 1 & IsU18 == 0 ~ 1,
    is.na(MarijuanaExp)  ~ NA,
    TRUE~ 0
  ),
  u18_alcExp = case_when(
    # これは正しそう
    AlcoholExp == 1 & IsU18 == 1 ~ 1,
    is.na(AlcoholExp)  ~ NA,
    TRUE ~ 0
  ),
  o18_alcExp = case_when(
    # これは正しそう
    AlcoholExp == 1 & IsU18 == 0 ~ 1,
    is.na(AlcoholExp)  ~ NA,
    TRUE ~ 0
  ),
  u18_tabcExp = case_when(
    # これは正しそう
    TabacoExp == 1 & IsU18 == 1 ~ 1,
    is.na(TabacoExp)  ~ NA,
    TRUE ~ 0
  ),
  o18_tabcExp = case_when(
    # これは正しそう
    TabacoExp == 1 & IsU18 == 0 ~ 1,
    is.na(TabacoExp)  ~ NA,
    TRUE ~ 0
  ),
  substanceExp = case_when(
    # これは正しそう
    AlcoholExp == 1 ~ 1,
    MarijuanaExp == 1 ~ 1,
    TabacoExp == 1 ~ 1,
    IsAbuse == 1 ~ 1,
    !is.na(AlcoholExp) | !is.na(MarijuanaExp) | !is.na(TabacoExp) | !is.na(IsAbuse) ~ 0,
    TRUE ~ NA
  ),
  u18_substanceExp = case_when(
    # これは正しそう
    u18_alcExp == 1 ~ 1,
    u18_mariExp == 1 ~ 1,
    u18_tabcExp == 1 ~ 1,
    u18_Abuse == 1 ~ 1,
    !is.na(u18_alcExp) | !is.na(u18_mariExp) | !is.na(u18_tabcExp) | !is.na(u18_Abuse) ~ 0,
    TRUE ~ NA
  ),
  IsBlack = case_when(
    race != "BLACK" ~ 0,
    race == "BLACK" ~ 1,
  ),
  IsHispanic = case_when(
    race != "HISPANIC" ~ 0,
    race == "HISPANIC" ~ 1,
  ),
  Is1th = case_when(
    birthOrder != 1 & !is.na(birthOrder) ~ 0,
    birthOrder == 1 ~ 1,
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

alldf <- alldf %>% group_by(childID,motherID) %>%
  mutate(
    motherAge = (year - firstSurveyYear) + motherAgeAtBirth,
    u18_alcExp = case_when(
      sum(u18_alcExp == 1, na.rm = TRUE) > 0 ~ 1,
      sum(u18_alcExp == 1, na.rm = TRUE) == 0 ~ 0,
      TRUE ~ NA
    ),
    u18_mariExp = case_when(
      sum(u18_mariExp == 1, na.rm = TRUE) > 0 ~ 1,
      sum(u18_mariExp == 1, na.rm = TRUE) == 0 ~ 0,
      TRUE ~ NA
    ),
    u18_tabcExp = case_when(
      sum(u18_tabcExp == 1, na.rm = TRUE) > 0 ~ 1,
      sum(u18_tabcExp == 1, na.rm = TRUE) == 0 ~ 0,
      TRUE ~ NA
    ),
    u18_Abuse = case_when(
      sum(u18_Abuse == 1, na.rm = TRUE) > 0 ~ 1,
      sum(u18_Abuse == 1, na.rm = TRUE) == 0 ~ 0,
      TRUE ~ NA
    ),
    substanceExp = case_when(
      sum(substanceExp == 1, na.rm = TRUE) > 0 ~ 1,
      sum(substanceExp == 1, na.rm = TRUE) == 0 ~ 0,
      TRUE ~ NA
    ),
    u18_substanceExp = case_when(
      sum(u18_substanceExp == 1, na.rm = TRUE) > 0 ~ 1,
      sum(u18_substanceExp == 1, na.rm = TRUE) == 0 ~ 0,
      TRUE ~ NA
    )
  ) %>% ungroup()


summary <- datasummary(All(alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = alldf,
                       na.rm = TRUE,
                       fmt = 3,
)
summary

alldf <- alldf %>% filter(
  Flag == 1
)

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
  "第1子ダミー" = Is1th,
  "第2子ダミー" = Is2th,
  "第3子ダミー" = Is3th,
  "第4子ダミー" = Is4th,
  "兄弟の数" = N_siblings,
  "弟妹の数" = NYS,
  "未成年の弟妹の数" = U18NYS,
  "下との年齢差" = AGAP,
  "教育年数(母)" = motherEduc,
  "母親の年齢" = motherAge,
  "家族サイズ" = familySize,
  "家族収入" = FamilyIncome,
  "数学スコア" = PIATmath_v,
  "読解力スコア" = PIATrecog_v,
  "読解把握スコア" = PIATcompreh_v,
  "非行スコア" = TroubleScore,
  "語彙力" = PIATvocab_v,
  "物質使用経験(総合)" = substanceExp,
  "飲酒経験" = AlcoholExp,
  "大麻経験" = MarijuanaExp,
  "喫煙経験" = TabacoExp,
  "乱用経験" = IsAbuse,
  "18歳以下ダミー" = IsU18,
  "教育年数" = educ,
  "高校卒業ダミー" = IsGraduate,
  "大学生ダミー" = IsCollegeStudent,
  "リスク_好みダミー" = IsEnjoyRisk,
  "未成年使用ダミー(総合)" = u18_substanceExp,
  "未成年飲酒ダミー" = u18_alcExp,
  "未成年大麻使用ダミー" = u18_mariExp,
  "未成年喫煙ダミー" = u18_tabcExp,
  "未成年乱用ダミー" = u18_Abuse
) %>% select(
    "子供ID",
    "母親ID",
    "出生年",
    "下との年齢差",
    "兄弟の数",
    "弟妹の数",
    "未成年の弟妹の数",
    "年齢",
    "教育年数",
    "調査開始年度",
    "調査年度",
    "女ダミー",
    "母親の年齢",
    "移転over50%ダミー(Y)",
    "出生順位",
    "第1子ダミー",
    "第2子ダミー",
    "第3子ダミー",
    "第4子ダミー",
    # "下との年齢差",
    "未成年使用ダミー(総合)",
    "未成年飲酒ダミー",
    "未成年大麻使用ダミー",
    "未成年喫煙ダミー",
    "未成年乱用ダミー",
    "物質使用経験(総合)",
    "家族サイズ",
    "家族収入",
    "18歳以下ダミー",
    "黒人ダミー",
    "ヒスパニックダミー",
    "リスク_好みダミー",
    "高校卒業ダミー",
    "大学生ダミー"
  )


# 基本統計量 -------------------------------------------------------------------

summary <- datasummary(All(f_alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = f_alldf,
                       na.rm = TRUE,
                       fmt = 3,
)
summary

sapply(alldf[, c("u18_alcExp", "u18_mariExp", "u18_tabcExp", "u18_Abuse")], function(x) sum(is.na(x)))
sum(is.na(alldf$u18_substanceExp))
sum(!is.na(alldf$u18_substanceExp) & !is.na(alldf$u18_alcExp))
sum(!is.na(alldf$u18_substanceExp) & !is.na(alldf$u18_alcExp) & !is.na(alldf$u18_mariExp) & !is.na(alldf$u18_tabcExp)& !is.na(alldf$u18_Abuse))



ggplot(alldf, aes(x = birthOrder, y = substanceExp)) +
  geom_smooth(method = "glm", 
              method.args = list(family = binomial(link = logit)),
              color = "blue", se = FALSE) +
  labs(x = "出生順位", y = "未成年使用ダミー") +
  theme_bw()+
  scale_x_continuous(limits = c(0, NA)) +  # x軸の最小値を0に設定
  scale_y_continuous(limits = c(0, NA))    # y軸の最小値を0に設定

