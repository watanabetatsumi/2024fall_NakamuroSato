
# Texについて -----------------------------------------------------------------

# Texの頭部と末尾に追加
# \documentclass{article}
# \usepackage{booktabs}
# \usepackage{fontspec}
# \usepackage{luatexja}
# \begin{document}

# ~hogehoge~

# \end{document}


# 依存関係 --------------------------------------------------------------------
# install.packages("margins")
library(tidyverse)
library(modelsummary)
library(gtsummary)
library(fixest)
library(broom)
library(margins)

setwd("C:/Users/watan/2024fall_NakamuroSato/")
# source("./code/clean.R")

# 中間ファイルを削除
RemoveFiles()

# ヒストグラム -------------------------------------------------------------------

hist(clean_alldf$AGAP_ij)
hist(clean_alldf$AGAP_ijt)
hist(clean_alldf$NYG_ij)
hist(clean_alldf$NYG_ijt)
hist(clean_alldf$Age)
hist(clean_alldf$BirthYear)
hist(clean_alldf$Year)
hist(clean_alldf$MainTransfer)
hist(clean_alldf$BirthOrder)
hist(clean_alldf$IsLiveTogether)
hist(clean_alldf$IsTransfered)
hist(clean_alldf$IsTransfer_over50)
hist(clean_alldf$U18_SubstanceExp)

# shinto()

# 一段階目データ -----------------------------------------------------------------

dfstage1 <- clean_alldf %>% filter(
  NYG_ij < 4
)

# 明らかにNYGの数が3以降少ないので、除去(1%以下)
table(dfstage1$NYG_ij)


f_dfstage1 <- makeStatic_df(dfstage1)

# 基本統計量 -------------------------------------------------------------------

summary <- datasummary(All(f_dfstage1) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = f_dfstage1,
                       na.rm = TRUE,
                       fmt = 3,
)
summary

# （総合）線形回帰モデル(1-1) -------------------------------------------------------------------

# 18歳以降の親への信念を更新しないとしたとき、すなわち、成人後の出生順位の影響を受けない。
# （固有のBithOrder（潜在的なNYG（親が計画的に生むことを仮定し、それに従って躾を行う））の影響のみをうける_ij）
# 経験ダミー(ij)は個人の固有の効果のみに依存する。

B1model_lm_factor <- feols(U18_SubstanceExp ~ factor(NYG_ij) + AGAP_ij
                    + Isfemale
                    + N_siblings
                    + IsUrban
                    + IsEnjoyRisk
                    | MotherID 
                    + BirthYear
                    , data = dfstage1
)
summary(B1model_lm_factor)
etable(B1model_lm_factor)

B1model_lm <- feols(U18_SubstanceExp ~ NYG_ij + AGAP_ij
                    + Isfemale
                    + N_siblings
                    + IsUrban
                    + IsEnjoyRisk
                    | MotherID
                    + BirthYear
                    , data = dfstage1
)
summary(B1model_lm)
etable(B1model_lm)


# （総合）ロジットモデル(1-2) -------------------------------------------------------------

B1model_glm_factor <- feglm(U18_SubstanceExp ~ factor(NYG_ij) + AGAP_ij
                     + Isfemale
                     + N_siblings
                     | MotherID 
                     + BirthYear
                     , family = binomial(link = logit)
                     , data = dfstage1
)
summary(B1model_glm_factor)
exp(coef(B1model_glm_factor))

B1model_glm <- feglm(U18_SubstanceExp ~ NYG_ij + AGAP_ij
                     + Isfemale
                     + N_siblings
                     | MotherID 
                     + BirthYear
                     , family = binomial(link = logit)
                     , data = dfstage1
)
summary(B1model_glm)
exp(coef(B1model_glm))


if (file.exists("./outputs/lm_result1-1.tex")) {
  file.remove("./outputs/lm_result1-1.tex")
}
etable(B1model_lm, B1model_glm,
       tex = TRUE, # LaTeXコードとして出力
       # headers = list("Sample:" = list("線形モデル","非線形モデル")),
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result1-1.tex" # 出力先のファイル
       # caption = "回帰モデルの結果" # テーブルのタイトル
)
AddHeader("lm_result1-1")
RemoveFiles()

if (file.exists("./outputs/lm_result1-2.tex")) {
  file.remove("./outputs/lm_result1-2.tex")
}
etable(B1model_lm_factor, B1model_glm_factor,
       tex = TRUE, # LaTeXコードとして出力
       # headers = list("Sample:" = list("線形モデル","非線形モデル")),
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result1-2.tex" # 出力先のファイル
       # caption = "回帰モデルの結果" # テーブルのタイトル
)
AddHeader("lm_result1-2")
RemoveFiles()


# 男女の異質性 ------------------------------------------------------------------

B1model_lm1 <- feols(U18_SubstanceExp ~ NYG_ij + AGAP_ij
                    + NYG_ij:Isfemale
                    + Isfemale
                    + N_siblings
                    + IsUrban
                    + IsEnjoyRisk
                    | MotherID
                    + BirthYear
                    , data = dfstage1
)
summary(B1model_lm1)

B1model_lm2 <- feols(U18_mariExp ~ NYG_ij + AGAP_ij
                    + NYG_ij:Isfemale
                    + Isfemale
                    + N_siblings
                    + IsUrban
                    + IsEnjoyRisk
                    | MotherID
                    + BirthYear
                    , data = dfstage1
)
summary(B1model_lm2)

B1model_lm3 <- feols(U18_alcExp ~ NYG_ij + AGAP_ij
                    + NYG_ij:Isfemale
                    + Isfemale
                    + N_siblings
                    + IsUrban
                    + IsEnjoyRisk
                    | MotherID
                    + BirthYear
                    , data = dfstage1
)
summary(B1model_lm3)

B1model_lm4 <- feols(U18_tabcExp ~ NYG_ij + AGAP_ij
                    + NYG_ij:Isfemale
                    + Isfemale
                    + N_siblings
                    + IsUrban
                    + IsEnjoyRisk
                    | MotherID
                    + BirthYear
                    , data = dfstage1
)
summary(B1model_lm4)


if (file.exists("./outputs/lm_result1-3.tex")) {
  file.remove("./outputs/lm_result1-3.tex")
}
etable(B1model_lm1,B1model_lm2,B1model_lm3,B1model_lm4,
       tex = TRUE, # LaTeXコードとして出力
       # headers = list("Sample:" = list("総合","大麻","飲酒","喫煙")),
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
                U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year",
                IsTransfer_over50 = "譲渡over50%ダミー",
                IsTransfered =　"譲渡ダミー",
                IsLiveTogether = "同居ダミー"),
       file = "./outputs/lm_result1-3.tex" # 出力先のファイル
       # caption = "回帰モデルの結果" # テーブルのタイトル
)
AddHeader_wide("lm_result1-3")
RemoveFiles()

# （その他）線形回帰モデル(2-1) ------------------------------------------------------------

# 先行研究との整合性
B1model_lm_nonegraduate <- feols(IsNoneGraduate ~ NYG_ij + AGAP_ij
                              + Isfemale
                              + IsUrban
                              # + N_siblings
                              + IsEnjoyRisk
                              | MotherID 
                              + BirthYear
                              , data = dfstage1
)
summary(B1model_lm_nonegraduate)

B1model_lm_marijuana <- feols(U18_mariExp ~ NYG_ij + AGAP_ij
                              + Isfemale
                              # + IsUrban
                              + N_siblings
                              | MotherID 
                              + BirthYear
                              , data = dfstage1
)
summary(B1model_lm_marijuana)

B1model_lm_alcohol <- feols(U18_alcExp ~ NYG_ij + AGAP_ij
                            + Isfemale
                            # + IsUrban
                            + N_siblings
                            | MotherID 
                            + BirthYear
                            , data = dfstage1
)
summary(B1model_lm_alcohol)

B1model_lm_tabaco <- feols(U18_tabcExp ~ NYG_ij + AGAP_ij
                           + Isfemale
                           # + IsUrban
                           + N_siblings
                           | MotherID 
                           + BirthYear
                           , data = dfstage1
)
summary(B1model_lm_tabaco)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result2-1.tex")) {
  file.remove("./outputs/lm_result2-1.tex")
}
etable(B1model_lm, B1model_lm_marijuana, B1model_lm_alcohol, B1model_lm_tabaco, B1model_lm_nonegraduate,
  tex = TRUE, # LaTeXコードとして出力
  headers = list("Sample:" = list("OLS")),
  dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
           U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
           IsNoneGraduate = "高校中退ダミー",
           NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
           Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
           MotherID = "Mother ID", BirthYear = "Birth Year"),
  file = "./outputs/lm_result2-1.tex" # 出力先のファイル
)
AddHeader_wide("lm_result2-1")
RemoveFiles()


# （その他）ロジット回帰モデル(2-2) ---------------------------------------------------------------

# 先行研究との整合性
B1model_glm_nonegraduate <- feglm(IsNoneGraduate ~ NYG_ij + AGAP_ij
                                 + Isfemale
                                 # + IsUrban
                                 + N_siblings
                                 | MotherID 
                                 + BirthYear
                                 , family = binomial(link = logit)
                                 , data = dfstage1
)
summary(B1model_glm_nonegraduate)
exp(coef(B1model_glm_nonegraduate))


B1model_glm_marijuana <- feglm(U18_mariExp ~ NYG_ij + AGAP_ij
                               + Isfemale
                               # + IsUrban
                               + N_siblings
                               | MotherID 
                               + BirthYear
                               , family = binomial(link = logit)
                               , data = dfstage1
)
summary(B1model_glm_marijuana)
exp(coef(B1model_glm_marijuana))

B1model_glm_alcohol <- feglm(U18_alcExp ~ NYG_ij + AGAP_ij
                             + Isfemale
                             # + IsUrban
                             + N_siblings
                             | MotherID
                             + BirthYear
                             , family = binomial(link = logit)
                             , data = dfstage1
)
summary(B1model_glm_alcohol)
exp(coef(B1model_glm_alcohol))

B1model_glm_tabaco <- feglm(U18_tabcExp ~ NYG_ij + AGAP_ij
                            + Isfemale
                            # + IsUrban
                            + N_siblings
                            | MotherID 
                            + BirthYear
                            , family = binomial(link = logit)
                            , data = dfstage1
)
summary(B1model_glm_tabaco)
exp(coef(B1model_glm_tabaco))

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result2-2.tex")) {
  file.remove("./outputs/lm_result2-2.tex")
}
etable(B1model_glm, B1model_glm_marijuana, B1model_glm_alcohol, B1model_glm_tabaco, B1model_glm_nonegraduate,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("Logit")),
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
                U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
                IsNoneGraduate = "高校中退ダミー",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result2-2.tex" # 出力先のファイル
)
AddHeader_wide("lm_result2-2")
RemoveFiles()

