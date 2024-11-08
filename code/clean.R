# ctrl + shift + h で　作業ファイル開ける
# ctrl + shift + c コメントアウト
# ctrl + shift + r ラベル
# ctrl + shift + s sourceコマンドと同じ
# ctrl + shift + j ctrl + f と同じ
# ctrl + shift + o ラベル先に移動
# ctrl + shift + d 行をコピー


# 依存関係 --------------------------------------------------------------------

library(tidyverse)

# データのインポート ---------------------------------------------------------------
dfNaive <- read.csv("./data/NLSYdata.csv") 

dfNaive <- dfNaive %>% rename(
  childID = C0000100
)
