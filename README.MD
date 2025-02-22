# Overview

The codes here are R based re-write of exercises in a book "Modern Marketing Research(現代マーケティングリサーチ)". The book uses GUI based tool for R without no explanation of R commands. 

# Reference book
http://www.yuhikaku.co.jp/books/detail/9784641164161

## Table of contents
- 第１章　マーケティング意思決定とリサーチ・デザイン
- 第１部　調査編
  - 第２章　データの取得と整理
  - 第３章　サンプリング
    - sample_n, sample_frac (dplyr)
  - 第４章　質問紙の作成と測定尺度
- 第２部　分析編
  - 第５章　市場反応分析１：回帰モデル
    - cor.test
    - lm
  - 第６章　市場の発見と知覚マップ：因子分析
    - factanal
    - hclust
  - 第７章　市場セグメンテーション：クラスター分析
    - kmeans
    - hclust
  - 第８章　製品開発：コンジョイント分析
    - lm
  - 第９章　新製品の普及：バスモデル
  - 第10章　顧客の管理：RFM分析，分散分析，ロジスティック回帰分析
    - aov
    - TukeyHSD
    - glm 
- 第３部　上級編
  - 第11章　市場反応分析２：離散選択モデル
    - glm
    - mlogit (mlogit)
  - 第12章　ブランドと属性の同時マップ：コレスポンデンス分析
    - chisq.test
    - ca (ca)
  - 第13章　マーケットバスケットとクロスセリング：アソシエーション分析
    - apriori (arules)
  - 第14章　定性調査データの分析：潜在変数の構造分析
    - sem (sem, lavaan)
- 補　論
  - 補論Ａ　確率と統計の基礎
  - 補論Ｂ　回帰分析の統計理論およびその周辺
  - 補論Ｃ　RおよびRコマンダーの準備
