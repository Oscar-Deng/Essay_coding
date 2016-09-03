# 處理論文統計分析過程說明
Oscar-Deng  
2016年7月31日  
##**前言**
編輯人：鄧孝航
聯絡信箱：[402391174@mail.fju.edu.tw](mailto:402391174@mail.fju.edu.tw,402391174@mail.fju.edu.tw?subject=Questions&body=你好，我想請教關於...%0A請盡速回復，謝謝)
內容如有不當煩請告知，謝謝！
為了推廣「可重複研究**(Reproducible Research)**」的概念並方便將來再次研究分析，故建立此說明檔解釋相關的R語言函數及數據處理過程。
有關於可重複研究的概念，可參考維基百科[**(Reproducible Research)**](https://en.wikipedia.org/wiki/Reproducibility#Reproducible_research)。

本分析使用R語言作為統計分析之工具，並搭配R、Rstudio、Excel、TEJ資料庫。
\n
\n



```r
png(filename = "flow1.png")
```


###**程式碼架構圖**

> 
1. TEJ資料庫抓取資料建立分析資料庫。**(Getting Data)**
2. 整理資料至可使用程度(排除不需要的欄位)。**(Preparing Data)**
3. 產生虛擬變數及可供分析建模的變數。**(Produce Variables)**
4. 以線性多變量回歸模型分析資料，並製作相關分析表。**(Analyze)**
5. 產生報表。**(Produce reports and graphs)**
6. 解釋分析結果。**(Explain)**

---
title: "makermd.R"
author: "OOSKA"
date: "Wed Aug 31 17:21:10 2016"
---
