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
<br>
<br>
###**研究流程圖**

<!--html_preserve--><div id="htmlwidget-e1b112b48a39415ce27a" style="width:672px;height:480px;" class="DiagrammeR html-widget"></div>
<script type="application/json" data-for="htmlwidget-e1b112b48a39415ce27a">{"x":{"diagram":"\ngraph TB\nA(<U+7814><U+7A76><U+52D5><U+6A5F>) --> B(<U+7814><U+7A76><U+76EE><U+7684>)\nB --> C(<U+7814><U+7A76><U+67B6><U+69CB><U+53CA><U+6D41><U+7A0B>)\nC --> D1(<U+63A2><U+8A0E><U+4F01><U+696D><U+907F><U+7A05><U+884C><U+70BA><U+4E4B><U+76F8><U+95DC><U+6587><U+737B>)\nC --> D2(<U+63A2><U+8A0E><U+4F01><U+696D><U+7AF6><U+722D><U+7B56><U+7565><U+4E4B><U+76F8><U+95DC><U+6587><U+737B>)\nC --> D3(<U+63A2><U+8A0E><U+7522><U+696D><U+7AF6><U+722D><U+7A0B><U+5EA6><U+4E4B><U+76F8><U+95DC><U+6587><U+737B>)\nD1 --> E(<U+7814><U+7A76><U+5047><U+8AAA><U+8207><U+8A2D><U+8A08>)\nD2 --> E\nD3 --> E\nE --> F(<U+6A23><U+672C><U+9078><U+53D6>)\nF --> G(<U+5BE6><U+8B49><U+5206><U+6790>)\nG --> H(<U+7D50><U+8AD6><U+8207><U+5EFA><U+8B70>)\n"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

程式碼架構圖

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
date: "Sun Aug 14 16:46:44 2016"
---
