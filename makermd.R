#' ---
#' title: "處理論文統計分析過程說明"
#' author: "Oscar-Deng"
#' date: "2016年7月31日"
#' output: 
#'   html_document:
#'     keep_md: yes
#'     theme: cosmo
#'     toc: yes
#'     toc_depth: 5
#' ---

#' ##**前言**
#' 編輯人：鄧孝航
#' 聯絡信箱：[402391174@mail.fju.edu.tw](mailto:402391174@mail.fju.edu.tw,402391174@mail.fju.edu.tw?subject=Questions&body=你好，我想請教關於...%0A請盡速回復，謝謝)
#' 內容如有不當煩請告知，謝謝！
#' 為了推廣「可重複研究**(Reproducible Research)**」的概念並方便將來再次研究分析，故建立此說明檔解釋相關的R語言函數及數據處理過程。
#' 有關於可重複研究的概念，可參考維基百科[**(Reproducible Research)**](https://en.wikipedia.org/wiki/Reproducibility#Reproducible_research)。
#' 
#' 本分析使用R語言作為統計分析之工具，並搭配R、Rstudio、Excel、TEJ資料庫。
#' <br>
#' <br>
#' ###**研究流程圖**
#+ flowchart ,echo=FALSE, results="asis"

flowchart1 <- function(){
  nodes <-
    create_nodes(nodes = c("研究動機","研究目的","研究架構及流程","探討企業避稅\n行為之相關文獻","探討企業競爭\n策略之相關文獻","探討產業競爭\n程度之相關文獻","研究假說與設計","樣本選取","實證分析","結論與建議"),
                 label = FALSE,
                 shape = "rectangle"
    )
  
  edges <-
    create_edges(from = c("研究動機","研究目的","研究架構及流程","研究架構及流程","研究架構及流程","探討企業避稅\n行為之相關文獻","探討企業競爭\n策略之相關文獻","探討產業競爭\n程度之相關文獻","研究假說與設計","樣本選取","實證分析"),
                 to = c("研究目的","研究架構及流程","探討企業避稅\n行為之相關文獻","探討企業競爭\n策略之相關文獻","探討產業競爭\n程度之相關文獻","研究假說與設計","研究假說與設計","研究假說與設計","樣本選取","實證分析","結論與建議"),
                 rel = "requires",
                 color = "black"
    )
  
  graph <-
    create_graph(nodes_df = nodes,
                 edges_df = edges,
                 node_attrs = c("fontname = Helvetica",
                                "width = 2",
                                "height = 0.8",
                                "fontsize = 15"),
                 edge_attrs = c("color = gray20",
                                "arrowsize = 0.5"))
  
  # View the graph in the RStudio Viewer
  render_graph(graph)
}
fn1 <- cmpfun(flowchart1)
#,width=15,height=20,units = 'cm'
png('flowchart1.png')
fn1()
dev.off()

#' 程式碼架構圖
#' 
#' > 
#' 1. TEJ資料庫抓取資料建立分析資料庫。**(Getting Data)**
#' 2. 整理資料至可使用程度(排除不需要的欄位)。**(Preparing Data)**
#' 3. 產生虛擬變數及可供分析建模的變數。**(Produce Variables)**
#' 4. 以線性多變量回歸模型分析資料，並製作相關分析表。**(Analyze)**
#' 5. 產生報表。**(Produce reports and graphs)**
#' 6. 解釋分析結果。**(Explain)**
