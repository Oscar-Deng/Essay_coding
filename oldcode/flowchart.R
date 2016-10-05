#' ###**研究流程圖**
#+ flowchart ,echo=FALSE, include=FALSE
flowchart1 <- function(
  nodes = c('研究動機','研究目的','研究架構及流程','探討企業避稅行為之相關文獻','探討企業競爭策略之相關文獻','探討產業競爭程度之相關文獻','研究假說與設計','樣本選取','實證分析','結論與建議'),
  from = c('研究動機','研究目的','研究架構及流程','研究架構及流程','研究架構及流程','探討企業避稅行為之相關文獻','探討企業競爭策略之相關文獻','探討產業競爭程度之相關文獻','研究假說與設計','樣本選取','實證分析'),
  to = c('研究目的','研究架構及流程','探討企業避稅行為之相關文獻','探討企業競爭策略之相關文獻','探討產業競爭程度之相關文獻','研究假說與設計','研究假說與設計','研究假說與設計','樣本選取','實證分析','結論與建議')){
  flow <- create_graph(
  nodes_df = create_nodes(nodes = nodes,
                          label = FALSE,
                          shape = 'rectangle'
  ),
  edges_df = create_edges(from = from,
                          to = to,
                          rel = 'requires',
                          color = 'black'
  ),
  node_attrs = c("fontname = Helvetica",
                 "width = 2",
                 "height = 0.7",
                 "fontsize = 15"),
  edge_attrs = c("color = gray20",
                 "arrowsize = 0.5"))
flow %>% export_graph(file_name = "flow1.png", file_type = "PNG",height = 3620, width = 2500)
}
flowchart1()

#+ chart2 
