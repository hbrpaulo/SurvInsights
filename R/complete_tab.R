#' @importFrom dplyr filter pull
#' @importFrom kableExtra kbl kable_classic row_spec column_spec scroll_box footnote
NULL

complete_tab <- function(data, time_col = "tempos", event_col = "censura", scroll = TRUE){

  columns <- column_classifier(data) %>%
    filter(!(column %in% c(time_col, event_col, '.y.'))) %>%
    pull(column)

  table <- do.call(rbind,
                   lapply(as.list(columns),
                          FUN = function(x) tab_desc(data, x,
                                                      time_col = time_col,
                                                      event_col = event_col)))
  
  table <- table %>%
    kbl('html', digits = 4, escape = FALSE, booktabs = TRUE,
        longtable = TRUE, align = 'c', decimal.mark = ',') %>%
    kable_classic %>%
    row_spec(which(table$highlight=='J1'), bold = T, color = "black",
             align = 'c', background = "grey95") %>%
    column_spec(ncol(table), color = 'white', background = 'white') %>%
    footnote(general = "Here is a general comments of the table. ",
             number = c("Footnote 1; ", "Footnote 2; "),
             alphabet = c("Footnote A; ", "Footnote B; "),
             symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
    )
  if(scroll){
    table <- table %>% 
    scroll_box(height = "600px")}
  return(table)
}
