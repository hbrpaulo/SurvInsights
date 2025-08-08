#' @importFrom dplyr filter pull
#' @importFrom kableExtra kbl kable_classic row_spec column_spec scroll_box footnote
NULL

complete_tab <- function(aux){
  
  columns <- column_classifier(aux) %>% 
    filter(!(column %in% c('tempos', 'censura', '.y.'))) %>% 
    pull(column)
  
  table <- do.call(rbind,
                   lapply(as.list(columns), FUN = function(x)tab_desc(aux,x)))
  
  table %>%
    kbl('html', digits = 4, escape = FALSE, booktabs = TRUE,
        longtable = TRUE, align = 'c', decimal.mark = ',') %>%
    kable_classic %>%
    row_spec(which(table$highlight=='J1'), bold = T, color = "black",
             align = 'c', background = "grey95") %>%
    column_spec(ncol(table), color = 'white', background = 'white') %>%
    scroll_box(height = "1000px") %>%
    footnote(general = "Here is a general comments of the table. ",
             number = c("Footnote 1; ", "Footnote 2; "),
             alphabet = c("Footnote A; ", "Footnote B; "),
             symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
    )
}
