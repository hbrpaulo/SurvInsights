source('https://raw.githubusercontent.com/hbrpaulo/Misc/refs/heads/main/format_sig.R')
source('https://raw.githubusercontent.com/hbrpaulo/Misc/refs/heads/main/msdr.R')


# pegar todas as covariaveis para tempo, exceto censura
vetor <- sort(setdiff( names(df), c('censura', 'tempos') ))

# ideia: ordenar variaveis por numericas e categoricas
column_classifier <- Vectorize(function(column){
  class(pull(df, column))
})

vetor <- column_classifier(vetor) %>%
  data.frame %>%
  tibble::rownames_to_column() %>% 
  pull(rowname)

# testar vetores um por um
# for(i in vetor){
#   print(i)
#   tab_desc(column = i, df = df) %>%
#     print
#   Sys.sleep(.1)
# }

link_logrank <- 'https://en.wikipedia.org/wiki/Logrank_test'
tabelao <- do.call(rbind, lapply(as.list(vetor), 
                                 FUN = function(x){tab_desc(df_fic, x)})) %>% 
  # adicionar link sobre cada teste utilizado
  mutate(test = case_when(
    test=='(Logrank)' ~ cell_spec(test, link = link_logrank)))

# a fazer:
# adicionar p
# fazer coloracoes
# adicionar algo para censura

# comentarios gerais
# formula n funciona com variavel contendo espaco na escrita
# survdiff(data = df, Surv(df$tempos, df$censura)~`Variavel binaria`)
## solucao paleativa: janitor::clean_names()