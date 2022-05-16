#antes de importar faça a divisão de acordo com as categorias --------caminho 2
sec_noms_change2 <-readxl::read_excel("sec_noms_change2.xlsx")


#-----------------------------------------------------------------------------------------------------
#TIRANDO ESSES NUMEROS ROMANOS

for(i in 1:dim(sec_noms_change2)[1]){
  x <- sec_noms_change2[i,1] %>% stringr::str_extract_all(.,"\\w+") %>% unlist()
  x <- x[-1]
  x <- paste(x,collapse=" ")
  sec_noms_change2[i,1] <- x
}

sec_noms_change2 <-  sec_noms_change2 %>% unique()

#_______________________________________________________________________________

#ADD as combinações de numero romanos para determinar um capitulo
combinacoes_sec_noms_change2 <- list()
for(i in 1:dim(sec_noms_change2)[1]){
  combinacoes_sec_noms_change2[[i]] <-   paste(as.roman(1:100), sec_noms_change2[i,1]) %>% data.frame()
}
db_combinacoes <-  do.call(rbind,combinacoes_sec_noms_change2) 
#vou usar isso para buscar em cada cooperativa
#_______________________________________________________________________________


#agora criar código que separa buscando os padrões na lista acima

base3<-db_statutes$statutes_complete %>% as.character() %>% tm::stripWhitespace()


result <- list()
for(i in 1:dim(db_combinacoes)[1]){
  aux <- stringr::str_detect(base3[j], stringr::fixed(db_combinacoes[i,1]) )
      if(aux == TRUE){
        result[i] <-  (db_combinacoes[i,1])
      }
}

    result <- result[result !="NULL"]


#------------------------------------------------------------------------------