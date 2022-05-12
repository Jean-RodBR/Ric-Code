#usando código do bruno para extrair apenas seções

library(magrittr)

base2<-db_statutes$statutes_complete %>% as.character() %>% tm::stripWhitespace()
titulo_names <- list()
capitulo_names <- list()

n<- length(files)
for(i in 1:n){
  {#alterar nomes errados
    if(stringr::str_detect(base2[i],"TITULO")){
      base2[i] <- base2[i] %>% stringr::str_replace_all(.,"TITULO","TÍTULO")
    }
    if(stringr::str_detect(base2[i],"CAPITULO")){ 
      base2[i] <- base2[i] %>% stringr::str_replace_all(.,"CAPITULO","CAPÍTULO")
    }
  }
  
  base2[i] <- base2[i] %>% stringr::str_replace_all(.,"TÍTULO","TÍTULO @t")
  base2[i] <- base2[i] %>% stringr::str_replace_all(.,"CAPÍTULO","CAPÍTULO @c")
  base2[i] <- base2[i] %>% stringr::str_replace_all(.,"Art","Art @a")
  base2[i] <- base2[i] %>% stringr::str_replace_all(.,"ART","Art @a")
  
  
  titulo_names[[i]] <-  gsubfn::strapplyc(base2[i], "TÍTULO @t", simplify = TRUE)
  capitulo_names[[i]] <- gsubfn::strapplyc(base2[i], "CAPÍTULO @c", simplify = TRUE)
  
}
#------------------------------------------------------------------------------
#SEPARAÇÃO POR TITULO
session.save.titulo<-list()
for(j in 1:length(files)){
  session.save.titulo[[j]] <- stringr::str_split(base2[j], "TÍTULO @t") %>% unlist() %>% data.frame()
}

#SEPARAÇÃO POR CAPITULO
session.save.capitulo<-list()
for(j in 1:length(files)){
  session.save.capitulo[[j]] <- stringr::str_split(base2[j], "CAPÍTULO @c") %>% unlist() %>% data.frame()
}
#-------------------------------------------------------------------------------

db_cap_titulos <- list(session.save.titulo,session.save.capitulo)
rm(session.save.capitulo,session.save.titulo)

#----------------
#--------------
#-------------
# capturando os nomes das sessões e colocando em uma lista para futuro uso:
#todo nome acaba antes de um art° de acordo com exemplo
#Vou fazer o programa pegar do começo da string até o simbolo 
save.sec_noms <- list()
save.sec_noms2 <- list()
save.art_sep <- list()
noms_sec <- c()
cap_get <- c()
art_sep <- list()

m <- length(db_cap_titulos[[2]])
for(j in 1:m){  
  cap_get <- db_cap_titulos[[2]][[j]] #1 titulos, 2 para capítulos
  n <- dim(cap_get)[1]
  if(n > 1){
    for(i in 1:n){
      art_sep[[i]] <-  cap_get[i,1] %>% stringr::str_split(.,"Art @a.") #separa por artigos
      noms_sec[i]   <-  art_sep[[i]][[1]][1]
      # {#TIRANDO ESSES NUMEROS ROMANOS
      #   x <- noms_sec[i] %>% stringr::str_extract_all(.,"\\w+") %>% unlist()
      #   x <- x[-1]
      #   x <- paste(x,collapse=" ")
      #   noms_sec[i] <- x
      # }
    }
    save.art_sep[[j]] <- art_sep #separação por artigos
    save.sec_noms[[j]] <- noms_sec %>% data.frame() #separado por sessão
    
    save.sec_noms2[[j]] <- cbind(db_cap_titulos[[2]][[j]],save.sec_noms[[j]])
    noms_sec <- c()
    art_sep <- c()
    
  }
}


#pegando elementos não nulos do resultado
save.sec_noms2 <- save.sec_noms2 %>% .[. != "NULL"]


  x <- do.call(rbind,save.sec_noms2)
  
  
  
noms_change <- x[,2] %>%  unlist() %>% unique() %>% data.frame() #colocando tudo em um data.frame depois dando unique
#Agora pegar esse resultado, comparar manualmente (n sei outro jeito) e criar um sistema de classificação
#noms_change <-noms_change %>% stringr::str_detect(.,"ESTATUTO")


#_______________________________________________________________________________
rio::export(noms_change,"sec_noms_change.xlsx")
sec_noms_change2 <- readxl::read_excel("sec_noms_change2.xlsx") #manualmente criar uma coluna do lado com o padrão desejado
sec_pure <- sec_noms_change2$ALTERADO %>% unique()  %>% .[order(nchar(.), .)]
rio::export(sec_pure,"add-cod.xlsx")
#_______________________________________________________________________________





