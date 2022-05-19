library(magrittr)

files <- list.files(
  #path = "estatutos-jean/txt/", pattern = "\\.txt$",
  path = "estatutos/txt/", pattern = "\\.txt$",
  full.names = TRUE, recursive = TRUE, include.dirs = TRUE
)

db_statutes <- data.frame(
  cnpj = as.numeric(),
  sistema = as.character(),
  date = as.character(),
  statutes_complete = as.character()
)

for (i in 1:length(files)) {
  txt <- readr::read_file(files[i])
  
  db_statutes_i <- data.frame(
    cnpj = paste(stringr::str_extract(files[i], "[0-9]+")),
    sistema = dplyr::case_when(
      stringr::str_detect(txt, stringr::regex("\\bsicoob\\b", ignore_case = TRUE)) ~ "sicoob",
      stringr::str_detect(txt, stringr::regex("\\bsicredi\\b", ignore_case = TRUE)) ~ "sicredi",
      stringr::str_detect(txt, stringr::regex("\\bcresol\\b", ignore_case = TRUE)) ~ "cresol",
      stringr::str_detect(txt, stringr::regex("\\bunicred\\b", ignore_case = TRUE)) ~ "unicred",
      TRUE ~ "outros",
    ),
    date = paste(lubridate::dmy(stringr::str_extract(files[i], "[0-9]*[.]"))),
    statutes_complete = txt
  )
  
  db_statutes <- db_statutes %>% rbind(db_statutes_i)
  rm(db_statutes_i)
  
  cat("Processing ", files[i], " - ", i, "of", length(files), "\n")
}

rm(i, txt)

#____________________________________________________________________________________________________________
base2<-db_statutes$statutes_complete %>% as.character() %>% tm::stripWhitespace()
base3 <- base2
titulo_names <- list()
capitulo_names <- list()

n<- length(files)
count_capit <- c()
for(i in 1:n){
    if(stringr::str_detect(base2[i],"CAPITULO")){ 
      base2[i] <- base2[i] %>% stringr::str_replace_all(.,"CAPITULO","CAPÍTULO")
    }
    base2[i] <- base2[i] %>% stringr::str_replace_all(.,"CAPÍTULO","CAPÍTULO @c")
    base2[i] <- base2[i] %>% stringr::str_replace_all(.,"Art","Art @a")
    base2[i] <- base2[i] %>% stringr::str_replace_all(.,"ART","Art @a")
    if(stringr::str_detect(base2[i],  "CAPÍTULO @c") == TRUE){
      count_capit[i] <- i}
}
     count_capit <- count_capit[!is.na(count_capit)]   
     
n<- length(files)
count_titulo<-c()
for(i in 1:n){
  if(stringr::str_detect(base2[i],"TITULO")){
    base3[i] <- base3[i] %>% stringr::str_replace_all(.,"TITULO","TÍTULO")
  }
  base3[i] <- base3[i] %>% stringr::str_replace_all(.,"TÍTULO","TÍTULO @t")
  base3[i] <- base3[i] %>% stringr::str_replace_all(.,"Art","Art @a")
  base3[i] <- base3[i] %>% stringr::str_replace_all(.,"ART","Art @a")
        if(stringr::str_detect(base3[i], "TÍTULO @t") == TRUE){
            count_titulo[i] <- i }
}

  count_titulo<-count_titulo[!is.na(count_titulo)]

#--------indicadores de capitulo e/ou titulo
  indicador.cap_titulo <- list(count_capit,count_titulo)
  
#------------------------------------------------------------------------------
#SEPARAÇÃO POR TITULO
 session.save.titulo<-list()
 for(j in count_titulo){
   session.save.titulo[[j]] <- stringr::str_split(base3[j], "TÍTULO @t") %>% unlist() %>% data.frame()
     #quero excluir a primeira linha de cada um deles
   #session.save.titulo[[j]] <- session.save.titulo[[j]][[1]][-1] %>% data.frame()
 }
 
 
   #SEPARAÇÃO POR CAPITULO
session.save.capitulo<-list()
for(j in count_capit){
  session.save.capitulo[[j]] <- stringr::str_split(base2[j], "CAPÍTULO @c") %>% unlist() %>% data.frame()
  session.save.capitulo[[j]] <- session.save.capitulo[[j]][[1]][-1] %>% data.frame()
}
  
db_cap_titulos <- list(session.save.titulo,session.save.capitulo)
rm(session.save.capitulo,session.save.titulo)
#-------------------------------------------------------------------------------


# capturando os nomes das sessões e colocando em uma lista para futuro uso:
#todo nome acaba antes de um art° de acordo com exemplo
#Vou fazer o programa pegar do começo da string até o simbolo 
save.sec_noms <- list()
save.sec_noms2 <- list()
save.art_sep <- list()
noms_sec <- c()
cap_get <- c()
art_sep <- list()


z<-2
db_cap_titulos[[z]] <- db_cap_titulos[[z]] %>% .[. != "NULL"] #tirando elementos nulos do código, lembrar de indicador.cap-titulo
m <- length(db_cap_titulos[[z]])
for(j in 1:m){  
  cap_get <- db_cap_titulos[[z]][[j]] #1 titulos, 2 para capítulos
  n <- dim(cap_get)[1]
  
  if(n > 1){
    for(i in 1:n){
      art_sep[[i]] <-  cap_get[i,1] %>% stringr::str_split(.,"Art @a.") #separa por artigos
      noms_sec[i]   <-  art_sep[[i]][[1]][1]
      
    }
    save.art_sep[[j]] <- art_sep #separação por artigos
    save.sec_noms[[j]] <- noms_sec %>% data.frame() #separado por sessão
    
    save.sec_noms2[[j]] <- cbind(db_cap_titulos[[2]][[j]],save.sec_noms[[j]])
    noms_sec <- c()
    art_sep <- c()
    
  }
}


x <- do.call(rbind,save.sec_noms2)
noms_change <- x[,2] %>%  unlist()   %>% unique()   %>% data.frame() 
 
# #problema está excluindo a pontuação
 for(i in 1:dim(noms_change )[1]){
    x <- noms_change[i,1]  %>% stringr::word(.,1,2) 
   noms_change[i,1] <- gsub(x,"", noms_change[i,1])
    }
 

noms_change2 <- noms_change
for(i in 1:dim(noms_change)[1]){
  noms_change2[i,1] <- noms_change[i,1] %>%   stringr::str_extract(., "\\b[A-Z](?:[A-Z ]|[À-Ü]|[:punct:])*[A-Z]\\b") #NA PORQUE ESTÃO EM MINUSCULO
}
  

  noms_change2 <- noms_change2 %>% na.omit()  %>% unlist() %>%.[order(nchar(.), .)] %>% data.frame() #é um problema por causa das sessões com letras minusculas, preciso resolver isso depois
  base3<-db_statutes$statutes_complete %>% as.character() %>% tm::stripWhitespace()
 
  result <- list()
  save.result <- list()
  result.cat <- list()
  save.cat <- list()
  
  for(j in 1:length(base3)){
    for(i in 1: dim(noms_change2)[1]){
      aux <- stringr::str_detect(base3[j], noms_change2[i,1] %>% stringr::fixed())
      if(aux == TRUE){
        result[i] <-  noms_change2[i,1]
        base3[j] <-  stringr::str_replace(base3[j],noms_change2[i,1], paste0("cat-",j,"/",i))
        
      }
   }
    result <- result %>% unique()
    save.result[[j]] <-  result[result !="NULL"] 
    result <- list()
}
  
 #dividindo de acordo com as os nomes coletados
  y <- base3[[1]] %>% stringr::str_extract_all(., "cat-\\d+/\\d+") %>% data.frame()
  x <- base3[[1]] %>% stringr::str_split(., "cat-\\d+/\\d+") %>% data.frame()
  View(x)
  
  #Agora só preciso cortar base2 usando os dados de save.result
  #Lembrar de depois usar os cat-j-i para dividir por categorias
  
  
  
  
  
  
  
  # 
  # data.frame(noms_change,noms_change2) %>% rio::export(.,"Noms-Sec.xlsx")  #exportando colocar as categorias manualmente
  # noms_change3 <- readxl::read_excel("Noms-Sec2.xlsx") %>% data.frame()#tabela já com as categorias
  
  

    