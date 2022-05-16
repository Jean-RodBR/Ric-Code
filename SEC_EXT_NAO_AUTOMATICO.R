#SCRIPT NÃO AUTOMATICO


base2<-db_statutes$statutes_complete %>% as.character() %>% tm::stripWhitespace()

sessao_nome <- readxl::read_excel("manual_coleta_session.xlsx")No

