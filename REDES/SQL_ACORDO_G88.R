library(DBI)
library(tidyverse)

con3 <- dbConnect(odbc::odbc(), "repro_prod", timeout = 10)



PVC_G88 <- 
  
  left_join(PVC_G88_EYEZENKIDS,
            
            PVC_G88_EYEZENKIDS %>% 
              filter(!is.na(TRATAMENTO)) %>% 
              filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
              distinct(TRATAMENTO) %>% 
              data.frame(PROCODIGO2=c("TRPV309","TRHR309","TRCR309","TRCE309","TROP309","TRARSTAND")) ,
            
            by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),]  %>%  
  
            mutate(TBPCODIGO=881) %>%  
  
    select(TBPCODIGO,PROCODIGO,PROCODIGO2,DESCONTOA,DESCONTOB) %>% 
      rename(PROCODIGOA=1,PROCODIGOB=2,CCINDICEPROA2=3,CCINDICEPROB2=4)

q <- data.frame(TBPCODIGO=NA,PROCODIGOA=NA,PROCODIGOB=NA,CCINDICEPROA2=NA,CCINDICEPROB2=NA)


for (i in 1:nrow(PVC_G88)) {
  q[i,] <- PVC_G88[i,]
  query5 <- paste("INSERT INTO TBPCOMBPROPRO (TBPCODIGO,PROCODIGOA,CCINDICEPROA2,PROCODIGOB,CCINDICEPROB2) VALUES (",q[i,"TBPCODIGO"],",'",q[i,"PROCODIGOA"],"',",q[i,"CCINDICEPROA2"],",'",q[i,"PROCODIGOB"],"',",q[i,"CCINDICEPROB2"],");", sep = "")
  dbSendQuery(con3,query5)
}






