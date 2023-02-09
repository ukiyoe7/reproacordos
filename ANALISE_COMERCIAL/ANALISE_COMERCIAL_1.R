library(DBI)
library(tidyverse)
library(googlesheets4)
library(xlsx)

con2 <- dbConnect(odbc::odbc(), "reproreplica")

cli <- dbGetQuery(con2,"SELECT DISTINCT C.CLICODIGO,
                           CLINOMEFANT,
                            CLIRAZSOCIAL,
                             CLICNPJCPF,
                              REPLACE(
                               REPLACE(
                                REPLACE(CLICNPJCPF,'.',''),'/',''),'-','') CNPJ,
                                   C.GCLCODIGO,
                                    GCLNOME,  
                                     IIF(C.GCLCODIGO IS NULL,C.CLICODIGO || ' ' || CLINOMEFANT,'G' || C.GCLCODIGO || ' ' || GCLNOME) CLIENTE,
                                      SETOR
                                       FROM CLIEN C
                                        LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,CIDNOME,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                                         LEFT JOIN CIDADE CID ON E.CIDCODIGO=CID.CIDCODIGO
                                          LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28))Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                           LEFT JOIN GRUPOCLI GR ON C.GCLCODIGO=GR.GCLCODIGO
                                            WHERE CLICLIENTE='S'")



cli %>% 
   group_by(CLIRAZSOCIAL,CLINOMEFANT,CLICNPJCPF,GCLCODIGO) %>% 
        summarize(C=n_distinct(CNPJ)) %>%
          filter(is.na(GCLCODIGO)) %>% 
            filter(CNPJ!="00000000000000") %>% View()

## CLIENTES LOJAS

lojas <- 
      cli %>% 
        filter(is.na(GCLCODIGO)) %>% 
          group_by(CLIRAZSOCIAL,CLINOMEFANT,CNPJ,GCLCODIGO) %>% 
           summarize(C=n_distinct(CNPJ)) %>%
            mutate(CNPJ=as.character(CNPJ)) %>%  
             filter(CNPJ!="00000000000000")

View(lojas)


write.csv2(lojas,file="TEST.csv",row.names = FALSE)


# CLIENTES GRUPOS

grupos <- 
  cli %>% 
   group_by(GCLNOME,GCLCODIGO) %>% 
    summarize(C=n_distinct(GCLNOME)) %>% 
     filter(CNPJ!="00000000000000") %>% 
       write.csv2(.,file="ANALISE_COMERCIAL/grupos.csv",row.names = FALSE)




