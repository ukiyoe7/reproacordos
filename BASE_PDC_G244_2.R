# DATA MANIPULATION 


library(DBI)
library(tidyverse)
library(readr)


con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

## PVO ===================================================

acordos_tab_G244 <-
  dbGetQuery(con2,"
              WITH TB AS (SELECT TBPCODIGO,TBPDESCRICAO,TBPDTVALIDADE FROM TABPRECO WHERE 
              (TBPDTVALIDADE >='TODAY' OR TBPDTVALIDADE IS NULL) AND TBPCODIGO NOT IN (6,125,126,2028)),
              
               TPRODU AS (SELECT TP.TBPCODIGO,
                                     PROCODIGO,
                                      TBPDESCRICAO,
                                       TBPDTVALIDADE,
                                         TBPPCOVENDA,
                                          TBPPCDESCTO2,
                                           TBPPCOVENDA2,
                                            TBPPCDESCTO 
                                             FROM TBPPRODU TP
                 INNER JOIN TB T ON T.TBPCODIGO=TP.TBPCODIGO)
              
                  SELECT C.TBPCODIGO,
                            TBPDESCRICAO,
                             PROCODIGO,
                              TP.TBPDTVALIDADE,
                               TBPPCOVENDA,
                                TBPPCDESCTO2,
                                 TBPPCOVENDA2,
                                  TBPPCDESCTO
                                   FROM CLITBP C 
                    INNER JOIN TPRODU TP ON C.TBPCODIGO=TP.TBPCODIGO WHERE CLICODIGO=241") %>% 
                      mutate(PROCODIGO=trimws(PROCODIGO))


View(acordos_tab_G244)

premp <-
  dbGetQuery(con2,"
  WITH PROD AS (SELECT PROCODIGO,PROUN FROM PRODU WHERE PROSITUACAO='A')
  SELECT PR.PROCODIGO,IIF(PROUN='PC',PREPCOVENDA2*2,PREPCOVENDA) PRECO FROM PREMP PR
  INNER JOIN PROD P ON P.PROCODIGO=PR.PROCODIGO
  WHERE EMPCODIGO=1") %>% mutate(PROCODIGO=trimws(PROCODIGO))


View(premp)

## ACORDOS ===============================================

acordos_relrepro_G244 <-
  dbGetQuery(con2,"
              WITH TB AS (SELECT TBPCODIGO FROM TABPRECO WHERE TBPCODIGO IN ('100','201','202','101','102','103','104','304','305','309','308','400')),
              
               TPRODU AS (SELECT TP.TBPCODIGO,PROCODIGO FROM TBPPRODU TP
                 INNER JOIN TB T ON T.TBPCODIGO=TP.TBPCODIGO)
              
                  SELECT PROCODIGO,TBPDESC2 DESCONTO FROM CLITBP C 
                    INNER JOIN TPRODU TP ON C.TBPCODIGO=TP.TBPCODIGO WHERE CLICODIGO=241") %>% 
  mutate(DATA_VALIDADE='05.04.2024') %>% mutate(PROCODIGO=trimws(PROCODIGO))


acordos_relrepro_G244 <-
  dbGetQuery(con2,"
              WITH TB AS (SELECT TBPCODIGO FROM TABPRECO WHERE TBPCODIGO IN ('100','201','202','101','102','103','104','304','305','309','308','400')),
              
               TPRODU AS (SELECT TP.TBPCODIGO,PROCODIGO FROM TBPPRODU TP
                 INNER JOIN TB T ON T.TBPCODIGO=TP.TBPCODIGO)
              
                  SELECT PROCODIGO,TBPDESC2 DESCONTO FROM CLITBP C 
                    INNER JOIN TPRODU TP ON C.TBPCODIGO=TP.TBPCODIGO WHERE CLICODIGO=241") %>% 
  mutate(DATA_VALIDADE='05.04.2024') %>% mutate(PROCODIGO=trimws(PROCODIGO))



View(acordos_relrepro_G244)


acordos_comb_g244 <- 
 left_join(comb_tabpreco,acordos_relrepro_G244,by=c("PROCODIGOA"="PROCODIGO")) %>%
  mutate(VALOR_ACORDO=PRECOA*(1-(DESCONTO/100))*2+PRECOB*(1-(DESCONTO/100))) %>% 
   mutate(MONTAGEM='MOMF') %>% left_join(.,premp,by=c("MONTAGEM"="PROCODIGO")) %>% 
    rename(PRECO_MONTAGEM=PRECO) %>% 
     left_join(.,acordos_tab_G244 %>% 
      select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
       mutate(VALOR_ACORDO_MONTAGEM=PRECO_MONTAGEM*(1-(TBPPCDESCTO2/100))) %>% 
        mutate(VALOR_FINAL=VALOR_ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
         mutate(CHAVE=paste0(PROCODIGOA,PROCODIGOB,MONTAGEM)) %>% 
          mutate(CHAVE=str_trim(CHAVE))
       

View(acordos_comb_g244)

#PDC ==============================


composicao1 <- dbGetQuery(con2,"SELECT COMPOPROCODIGO PROCODIGO,COMPOPRODUCODIGO,COMPODESCRICAO ID FROM COMPOSICAO") %>% 
  mutate(PROCODIGO=str_trim(PROCODIGO)) 


View(composicao1)


composicao2 <- dbGetQuery(con2,"SELECT COMPOPROCODIGO PROCODIGO,COMPOPRODUCODIGO,COMPODESCRICAO ID FROM COMPOSICAO") %>% 
  mutate(PROCODIGO=str_trim(PROCODIGO)) 


View(composicao2)


composicaoitem <- dbGetQuery(con2,"SELECT COMPPRODUCOD ID,CPIPROCODIGO PROCODIGO2 FROM COMPOPRODUITEM") %>% 
  mutate(PROCODIGO2=str_trim(PROCODIGO2))

View(composicaoitem)

## GET WIDE FORMAT

df <-
composicaoitem %>% 
 group_by(ID) %>% 
  mutate(ITEM_NUM = row_number())


df_wide <- df %>%
  pivot_wider(names_from = "ITEM_NUM", values_from = "PROCODIGO2", names_prefix = "ITEM_") %>% 
  rowwise() %>%
  mutate(PDC = paste(coalesce(c_across(starts_with("ITEM_")), ""), collapse = ""))%>%
  ungroup()

View(df_wide)

inner_join(df_wide,df_wide,by=c("ID"="COMPOPRODUCODIGO")) %>% View()



## COMPILE ALL

left_join(acordos_comb_g244,composicao1,by=c("CHAVE"="ID")) %>% View()



