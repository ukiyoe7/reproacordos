## ACORDOS


# LIBS ==================================================


library(DBI)
library(tidyverse)
library(readr)


con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

## ACORDO GERAL =====================================================

descto_geral <- 
dbGetQuery(con2,"
SELECT CLIPCDESCPRODU DESCTO_GERAL FROM CLIEN WHERE CLICODIGO=241")




## ACORDOS RELREPRO =====================================================

acordos_relrepro_G244 <-
  dbGetQuery(con2,"
              WITH TB AS (SELECT TBPCODIGO FROM TABPRECO 
               WHERE TBPCODIGO IN ('100','101','102','103','104','201','202','304','305','308','309','313','400','3660','3661')),
              
               TPRODU AS (SELECT TP.TBPCODIGO,PROCODIGO FROM TBPPRODU TP
                 INNER JOIN TB T ON T.TBPCODIGO=TP.TBPCODIGO)
              
                  SELECT PROCODIGO,TBPDESC2 DESCONTO FROM CLITBP C 
                    INNER JOIN TPRODU TP ON C.TBPCODIGO=TP.TBPCODIGO WHERE CLICODIGO=241") %>% 
  mutate(PROCODIGO=trimws(PROCODIGO)) %>% 
  # FILTRA LENTES
  inner_join(.,dbGetQuery(con2,"SELECT PROCODIGO,
                           PRODESCRICAO, 
                            GR2DESCRICAO
                             FROM PRODU P
                              LEFT JOIN (SELECT GR2CODIGO,GR2DESCRICAO FROM GRUPO2)A ON P.GR2CODIGO=A.GR2CODIGO
                              WHERE PROSITUACAO='A' AND PROTIPO IN ('F','P') 
                               AND GR1CODIGO<>17 AND PROCODIGO2 IS NULL") %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO")



View(acordos_relrepro_G244) 


## COMBINADOS ========================================================

combinados_g244 <- get(load(file="C:\\Users\\REPRO SANDRO\\Documents\\R\\ACORDOS\\PDC\\combinados_g244.RData"))

combinados_g244 <- 
dbGetQuery(con2,"
           WITH CLI AS (SELECT TBPCODIGO FROM CLITBPCOMB WHERE CLICODIGO=241),
           
           TB AS (SELECT T.TBPCODIGO FROM TABPRECO T
           INNER JOIN CLI C ON C.TBPCODIGO=T.TBPCODIGO
           WHERE TBPSITUACAO='A' AND (TBPDTVALIDADE IS NULL OR TBPDTVALIDADE >='TODAY') )
           
           SELECT TC.TBPCODIGO,PROCODIGOA,CCINDICEPROA,CCINDICEPROA2,CCPCOVENDAPROA,
            CCPCOVENDAPROA2,PROCODIGOB,CCINDICEPROB,CCINDICEPROB2,CCPCOVENDAPROB,
             CCPCOVENDAPROB2 FROM TBPCOMBPROPRO TC
            INNER JOIN TB T ON TC.TBPCODIGO=T.TBPCODIGO") %>% 
             mutate(PROCODIGOA=trimws(PROCODIGOA)) %>% 
              mutate(PROCODIGOB=trimws(PROCODIGOB)) %>% .[,-12]


View(combinados_g244)


## TRAT BONIFICADOS ========================================================

trat_bonif_g244 <- 
combinados_g244 %>% filter(TBPCODIGO==29) %>% mutate(PROCODIGOA=str_trim(PROCODIGOA)) %>% mutate(PROCODIGOB=str_trim(PROCODIGOB)) %>% select(PROCODIGOA,PROCODIGOB)

View(trat_bonif_g244)

#ajuste trat bonif
left_join(trat_bonif_g244 ,combinados_g244 %>% filter(TBPCODIGO==1),by=c("PROCODIGOA","PROCODIGOB")) %>% filter(!is.na(.[,16]))%>% 
  write.csv2(.,file="C:\\Users\\REPRO SANDRO\\Documents\\R\\ACORDOS\\PDC\\trat_bonif_g244.csv",row.names = FALSE,na="")


## TABELA PVO ==================================================================


combinados_g244_pvo <- 
  
   # obtem combinados e filtra a tab pvo
   combinados_g244 %>% filter(TBPCODIGO==1) %>% 
  
  # join acordos relrepro 
  left_join(.,acordos_relrepro_G244,by=c("PROCODIGOA"="PROCODIGO")) %>% 
  
  # verifica trat bonif
  left_join(.,trat_bonif_g244 %>% mutate(TRAT=1),by=c("PROCODIGOA","PROCODIGOB")) %>% 
  
   # calc acordo comb
   mutate(VALOR_ACORDO=CCPCOVENDAPROA2*(1-(DESCONTO/100))*2+ mutate(CCPCOVENDAPROB2 = if_else(TRAT == 1, 1, CCPCOVENDAPROB2*(1-(DESCONTO/100))) )) %>% View()
  
   # adiciona montagem
    mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
   
    # acordos montagens
      left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
        mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
       # calculo total
         mutate(VALOR_FINAL=VALOR_ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
  
         # chave pdc
          mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
          # get pdc
           left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") %>% 

            #exclui colunas vazias
 
              .[,c(-3,-4,-8,-9)]

View(combinados_g244_pvo)



combinados_g244_pvo %>% filter(!is.na(PDC))%>% View()


## LENTES MULTIFOCAIS SEM TRATAMENTO ===========================================


lentes_mf_sem_ar <-

 # extrai todas multifocais    
dbGetQuery(con2,"SELECT PROCODIGO,PRODESCRICAO FROM PRODU 
                   WHERE GR1CODIGO<>17 AND PROSITUACAO='A'
                    AND PROCODIGO2 IS NULL AND GR2CODIGO=1 AND PROCODIGO
                     NOT IN (SELECT PROCODIGO FROM NGRUPOS WHERE GRCODIGO=162) ") %>% 
                      mutate(PROCODIGO=trimws(PROCODIGO)) %>% 
  
  # acordo relrepro
  left_join(.,acordos_relrepro_G244 ,by=c("PROCODIGO"="PROCODIGO")) %>% 
  
  
  
  
  # calc acordo comb
  mutate(VALOR_ACORDO=CCPCOVENDAPROA2*(1-(DESCONTO/100))*2+CCPCOVENDAPROB2*(1-(DESCONTO/100))) %>% 


View(lentes_mf_sem_ar)







