## BASE DE ACORDOS PDC


# LIBS ==================================================


library(DBI)
library(tidyverse)
library(readr)


con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

## PRODUTOS =======================================================

produ <- 
  dbGetQuery(con2," WITH PRECO AS(SELECT PROCODIGO,PREPCOVENDA,PREPCOVENDA2 FROM PREMP WHERE EMPCODIGO=1)
               
               SELECT PD.PROCODIGO,
                           PRODESCRICAO,
                             GR1CODIGO,
                              GR2CODIGO,
                              TPLCODIGO,
                               PD.MARCODIGO,
                                MARNOME MARCA,
                                 PROTIPO,
                                  PROUN,
                                  PREPCOVENDA,
                                   PREPCOVENDA2,
                                    PREPCOVENDA*2 PAR_ATC,
                                     PREPCOVENDA2*2 PAR_LAB
               FROM PRODU PD
               
               INNER JOIN PRECO P ON PD.PROCODIGO=P.PROCODIGO
               LEFT JOIN MARCA M ON PD.MARCODIGO=M.MARCODIGO 
               WHERE GR1CODIGO<>17 AND PROSITUACAO='A'
                 AND PROCODIGO2 IS NULL") %>% mutate(PROCODIGO=trimws(PROCODIGO))

View(produ)  


## ACORDO GERAL =====================================================

descto_geral <- 
dbGetQuery(con2,"
SELECT CLIPCDESCPRODU DESCTO_GERAL FROM CLIEN WHERE CLICODIGO=241")


## ACORDOS RELREPRO =====================================================

acordos_relrepro_G244 <- get(load(file="C:\\Users\\REPRO SANDRO\\Documents\\R\\ACORDOS\\PDC\\acordos_relrepro_G244.RData"))


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


combinados_g244 %>% filter(TBPCODIGO==1) %>% left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO")) %>% View()

## ACORDOS TAB =================================================

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



## TRAT BONIFICADOS ========================================================

trat_bonif_g244 <- 
combinados_g244 %>% filter(TBPCODIGO==29) %>% mutate(PROCODIGOA=str_trim(PROCODIGOA)) %>% mutate(PROCODIGOB=str_trim(PROCODIGOB)) %>% select(PROCODIGOA,PROCODIGOB)

View(trat_bonif_g244)

#ajuste trat bonif
left_join(trat_bonif_g244 ,combinados_g244 %>% filter(TBPCODIGO==1),by=c("PROCODIGOA","PROCODIGOB")) %>% filter(!is.na(.[,16]))%>% 
  write.csv2(.,file="C:\\Users\\REPRO SANDRO\\Documents\\R\\ACORDOS\\PDC\\trat_bonif_g244.csv",row.names = FALSE,na="")


## TABELA PVO COMBINADO ==================================================================


combinados_g244_pvo <- 
  
   # obtem combinados e filtra a tab pvo
   combinados_g244 %>% filter(TBPCODIGO==1) %>% 
  
  # join acordos relrepro 
  left_join(.,acordos_relrepro_G244,by=c("PROCODIGOA"="PROCODIGO")) %>% mutate(DESCONTO2=DESCONTO) %>% left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO")) %>% 
  
  # verifica trat bonif
  left_join(.,trat_bonif_g244 %>% mutate(TRAT=1),by=c("PROCODIGOA","PROCODIGOB")) %>% 
  
   # calc acordo comb com a condicional de trat
   mutate(VALOR_ACORDOA=CCPCOVENDAPROA2*(1-(DESCONTO/100))*2) %>% 
    mutate(VALOR_ACORDOB= if_else(!is.na(TRAT), 1, CCPCOVENDAPROB2*(1-(DESCONTO/100))) ) %>% 
     mutate(VALOR_ACORDO=VALOR_ACORDOA+VALOR_ACORDOB)  %>% 

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
  
           select(PRODESCRICAO,PROCODIGOA,CCPCOVENDAPROA2,DESCONTO,VALOR_ACORDOA,PROCODIGOB,CCPCOVENDAPROB2,DESCONTO2,VALOR_ACORDOB,MONTAGEM,PRECO,TBPPCDESCTO2,VALOR_ACORDO_MONTAGEM,VALOR_FINAL,CHAVE,PDC) 
 
new_column_names <- c("LENTE","COD_LEN","PRECO_LEN","DESCONTO_LEN","ACORDO_LEN","COD_TRAT","PRECO_TRAT","DESCONTO_TRAT","ACORDO_TRAT","COD_MONT","PRECO_MONT","DESCONTO_MONT","ACORDO_MONT","ACORDO_TOTAL","CHAVE","PDC")           

combinados_g244_pvo <-
combinados_g244_pvo %>% rename_with(~ new_column_names, .cols = everything())


View(combinados_g244_pvo)

combinados_g244_pvo %>% filter(!is.na(PDC))%>% View()

# RELREPRO ==================================

## LENTES COM TRATAMENTO ====================================================

## LENTES 100 VLX XR =====================================================================

relrepro_100_G244 <- 

# lentes
cross_join(
  acordos_relrepro_G244 %>% filter(TBPCODIGO==100) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>%  left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO"))
  ,
  
  #trat
  acordos_relrepro_G244 %>% filter(TBPCODIGO==100) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)
  
)%>% 
  
  #calc
  
  mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_100_G244)

## LENTES 101 VLX TRAD =====================================================================

relrepro_101_G244 <-
# lentes
cross_join(
  acordos_relrepro_G244 %>% filter(TBPCODIGO==101) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>% left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO"))
  ,
  
  #trat
  acordos_relrepro_G244 %>% filter(TBPCODIGO==101) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)
  
)%>% 
  
  #calc
  
  mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  
  View(relrepro_101_G244)

## LENTES 102 VLX DIGI =====================================================================

relrepro_102_G244 <-

# lentes
cross_join(
  acordos_relrepro_G244 %>% filter(TBPCODIGO==102) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>% left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO"))
  ,
  
  #trat
  acordos_relrepro_G244 %>% filter(TBPCODIGO==102) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)
  
)%>% 
  
  #calc
  
  mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_102_G244)


## LENTES 103 LA CRIZAL =====================================================================

relrepro_103_G244 <- 

# lentes
  acordos_relrepro_G244 %>% filter(TBPCODIGO==103) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% left_join(.,prod,by="PROCODIGO") %>% 

  #calc
  
  mutate(ACORDO=PRECO*(1-DESCONTO/100)) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOVS') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO.y*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGO,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_103_G244)


## LENTES 104 LA KODAK =====================================================================



relrepro_104_G244 <-

# lentes
acordos_relrepro_G244 %>% filter(TBPCODIGO==104) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% left_join(.,prod,by="PROCODIGO") %>% 
  
  #calc
  
  mutate(ACORDO=PRECO*(1-DESCONTO/100)) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOVS') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO.y*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGO,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE")
  
  View(relrepro_104_G244)


## LENTES 201 KODAK TRAD  =====================================================================


relrepro_201_G244 <- 

# lentes
cross_join(
acordos_relrepro_G244 %>% filter(TBPCODIGO==201) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>%  left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO"))
  ,

#trat
acordos_relrepro_G244 %>% filter(TBPCODIGO==201) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)

)%>% 
  
#calc
  
mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_201_G244)


## LENTES 202 KODAK DIGI =====================================================================

relrepro_202_G244 <- 

# lentes
cross_join(
  acordos_relrepro_G244 %>% filter(TBPCODIGO==202) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>%  left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO"))
  ,
  
  #trat
  acordos_relrepro_G244 %>% filter(TBPCODIGO==202) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)
  
)%>% 
  
  #calc
  
  mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE")
  
  View(relrepro_202_G244)


## LENTES 304 ACTUALITE =====================================================================

relrepro_304_G244 <-

# lentes
cross_join(
  acordos_relrepro_G244 %>% filter(TBPCODIGO==304) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>% left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO"))
  ,
  
  #trat
  acordos_relrepro_G244 %>% filter(TBPCODIGO==304) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)
  
)%>% 
  
  #calc
  
  mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_304_G244)

## LENTES 305 AVANCE =====================================================================

relrepro_305_G244 <- 

# lentes
cross_join(
  acordos_relrepro_G244 %>% filter(TBPCODIGO==305) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>% left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO"))
  ,
  
  #trat
  acordos_relrepro_G244 %>% filter(TBPCODIGO==305) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)
  
)%>% 
  
  #calc
  
  mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_305_G244)


## LENTES 308 INSIGNE =====================================================================

relrepro_308_G244 <-

# lentes
cross_join(
  acordos_relrepro_G244 %>% filter(TBPCODIGO==308) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>% left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO"))
  ,
  
  #trat
  acordos_relrepro_G244 %>% filter(TBPCODIGO==308) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)
  
)%>% 
  
  #calc
  
  mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE")
  
  View(relrepro_308_G244)

## LENTES 309 EYEZEN =====================================================================


relrepro_309_G244 <-
# lentes
cross_join(
  acordos_relrepro_G244 %>% filter(TBPCODIGO==309) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>%  left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO"))
  ,
  
  #trat
  acordos_relrepro_G244 %>% filter(TBPCODIGO==309) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)
  
)%>% 
  
  #calc
  
  mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_309_G244)

## LENTES VLX 313 PRIMO =====================================================================


relrepro_313_G244 <- 
# lentes
cross_join(
  acordos_relrepro_G244 %>% filter(TBPCODIGO==313) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGOA=PROCODIGO) %>% left_join(.,prod,by=c("PROCODIGOA"="PROCODIGO")) 
  ,
  
  #trat
  acordos_relrepro_G244 %>% filter(TBPCODIGO==313) %>% filter(substr(PROCODIGO, 1, 1) == "T") %>% select(PROCODIGO,DESCONTO) %>%  left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>%  filter(!is.na(PRECO)) %>% rename(PROCODIGOB=PROCODIGO)
  
)%>% 
  
  #calc
  
  mutate(ACORDOA=PRECO.x*(1-DESCONTO.x/100)) %>% mutate(ACORDOB=PRECO.y*(1-DESCONTO.y/100)) %>% 
  mutate(ACORDOAB=(ACORDOA)+ACORDOB) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDOAB+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGOA,PROCODIGOB,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_313_G244)
  
  

## LENTES 3660 PROSING =====================================================================

relrepro_3660_G244 <- 

# lentes
acordos_relrepro_G244 %>% filter(TBPCODIGO==3660) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% left_join(.,prod,by="PROCODIGO") %>% 

#calc

mutate(ACORDO=PRECO*(1-DESCONTO/100)) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOVS') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO.y*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGO,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_3660_G244)

## LENTES 3661 PROSING 2 =====================================================================

relrepro_3661_G244 <- 

# lentes
acordos_relrepro_G244 %>% filter(TBPCODIGO==3661) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% left_join(.,prod,by="PROCODIGO") %>% 

#calc

mutate(ACORDO=PRECO*(1-DESCONTO/100)) %>% 
  
  # adiciona montagem
  mutate(MONTAGEM='MOVS') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
  
  # acordos montagens
  left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
  mutate(VALOR_ACORDO_MONTAGEM=PRECO.y*(1-(TBPPCDESCTO2/100))) %>% 
  
  
  # calculo total
  mutate(VALOR_FINAL=ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
  
  # chave pdc
  mutate(CHAVE=str_trim(paste0(PROCODIGO,MONTAGEM))) %>% 
  
  # get pdc
  left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  View(relrepro_3661_G244)
  
  

## LENTES ESPACE =====================================================================
 
lentes_espace <-   
produ %>% filter(MARCA=='ESPACE') %>% select(PROCODIGO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% left_join(.,acordos_tab_G244 %>% select(PROCODIGO,TBPPCDESCTO2),by="PROCODIGO") %>%  cross_join(.,descto_geral) %>% mutate(TBPPCDESCTO2=if_else(is.na(TBPPCDESCTO2),DESCTO_GERAL,TBPPCDESCTO2)) %>% select(-DESCTO_GERAL)
  

trat_espace_310 <- 
produ %>% filter(PROTIPO=='T') %>% filter(str_detect(PROCODIGO,'310')) %>% select(PROCODIGO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% cross_join(.,descto_geral)


espace_3687_g244 %>% left_join(.,premp,by="PROCODIGO") %>% View()

cross_join(espace_3687_g244,trat_espace_310) %>% cross_join(.,descto_geral) %>% View() 
  
  


## LENTES MULTIFOCAIS SEM TRATAMENTO ============================================
  

# VARILUX  TRAD 101 ST ======================
  
  relrepro_101_ST_G244 <- 
  
    # lentes
      acordos_relrepro_G244 %>% filter(TBPCODIGO==101) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGO=PROCODIGO) %>% left_join(.,prod,by="PROCODIGO") %>% 
  
    #calc
    
    mutate(ACORDO=PRECO*(1-DESCONTO/100)) %>%    
    
    # adiciona montagem
    mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
    
    # acordos montagens
    left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
    mutate(VALOR_ACORDO_MONTAGEM=PRECO.y*(1-(TBPPCDESCTO2/100))) %>% 
    
    
    # calculo total
    mutate(VALOR_FINAL=ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
    
    # chave pdc
    mutate(CHAVE=str_trim(paste0(PROCODIGO,MONTAGEM))) %>% 
    
    # get pdc
    left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  
  View(relrepro_101_ST_G244)

  
  # VARILUX  DIGI 102 ST  =============================
  
  relrepro_102_ST_G244 <- 
    
    # lentes
    acordos_relrepro_G244 %>% filter(TBPCODIGO==102) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGO=PROCODIGO) %>% left_join(.,prod,by="PROCODIGO") %>% 
    
     filter(str_detect(PRODESCRICAO,"COMFORT | LIBERTY" )) %>% 
    
    #calc
    
    mutate(ACORDO=PRECO*(1-DESCONTO/100)) %>%    
    
    # adiciona montagem
    mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
    
    # acordos montagens
    left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
    mutate(VALOR_ACORDO_MONTAGEM=PRECO.y*(1-(TBPPCDESCTO2/100))) %>% 
    
    
    # calculo total
    mutate(VALOR_FINAL=ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
    
    # chave pdc
    mutate(CHAVE=str_trim(paste0(PROCODIGO,MONTAGEM))) %>% 
    
    # get pdc
    left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  
  View(relrepro_102_ST_G244)
  
  
  # KODAK TRAD 201 ST ========================================
  
  
  relrepro_201_ST_G244  <- 
  
  # lentes
  acordos_relrepro_G244 %>% filter(TBPCODIGO==201) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGO=PROCODIGO) %>% left_join(.,prod,by="PROCODIGO") %>% 
    
    #calc
    
    mutate(ACORDO=PRECO*(1-DESCONTO/100)) %>%    
    
    # adiciona montagem
    mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
    
    # acordos montagens
    left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
    mutate(VALOR_ACORDO_MONTAGEM=PRECO.y*(1-(TBPPCDESCTO2/100))) %>% 
    
    
    # calculo total
    mutate(VALOR_FINAL=ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
    
    # chave pdc
    mutate(CHAVE=str_trim(paste0(PROCODIGO,MONTAGEM))) %>% 
    
    # get pdc
    left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  
  View(relrepro_201_ST_G244)


  # KODAK  DIGI 202 ST =============================
  
  relrepro_202_ST_G244 <- 
    
    # lentes
    acordos_relrepro_G244 %>% filter(TBPCODIGO==202) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% rename(PROCODIGO=PROCODIGO) %>% left_join(.,prod,by="PROCODIGO") %>% 
    
    #calc
    
    mutate(ACORDO=PRECO*(1-DESCONTO/100)) %>%    
    
    # adiciona montagem
    mutate(MONTAGEM='MOMF') %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by=c("MONTAGEM"="PROCODIGO")) %>% 
    
    # acordos montagens
    left_join(.,acordos_tab_G244 %>% mutate(PROCODIGO=trimws(PROCODIGO)) %>% select(PROCODIGO,TBPPCDESCTO2),by=c("MONTAGEM"="PROCODIGO")) %>% 
    mutate(VALOR_ACORDO_MONTAGEM=PRECO.y*(1-(TBPPCDESCTO2/100))) %>% 
    
    
    # calculo total
    mutate(VALOR_FINAL=ACORDO+VALOR_ACORDO_MONTAGEM) %>% 
    
    # chave pdc
    mutate(CHAVE=str_trim(paste0(PROCODIGO,MONTAGEM))) %>% 
    
    # get pdc
    left_join(.,pdc_df2 %>% select(CHAVE,PDC),by="CHAVE") 
  
  
  View(relrepro_202_ST_G244)
  
## MERGE BASES ====================================================

  
# RELREPRO TRAT ================   
relrepro_trat_G244 <-   
    
 union_all(relrepro_100_G244,relrepro_101_G244) %>% 
  union_all(.,relrepro_102_G244) %>%     
   union_all(.,relrepro_201_G244) %>%   
    union_all(.,relrepro_202_G244) %>%
     union_all(.,relrepro_304_G244) %>% 
      union_all(.,relrepro_305_G244) %>%
       union_all(.,relrepro_308_G244) %>%
        union_all(.,relrepro_309_G244) %>% 
         select(PRODESCRICAO,PROCODIGOA,PRECO.x,DESCONTO.x,ACORDOA,PROCODIGOB,PRECO.y,DESCONTO.y,ACORDOB,ACORDOB,MONTAGEM,PRECO,TBPPCDESCTO2,VALOR_ACORDO_MONTAGEM,VALOR_FINAL,CHAVE,PDC)     


  
  new_column_names <- c("LENTE","COD_LEN","PRECO_LEN","DESCONTO_LEN","ACORDO_LEN","COD_TRAT","PRECO_TRAT","DESCONTO_TRAT","ACORDO_TRAT","COD_MONT","PRECO_MONT","DESCONTO_MONT","ACORDO_MONT","ACORDO_TOTAL","CHAVE","PDC")           
  
relrepro_trat_G244 <-
  relrepro_trat_G244 %>% rename_with(~ new_column_names, .cols = everything())

View(relrepro_trat_G244)


# JOIN TAB COMB =====================

  
pvo_relrepro_trat_g244 <-   
union_all(relrepro_trat_G244,relrepro_trat_G244) %>% 
   anti_join(.,inner_join(combinados_g244_pvo,relrepro_trat_G244,by=c("COD_LEN","COD_TRAT")),by=c("COD_LEN","COD_TRAT"))  %>% 
    union_all(.,combinados_g244_pvo)  %>% distinct()

View(pvo_relrepro_trat_g244)
  

# RELREPRO LA  ==========================

relrepro_LA_G244 <-    

  union_all(relrepro_103_G244,relrepro_104_G244) %>% 
   union_all(.,relrepro_3660_G244) %>%     
    union_all(.,relrepro_3661_G244) %>% 
      select(PRODESCRICAO,PROCODIGO,PRECO.x,DESCONTO,ACORDO,MONTAGEM,MONTAGEM,PRECO.y,TBPPCDESCTO2,VALOR_ACORDO_MONTAGEM,VALOR_FINAL,CHAVE,PDC)     

new_column_names <- c("LENTE","COD_LEN","PRECO_LEN","DESCONTO_LEN","ACORDO_LEN","COD_MONT","PRECO_MONT","DESCONTO_MONT","ACORDO_MONT","ACORDO_TOTAL","CHAVE","PDC")           

relrepro_LA_G244 <-
  relrepro_LA_G244 %>% rename_with(~ new_column_names, .cols = everything())


View(relrepro_LA_G244)


# RELREPRO SEM TRAT  ===========================

relrepro_ST_G244 <- 
  
 union_all(relrepro_101_ST_G244,relrepro_102_ST_G244) %>% 
  union_all(.,relrepro_201_ST_G244) %>%     
   union_all(.,relrepro_202_ST_G244) %>% 
     select(PRODESCRICAO,PROCODIGO,PRECO.x,DESCONTO,ACORDO,MONTAGEM,MONTAGEM,PRECO.y,TBPPCDESCTO2,VALOR_ACORDO_MONTAGEM,VALOR_FINAL,CHAVE,PDC)     

new_column_names <- c("LENTE","COD_LEN","PRECO_LEN","DESCONTO_LEN","ACORDO_LEN","COD_MONT","PRECO_MONT","DESCONTO_MONT","ACORDO_MONT","ACORDO_TOTAL","CHAVE","PDC")           

relrepro_ST_G244 <-
  relrepro_ST_G244 %>% rename_with(~ new_column_names, .cols = everything())
  
  View(relrepro_ST_G244)
  
  
# UNION RELREPRO LA + SEM TRAT
  
union_relrepro_LA_G244_relrepro_ST_G244 <-  
union_all(relrepro_LA_G244,relrepro_ST_G244)  
  
  
## CSV  
  
  pvo_relrepro_trat_g244 %>% write.csv2(.,file="C:\\Users\\REPRO SANDRO\\Documents\\R\\ACORDOS\\PDC\\pvo_relrepro_trat_g244.csv",row.names = FALSE,na="") 
  
  union_relrepro_LA_G244_relrepro_ST_G244 %>% write.csv2(.,file="C:\\Users\\REPRO SANDRO\\Documents\\R\\ACORDOS\\PDC\\union_relrepro_LA_G244_relrepro_ST_G244.csv",row.names = FALSE,na="")
  
## AJUSTES ========================================
  
-- face interna
-- colorações
-- bifocais  
  
  

