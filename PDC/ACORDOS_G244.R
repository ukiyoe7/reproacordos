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
  left_join(.,acordos_relrepro_G244,by=c("PROCODIGOA"="PROCODIGO")) %>% 
  
  # verifica trat bonif
  left_join(.,trat_bonif_g244 %>% mutate(TRAT=1),by=c("PROCODIGOA","PROCODIGOB")) %>% 
  
   # calc acordo comb com a condicional de trat
   mutate(VALOR_ACORDOA=CCPCOVENDAPROA2*(1-(DESCONTO/100))*2) %>% 
    mutate(VALOR_ACORDOB= if_else(!is.na(TRAT), 1, CCPCOVENDAPROB2*(1-(DESCONTO/100))) ) %>% 
     mutate(VALOR_ACORDO=VALOR_ACORDOA+VALOR_ACORDOB)  %>%  
  
  #exclui colunas de calculo
  select(-TRAT,-VALOR_ACORDOA,-VALOR_ACORDOB) %>% 

  
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

# RELREPRO ==================================

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
  acordos_relrepro_G244 %>% filter(TBPCODIGO==103) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% 

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
acordos_relrepro_G244 %>% filter(TBPCODIGO==104) %>% filter(substr(PROCODIGO, 1, 1) != "T") %>% select(PROCODIGO,DESCONTO) %>% left_join(.,premp %>% mutate(PROCODIGO=trimws(PROCODIGO)),by="PROCODIGO") %>% filter(!is.na(PRECO)) %>% 
  
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

  
## LENTES MULTIFOCAIS SEM TRATAMENTO ============================================
  

# VARILUX  TRAD ======================
  
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

  
  # VARILUX  DIGI =============================
  
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
  
  
  # KODAK TRAD ========================================
  
  
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


  # KODAK  DIGI =============================
  
  relrepro_102_ST_G244 <- 
    
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
  
  
  View(relrepro_102_ST_G244)


