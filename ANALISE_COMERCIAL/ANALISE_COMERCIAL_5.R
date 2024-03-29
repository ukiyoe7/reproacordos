library(scales)

# CONSTRUCT

## GERAL ======================================


cli_desctgeral_lojas <- dbGetQuery(con2,"SELECT CLICODIGO,CLIPCDESCPRODU DESCTGERAL FROM CLIEN")

View(cli_desctgeral_lojas)


cli_desctgeral_grupos <- dbGetQuery(con2,"SELECT CLICODIGO,GCLCODIGO,CLIPCDESCPRODU DESCTGERAL FROM CLIEN") %>% 
                          group_by(GCLCODIGO) %>% 
                           summarize(DESCTGERAL=round(mean(DESCTGERAL,na.rm = TRUE),2))

View(cli_desctgeral_grupos)


## VARILUX ========================================================

#LOJAS

varilux_lojas <- 
left_join(lojas,clitbp_vlx_3,by="CLICODIGO") %>% 
  ## ADD DESCTO GEAL TO NAs
  left_join(., cli_desctgeral_lojas, by = "CLICODIGO") %>%
  mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
  select(-DESCTGERAL) %>% .[,c(-1,-4)] 
  
View(varilux_lojas)

# GRUPOS
varilux_grupos <- 
  left_join(grupos,g_clitbp_vlx_2,by="GCLCODIGO")  %>% 
  ## ADD DESCTO GEAL TO NAs
  left_join(., cli_desctgeral_grupos, by = "GCLCODIGO") %>%
    mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
      select(-DESCTGERAL) %>% 
       .[,c(-1,-2,-3)]  %>% 
         mutate(CNPJ='') %>% .[,c(1,11,2:10)] %>% 
           rename(CLIRAZSOCIAL=1) 

View(varilux_grupos)
  

varilux <- rbind(varilux_lojas,varilux_grupos) %>% mutate_if(is.numeric,function(x) x/100)

write.csv2(varilux,file = "ANALISE_COMERCIAL/varilux.csv")


## KODAK MF =========================================================


#LOJAS

kodak_lojas <- 
  left_join(lojas,clitbp_kdk_3,by="CLICODIGO") %>% 
  ## ADD DESCTO GEAL TO NAs
  left_join(., cli_desctgeral_lojas, by = "CLICODIGO") %>%
  mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
  select(-DESCTGERAL) %>% 
  .[,c(-1,-4)] 

View(kodak_lojas)


# GRUPOS
kodak_grupos <- 
  left_join(grupos,g_clitbp_kdk,by="GCLCODIGO")  %>% 
  ## ADD DESCTO GEAL TO NAs
  left_join(., cli_desctgeral_grupos, by = "GCLCODIGO") %>%
  mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
  select(-DESCTGERAL) %>% 
  .[,c(-1,-2,-3)]  %>% 
  mutate(CNPJ='') %>% .[,c(1,10,2:9)] %>% rename(CLIRAZSOCIAL=1)


View(kodak_grupos)

kodak <- rbind(kodak_lojas,kodak_grupos) %>% mutate_if(is.numeric,function(x) x/100)


write.csv2(kodak,file = "ANALISE_COMERCIAL/kodak.csv")

## KODAK VS ====================================================


kodak_vs_lojas <- 
  left_join(lojas,clitbp_kdk_vs_3,by="CLICODIGO") %>% 
  ## ADD DESCTO GEAL TO NAs
  left_join(., cli_desctgeral_lojas, by = "CLICODIGO") %>%
  mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
  select(-DESCTGERAL) %>% 
  .[,c(-1,-4)] 

View(kodak_vs_lojas)


# GRUPOS
kodak_vs_grupos <- 
  left_join(grupos,clitbp_kdk_vs_4,by="GCLCODIGO")  %>% 
  ## ADD DESCTO GEAL TO NAs
  left_join(., cli_desctgeral_grupos, by = "GCLCODIGO") %>%
  mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
  select(-DESCTGERAL) %>% 
    .[,c(-1,-2,-3)]  %>% 
  mutate(CNPJ='') %>% 
.[,c(1,3,2)] %>% 
     rename(CLIRAZSOCIAL=1) 


View(kodak_vs_grupos)

kodak_vs <- rbind(kodak_vs_lojas,kodak_vs_grupos) %>% mutate_if(is.numeric,function(x) x/100)


write.csv2(kodak_vs,file = "ANALISE_COMERCIAL/kodak_vs.csv")


## EYEZEN ========================================================

#LOJAS

eyezen_lojas <- 
  left_join(lojas,clitbp_eyez_3,by="CLICODIGO") %>% 
  left_join(., cli_desctgeral_lojas, by = "CLICODIGO") %>%
  mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
  select(-DESCTGERAL) %>% 
  .[,c(-1,-4)] 

View(eyezen_lojas)


# GRUPOS
eyezen_grupos <- 
  left_join(grupos,g_clitbp_eyez,by="GCLCODIGO")  %>% 
  ## ADD DESCTO GEAL TO NAs
  left_join(., cli_desctgeral_grupos, by = "GCLCODIGO") %>%
  mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
  select(-DESCTGERAL) %>% 
  .[,c(-1,-2,-3)]  %>% 
  mutate(CNPJ='') %>%  rename(CLIRAZSOCIAL=1) %>% .[,c(1,3,2)]


View(eyezen_grupos)

eyezen <- rbind(eyezen_lojas,eyezen_grupos) %>% mutate_if(is.numeric,function(x) x/100)


write.csv2(eyezen,file = "ANALISE_COMERCIAL/eyezen.csv")


## CRIZAL VS =============================================

#LOJAS

crizalvs_loja <- 
  left_join(lojas,clitbp_crizal,by="CLICODIGO") %>% 
  ## ADD DESCTO GEAL TO NA
  left_join(., cli_desctgeral_lojas, by = "CLICODIGO") %>%
  mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
  select(-DESCTGERAL) %>% 
  .[,c(-1,-4)] 

View(crizalvs_loja)


# GRUPOS
crizalvs_grupos <- 
  left_join(grupos,clitbp_crizal_grupo,by="GCLCODIGO")  %>% 
  ## ADD DESCTO GEAL TO NA
  left_join(., cli_desctgeral_grupos, by = "GCLCODIGO") %>%
  mutate_all(~ifelse(is.na(.), DESCTGERAL, .)) %>%
  select(-DESCTGERAL) %>% 
  .[,c(-1,-2,-3)]  %>% 
  mutate(CNPJ='') %>%  rename(CLIRAZSOCIAL=1) %>% .[,c(1,3,2)]


View(crizalvs_grupos)

crizalvs <- rbind(crizalvs_lojas,crizalvs_grupos) %>% mutate_if(is.numeric,function(x) x/100)


write.csv2(crizalvs,file = "ANALISE_COMERCIAL/crizalvs.csv")











