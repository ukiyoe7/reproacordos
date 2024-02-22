## BASE PDC

# LIBS ==================================================


library(DBI)
library(tidyverse)
library(readr)


con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")


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

df <-
  composicaoitem %>% 
  group_by(ID) %>% 
  mutate(ITEM_NUM = row_number())


df_wide <- df %>%
  pivot_wider(names_from = "ITEM_NUM", values_from = "PROCODIGO2", names_prefix = "ITEM_") %>% 
  rowwise() %>%
  mutate(CHAVE = paste(coalesce(c_across(starts_with("ITEM_")), ""), collapse = ""))%>%
  ungroup()

View(df_wide)

pdc_df <- 
  inner_join(df_wide,composicao1,by=c("ID"="COMPOPRODUCODIGO")) %>% 
  inner_join(.,dbGetQuery(con2,"SELECT PROCODIGO FROM PRODU WHERE PROSITUACAO='A' AND PROTIPO IN ('P','F')") %>% 
               mutate(PROCODIGO=str_trim(PROCODIGO)),by=c("ITEM_1"="PROCODIGO")) %>% 
                rename(PDC=PROCODIGO)


View(pdc_df)

pdc_df2 <-
pdc_df %>% .[,c(-1,-11)]


View(pdc_df2)



