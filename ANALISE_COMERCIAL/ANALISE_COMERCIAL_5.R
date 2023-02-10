# CONSTRUCT


## VARILUX

#LOJAS

varilux_lojas <- 
left_join(lojas,clitbp_vlx_3,by="CLICODIGO") %>% .[,c(-1,-4)] 
  
View(varilux_lojas)

# GRUPOS
varilux_grupos <- 
  left_join(grupos,g_clitbp_vlx_2,by="GCLCODIGO")  %>% .[,c(-1,-2)]  %>% 
   mutate(GCLCODIGO='') %>% rename(CNPJ=2,CLIRAZSOCIAL=1)


View(varilux_grupos)

rbind(varilux_lojas,varilux_grupos) %>% View()






