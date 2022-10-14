

library(DBI)
library(tidyverse)

con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)

## XCLUSIVE ==================================================


left_join(PVC_G88_VLXXCLUSIVE %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),

PVC_G88_VLXXCLUSIVE %>% 
   filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
   distinct(TRATAMENTO) %>% 
    data.frame(PROCODIGO2=c("TRCS102","TRPV102","TROP102","TRHC")) ,

by="TRATAMENTO") %>% 
      .[,c(1,2,12,13,17)] %>% View()


## X4D ==================================================


left_join(PVC_G88_X4D %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_X4D  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRCS102","TRPV102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
           .[,c(1,2,12,13,17)] %>% View()

## XTRACK ==================================================

left_join(PVC_G88_XTRACK %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XTRACK  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRCS102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## XTRACK FIT ==================================================

left_join(PVC_G88_XTRACKFIT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XTRACKFIT  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRCS102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## XDESIGN  ==================================================

left_join(PVC_G88_XDESIGN %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XDESIGN  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRCS102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## XDESIGNFIT  ==================================================

left_join(PVC_G88_XDESIGNFIT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XDESIGNFIT  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRCS102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()

## XDESIGSHORT  ==================================================

left_join(PVC_G88_XDESIGNSHORT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XDESIGNSHORT  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRCS102","TRPV102","TRCR101","TRCE102","TROP102")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## E DESIGN ==================================================

left_join(PVC_G88_EDESIGN %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_EDESIGN  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRCS102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## E DESIGN ==================================================

left_join(PVC_G88_EDESIGN %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_EDESIGN  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRCS102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## PHYSIO ==================================================

left_join(PVC_G88_PHYSIO%>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_PHYSIO  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRCS102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## COMFORTMAX ==================================================

left_join(PVC_G88_COMFORTMAX %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_COMFORTMAX  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRCS102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## LIBERTY ==================================================

left_join(PVC_G88_LIBERTY %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_LIBERTY  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRCS102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## LIBERTY SHORT ==================================================

left_join(PVC_G88_LIBERTY_SHORT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_LIBERTY_SHORT  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRCS102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()

## DIGITIME ==================================================

left_join(PVC_G88_DIGITIME %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_DIGITIME  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRCS102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## SPORT ==================================================

left_join(PVC_G88_SPORT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_SPORT %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRCS102","TRCR102","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## COMFORT TRAD ==================================================


left_join(PVC_G88_COMFORTTRAD %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_COMFORTTRAD  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRCS102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## LIBERTY TRAD ==================================================


left_join(PVC_G88_LIBERTYTRAD %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_LIBERTYTRAD  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRCS102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## UNIQUE ==================================================

left_join(PVC_G88_KDKUNIQUE,
          
          PVC_G88_KDKUNIQUE  %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV202","TRCS202","TRCR202","TRCE202","TROP202","TRNR202","TRTEC202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()



## NETWORK ==================================================

left_join(PVC_G88_NETWORK,
          
          PVC_G88_NETWORK %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV202","TRCS202","TRCR202","TRCE202","TROP202","TRNR202","TRTEC202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## PRECISE ==================================================

left_join(PVC_G88_KDK_PRECISE,
          
          PVC_G88_KDK_PRECISE %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV202","TRCS202","TRCR202","TRCE202","TROP202","TRNR202","TRTEC202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()




