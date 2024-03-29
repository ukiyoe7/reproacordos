

library(DBI)
library(tidyverse)

con2 <- dbConnect(odbc::odbc(), "reproreplica", timeout = 10)

## XCLUSIVE ==================================================


left_join(PVC_G88_VLXXCLUSIVE %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),

PVC_G88_VLXXCLUSIVE %>% 
   filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
   distinct(TRATAMENTO) %>% 
    data.frame(PROCODIGO2=c("TRHR102","TRPV102","TROP102","TRHC")) ,

  by="TRATAMENTO") %>% 
        .[,c(1,2,12,13,17)] %>% View()


## X4D ==================================================


left_join(PVC_G88_X4D %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_X4D  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR102","TRPV102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
           .[,c(1,2,12,13,17)] %>% View()

## XTRACK ==================================================

left_join(PVC_G88_XTRACK %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XTRACK  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## XTRACK FIT ==================================================

left_join(PVC_G88_XTRACKFIT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XTRACKFIT  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## XDESIGN  ==================================================

left_join(PVC_G88_XDESIGN %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XDESIGN  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## XDESIGNFIT  ==================================================

left_join(PVC_G88_XDESIGNFIT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XDESIGNFIT  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()

## XDESIGSHORT  ==================================================

left_join(PVC_G88_XDESIGNSHORT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_XDESIGNSHORT  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR102","TRPV102","TRCR101","TRCE102","TROP102")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## E DESIGN ==================================================

left_join(PVC_G88_EDESIGN %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_EDESIGN  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## PHYSIO ==================================================

left_join(PVC_G88_PHYSIO%>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_PHYSIO  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR102","TRPV102","TRCR101","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## COMFORTMAX ==================================================

left_join(PVC_G88_COMFORTMAX %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_COMFORTMAX  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRHR102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## LIBERTY ==================================================

left_join(PVC_G88_LIBERTY %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_LIBERTY  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRHR102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## LIBERTY SHORT ==================================================

left_join(PVC_G88_LIBERTY_SHORT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_LIBERTY_SHORT  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRHR102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()

## DIGITIME ==================================================

left_join(PVC_G88_DIGITIME %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_DIGITIME  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRHR102","TRCR102","TRCE102","TROP102","TRTEC102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## SPORT ==================================================

left_join(PVC_G88_SPORT %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_SPORT %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV102","TRHR102","TRCR102","TRCE102","TROP102","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## COMFORT TRAD ==================================================


left_join(PVC_G88_COMFORTTRAD %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_COMFORTTRAD  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV101","TRHR101","TRCR101","TRCE101","TROP101","TRTEC101","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## LIBERTY TRAD ==================================================


left_join(PVC_G88_LIBERTYTRAD %>% filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)),
          
          PVC_G88_LIBERTYTRAD  %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV101","TRHR101","TRCR101","TRCE101","TROP101","TRTEC101","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


## UNIQUE ==================================================

left_join(PVC_G88_KDKUNIQUE,
          
          PVC_G88_KDKUNIQUE  %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV202","TRHR202","TRCR202","TRCE202","TROP202","TRNR202","TRTEC202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),] %>%  View()



## NETWORK ==================================================

left_join(PVC_G88_NETWORK,
          
          PVC_G88_NETWORK %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV202","TRHR202","TRCR202","TRCE202","TROP202","TRNR202","TRTEC202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),] %>%  View()


## PRECISE ==================================================

left_join(PVC_G88_KDK_PRECISE,
          
          PVC_G88_KDK_PRECISE %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV202","TRHR202","TRCR202","TRCE202","TROP202","TRNR202","TRTEC202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),]  %>% View()

## PRECISE TRAD ==================================================

left_join(PVC_G88_PRECISETRAD,
          
          PVC_G88_PRECISETRAD %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV201","TRHR201","TRCR201","TRCE201","TROP201","TRNR201","TRTEC201","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),]  %>% View()


##  KODAK SOFTWARE ==================================================

left_join(PVC_G88_KDKSOFTWARE,
          
          PVC_G88_KDKSOFTWARE %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV202","TRHR202","TRCR202","TRCE202","TROP202","TRNR202","TRTEC202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),]  %>%  View()


##  KODAK SINGLE ORMA ==================================================

left_join(PVC_G88_KDKDSINGLEORMA,
          
          PVC_G88_KDKDSINGLEORMA %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV202","TRHR202","TRCR202","TRCE202","TROP202","TRNR202","TRTEC202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),] %>%  View()


##  KODAK SINGLE POLY ==================================================

left_join(PVC_G88_KDKSINGLEPOLY,
          
          PVC_G88_KDKSINGLEPOLY %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV202","TRHR202","TRCR202","TRCE202","TROP202","TRNR202","TRTEC202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),] %>% View()



##  KODAK SINGLE SUN ==================================================

left_join(PVC_G88_KDKSINGLESUN,
          
          PVC_G88_KDKSINGLESUN %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR202","TROP202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$PROCODIGO2),]  %>% View()


##  KODAK EASY SUN ==================================================

left_join(PVC_G88_KDKEASYSUN,
          
          PVC_G88_KDKEASYSUN %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR202","TROP202","TRHC")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$PROCODIGO2),]  %>% View()


##  EYEZEN BOOST ==================================================

left_join(PVC_G88_EYEZENBOOST,
          
          PVC_G88_EYEZENBOOST %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV309","TRHR309","TRCR309","TRCE309","TROP309","TRARSTAND")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),]  %>% View()


##  EYEZEN START ==================================================

left_join(PVC_G88_EYEZENSTART,
          
          PVC_G88_EYEZENSTART %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV309","TRHR309","TRCR309","TRCE309","TROP309","TRARSTAND")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),]  %>% View()



##  EYEZEN KIDS ==================================================

left_join(PVC_G88_EYEZENKIDS,
          
          PVC_G88_EYEZENKIDS %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV309","TRHR309","TRCR309","TRCE309","TROP309","TRARSTAND")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% .[!is.na(.$LENTES),] %>% View()


##  INTERVIEW ==================================================

left_join(PVC_G88_INTERVIEW,
          
          PVC_G88_INTERVIEW %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV","TRHR","TRCR","TRCE","TROP","TRIC","")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


##  SURF ORMA ==================================================

left_join(PVC_G88_SURFORMA,
          
          PVC_G88_SURFORMA %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV","TRHR","TRCR","TRCE","TROP","TRIC","","TRNR")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


##  SURF AIR STY ==================================================

left_join(PVC_G88_SURFAIRSTY,
          
          PVC_G88_SURFAIRSTY %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRPV","TRHR","TRCR","TRCE","TROP","TRIC","","TRNR")) ,
          
          by="TRATAMENTO") %>% 
  .[,c(1,2,12,13,17)] %>% View()


##  POLAR ==================================================

left_join(PVC_G88_POLAR,
          
          PVC_G88_POLAR %>% 
            filter(!is.na(TRATAMENTO)) %>% 
            filter(!TRATAMENTO %in% c("Coloração","Coloração Especial") & !is.na(TRATAMENTO)) %>% 
            distinct(TRATAMENTO) %>% 
            data.frame(PROCODIGO2=c("TRHR","TROP","")) ,
          
          by="TRATAMENTO") %>% 
  .[!is.na(.$LENTES),c(1,2,12,13,17)]  %>% View()


##  VS ==================================================

PVC_G88_VS %>% 
  .[!is.na(.$LENTES),c(1,2,11)]  %>% View()











