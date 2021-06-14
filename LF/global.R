#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## ######################## ###
##     I) SETEO GENERAL     ####
## ######################## ###

# BORRAR TODO LO ANTERIOR
rm(list=ls())

# INFO DEL USUARIO
USER=Sys.info()
USER=USER[7]

# SETEO DEL WD
# setwd(paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs',sep = ""))

# CARGANDO ARCHIVO BASE
source(paste('C:/Users/',USER,'/Google Drive/4) R/1) Scripts/0) SCRIPT BASE/BASE_LF.R',sep = ""))

###############################
library(shiny)
library(networkD3)



#------DATA IGAS ------------

## ################################# ###
##    III) CARGAR Y TRABAJAR DATA    ####
## ################################# ###

# FUENTE
FUENTE_IGAS<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRafB2LaFQ0YIPfK5j1ofCaBz4oRz3-1Jqt3c9UST-WnJ7j2D0tbEMsPhTDBd5qhlk6gnnCXlw_CQDy/pub?output=xlsx"
# SETEANDO LA DIRECCION DEL DRIVE
archivo='IGAS_INAF.csv'
# GENERANDO UN TEMPORAL
tp1<-tempfile()
# DESCARGAR
download.file(FUENTE_IGAS,tp1,mode ="wb")
# SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_INAF<-read_xlsx(path = tp1, sheet = "BD_INAF")

# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

#ELIMINAR UNA VARIABLE
#BD_INAF <- BD_INAF[ ,-(14:16)]
BD_INAF=BD_INAF %>%
   subset(select = -c(Certificador,`Consultora ambiental`, `N° Documento de aprobación`)) %>%
   as.data.frame()


#RENOMBRANDO VARIABLES
names(BD_INAF)[names(BD_INAF) == 'Nombre del instrumento'] <- 'N_INST'
names(BD_INAF)[names(BD_INAF) == 'Administrado'] <- 'ADM_ACT'
names(BD_INAF)[names(BD_INAF) == 'Administrado original'] <- 'ADM_ORIG'
names(BD_INAF)[names(BD_INAF) == 'Unidad fiscalizable'] <- 'UF'
names(BD_INAF)[names(BD_INAF) == 'Subsector'] <- 'SUB_SECT'
names(BD_INAF)[names(BD_INAF) == 'Tipo de documento de aprobación'] <- 'T_DOC_APROB'
names(BD_INAF)[names(BD_INAF) == 'Fecha de aprobación'] <- 'F_APROB'
names(BD_INAF)[names(BD_INAF) == 'Archivos registrados'] <- 'N_ARCH'
names(BD_INAF)[names(BD_INAF) == 'Estado'] <- 'ESTADO'
names(BD_INAF)[names(BD_INAF) == 'Usuario de registro'] <- 'REGISTRADOR'
names(BD_INAF)[names(BD_INAF) == 'Fecha de registro'] <- 'F_REG'
names(BD_INAF)[names(BD_INAF) == 'COD ADMINISTRADO'] <- 'COD_ADM'
names(BD_INAF)[names(BD_INAF) == 'COD UF'] <- 'COD_UF'

#ELIMINANDO VACIOS
BD_INAF= BD_INAF[!is.na(BD_INAF$N_INST), ]

#TRANSFORMANDO VARIABLES
#BD_INAF$F_APROB=as.Date(as.numeric(BD_INAF$F_APROB,na.rm=TRUE),origin="1899-12-30")
BD_INAF$ETAPA=NA
BD_INAF$SUB_SECT_0=NA
BD_INAF$ESTADO=gsub("\n", "", BD_INAF$ESTADO)

#REEMPLAZAR LOS VALORES POR VALORES UN POCO MAS SIMPLES DE LEER
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Electricidad"]="CELE"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Minería"]="CMIN"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Hidrocarburos"]="CHID"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Industria"]="CIND"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Agricultura"]="CAGR"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Pesquería"]="CPES"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Residuos Sólidos"]="CRES"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Hidrocarburos, Industria"]="CHID"

BD_INAF$SUB_SECT_0[BD_INAF$SUB_SECT=="CELE"|BD_INAF$SUB_SECT=="CMIN"|BD_INAF$SUB_SECT=="CHID"]="DSEM"
BD_INAF$SUB_SECT_0[BD_INAF$SUB_SECT=="CIND"|BD_INAF$SUB_SECT=="CAGR"|BD_INAF$SUB_SECT=="CPES"]="DSAP"
BD_INAF$SUB_SECT_0[BD_INAF$SUB_SECT=="CRES"]="DSIS"

BD_INAF$ETAPA[is.na(BD_INAF$ESTADO)==T]="1) REGISTRADOR"
BD_INAF$ETAPA[BD_INAF$ESTADO=="En revisión [Coordinador]"|BD_INAF$ESTADO=="Observado [Coordinador]"]="2) COORDINADOR"
BD_INAF$ETAPA[BD_INAF$ESTADO=="En revisión [Especialista CSIG]"|BD_INAF$ESTADO=="Observado [Especialista CSIG]"|BD_INAF$ESTADO=="Validado [Especialista CSIG]"]="3) ESPECIALISTA CSIG"

BD_INAF$ESTADO[is.na(BD_INAF$ESTADO)==T]="PENDIENTE DE REVISION"
BD_INAF$ESTADO[BD_INAF$ESTADO=="En revisión [Coordinador]"]="EN REVISION"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Observado [Coordinador]"]="OBSERVADO"
BD_INAF$ESTADO[BD_INAF$ESTADO=="En revisión [Especialista CSIG]"]="EN REVISION"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Observado [Especialista CSIG]"]="OBSERVADO"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Validado [Especialista CSIG]"]="VALIDADO"



# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# #################### #
# ##     BD UFC     ## #
# #################### #

#FUENTE
FUENTE_UFC= "https://docs.google.com/spreadsheets/d/e/2PACX-1vQeH0CEF06jjKnKpeu3mylLMi0PA5OKjubQzxKKb8ZCODBKHnyXfnExFVaywIMl5woO_HakkNQIucSI/pub?output=xlsx"

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR
download.file(FUENTE_UFC,tp1,mode ="wb")

#SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_CRITICAS<-read_xlsx(path = tp1, sheet = "ORIGINAL")

#ELIMINAR UNA VARIABLE
BD_CRITICAS <- BD_CRITICAS[ ,-c(1,9,10)]

#REEMPLAZAR LOS VALORES POR VALORES UN POCO MAS SIMPLES DE LEER
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Electricidad"]="CELE"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Minería"]="CMIN"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Hidrocarburos"]="CHID"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Industria"]="CIND"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Agricultura"]="CAGR"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Pesca"]="CPES"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Residuos Sólidos"]="CRES"



# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ######################## #
# ##     BD PLANEFA     ## #
# ######################## #

#FUENTE
FUENTE_PLANEFA= "https://docs.google.com/spreadsheets/d/e/2PACX-1vRIScdxGBlkl_r2lf8GcW-F_RtRfKCtbD7xHnG0xcUxLnVdIR2lqVCeGMjuoeaIcRPLnsICAG-uwPYn/pub?output=xlsx"

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR
download.file(FUENTE_PLANEFA,tp1,mode ="wb")

#SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_PLANEFA<-read_xlsx(path = tp1, sheet = "CRITICOS")

#ELIMINAR UNA VARIABLE
BD_PLANEFA <- BD_PLANEFA[ ,-c(1,4,6,8:14)]

#ELIMINANDO CASOS DUPLICADOS
BD_PLANEFA=BD_PLANEFA[!duplicated(BD_PLANEFA$COD_UF), ]

#CREANDO VARIABLE QUE INDIQUE SI ES CASO PRIORIZADO
BD_PLANEFA$CASO_PRIOR="PRIORIZADO"



# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ################################# #
# ##    II) HISTORIAL DE IGAS    ## #
# ################################# #

#FUENTE
FUENTE_HISTORIAL<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSBErlOvYBET_39N9DVvanERIeuH1FlC-xDubfnvcvKWOOTpFEFSYR_HnQx9faFZCu_J6T-CgtwjZ6W/pub?output=xlsx"

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR
download.file(FUENTE_HISTORIAL,tp1,mode ="wb")

#SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_HISTORIAL<-read_xlsx(path = tp1, sheet = "BD_HISTORIAL")
BD_HISTORIAL_DICC<-read_xlsx(path = tp1, sheet = "DIC_DATOS")

# __________________________________________________________________________________________________________________________________________________________

# ############################ #
# ##  TRABAJAR ENCABEZADOS  ## #
# ############################ #

BD_HISTORIAL_DICC= BD_HISTORIAL_DICC[BD_HISTORIAL_DICC$INFO_IRRELEV!=1, ] #Se eliminan las variables irrelevantes
BD_HISTORIAL_DICC=BD_HISTORIAL_DICC[,c(1,2)] #solo me quedo con los campos en BD y el CODIGO CSEP
BD_HISTORIAL_DICC=as.list(BD_HISTORIAL_DICC) #Defino las cabeceras
BD_HISTORIAL=subset(BD_HISTORIAL, select = BD_HISTORIAL_DICC$ENCABEZADOS) #Limito los campos con los que se trabajar?
colnames(BD_HISTORIAL)=BD_HISTORIAL_DICC$COD_ENCAB #Renombro las cabeceras seg?n CODIGO CSEP
rm(BD_HISTORIAL_DICC) #Eliminando DIC

# #TRABAJANDO DATA DE HORAS
# BD_HISTORIAL$HORA=format(BD_HISTORIAL$F_ESTADO_HORA, "%Y/%m/%d - %H/%M")
# 
# 
# 
# BD_HISTORIAL$HORA = chron(dates=F_ESTADO_HORA,times=dtparts[,2], format=c('y-m-d','h:m:s'))


# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ################################# #
# ##    III) ARCHIVOS DE IGAS    ## #
# ################################# #

#FUENTE
FUENTE_ARCH<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSnQ63y1JKc00rbS0N2LIYg5QyaXIhbs9km-M2uCYsWVkjftSL4BljLgLUH64gG1RlrtOBCu4Ik4CUf/pub?output=xlsx"
#GENERANDO UN TEMPORAL
tp1<-tempfile()
#DESCARGAR
download.file(FUENTE_ARCH,tp1,mode ="wb")
#SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_ARCH<-read_xlsx(path = tp1, sheet = "BD_ARCH")
BD_DIC_ARCH<-read_xlsx(path = tp1, sheet = "DIC_DATOS")

# __________________________________________________________________________________________________________________________________________________________

# ################################# #
# ##     TRABAJAR ENCABEZADOS    ## #
# ################################# #

BD_DIC_ARCH= BD_DIC_ARCH[BD_DIC_ARCH$INFO_IRRELEV!=1, ] #Se eliminan las variables irrelevantes
BD_DIC_ARCH=BD_DIC_ARCH[,c(1,2)] #solo me quedo con los campos en BD y el CODIGO CSEP
BD_DIC_ARCH=as.list(BD_DIC_ARCH) #Defino las cabeceras
BD_ARCH=subset(BD_ARCH, select = BD_DIC_ARCH$ENCABEZADOS) #Limito los campos con los que se trabajar?
colnames(BD_ARCH)=BD_DIC_ARCH$COD_ENCAB #Renombro las cabeceras seg?n CODIGO CSEP
rm(BD_DIC_ARCH) #Eliminando DIC

# __________________________________________________________________________________________________________________________________________________________

# ################################ #
# ##      TRABAJAR LA DATA      ## #
# ################################ #

BD_ARCH$ARCH_SIZE_MB=round(BD_ARCH$ARCH_SIZE/1000000,2)
BD_ARCH=summaryBy(ARCH_SIZE_MB ~ COD_INST, FUN=sum, data =as.data.frame(BD_ARCH),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T

# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ################################ #
# ##    UNIENDO BD IGAS+ARCH    ## #
# ################################ #

#COMBINAR CON OTRAS COLUMNAS DE TABLA TAREAS
BD_INAF=merge(BD_INAF,BD_ARCH,
              by.x="Código",
              by.y="COD_INST",
              all.x=T)

#CREANDO ESTADO AUX
BD_INAF$ESTADO_AUX="En proceso"
BD_INAF$ESTADO_AUX[BD_INAF$ESTADO=="VALIDADO"]="Validado"
BD_INAF$ARCH_SIZE_MB[is.na(BD_INAF$ARCH_SIZE_MB)==T]=0


# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ################################## #
# ##   UNIENDO BD IGAS+CRITICAS   ## #
# ################################## #

#CREAMOS AUXILIARES
BD_INAF$AUX=paste(BD_INAF$COD_UF,BD_INAF$SUB_SECT,sep = "-")
BD_CRITICAS$AUX=paste(BD_CRITICAS$COD_UF,BD_CRITICAS$SUBSECTOR,sep = "-")

#COMBINAR CON OTRAS COLUMNAS DE TABLA TAREAS
BD_INAF_CRIT=merge(BD_INAF,BD_CRITICAS,
                   by.x="AUX",
                   by.y="AUX",
                   all.x=T)
BD_INAF_CRIT$UF_CRITICA=NA
BD_INAF_CRIT$UF_CRITICA[is.na(BD_INAF_CRIT$FUENTE)==T]="NO"
BD_INAF_CRIT$UF_CRITICA[is.na(BD_INAF_CRIT$FUENTE)==F]="SI"

#CREANDO ESTADO AUX
BD_INAF_CRIT$ESTADO_AUX="En proceso"
BD_INAF_CRIT$ESTADO_AUX[BD_INAF$ESTADO=="VALIDADO"]="Validado"



# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ##################################### #
# ##   UNIENDO BD IGAS+PRIORIZADOS   ## #
# ##################################### #

#COMBINAR CON OTRAS COLUMNAS DE TABLA TAREAS
BD_INAF_PRIOR=merge(BD_INAF,BD_PLANEFA,
                    by.x="COD_UF",
                    by.y="COD_UF",
                    all.x=T)

BD_INAF_PRIOR$CASO_PRIOR[is.na(BD_INAF_PRIOR$CASO_PRIOR)==T]="NO"
BD_INAF_PRIOR$CASO_PRIOR[BD_INAF_PRIOR$CASO_PRIOR=="PRIORIZADO"]="SI"

#CREANDO ESTADO AUX
BD_INAF_PRIOR$ESTADO_AUX="En proceso"
BD_INAF_PRIOR$ESTADO_AUX[BD_INAF$ESTADO=="VALIDADO"]="Validado"



# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ##################################### #
# ##    UNIENDO BD IGAS+HISTORIAL    ## #
# ##################################### #

#COMBINAR CON OTRAS COLUMNAS DE TABLA TAREAS
BD_HISTORIAL=merge(BD_HISTORIAL,subset(BD_INAF, select = c("Código","SUB_SECT_0","SUB_SECT","REGISTRADOR")),
                   by.x="COD_INST",
                   by.y="Código",
                   all.x=T)



# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ################################### #
# ##    IV) BD ADMINISTRADOS/UF    ## #
# ################################### #

#FUENTE
FUENTE_ARCH<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRdvKvyxqdRnyvkFngDf9SGvBZVZt9aHygcq6JWgMUVeDxJE6QqsgTCwsSdhq0xFQvMSLYOT1-XifxV/pub?output=xlsx"
#GENERANDO UN TEMPORAL
tp1<-tempfile()
#DESCARGAR
download.file(FUENTE_ARCH,tp1,mode ="wb")
#SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_ADM_UF<-read_xlsx(path = tp1, sheet = "BD_ADM_UF")

#REEMPLAZAR LOS VALORES POR VALORES UN POCO MAS SIMPLES DE LEER
BD_ADM_UF$SUB_SECT[BD_ADM_UF$SUB_SECT=="Electricidad"]="CELE"
BD_ADM_UF$SUB_SECT[BD_ADM_UF$SUB_SECT=="Minería"]="CMIN"
BD_ADM_UF$SUB_SECT[BD_ADM_UF$SUB_SECT=="Hidrocarburos"]="CHID"
BD_ADM_UF$SUB_SECT[BD_ADM_UF$SUB_SECT=="Industria"]="CIND"
BD_ADM_UF$SUB_SECT[BD_ADM_UF$SUB_SECT=="Agricultura"]="CAGR"
BD_ADM_UF$SUB_SECT[BD_ADM_UF$SUB_SECT=="Pesquería"]="CPES"
BD_ADM_UF$SUB_SECT[BD_ADM_UF$SUB_SECT=="Residuos Sólidos"]="CRES"
BD_ADM_UF$SUB_SECT[BD_ADM_UF$SUB_SECT=="Consultoras Ambientales"]="CAMB"



# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ########################################### #
# ##    IV) BD ADMINISTRADOS/UF+PLANEFA    ## #
# ########################################### #


BD_ADM_UF=merge(BD_ADM_UF,BD_PLANEFA,
                by.x="COD_UF",
                by.y="COD_UF",
                all.x=T)






########################################



# __________________________________________________________________________________________________________________________________________________________

## ############################################# ###
##    2) ERRORES: 1) AVANCE Y ERRORES: ACTUAL    ####
## ############################################# ###


# F_min=format(as.Date(F_MIN,"%Y-%m-%d"),"%d/%m/%Y")   #Optimizar esto
# F_max=format(as.Date(F_MAX,"%Y-%m-%d"),"%d/%m/%Y")




# GENERANDO DATA 1
SUB_BD_HISTORIAL=merge(BD_HISTORIAL,BD_INAF[,c(1,15)],
                       by.x="COD_INST",
                       by.y="Código",
                       all.x=T)  %>%
   mutate(OBS_ERRORES=ifelse(str_detect(OBS_DOC, "registrador|coordinador")==T & str_detect(OBS_DOC, "solicitud")==T,"T","F")) %>%
   # subset(F_ESTADO>=F_MIN & F_ESTADO<=F_MAX) %>%
   mutate(C_FLUJOS=1,OBS=ifelse(PROC_ESTADO=="OBSERVADO",1,0)) %>%
   mutate(MES=month(F_ESTADO)) %>%
   group_by(SUB_SECT,ESTADO,COD_INST,ETAPA,OBS_ERRORES) %>% 
   summarise(C_FLUJOS = sum(C_FLUJOS,na.rm = TRUE),OBS = sum(OBS,na.rm = TRUE)) %>% 
   mutate(OBS=ifelse(OBS>=1,1,0)) %>% 
   mutate(CASOS_CSIG=ifelse(ETAPA=="3) ESPECIALISTA CSIG",1,0)) %>% 
   mutate(OBS_COORD=ifelse(ETAPA=="2) COORDINADOR",1,0)) %>% 
   mutate(OBS_CSIG=ifelse(ETAPA=="3) ESPECIALISTA CSIG"&OBS==1&OBS_ERRORES!="T",1,0)) %>% 
   group_by(COD_INST,SUB_SECT,ESTADO) %>%
   summarise(C_FLUJOS=sum(C_FLUJOS,na.rm = TRUE), OBS=sum(OBS,na.rm = TRUE),CASOS_CSIG=sum(CASOS_CSIG,na.rm = TRUE), OBS_COORD=sum(OBS_COORD,na.rm = TRUE), OBS_CSIG=sum(OBS_CSIG,na.rm = TRUE)) %>%
   mutate(OBS=ifelse(OBS>=1,1,0),CASOS_COORD=1) %>% 
   group_by(SUB_SECT) %>%
   summarise(C_FLUJOS=sum(C_FLUJOS,na.rm = TRUE), OBS=sum(OBS,na.rm = TRUE),CASOS_CSIG=sum(CASOS_CSIG,na.rm = TRUE), OBS_COORD=sum(OBS_COORD,na.rm = TRUE), OBS_CSIG=sum(OBS_CSIG,na.rm = TRUE), CASOS_COORD=sum(CASOS_COORD,na.rm = TRUE)) %>%
   mutate(PORC_CASOS_OBS_COORD=round(100*OBS_COORD/CASOS_COORD,1), PORC_CASOS_OBS_CSIG=round(100*OBS_CSIG/CASOS_CSIG,1)) %>%
   subset(is.na(SUB_SECT)==F)


BD_UNIV_ADM_UF=BD_ADM_UF %>%
   group_by(SUB_SECT) %>%
   summarise(UNIV_UF=n_distinct(COD_UF))


BD_RESUMEN_2=subset(BD_INAF) %>% #,F_REG>=F_MIN & F_REG<=F_MAX
   group_by(COD_UF,SUB_SECT) %>%
   summarise(C_IGAS = n()) %>%
   merge(distinct(BD_ADM_UF, COD_UF, .keep_all = TRUE),
         by.x="COD_UF",
         by.y="COD_UF",
         all.x=T)%>%
   rename( SUB_SECT=SUB_SECT.x) %>%
   group_by(SUB_SECT) %>%
   summarise(TOT_IGA_INAF=sum(C_IGAS),MEDIA_IGA_UF=mean(C_IGAS),MEDIANA_IGA_UF=median(C_IGAS),DS_IGA_UF=sd(C_IGAS),media_T=mean(C_IGAS), Q3_IGA_UF=quantile(C_IGAS, 0.74),UNIV_UF_INAF=n_distinct(COD_UF)) %>%
   merge(BD_UNIV_ADM_UF,
         by.x="SUB_SECT",
         by.y="SUB_SECT",
         all.x=T) %>%
   subset(is.na(SUB_SECT)==F) %>%
   mutate(MEDIA_AJUST=MEDIA_IGA_UF+DS_IGA_UF*(1-(UNIV_UF_INAF/UNIV_UF)), E_UNIV_IGAS=ceiling(MEDIA_AJUST*UNIV_UF), AVANCE_IGA=TOT_IGA_INAF/E_UNIV_IGAS) %>%
   merge(SUB_BD_HISTORIAL,
         by.x="SUB_SECT",
         by.y="SUB_SECT",
         all.x=T) %>%
   mutate(AVANCE_IGA=100*round(AVANCE_IGA,3)) %>%
   subset(is.na(TOT_IGA_INAF)==F) %>%
   mutate(AVANCE_IGA=ifelse(SUB_SECT=="CRES",100,AVANCE_IGA)) %>%
   as.data.frame()


BD_RESUMEN_2=subset(BD_INAF,ETAPA=="3) ESPECIALISTA CSIG") %>%
   group_by(COD_UF,SUB_SECT) %>%
   summarise(C_IGAS = n()) %>%
   group_by(SUB_SECT) %>%
   summarise(IGAS_CSIG=sum(C_IGAS)) %>%
   merge(BD_RESUMEN_2,
         by.x="SUB_SECT",
         by.y="SUB_SECT",
         all.x=T) %>%
   mutate(AVANCE_IGA_CSIG=100*round(IGAS_CSIG/E_UNIV_IGAS,3)) %>%
   mutate(AVANCE_IGA_CSIG=ifelse(SUB_SECT=="CRES",100,AVANCE_IGA_CSIG))










####################################################
