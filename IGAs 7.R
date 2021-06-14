# ########################################################### #
# ########  MONITOREO DE LOS AVANCES DE LA BD IGA's  ######## #
# #########################  BY LF  ######################### #
# ########################################################### #


## ######################## ###
##     I) SETEO GENERAL     ####
## ######################## ###

# BORRAR TODO LO ANTERIOR
rm(list=ls())

# INFO DEL USUARIO
USER=Sys.info()
USER=USER[7]

# SETEO DEL WD
setwd(paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs',sep = ""))

# CARGANDO ARCHIVO BASE
source(paste('C:/Users/',USER,'/Google Drive/4) R/1) Scripts/0) SCRIPT BASE/BASE_LF.R',sep = ""))

# CARGAR LA LIBRER?A PARA ORACLE
# library(RJDBC)

# OTRAS LIB
library(profvis)


###############################



# __________________________________________________________________________________________________________________________________________________________

## ################################## ###
##    II) PROCESAMIENTO DE PRE-DATA   ####
## ################################## ###

# VERIFICAR SI EXISTE EL COMANDO DE AUTORIZACIÓN
if (exists("AUTORIZACION")==TRUE) {
  rm(AUTORIZACION) #source(paste('C:/Users/',USER,'/Google Drive/4) R/1) Scripts/1) OEFA/3) IGAS/PROC_DATA_IGA.R',sep = ""), encoding="utf-8")
} 


# SOLICITAR LA AUTORIZACIÓN
AUTORIZACION= askYesNo("¿Desea pre procesar y subir la data a Drive?", default = TRUE,prompts = getOption("askYesNo", gettext(c("SÍ", "NO", "CANCELAR"))))


#EJECUTAR COMANDO DE ACTUALIZACIÓN
if (AUTORIZACION==TRUE & is.na(AUTORIZACION)==FALSE) {
  source(paste('C:/Users/',USER,'/Google Drive/4) R/1) Scripts/1) OEFA/3) IGAS/PROC_DATA_IGA.R',sep = ""), encoding="utf-8")
} else if (AUTORIZACION==FALSE& is.na(AUTORIZACION)==FALSE) {
  message("NO SE HA REALIZADO LA CARGA DE DATOS A DRIVE")
  } else {
    stop("SE HA CANCELADO EL PROCESO", call. = FALSE)
      }
  


#########################################



# __________________________________________________________________________________________________________________________________________________________

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
BD_INAF <- BD_INAF[ ,!colnames(BD_INAF)=="Certificador"]
BD_INAF <- BD_INAF[ ,!colnames(BD_INAF)=="Consultora ambiental"]
BD_INAF <- BD_INAF[ ,!colnames(BD_INAF)=="Documento de aprobación"]

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

## ########################################### ###
##    IV) CREANDO UNAS FORMULAS PARA CONTEO    ####
## ########################################### ###

fun <- function(x){
  c(cuenta=sum(!is.na(x)))
}
fun1 <- function(x){
  c(cuenta=sum((x)))
}
fun2 <- function(x){
  c(cuenta=count((x),na.rm =T ))
}
fun3 <- function(x){
  c(cuenta=unique((x),na.rm =T ))
}

##################################################


# __________________________________________________________________________________________________________________________________________________________

## ################################### ###
##    V) PARAMETROS DE LOS GRÁFICOS    ####
## ################################### ###


# #################################################### #
# ##    SETEANDO FECHAS Y PARAMETROS DE GRÁFICOS    ## #
# #################################################### #
fecha_inicio=format(as.Date(min(BD_INAF$F_REG),"%Y-%m-%d"),"%d/%m/%Y")
fecha_final=format(as.Date(max(BD_INAF$F_REG),"%Y-%m-%d"),"%d/%m/%Y")

DURACION=8
FPS=15
RES=250
ALTO=1600
ANCHO=2000
ESCAL_CONV=0.026458333

NOTA_1 <- expression(paste(""^"1"))
NOTA_2 <- expression(paste(" ",2))
NOTA_3 <- expression(paste(" ",3))


F_MIN="2020-06-08"
F_MAX=Sys.Date()

pause(10)

##########################################


# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

## ############################################# ###
##    2) ERRORES: 1) AVANCE Y ERRORES: ACTUAL    ####
## ############################################# ###

#GENERANDO DATA 1
SUB_BD_HISTORIAL=merge(BD_HISTORIAL,BD_INAF[,c(1,16)],
                       by.x="COD_INST",
                       by.y="Código",
                       all.x=T)

# SUB_BD_HISTORIAL$OBSXXX=str_detect(SUB_BD_HISTORIAL$OBS_DOC, "registrador|coordinador")
# SUB_BD_HISTORIAL$OBSXXX=str_detect(SUB_BD_HISTORIAL$OBS_DOC, "solicitud")

SUB_BD_HISTORIAL$OBSXXX=ifelse(str_detect(SUB_BD_HISTORIAL$OBS_DOC, "registrador|coordinador")==T & str_detect(SUB_BD_HISTORIAL$OBS_DOC, "solicitud")==T,"T","F")




SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO>=F_MIN,]  #F MIN
SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO<=F_MAX,]  #F MAX

F_min=format(as.Date(F_MIN,"%Y-%m-%d"),"%d/%m/%Y")
F_max=format(as.Date(F_MAX,"%Y-%m-%d"),"%d/%m/%Y")

SUB_BD_HISTORIAL$AUX1=1
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX1'] <- 'C_FLUJOS'

SUB_BD_HISTORIAL$AUX2=0
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX2'] <- 'OBS'

SUB_BD_HISTORIAL$OBS[SUB_BD_HISTORIAL$PROC_ESTADO=="OBSERVADO"]=1
SUB_BD_HISTORIAL$MES=month(SUB_BD_HISTORIAL$F_ESTADO)
SUB_BD_HISTORIAL=summaryBy(C_FLUJOS+OBS ~ SUB_SECT+ESTADO+COD_INST+ETAPA+OBSXXX, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$OBS[SUB_BD_HISTORIAL$OBS>=1]=1

SUB_BD_HISTORIAL$AUX3=0
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX3'] <- 'CASOS_CSIG'
SUB_BD_HISTORIAL$CASOS_CSIG[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"]=1

SUB_BD_HISTORIAL$AUX4=0
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX4'] <- 'OBS_COORD'
SUB_BD_HISTORIAL$OBS_COORD[SUB_BD_HISTORIAL$ETAPA=="2) COORDINADOR"&SUB_BD_HISTORIAL$OBS==1]=1

SUB_BD_HISTORIAL$AUX5=0
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX5'] <- 'OBS_CSIG'
SUB_BD_HISTORIAL$OBS_CSIG[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"&SUB_BD_HISTORIAL$OBS==1&SUB_BD_HISTORIAL$OBSXXX!="T"]=1
SUB_BD_HISTORIAL=summaryBy(C_FLUJOS+OBS+CASOS_CSIG+OBS_COORD+OBS_CSIG ~ COD_INST+SUB_SECT+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$OBS[SUB_BD_HISTORIAL$OBS>=1]=1
SUB_BD_HISTORIAL$CASOS_COORD=1
SUB_BD_HISTORIAL=summaryBy(C_FLUJOS+OBS+CASOS_CSIG+OBS_COORD+OBS_CSIG+CASOS_COORD ~ SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T


SUB_BD_HISTORIAL$PORC_CASOS_OBS_COORD=round(100*SUB_BD_HISTORIAL$OBS_COORD/SUB_BD_HISTORIAL$CASOS_COORD,1)
SUB_BD_HISTORIAL$PORC_CASOS_OBS_CSIG=round(100*SUB_BD_HISTORIAL$OBS_CSIG/SUB_BD_HISTORIAL$CASOS_CSIG,1)

#TEMPORAL
SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[is.na(SUB_BD_HISTORIAL$SUB_SECT)==F,]


#Arreglando la data
BD_RESUMEN=subset(BD_INAF) %>%
  group_by(COD_UF,SUB_SECT) %>%
  summarise(C_IGAS = n(),F_MIN=min(F_APROB))
BD_RESUMEN$T_AÑOS=as.numeric(difftime(Sys.Date(), BD_RESUMEN$F_MIN, units = c("days")))/365.25
  
  
BD_RESUMEN=merge(BD_RESUMEN, BD_ADM_UF[!duplicated(BD_ADM_UF$COD_UF),],
         by.x="COD_UF",
         by.y="COD_UF",
         all.x=T)


BD_RESUMEN=BD_RESUMEN %>%
  group_by(SUB_SECT.x) %>%
  summarise(TOT_IGA_INAF=sum(C_IGAS),MEDIA_IGA_UF=mean(C_IGAS),MEDIANA_IGA_UF=median(C_IGAS),DS_IGA_UF=sd(C_IGAS),media_T=mean(C_IGAS), Q3_IGA_UF=quantile(C_IGAS, 0.74),UNIV_UF_INAF=n_distinct(COD_UF))


BD_ADM_UF_AUX=BD_ADM_UF %>%
  group_by(SUB_SECT) %>%
  summarise(UNIV_UF=n_distinct(COD_UF))


BD_RESUMEN=merge(BD_ADM_UF_AUX, BD_RESUMEN,
                 by.x="SUB_SECT",
                 by.y="SUB_SECT.x",
                 all.x=T)


BD_RESUMEN=BD_RESUMEN[!is.na(BD_RESUMEN$SUB_SECT)==T,]

BD_RESUMEN$MEDIA_AJUST=BD_RESUMEN$MEDIA_IGA_UF+BD_RESUMEN$DS_IGA_UF*(1-(BD_RESUMEN$UNIV_UF_INAF/BD_RESUMEN$UNIV_UF)) 
BD_RESUMEN$E_UNIV_IGAS=ceiling(BD_RESUMEN$MEDIA_AJUST*BD_RESUMEN$UNIV_UF)
BD_RESUMEN$AVANCE_IGA=BD_RESUMEN$TOT_IGA_INAF/BD_RESUMEN$E_UNIV_IGAS


BD_RESUMEN=merge(BD_RESUMEN, SUB_BD_HISTORIAL,
                 by.x="SUB_SECT",
                 by.y="SUB_SECT",
                 all.x=T)


  

BD_RESUMEN$AVANCE_IGA=100*round(BD_RESUMEN$AVANCE_IGA,3)
BD_RESUMEN=BD_RESUMEN[!is.na(BD_RESUMEN$TOT_IGA_INAF)==T,]
#BD_RESUMEN$SUB_SECT=as.factor(BD_RESUMEN$SUB_SECT)


#Caso especial
BD_RESUMEN$AVANCE_IGA[BD_RESUMEN$SUB_SECT=="CRES"]=100

#GRAFICANDO
BUBBLE_GRAPH=ggplot(BD_RESUMEN)+
  annotate("rect", xmin = 0, xmax = 60, ymin = 0, ymax = 60, 
           alpha = 0.1, fill = "firebrick") + 
  annotate("rect", xmin = 0, xmax = 100, ymin = 10, ymax = 60, 
           alpha = 0.1, fill = "firebrick") + 
  geom_point(aes(x=AVANCE_IGA,y=PORC_CASOS_OBS_CSIG,size=TOT_IGA_INAF,color=SUB_SECT), alpha=0.8)+
  scale_color_manual(values=c(PALETA.PRINCIPAL))+
  ggeasy::easy_all_text_size(size = 15)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
        plot.title = element_text(size = 25, face = "bold"))+
  
  scale_x_continuous(limits = c(0, 102), breaks = seq(0,100,10), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0,100,10), expand = c(0, 0))+

  labs(x="IGAs en INAF* (%)",
       y="\nIGA con observaciones (%)",
       title = paste("IGA con observaciones en la revisión de CSIG y llenado."),
       subtitle = paste("(Revisión de CSIG del ",
                        F_min,
                        " al ",
                        F_max,")\n",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Estimado\nNota1: Los casos óptimos son aquellos con una tasa de observaciones de menos del 10% y un llenado de más de 60% de los caos, delimitado por el área de color rosado\nNota2: Para CRES, debido a que la mayoría de UF no cuenta con IGA, los que han registrado ya constituye el universo. Es por ello que se ha considerado un avance del 100%")+
  scale_size(range = c(0.5,50), name="Cantidad de IGA\nregistrados en INAF")+
  geom_point(aes(x=AVANCE_IGA,y=PORC_CASOS_OBS_CSIG,size=1))+
  geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept=60, linetype="dashed", color = "red", size=1)+
  
  geom_text( x = 60, y = 60, angle = 90, label = paste("Umbral en llenado (60%)", sep=""), 
           hjust = 1,vjust = -0.5,size =5)+
  geom_text( x = 100, y = 10, angle = 0, label = paste("Umbral en observaciones (10%)", sep=""), 
             hjust = 1,vjust = -0.5,size =5)+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  
  ggeasy::easy_add_legend_title("Área")+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

  
BUBBLE_GRAPH

ggsave("0.3.1) OBSERVACIONES-ACTUAL.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)




####################################################


# __________________________________________________________________________________________________________________________________________________________

## ############################################### ###
##    2) ERRORES: 1) AVANCE Y ERRORES: ANTERIOR    ####
## ############################################### ###

li=0
ls=10


#GENERANDO DATA 1
SUB_BD_HISTORIAL=merge(BD_HISTORIAL,BD_INAF[,c(1,16)],
                       by.x="COD_INST",
                       by.y="Código",
                       all.x=T)

SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO>=F_MIN,]  #F MIN
SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO<=as.character(as.Date(F_MAX)),]  #F MAX

SUB_BD_HISTORIAL$OBSXXX=ifelse(str_detect(SUB_BD_HISTORIAL$OBS_DOC, "registrador|coordinador")==T & str_detect(SUB_BD_HISTORIAL$OBS_DOC, "solicitud")==T,"T","F")


F_min=format(as.Date(F_MIN,"%Y-%m-%d"),"%d/%m/%Y")
F_max=format(as.Date(as.character(as.Date(F_MAX)),"%Y-%m-%d"),"%d/%m/%Y")

SUB_BD_HISTORIAL$AUX1=1
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX1'] <- 'C_FLUJOS'

SUB_BD_HISTORIAL$AUX2=0
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX2'] <- 'OBS'

SUB_BD_HISTORIAL$OBS[SUB_BD_HISTORIAL$PROC_ESTADO=="OBSERVADO"]=1
SUB_BD_HISTORIAL$MES=month(SUB_BD_HISTORIAL$F_ESTADO)
SUB_BD_HISTORIAL=summaryBy(C_FLUJOS+OBS ~ SUB_SECT+ESTADO+COD_INST+ETAPA+OBSXXX, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$OBS[SUB_BD_HISTORIAL$OBS>=1]=1

SUB_BD_HISTORIAL$AUX3=0
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX3'] <- 'CASOS_CSIG'
SUB_BD_HISTORIAL$CASOS_CSIG[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"]=1

SUB_BD_HISTORIAL$AUX4=0
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX4'] <- 'OBS_COORD'
SUB_BD_HISTORIAL$OBS_COORD[SUB_BD_HISTORIAL$ETAPA=="2) COORDINADOR"&SUB_BD_HISTORIAL$OBS==1]=1

SUB_BD_HISTORIAL$AUX5=0
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX5'] <- 'OBS_CSIG'
SUB_BD_HISTORIAL$OBS_CSIG[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"&SUB_BD_HISTORIAL$OBS==1&SUB_BD_HISTORIAL$OBSXXX!="T"]=1
SUB_BD_HISTORIAL=summaryBy(C_FLUJOS+OBS+CASOS_CSIG+OBS_COORD+OBS_CSIG ~ COD_INST+SUB_SECT+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$OBS[SUB_BD_HISTORIAL$OBS>=1]=1
SUB_BD_HISTORIAL$CASOS_COORD=1
SUB_BD_HISTORIAL=summaryBy(C_FLUJOS+OBS+CASOS_CSIG+OBS_COORD+OBS_CSIG+CASOS_COORD ~ SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T


SUB_BD_HISTORIAL$PORC_CASOS_OBS_COORD=round(100*SUB_BD_HISTORIAL$OBS_COORD/SUB_BD_HISTORIAL$CASOS_COORD,1)
SUB_BD_HISTORIAL$PORC_CASOS_OBS_CSIG=round(100*SUB_BD_HISTORIAL$OBS_CSIG/SUB_BD_HISTORIAL$CASOS_CSIG,1)

#TEMPORAL
SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[is.na(SUB_BD_HISTORIAL$SUB_SECT)==F,]


## ################### ##
##   GENERANDO TABLA   ##
## ################### ##

#CANTIDAD DE IGA POR CADA UF
BD_RESUMEN=subset(BD_INAF) %>%
  group_by(COD_UF,SUB_SECT) %>%
  summarise(C_IGAS = n())
# BD_RESUMEN$T_A?OS=as.numeric(difftime(Sys.Date(), BD_RESUMEN$F_MIN, units = c("days")))/365.25

#INCORPORANDO INFO RELEVANTE DE CADA UF
BD_RESUMEN=merge(BD_RESUMEN, BD_ADM_UF[!duplicated(BD_ADM_UF$COD_UF),],
                 by.x="COD_UF",
                 by.y="COD_UF",
                 all.x=T)

#PRIMERA TABLA RESUMEN
BD_RESUMEN=BD_RESUMEN %>%
  group_by(SUB_SECT.x) %>%
  summarise(TOT_IGA_INAF=sum(C_IGAS),MEDIA_IGA_UF=mean(C_IGAS),MEDIANA_IGA_UF=median(C_IGAS),DS_IGA_UF=sd(C_IGAS),media_T=mean(C_IGAS), Q3_IGA_UF=quantile(C_IGAS, 0.74),UNIV_UF_INAF=n_distinct(COD_UF))

#CALCULANDO UNIV DE UF
BD_ADM_UF_AUX=BD_ADM_UF %>%
  group_by(SUB_SECT) %>%
  summarise(UNIV_UF=n_distinct(COD_UF))


#COMPLEMENTANDO LA PRIMERA TABLA CON EL CALCULO DE UNIV DE UF
BD_RESUMEN=merge(BD_ADM_UF_AUX, BD_RESUMEN,
                 by.x="SUB_SECT",
                 by.y="SUB_SECT.x",
                 all.x=T)


#CANTIDAD DE IGA EN CSIG
BD_RESUMEN2=subset(BD_INAF,ETAPA=="3) ESPECIALISTA CSIG") %>%
  group_by(COD_UF,SUB_SECT) %>%
  summarise(C_IGAS = n()) %>%
  group_by(SUB_SECT) %>%
  summarise(TOT_IGA_INAF=sum(C_IGAS))
colnames(BD_RESUMEN2)[2] <-"IGAS_CSIG"

#COMPLEMENTANDO LA TABLA GENERAL CON LA CANTIDAD DE IGAS EN CSIG
BD_RESUMEN=merge(BD_RESUMEN, BD_RESUMEN2,
                 by.x="SUB_SECT",
                 by.y="SUB_SECT",
                 all.x=T)

#BORRANDO EL CASO ANOMALO
BD_RESUMEN=BD_RESUMEN[!is.na(BD_RESUMEN$SUB_SECT)==T,]

#CREANDO NUEVAS VARIABLES DE CALCULO
BD_RESUMEN$MEDIA_AJUST=BD_RESUMEN$MEDIA_IGA_UF+BD_RESUMEN$DS_IGA_UF*(1-(BD_RESUMEN$UNIV_UF_INAF/BD_RESUMEN$UNIV_UF)) 
BD_RESUMEN$E_UNIV_IGAS=ceiling(BD_RESUMEN$MEDIA_AJUST*BD_RESUMEN$UNIV_UF)
BD_RESUMEN$AVANCE_IGA=BD_RESUMEN$TOT_IGA_INAF/BD_RESUMEN$E_UNIV_IGAS
BD_RESUMEN$AVANCE_IGA_CSIG=BD_RESUMEN$IGAS_CSIG/BD_RESUMEN$E_UNIV_IGAS

#UNIENDO ESTA TABLA CON LA DE ERRORES
BD_RESUMEN=merge(BD_RESUMEN, SUB_BD_HISTORIAL,
                 by.x="SUB_SECT",
                 by.y="SUB_SECT",
                 all.x=T)



#
BD_RESUMEN$AVANCE_IGA=100*round(BD_RESUMEN$AVANCE_IGA,3)
BD_RESUMEN$AVANCE_IGA_CSIG=100*round(BD_RESUMEN$AVANCE_IGA_CSIG,3)
BD_RESUMEN=BD_RESUMEN[!is.na(BD_RESUMEN$TOT_IGA_INAF)==T,]
#BD_RESUMEN$SUB_SECT=as.factor(BD_RESUMEN$SUB_SECT)


#Caso especial
BD_RESUMEN$AVANCE_IGA[BD_RESUMEN$SUB_SECT=="CRES"]=100
BD_RESUMEN$AVANCE_IGA_CSIG[BD_RESUMEN$SUB_SECT=="CRES"]=100

#GRAFICANDO
BUBBLE_GRAPH=ggplot(BD_RESUMEN)+
  annotate("rect", xmin = 0, xmax = 50, ymin = 0, ymax = 60, 
           alpha = 0.1, fill = "firebrick") + 
  annotate("rect", xmin = 0, xmax = 100, ymin = 10, ymax = 60, 
           alpha = 0.1, fill = "firebrick") + 
  
  geom_point(aes(x=AVANCE_IGA_CSIG,y=PORC_CASOS_OBS_CSIG,size=TOT_IGA_INAF,color=SUB_SECT), alpha=0.8)+
  # scale_color_manual(values=c(OEFA.AZUL1,OEFA.TURQUEZA,OEFA.GRIS,OEFA.VERDE,OEFA.MOSTAZA,OEFA.JADE,OEFA.AZUL2))+
  scale_color_manual(values=c(PALETA.PRINCIPAL))+
  
  ggeasy::easy_all_text_size(size = 15)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
        plot.title = element_text(face = "bold"))+
  
  scale_x_continuous(limits = c(0, 102), breaks = seq(0,100,10), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0,100,10), expand = c(0, 0))+
  
  labs(x="IGAs aprobado por los coordinadores en INAF* (%)",
       y="\nIGA con observaciones (%)",
       title = paste("IGA con observaciones en la revisión de CSIG y aprobados por coordinador"),
       subtitle = paste("(Revisión de CSIG del ",
                        F_min,
                        " al ",
                        F_max,")\n",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Estimado\nNota1: Los casos óptimos son aquellos con una tasa de observaciones de menos del 10% y un llenado de más de 50% de los casos, delimitado por el Área de color rosado\nNota2: Para CRES, debido a que la mayoría de UF no cuenta con IGA, los que han registrado ya constituye el universo. Es por ello que se ha considerado un avance del 100%")+
  scale_size(range = c(0.5,50), name="Cantidad de IGA\nregistrados en INAF")+
  geom_point(aes(x=AVANCE_IGA_CSIG,y=PORC_CASOS_OBS_CSIG,size=1))+
  geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept=50, linetype="dashed", color = "red", size=1)+
  
  geom_text( x = 50, y = 60, angle = 90, label = paste("Umbral en llenado (50%)", sep=""), 
             hjust = 1,vjust = -0.5,size =5)+
  geom_text( x = 100, y = 10, angle = 0, label = paste("Umbral en observaciones (10%)", sep=""), 
             hjust = 1,vjust = -0.5,size =5)+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  
  ggeasy::easy_add_legend_title("Área")+
  ggeasy::easy_text_size(c("plot.title"),size = 22)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)


BUBBLE_GRAPH

ggsave("0.3.2) OBSERVACIONES-ANTERIOR.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


# write.csv(BD_RESUMEN,file="Tabla intermedia.csv", na="")



######################################################


# __________________________________________________________________________________________________________________________________________________________

## ######################################################### ###
##   3) AVANCE TOTAL: 1) Evolución de la cantidad de IGA's   ####
## ######################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF=as.data.frame(BD_INAF)
SUB_BD_INAF$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF=summaryBy(AUX ~ F_REG, FUN=fun1, data =as.data.frame(SUB_BD_INAF),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF$F_REG=as.Date(SUB_BD_INAF$F_REG)

#GR?FICO ACUMULADO
AVANCE_REG_DIA_TOT <- ggplot(SUB_BD_INAF,aes(F_REG, cumsum(AUX))) +
  geom_line(color=OEFA.AZUL1, size=1.5) +
  #stat_smooth(se=FALSE)+
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_y_continuous(breaks = seq(0,20000,2000))+
  theme_minimal()+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de IGA",
       title = "Evolución de la cantidad de IGA en INAF (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_DIA_TOT

#GUARDANDO EL GR?FICO
ggsave("1.1) Avance acumulado total.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)




################################################################


# __________________________________________________________________________________________________________________________________________________________

## ############################################################ ###
##   3) AVANCE TOTAL: 2) Evolución de la cantidad de archivos   ####
## ############################################################ ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF=as.data.frame(BD_INAF)
SUB_BD_INAF$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF=summaryBy(N_ARCH ~ F_REG, FUN=fun1, data =as.data.frame(SUB_BD_INAF),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF$F_REG=as.Date(SUB_BD_INAF$F_REG)

#GR?FICO ACUMULADO
AVANCE_REG_DIA_TOT <- ggplot(SUB_BD_INAF,aes(F_REG, cumsum(N_ARCH))) +
  geom_line(color=OEFA.AZUL1, size=1.5) +
  #stat_smooth(se=FALSE)+
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  theme_minimal()+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de archivos",
       title = "Evolución de la cantidad de archivos en INAF (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_DIA_TOT

#GUARDANDO EL GR?FICO
ggsave("1.2) Avance ARCHIVOS acumulado total.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)



###################################################################


# __________________________________________________________________________________________________________________________________________________________

## ###################################################### ###
##   3) AVANCE TOTAL: 3) Evolución de la cantidad de MB   ####
######################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF=as.data.frame(BD_INAF)
SUB_BD_INAF$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF=summaryBy(ARCH_SIZE_MB ~ F_REG, FUN=fun1, data =as.data.frame(SUB_BD_INAF),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF$F_REG=as.Date(SUB_BD_INAF$F_REG)

#GR?FICO ACUMULADO
AVANCE_REG_DIA_TOT <- ggplot(SUB_BD_INAF,aes(F_REG, cumsum(ARCH_SIZE_MB))) +
  geom_line(color=OEFA.AZUL1, size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  theme_minimal()+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de MB",
       title = "Evolución de la cantidad de MB en INAF (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_DIA_TOT

#GUARDANDO EL GR?FICO
ggsave("1.3) Avance MB acumulado total.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)




#############################################################


# __________________________________________________________________________________________________________________________________________________________

## ############################################################ ###
##   3) AVANCE TOTAL: 4) Evolución de IGAs total y validados    ####
## ############################################################ ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF_X=as.data.frame(subset(BD_INAF))
SUB_BD_INAF_X$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF_X=summaryBy(AUX ~ F_REG+ESTADO_AUX, FUN=fun1, data =as.data.frame(SUB_BD_INAF_X),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF_X$F_REG=as.Date(SUB_BD_INAF_X$F_REG)

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF_X = mutate(group_by(SUB_BD_INAF_X,ESTADO_AUX),"AUX2" = cumsum(AUX))


#GR?FICO ACUMULADO SOLO UF CRITICAS
AVANCE_REG_DIA_TOT_VAL <- ggplot(SUB_BD_INAF_X,aes(F_REG, AUX2, group = ESTADO_AUX, color = factor(ESTADO_AUX))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(OEFA.AZUL1, OEFA.JADE))+
    #stat_smooth(se=FALSE)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de registros",
       title = "Evolución de la cantidad de registros en INAF, por estado del proceso de validación*.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Los casos validados son aquellos que pasaron por la revisión de los coordinadores y de CSIG")+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(legend.position = "bottom",legend.title = element_blank())+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)


AVANCE_REG_DIA_TOT_VAL

#GUARDANDO EL GR?FICO
ggsave("1.4) Avance por proceso.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)




###################################################################


# __________________________________________________________________________________________________________________________________________________________

## ################################################################## ###
##   3) AVANCE TOTAL: 5) Evolución de archivos totales y validados    ####
## ################################################################## ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF_X=as.data.frame(subset(BD_INAF))
SUB_BD_INAF_X$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF_X=summaryBy(N_ARCH ~ F_REG+ESTADO_AUX, FUN=sum, data =as.data.frame(SUB_BD_INAF_X),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF_X$F_REG=as.Date(SUB_BD_INAF_X$F_REG)

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF_X = mutate(group_by(SUB_BD_INAF_X,ESTADO_AUX),"AUX2" = cumsum(N_ARCH))


#GR?FICO ACUMULADO SOLO UF CRITICAS
AVANCE_REG_DIA_TOT_VAL <- ggplot(SUB_BD_INAF_X,aes(F_REG, AUX2, group = ESTADO_AUX, color = factor(ESTADO_AUX))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(OEFA.AZUL1, OEFA.JADE))+
  #stat_smooth(se=FALSE)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de archivos",
       title = "Evolución de la cantidad de archivos en INAF, por estado del proceso de validación*.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Los casos validados son aquellos que pasaron por la revisión de los coordinadores y de CSIG")+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(legend.position = "bottom",legend.title = element_blank())+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_DIA_TOT_VAL

#GUARDANDO EL GR?FICO
ggsave("1.5) Avance por proceso.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)




#########################################################################


# __________________________________________________________________________________________________________________________________________________________

## ################################################### ###
##   3) AVANCE TOTAL: 6) Evolución de IGA de UF y UFC  ####
## ################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF_3=as.data.frame(BD_INAF_PRIOR)
SUB_BD_INAF_3$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF_3=summaryBy(AUX ~ F_REG+CASO_PRIOR, FUN=fun1, data =as.data.frame(SUB_BD_INAF_3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF_3$F_REG=as.Date(SUB_BD_INAF_3$F_REG)

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF_3 = mutate(group_by(SUB_BD_INAF_3,CASO_PRIOR),"AUX2" = cumsum(AUX))

#GR?FICO ACUMULADO
AVANCE_REG_DIA_TOT_Y_UFC <- ggplot(SUB_BD_INAF_3,aes(F_REG, AUX2, group = CASO_PRIOR, color = factor(CASO_PRIOR))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(OEFA.AZUL1,OEFA.TURQUEZA),labels=c("UF no priorizados","UF priorizados"),name = "")+
  #stat_smooth(se=FALSE)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de IGA",
       title = "Evolución de la cantidad de IGA en INAF (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)
  
AVANCE_REG_DIA_TOT_Y_UFC

#GUARDANDO EL GR?FICO
ggsave("1.6) Avance acumulado Totales y UFC.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)




##########################################################

# __________________________________________________________________________________________________________________________________________________________

## ########################################################## ###
##   3) AVANCE TOTAL: 7) Evolución de archivos de UF y UFC    ####
## ########################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF_3=as.data.frame(BD_INAF_PRIOR)
SUB_BD_INAF_3$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF_3=summaryBy(N_ARCH ~ F_REG+CASO_PRIOR, FUN=sum, data =as.data.frame(SUB_BD_INAF_3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF_3$F_REG=as.Date(SUB_BD_INAF_3$F_REG)

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF_3 = mutate(group_by(SUB_BD_INAF_3,CASO_PRIOR),"AUX2" = cumsum(N_ARCH))

#GR?FICO ACUMULADO
AVANCE_REG_DIA_TOT_Y_UFC <- ggplot(SUB_BD_INAF_3,aes(F_REG, AUX2, group = CASO_PRIOR, color = factor(CASO_PRIOR))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(OEFA.AZUL1,OEFA.TURQUEZA),labels=c("UF no priorizadas","UF priorizadas"),name = "")+
  #stat_smooth(se=FALSE)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de archivos",
       title = "Evolución de la cantidad de archivos en INAF (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_DIA_TOT_Y_UFC

#GUARDANDO EL GR?FICO
ggsave("1.7) Avance archivos acumulado Totales y UFC.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


# #CREANDO GIF
# AVANCE_REG_DIA_TOT_Y_UFC=AVANCE_REG_DIA_TOT_Y_UFC + transition_reveal(F_REG)
# animate(AVANCE_REG_DIA_TOT_Y_UFC, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("1.7) Avance archivos acumulado Totales y UFC.gif")


#################################################################


# __________________________________________________________________________________________________________________________________________________________

## ######################################################## ###
##   4) AVANCE POR AREA: 1) Evolución del registro de IGA   ####
## ######################################################## ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF1=as.data.frame(BD_INAF)
SUB_BD_INAF1$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF1=summaryBy(AUX ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF1),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF1$F_REG=as.Date(SUB_BD_INAF1$F_REG)

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF1 = mutate(group_by(SUB_BD_INAF1,SUB_SECT),"AUX2" = cumsum(AUX))
AVANCE_REG_DIA <- ggplot(SUB_BD_INAF1,aes(F_REG, AUX2, group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(PALETA.PRINCIPAL,PALETA.SECUNDARIA))+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de IGA",
       title = "Evolución de la cantidad de IGA en INAF, por coordinación (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        F_max,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_DIA

#GUARDANDO EL GR?FICO
ggsave("2.1) Evolución por area.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)



###############################################################


# __________________________________________________________________________________________________________________________________________________________

## ############################################################## ###
##   4) AVANCE POR AREA: 2) Evolución del registro de archivos    ####
## ############################################################## ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF1=as.data.frame(BD_INAF)
SUB_BD_INAF1$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF1=summaryBy(N_ARCH ~ F_REG+SUB_SECT, FUN=sum, data =as.data.frame(SUB_BD_INAF1),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF1$F_REG=as.Date(SUB_BD_INAF1$F_REG)

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF1 = mutate(group_by(SUB_BD_INAF1,SUB_SECT),"AUX2" = cumsum(N_ARCH))
AVANCE_REG_DIA <- ggplot(SUB_BD_INAF1,aes(F_REG, AUX2, group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(PALETA.PRINCIPAL,PALETA.SECUNDARIA))+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de archivos",
       title = "Evolución de la cantidad de archivos en INAF, por coordinación (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        F_max,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_DIA

#GUARDANDO EL GR?FICO
ggsave("2.2) Evolución archivos por area.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)



#####################################################################


# __________________________________________________________________________________________________________________________________________________________

## ######################################################### ###
##   4) AVANCE POR AREA: 3) Evolución de la cantidad de MB   ####
## ######################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF1=as.data.frame(BD_INAF)
SUB_BD_INAF1$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF1=summaryBy(ARCH_SIZE_MB ~ F_REG+SUB_SECT, FUN=sum, data =as.data.frame(SUB_BD_INAF1),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF1$F_REG=as.Date(SUB_BD_INAF1$F_REG)


#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF1 = mutate(group_by(SUB_BD_INAF1,SUB_SECT),"AUX2" = cumsum(ARCH_SIZE_MB))
AVANCE_REG_DIA <- ggplot(SUB_BD_INAF1,aes(F_REG, AUX2, group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(PALETA.PRINCIPAL,PALETA.SECUNDARIA))+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de MB",
       title = "Evolución de la cantidad de MB en INAF, por coordinación (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        F_max,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  
  ggeasy::easy_rotate_x_labels(angle = 90)+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c("axis.text.x", "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_DIA

#GUARDANDO EL GR?FICO
ggsave("2.3) Evolución MB por area.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)



################################################################



# __________________________________________________________________________________________________________________________________________________________

## ################################################################### ###
##   5) AVANCE POR AREA: 4) Tendencia del registro per c?pita de IGA   ####
# #################################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF5=as.data.frame(BD_INAF)
SUB_BD_INAF5$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF5=summaryBy(AUX ~ F_REG+SUB_SECT+REGISTRADOR, FUN=fun1, data =as.data.frame(SUB_BD_INAF5),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF5$N_REG=1
SUB_BD_INAF5=summaryBy(AUX+N_REG ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF5),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF5$REGISTROS_PERCAP=SUB_BD_INAF5$AUX/SUB_BD_INAF5$N_REG
SUB_BD_INAF5$F_REG=as.Date(SUB_BD_INAF5$F_REG)
SUB_BD_INAF5=subset(SUB_BD_INAF5, SUB_SECT!="Industria, Residuos Sólidos")
SUB_BD_INAF5=subset(SUB_BD_INAF5, SUB_SECT!="Hidrocarburos, Industria")



#GRAFICAR
AVANCE_REG_PERCAP_DIA <- ggplot(SUB_BD_INAF5,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("4 weeks"))+
  geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray60")+
  scale_color_manual(values=c(PALETA.PRINCIPAL,PALETA.SECUNDARIA))+
  #theme_minimal()+
  ylim(0,20)+
  labs(x="",
       y="Cantidad de IGA por registrador",
       title = "Evolución de la cantidad de IGA per cápita* en INAF, por coordinación**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        F_max,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realizó la migración de información del DRIVE\ngenerando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom",  legend.title = element_blank(),
        panel.grid.minor = element_blank())+  
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  
  ggeasy::easy_rotate_x_labels(angle=90)+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c( "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_PERCAP_DIA

ggsave("3.4) Tendencia IGAs per cápita.jpg",  width = 12, height = 7)


##########################################################################





# __________________________________________________________________________________________________________________________________________________________

# __________________________________________________________________________________________________________________________________________________________

## ######################################################################## ###
##   5) AVANCE POR AREA: 5) Tendencia del registro per c?pita de archivos   ####
## ######################################################################## ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF5X=as.data.frame(BD_INAF)
SUB_BD_INAF5X$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF5X=summaryBy(N_ARCH ~ F_REG+SUB_SECT+REGISTRADOR, FUN=sum, data =as.data.frame(SUB_BD_INAF5X),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF5X$N_REG=1
SUB_BD_INAF5X=summaryBy(N_ARCH+N_REG ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF5X),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF5X$REGISTROS_PERCAP=SUB_BD_INAF5X$N_ARCH/SUB_BD_INAF5X$N_REG
SUB_BD_INAF5X$F_REG=as.Date(SUB_BD_INAF5X$F_REG)
SUB_BD_INAF5X=subset(SUB_BD_INAF5X, SUB_SECT!="Industria, Residuos Sólidos")
SUB_BD_INAF5X=subset(SUB_BD_INAF5X, SUB_SECT!="Hidrocarburos, Industria")



#GRAFICAR
AVANCE_ARCH_PERCAP_DIA <- ggplot(SUB_BD_INAF5X,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("4 weeks"))+
  geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray60")+
  scale_color_manual(values=c(PALETA.PRINCIPAL,PALETA.SECUNDARIA))+
  ylim(0,40)+
  labs(x="",
       y="Cantidad de archivos por registrador",
       title = "Evolución de la cantidad de archivos per cápita* en INAF, por coordinación**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realizó la migración de información del DRIVE\ngenerando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank())+  
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  ggeasy::easy_rotate_x_labels(angle=90)+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c( "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

  AVANCE_ARCH_PERCAP_DIA

ggsave("3.5) Tendencia ARCHIVOS per cápita.jpg",  width = 12, height = 7)


###############################################################################


# __________________________________________________________________________________________________________________________________________________________

## ######################################################################## ###
##   5) AVANCE POR AREA: 5) Tendencia del registro per c?pita de archivos   ####
## ######################################################################## ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF5X=as.data.frame(BD_INAF)
SUB_BD_INAF5X$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF5X=summaryBy(ARCH_SIZE_MB ~ F_REG+SUB_SECT+REGISTRADOR, FUN=sum, data =as.data.frame(SUB_BD_INAF5X),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF5X$N_REG=1
SUB_BD_INAF5X=summaryBy(ARCH_SIZE_MB+N_REG ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF5X),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF5X$REGISTROS_PERCAP=SUB_BD_INAF5X$ARCH_SIZE_MB/SUB_BD_INAF5X$N_REG
SUB_BD_INAF5X$F_REG=as.Date(SUB_BD_INAF5X$F_REG)
SUB_BD_INAF5X=subset(SUB_BD_INAF5X, SUB_SECT!="Industria, Residuos Sólidos")
SUB_BD_INAF5X=subset(SUB_BD_INAF5X, SUB_SECT!="Hidrocarburos, Industria")


#GRAFICAR
AVANCE_ARCH_PERCAP_DIA <- ggplot(SUB_BD_INAF5X,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("4 weeks"))+
  geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray60")+
  scale_color_manual(values=c(PALETA.PRINCIPAL,PALETA.SECUNDARIA))+
  ylim(0,7500)+
  labs(x="",
       y="Cantidad de MB por registrador",
       title = "Evolución de la cantidad de MB per cápita* en INAF, por coordinación**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realizó la migración de información del DRIVE\ngenerando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank())+  
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  ggeasy::easy_rotate_x_labels(angle=90)+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c( "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_ARCH_PERCAP_DIA

ggsave("3.6) Tendencia MB per cápita.jpg",  width = 12, height = 7)


###############################################################################


# __________________________________________________________________________________________________________________________________________________________

## #################################################### ###
##   6) AVANCE POR AREA: 1) Cantidad de registradores   ####
## #################################################### ###

#GRAFICAR
AVANCE_REG_PERCAP_DIA <- ggplot(SUB_BD_INAF5,aes(F_REG, (N_REG), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_step(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("4 weeks"))+
  
  geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray60")+
  scale_color_manual(values=c(PALETA.PRINCIPAL,PALETA.SECUNDARIA))+
  #theme_minimal()+
  #ylim(0,15)+
  scale_y_continuous(breaks = seq(0, 15, by = 2))+
  labs(x="",
       y="Cantidad de registradores",
       title = "Evolución de la cantidad de registradores.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank())+  
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 2,
             labeller = label_value,
             strip.position = "bottom")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  ggeasy::easy_rotate_x_labels(angle=90)+
  
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_text_size(c("plot.title"),size = 25)+
  ggeasy::easy_text_size(c( "axis.text.y","legend.title","plot.subtitle"),size = 20)+
  ggeasy::easy_text_size(c("legend.text","axis.title"),size = 15)

AVANCE_REG_PERCAP_DIA

ggsave("4) Cantidad de registradores.jpg",  width = 12, height = 7)


###########################################################


# __________________________________________________________________________________________________________________________________________________________

## ############################## ###
##   7) SANKEY + TABLA: 1) CMIN   ####
## ############################## ###

#GENERANDO LA NUEVA BD
TAB_ESTADO_CMIN=as.data.frame(subset(BD_INAF,SUB_SECT=="CMIN"))
TAB_ESTADO_CMIN$AUX=1

#CREAR TABLA RESUMEN
TAB_ESTADO_CMIN=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(TAB_ESTADO_CMIN),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
TAB_ESTADO_CMIN$AUX2=paste(round(100*TAB_ESTADO_CMIN$AUX/sum(TAB_ESTADO_CMIN$AUX),digits = 1),"%",sep = "")
colnames(TAB_ESTADO_CMIN)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
TAB_ESTADO_CMIN$Etapa[TAB_ESTADO_CMIN$Etapa=="1) REGISTRADOR"]="Registrador"
TAB_ESTADO_CMIN$Etapa[TAB_ESTADO_CMIN$Etapa=="2) COORDINADOR"]="Coordinador"
TAB_ESTADO_CMIN$Etapa[TAB_ESTADO_CMIN$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
TAB_ESTADO_CMIN$Estado[TAB_ESTADO_CMIN$Estado=="PENDIENTE DE REVISION"]="Incompletos"
TAB_ESTADO_CMIN$Estado[TAB_ESTADO_CMIN$Estado=="EN REVISION"]="En revisión"
TAB_ESTADO_CMIN$Estado[TAB_ESTADO_CMIN$Estado=="OBSERVADO"]="Observado"
TAB_ESTADO_CMIN$Estado[TAB_ESTADO_CMIN$Estado=="VALIDADO"]="Validado"


#GENERANDO TABLA AUXILIAR
M <- data.frame(
  "Etapa" = factor(c("Registrador", "Coordinador", "Coordinador", "CSIG", "CSIG", "CSIG"),levels = c("Registrador","Coordinador","CSIG")), 
  "Estado" = factor(c("Incompletos", "En revisión", "Observado", "En revisión", "Observado", "Validado"), levels = c("Incompletos","En revisión","Observado", "Validado"))#, 
)#%>%

#JUNTANDO LA NUEVA TABLA Y ORDENANDOLA
M=merge(M, TAB_ESTADO_CMIN, by=c("Etapa","Estado"),all.x=T )
M <-M[ order(xtfrm(M$Etapa),xtfrm(M$Estado)), ]

#COMPLETAR VACIOS
M$Registros[is.na(M$Registros)==T]=0
M$`%del total`[is.na(M$`%del total`)==T]="0%"
rownames(M) <- 1:nrow(M)

#CAMBIAR NOMBRE Y ELIMINANDO TABLA AUXILIAR
TAB_ESTADO_CMIN=M
rm(M)



# ########################### #
# ##   GRÁFICO DE SANKEY   ## #
# ########################### #

#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       "Aprobado", #Tercer nivel (3)
                       "En revisión", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       "En revisión", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, TAB_ESTADO_CMIN[2,3]+TAB_ESTADO_CMIN[3,3]+TAB_ESTADO_CMIN[4,3]+TAB_ESTADO_CMIN[5,3]+TAB_ESTADO_CMIN[6,3], # Casos que tienen primera versi?n
  0, 2, TAB_ESTADO_CMIN[1,3], # Casos que no tienen primera versi?n.
  1, 3, TAB_ESTADO_CMIN[4,3]+TAB_ESTADO_CMIN[5,3]+TAB_ESTADO_CMIN[6,3], # Cantidad de casos archivados.
  1, 4, TAB_ESTADO_CMIN[2,3], # Casos con version final
  1, 5, TAB_ESTADO_CMIN[3,3], # Casos sin veri?n final
  3, 6, TAB_ESTADO_CMIN[4,3], # Casos con revisión de ?rika
  3, 7, TAB_ESTADO_CMIN[5,3], # Casos sin revisión de ?rika
  3, 8, TAB_ESTADO_CMIN[6,3]), # Casos con cargo (revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

my_color <- 'd3.scaleOrdinal() .domain([0, 1, 2, 3, 4, 5, 6, 7, 8]) .range(["#144AA7", "#144AA7","#FFB500", "#144AA7", "#0BC7E0", "#696A6A", "#144AA7", "#696A6A", "#144AA7"])'

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA.AZUL1,
                    units = "IGAs",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100,
                    colourScale=my_color)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.1) SANKEY-CMIN.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"



# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(TAB_ESTADO_CMIN, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("white", OEFA.MOSTAZA)))

TAB1

#GUARDANDO TABLA
html_header="
<head> 
<charset=\"UTF-8\"\r\n> 
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"> 
<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\">
</head>
<body>
"
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6.1) AVANCE-CMIN.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"



#####################################


# __________________________________________________________________________________________________________________________________________________________

## ############################## ###
##   7) SANKEY + TABLA: 2) CELE   ####
## ############################## ###

#GENERANDO LA NUEVA BD
TAB_ESTADO_CELE=as.data.frame(subset(BD_INAF,SUB_SECT=="CELE"))
TAB_ESTADO_CELE$AUX=1

#CREAR TABLA RESUMEN
TAB_ESTADO_CELE=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(TAB_ESTADO_CELE),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
TAB_ESTADO_CELE$AUX2=paste(round(100*TAB_ESTADO_CELE$AUX/sum(TAB_ESTADO_CELE$AUX),digits = 1),"%",sep = "")
colnames(TAB_ESTADO_CELE)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
TAB_ESTADO_CELE$Etapa[TAB_ESTADO_CELE$Etapa=="1) REGISTRADOR"]="Registrador"
TAB_ESTADO_CELE$Etapa[TAB_ESTADO_CELE$Etapa=="2) COORDINADOR"]="Coordinador"
TAB_ESTADO_CELE$Etapa[TAB_ESTADO_CELE$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
TAB_ESTADO_CELE$Estado[TAB_ESTADO_CELE$Estado=="PENDIENTE DE REVISION"]="Incompletos"
TAB_ESTADO_CELE$Estado[TAB_ESTADO_CELE$Estado=="EN REVISION"]="En revisión"
TAB_ESTADO_CELE$Estado[TAB_ESTADO_CELE$Estado=="OBSERVADO"]="Observado"
TAB_ESTADO_CELE$Estado[TAB_ESTADO_CELE$Estado=="VALIDADO"]="Validado"


#GENERANDO TABLA AUXILIAR
M <- data.frame(
  "Etapa" = factor(c("Registrador", "Coordinador", "Coordinador", "CSIG", "CSIG", "CSIG"),levels = c("Registrador","Coordinador","CSIG")), 
  "Estado" = factor(c("Incompletos", "En revisión", "Observado", "En revisión", "Observado", "Validado"), levels = c("Incompletos","En revisión","Observado", "Validado"))#, 
)#%>%

#JUNTANDO LA NUEVA TABLA Y ORDENANDOLA
M=merge(M, TAB_ESTADO_CELE, by=c("Etapa","Estado"),all.x=T )
M <-M[ order(xtfrm(M$Etapa),xtfrm(M$Estado)), ]

#COMPLETAR VACIOS
M$Registros[is.na(M$Registros)==T]=0
M$`%del total`[is.na(M$`%del total`)==T]="0%"
rownames(M) <- 1:nrow(M)

#CAMBIAR NOMBRE Y ELIMINANDO TABLA AUXILIAR
TAB_ESTADO_CELE=M
rm(M)


# ########################### #
# ##   GRÁFICO DE SANKEY   ## #
# ########################### #

#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       "Aprobado", #Tercer nivel (3)
                       "En revisión", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       "En revisión", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, TAB_ESTADO_CELE[2,3]+TAB_ESTADO_CELE[3,3]+TAB_ESTADO_CELE[4,3]+TAB_ESTADO_CELE[5,3]+TAB_ESTADO_CELE[6,3], # Casos que tienen primera versi?n
  0, 2, TAB_ESTADO_CELE[1,3], # Casos que no tienen primera versi?n.
  1, 3, TAB_ESTADO_CELE[4,3]+TAB_ESTADO_CELE[5,3]+TAB_ESTADO_CELE[6,3], # Cantidad de casos archivados.
  1, 4, TAB_ESTADO_CELE[2,3], # Casos con version final
  1, 5, TAB_ESTADO_CELE[3,3], # Casos sin veri?n final
  3, 6, TAB_ESTADO_CELE[4,3], # Casos con revisión de ?rika
  3, 7, TAB_ESTADO_CELE[5,3], # Casos sin revisión de ?rika
  3, 8, TAB_ESTADO_CELE[6,3]), # Casos con cargo (revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

my_color <- 'd3.scaleOrdinal() .domain([0, 1, 2, 3, 4, 5, 6, 7, 8]) .range(["#144AA7", "#144AA7","#FFB500", "#144AA7", "#0BC7E0", "#696A6A", "#144AA7", "#696A6A", "#144AA7"])'

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA.AZUL1,
                    units = "IGAs",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100,
                    colourScale=my_color)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.2) SANKEY-CELE.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"



# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(TAB_ESTADO_CELE, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("white", OEFA.TURQUEZA)))

TAB1

#GUARDANDO TABLA
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6.2) AVANCE-CELE.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"

#####################################


# __________________________________________________________________________________________________________________________________________________________

## ############################## ###
##   7) SANKEY + TABLA: 3) CHID   ####
## ############################## ###

#GENERANDO LA NUEVA BD
TAB_ESTADO_CHID=as.data.frame(subset(BD_INAF,SUB_SECT=="CHID"))
TAB_ESTADO_CHID$AUX=1

#CREAR TABLA RESUMEN
TAB_ESTADO_CHID=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(TAB_ESTADO_CHID),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
TAB_ESTADO_CHID$AUX2=paste(round(100*TAB_ESTADO_CHID$AUX/sum(TAB_ESTADO_CHID$AUX),digits = 1),"%",sep = "")
colnames(TAB_ESTADO_CHID)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
TAB_ESTADO_CHID$Etapa[TAB_ESTADO_CHID$Etapa=="1) REGISTRADOR"]="Registrador"
TAB_ESTADO_CHID$Etapa[TAB_ESTADO_CHID$Etapa=="2) COORDINADOR"]="Coordinador"
TAB_ESTADO_CHID$Etapa[TAB_ESTADO_CHID$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
TAB_ESTADO_CHID$Estado[TAB_ESTADO_CHID$Estado=="PENDIENTE DE REVISION"]="Incompletos"
TAB_ESTADO_CHID$Estado[TAB_ESTADO_CHID$Estado=="EN REVISION"]="En revisión"
TAB_ESTADO_CHID$Estado[TAB_ESTADO_CHID$Estado=="OBSERVADO"]="Observado"
TAB_ESTADO_CHID$Estado[TAB_ESTADO_CHID$Estado=="VALIDADO"]="Validado"


#GENERANDO TABLA AUXILIAR
M <- data.frame(
  "Etapa" = factor(c("Registrador", "Coordinador", "Coordinador", "CSIG", "CSIG", "CSIG"),levels = c("Registrador","Coordinador","CSIG")), 
  "Estado" = factor(c("Incompletos", "En revisión", "Observado", "En revisión", "Observado", "Validado"), levels = c("Incompletos","En revisión","Observado", "Validado"))#, 
)#%>%

#JUNTANDO LA NUEVA TABLA Y ORDENANDOLA
M=merge(M, TAB_ESTADO_CHID, by=c("Etapa","Estado"),all.x=T )
M <-M[ order(xtfrm(M$Etapa),xtfrm(M$Estado)), ]

#COMPLETAR VACIOS
M$Registros[is.na(M$Registros)==T]=0
M$`%del total`[is.na(M$`%del total`)==T]="0%"
rownames(M) <- 1:nrow(M)


#CAMBIAR NOMBRE Y ELIMINANDO TABLA AUXILIAR
TAB_ESTADO_CHID=M
rm(M)


# ########################### #
# ##   GR?FICO DE SANKEY   ## #
# ########################### #

#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       "Aprobado", #Tercer nivel (3)
                       "En revisión", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       "En revisión", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, TAB_ESTADO_CHID[2,3]+TAB_ESTADO_CHID[3,3]+TAB_ESTADO_CHID[4,3]+TAB_ESTADO_CHID[5,3]+TAB_ESTADO_CHID[6,3], # Casos que tienen primera versi?n
  0, 2, TAB_ESTADO_CHID[1,3], # Casos que no tienen primera versi?n.
  1, 3, TAB_ESTADO_CHID[4,3]+TAB_ESTADO_CHID[5,3]+TAB_ESTADO_CHID[6,3], # Cantidad de casos archivados.
  1, 4, TAB_ESTADO_CHID[2,3], # Casos con version final
  1, 5, TAB_ESTADO_CHID[3,3], # Casos sin veri?n final
  3, 6, TAB_ESTADO_CHID[4,3], # Casos con revisión de ?rika
  3, 7, TAB_ESTADO_CHID[5,3], # Casos sin revisión de ?rika
  3, 8, TAB_ESTADO_CHID[6,3]), # Casos con cargo (revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

my_color <- 'd3.scaleOrdinal() .domain([0, 1, 2, 3, 4, 5, 6, 7, 8]) .range(["#144AA7", "#144AA7","#FFB500", "#144AA7", "#0BC7E0", "#696A6A", "#144AA7", "#696A6A", "#144AA7"])'

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA.AZUL1,
                    units = "IGAs",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100,
                    colourScale=my_color)
#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.3) SANKEY-CHID.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"




# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(TAB_ESTADO_CHID, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("white", OEFA.AZUL2)))

TAB1

#GUARDANDO TABLA
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6.3) AVANCE-CHID.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"

#####################################


# __________________________________________________________________________________________________________________________________________________________

## ############################## ###
##   7) SANKEY + TABLA: 4) CIND   ####
## ############################## ###

#GENERANDO LA NUEVA BD
TAB_ESTADO_CIND=as.data.frame(subset(BD_INAF,SUB_SECT=="CIND"))
TAB_ESTADO_CIND$AUX=1

#CREAR TABLA RESUMEN
TAB_ESTADO_CIND=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(TAB_ESTADO_CIND),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
TAB_ESTADO_CIND$AUX2=paste(round(100*TAB_ESTADO_CIND$AUX/sum(TAB_ESTADO_CIND$AUX),digits = 1),"%",sep = "")
colnames(TAB_ESTADO_CIND)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
TAB_ESTADO_CIND$Etapa[TAB_ESTADO_CIND$Etapa=="1) REGISTRADOR"]="Registrador"
TAB_ESTADO_CIND$Etapa[TAB_ESTADO_CIND$Etapa=="2) COORDINADOR"]="Coordinador"
TAB_ESTADO_CIND$Etapa[TAB_ESTADO_CIND$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
TAB_ESTADO_CIND$Estado[TAB_ESTADO_CIND$Estado=="PENDIENTE DE REVISION"]="Incompletos"
TAB_ESTADO_CIND$Estado[TAB_ESTADO_CIND$Estado=="EN REVISION"]="En revisión"
TAB_ESTADO_CIND$Estado[TAB_ESTADO_CIND$Estado=="OBSERVADO"]="Observado"
TAB_ESTADO_CIND$Estado[TAB_ESTADO_CIND$Estado=="VALIDADO"]="Validado"


#GENERANDO TABLA AUXILIAR
M <- data.frame(
  "Etapa" = factor(c("Registrador", "Coordinador", "Coordinador", "CSIG", "CSIG", "CSIG"),levels = c("Registrador","Coordinador","CSIG")), 
  "Estado" = factor(c("Incompletos", "En revisión", "Observado", "En revisión", "Observado", "Validado"), levels = c("Incompletos","En revisión","Observado", "Validado"))#, 
)#%>%

#JUNTANDO LA NUEVA TABLA Y ORDENANDOLA
M=merge(M, TAB_ESTADO_CIND, by=c("Etapa","Estado"),all.x=T )
M <-M[ order(xtfrm(M$Etapa),xtfrm(M$Estado)), ]

#COMPLETAR VACIOS Y RENOMBRAR FILAS
M$Registros[is.na(M$Registros)==T]=0
M$`%del total`[is.na(M$`%del total`)==T]="0%"
rownames(M) <- 1:nrow(M)

#CAMBIAR NOMBRE Y ELIMINANDO TABLA AUXILIAR
TAB_ESTADO_CIND=M
rm(M)



# ########################### #
# ##   GR?FICO DE SANKEY   ## #
# ########################### #

#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       "Aprobado", #Tercer nivel (3)
                       "En revisión", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       "En revisión", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

links=as.data.frame(matrix(c(
  0, 1, TAB_ESTADO_CIND[2,3]+TAB_ESTADO_CIND[3,3]+TAB_ESTADO_CIND[4,3]+TAB_ESTADO_CIND[5,3]+TAB_ESTADO_CIND[6,3], # Casos que tienen primera versi?n
  0, 2, TAB_ESTADO_CIND[1,3], # Casos que no tienen primera versi?n.
  1, 3, TAB_ESTADO_CIND[4,3]+TAB_ESTADO_CIND[5,3]+TAB_ESTADO_CIND[6,3], # Cantidad de casos archivados.
  1, 4, TAB_ESTADO_CIND[2,3], # Casos con version final
  1, 5, TAB_ESTADO_CIND[3,3], # Casos sin veri?n final
  3, 6, TAB_ESTADO_CIND[4,3], # Casos con revisión de ?rika
  3, 7, TAB_ESTADO_CIND[5,3], # Casos sin revisión de ?rika
  3, 8, TAB_ESTADO_CIND[6,3]), # Casos con cargo (revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

rownames(TAB_ESTADO_CIND) <- 1:nrow(SUB_BD_INA9)

my_color <- 'd3.scaleOrdinal() .domain([0, 1, 2, 3, 4, 5, 6, 7, 8]) .range(["#144AA7", "#144AA7","#FFB500", "#144AA7", "#0BC7E0", "#696A6A", "#144AA7", "#696A6A", "#144AA7"])'

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA.AZUL1,
                    units = "IGAs",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100,
                    colourScale=my_color)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.4) SANKEY-CIND.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"




# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(TAB_ESTADO_CIND, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("white", OEFA.VERDE)))

TAB1

#GUARDANDO TABLA
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6.4) AVANCE-CIND.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"

#####################################


# __________________________________________________________________________________________________________________________________________________________

## ############################## ###
##   7) SANKEY + TABLA: 5) CPES   ####
## ############################## ###

#GENERANDO LA NUEVA BD
TAB_ESTADO_CPES=as.data.frame(subset(BD_INAF,SUB_SECT=="CPES"))
TAB_ESTADO_CPES$AUX=1

#CREAR TABLA RESUMEN
TAB_ESTADO_CPES=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(TAB_ESTADO_CPES),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
TAB_ESTADO_CPES$AUX2=paste(round(100*TAB_ESTADO_CPES$AUX/sum(TAB_ESTADO_CPES$AUX),digits = 1),"%",sep = "")
colnames(TAB_ESTADO_CPES)=c("Etapa","Estado","Registros","%del total")


#Mejorando un poco el contenido
TAB_ESTADO_CPES$Etapa[TAB_ESTADO_CPES$Etapa=="1) REGISTRADOR"]="Registrador"
TAB_ESTADO_CPES$Etapa[TAB_ESTADO_CPES$Etapa=="2) COORDINADOR"]="Coordinador"
TAB_ESTADO_CPES$Etapa[TAB_ESTADO_CPES$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
TAB_ESTADO_CPES$Estado[TAB_ESTADO_CPES$Estado=="PENDIENTE DE REVISION"]="Incompletos"
TAB_ESTADO_CPES$Estado[TAB_ESTADO_CPES$Estado=="EN REVISION"]="En revisión"
TAB_ESTADO_CPES$Estado[TAB_ESTADO_CPES$Estado=="OBSERVADO"]="Observado"
TAB_ESTADO_CPES$Estado[TAB_ESTADO_CPES$Estado=="VALIDADO"]="Validado"



#GENERANDO TABLA AUXILIAR
M <- data.frame(
  "Etapa" = factor(c("Registrador", "Coordinador", "Coordinador", "CSIG", "CSIG", "CSIG"),levels = c("Registrador","Coordinador","CSIG")), 
  "Estado" = factor(c("Incompletos", "En revisión", "Observado", "En revisión", "Observado", "Validado"), levels = c("Incompletos","En revisión","Observado", "Validado"))#, 
)#%>%

#JUNTANDO LA NUEVA TABLA Y ORDENANDOLA
M=merge(M, TAB_ESTADO_CPES, by=c("Etapa","Estado"),all.x=T )
M <-M[ order(xtfrm(M$Etapa),xtfrm(M$Estado)), ]

#COMPLETAR VACIOS Y RENOMBRAR FILAS
M$Registros[is.na(M$Registros)==T]=0
M$`%del total`[is.na(M$`%del total`)==T]="0%"
rownames(M) <- 1:nrow(M)

#CAMBIAR NOMBRE Y ELIMINANDO TABLA AUXILIAR
TAB_ESTADO_CPES=M
rm(M)



# ########################### #
# ##   GRÁFICO DE SANKEY   ## #
# ########################### #

#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       
                       "Aprobado", #Tercer nivel (3)
                       "En revisión", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       
                       "En revisión", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, TAB_ESTADO_CPES[2,3]+TAB_ESTADO_CPES[3,3]+TAB_ESTADO_CPES[4,3]+TAB_ESTADO_CPES[5,3]+TAB_ESTADO_CPES[6,3], # Casos que tienen primera versi?n
  0, 2, TAB_ESTADO_CPES[1,3], # Casos que no tienen primera versi?n.
  1, 3, TAB_ESTADO_CPES[4,3]+TAB_ESTADO_CPES[5,3]+TAB_ESTADO_CPES[6,3], # Cantidad de casos archivados.
  1, 4, TAB_ESTADO_CPES[2,3], # Casos con version final
  1, 5, TAB_ESTADO_CPES[3,3], # Casos sin veri?n final
  3, 6, TAB_ESTADO_CPES[4,3], # Casos con revisión de ?rika
  3, 7, TAB_ESTADO_CPES[5,3], # Casos sin revisión de ?rika
  3, 8, TAB_ESTADO_CPES[6,3]), # Casos con cargo (revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

my_color <- 'd3.scaleOrdinal() .domain([0, 1, 2, 3, 4, 5, 6, 7, 8]) .range(["#144AA7", "#144AA7","#FFB500", "#144AA7", "#0BC7E0", "#696A6A", "#144AA7", "#696A6A", "#144AA7"])'

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA.AZUL1,
                    units = "IGAs",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100,
                    colourScale=my_color)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.5) SANKEY-CPES.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"




# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(TAB_ESTADO_CPES, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("white", OEFA.JADE)))

TAB1

#GUARDANDO TABLA
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6.5) AVANCE-CPES.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"

#####################################


# __________________________________________________________________________________________________________________________________________________________

## ############################## ###
##   7) SANKEY + TABLA: 6) CRES   ####
## ############################## ###

#GENERANDO LA NUEVA BD
TAB_ESTADO_CRES=as.data.frame(subset(BD_INAF,SUB_SECT=="CRES"))
TAB_ESTADO_CRES$AUX=1

#CREAR TABLA RESUMEN
TAB_ESTADO_CRES=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(TAB_ESTADO_CRES),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
TAB_ESTADO_CRES$AUX2=paste(round(100*TAB_ESTADO_CRES$AUX/sum(TAB_ESTADO_CRES$AUX),digits = 1),"%",sep = "")
colnames(TAB_ESTADO_CRES)=c("Etapa","Estado","Registros","%del total")


#Mejorando un poco el contenido
TAB_ESTADO_CRES$Etapa[TAB_ESTADO_CRES$Etapa=="1) REGISTRADOR"]="Registrador"
TAB_ESTADO_CRES$Etapa[TAB_ESTADO_CRES$Etapa=="2) COORDINADOR"]="Coordinador"
TAB_ESTADO_CRES$Etapa[TAB_ESTADO_CRES$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
TAB_ESTADO_CRES$Estado[TAB_ESTADO_CRES$Estado=="PENDIENTE DE REVISION"]="Incompletos"
TAB_ESTADO_CRES$Estado[TAB_ESTADO_CRES$Estado=="EN REVISION"]="En revisión"
TAB_ESTADO_CRES$Estado[TAB_ESTADO_CRES$Estado=="OBSERVADO"]="Observado"
TAB_ESTADO_CRES$Estado[TAB_ESTADO_CRES$Estado=="VALIDADO"]="Validado"



#GENERANDO TABLA AUXILIAR
M <- data.frame(
  "Etapa" = factor(c("Registrador", "Coordinador", "Coordinador", "CSIG", "CSIG", "CSIG"),levels = c("Registrador","Coordinador","CSIG")), 
  "Estado" = factor(c("Incompletos", "En revisión", "Observado", "En revisión", "Observado", "Validado"), levels = c("Incompletos","En revisión","Observado", "Validado"))#, 
)#%>%

#JUNTANDO LA NUEVA TABLA Y ORDENANDOLA
M=merge(M, TAB_ESTADO_CRES, by=c("Etapa","Estado"),all.x=T )
M <-M[ order(xtfrm(M$Etapa),xtfrm(M$Estado)), ]

#COMPLETAR VACIOS Y RENOMBRAR FILAS
M$Registros[is.na(M$Registros)==T]=0
M$`%del total`[is.na(M$`%del total`)==T]="0%"
rownames(M) <- 1:nrow(M)

#CAMBIAR NOMBRE Y ELIMINANDO TABLA AUXILIAR
TAB_ESTADO_CRES=M
rm(M)




# ########################### #
# ##   GR?FICO DE SANKEY   ## #
# ########################### #

#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       "Aprobado", #Tercer nivel (3)
                       "En revisión", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       "En revisión", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)


# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, TAB_ESTADO_CRES[2,3]+TAB_ESTADO_CRES[3,3]+TAB_ESTADO_CRES[4,3]+TAB_ESTADO_CRES[5,3]+TAB_ESTADO_CRES[6,3], # Casos que tienen primera versi?n
  0, 2, TAB_ESTADO_CRES[1,3], # Casos que no tienen primera versi?n.
  1, 3, TAB_ESTADO_CRES[4,3]+TAB_ESTADO_CRES[5,3]+TAB_ESTADO_CRES[6,3], # Cantidad de casos archivados.
  1, 4, TAB_ESTADO_CRES[2,3], # Casos con version final
  1, 5, TAB_ESTADO_CRES[3,3], # LLENADOS APROBADOS 
  3, 6, TAB_ESTADO_CRES[4,3], # APROBADOS EN REVISION
  3, 7, TAB_ESTADO_CRES[5,3], # APROBADOS VALIDADOS
  3, 8, TAB_ESTADO_CRES[6,3]), # APROBADOS  OBSERVADOS
  byrow = T, ncol = 3))




names(links) = c("source", "target", "value")

my_color <- 'd3.scaleOrdinal() .domain([0, 1, 2, 3, 4, 5, 6, 7, 8]) .range(["#144AA7", "#144AA7","#FFB500", "#144AA7", "#0BC7E0", "#696A6A", "#144AA7", "#696A6A", "#144AA7"])'

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA.AZUL1,
                    units = "IGAs",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100,
                    colourScale=my_color)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.6) SANKEY-CRES.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"



# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(TAB_ESTADO_CRES, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("white", OEFA.GRIS)))

TAB1

#GUARDANDO TABLA
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6.6) AVANCE-CRES.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"

#####################################


# __________________________________________________________________________________________________________________________________________________________

## ############################## ###
##   7) SANKEY + TABLA: 7) CAGR   ####
## ############################## ###

#GENERANDO LA NUEVA BD
TAB_ESTADO_CAGR=as.data.frame(subset(BD_INAF,SUB_SECT=="CAGR"))
TAB_ESTADO_CAGR$AUX=1

#CREAR TABLA RESUMEN
TAB_ESTADO_CAGR=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(TAB_ESTADO_CAGR),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
TAB_ESTADO_CAGR$AUX2=paste(round(100*TAB_ESTADO_CAGR$AUX/sum(TAB_ESTADO_CAGR$AUX),digits = 1),"%",sep = "")
colnames(TAB_ESTADO_CAGR)=c("Etapa","Estado","Registros","%del total")


#Mejorando un poco el contenido
TAB_ESTADO_CAGR$Etapa[TAB_ESTADO_CAGR$Etapa=="1) REGISTRADOR"]="Registrador"
TAB_ESTADO_CAGR$Etapa[TAB_ESTADO_CAGR$Etapa=="2) COORDINADOR"]="Coordinador"
TAB_ESTADO_CAGR$Etapa[TAB_ESTADO_CAGR$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
TAB_ESTADO_CAGR$Estado[TAB_ESTADO_CAGR$Estado=="PENDIENTE DE REVISION"]="Incompletos"
TAB_ESTADO_CAGR$Estado[TAB_ESTADO_CAGR$Estado=="EN REVISION"]="En revisión"
TAB_ESTADO_CAGR$Estado[TAB_ESTADO_CAGR$Estado=="OBSERVADO"]="Observado"
TAB_ESTADO_CAGR$Estado[TAB_ESTADO_CAGR$Estado=="VALIDADO"]="Validado"



#GENERANDO TABLA AUXILIAR
M <- data.frame(
  "Etapa" = factor(c("Registrador", "Coordinador", "Coordinador", "CSIG", "CSIG", "CSIG"),levels = c("Registrador","Coordinador","CSIG")), 
  "Estado" = factor(c("Incompletos", "En revisión", "Observado", "En revisión", "Observado", "Validado"), levels = c("Incompletos","En revisión","Observado", "Validado"))#, 
)#%>%

#JUNTANDO LA NUEVA TABLA Y ORDENANDOLA
M=merge(M, TAB_ESTADO_CAGR, by=c("Etapa","Estado"),all.x=T )
M <-M[ order(xtfrm(M$Etapa),xtfrm(M$Estado)), ]

#COMPLETAR VACIOS Y RENOMBRAR FILAS
M$Registros[is.na(M$Registros)==T]=0
M$`%del total`[is.na(M$`%del total`)==T]="0%"
rownames(M) <- 1:nrow(M)

#CAMBIAR NOMBRE Y ELIMINANDO TABLA AUXILIAR
TAB_ESTADO_CAGR=M
rm(M)




# ########################### #
# ##   GR?FICO DE SANKEY   ## #
# ########################### #

#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       "Aprobado", #Tercer nivel (3)
                       "En revisión", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       "En revisión", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)


# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, TAB_ESTADO_CAGR[2,3]+TAB_ESTADO_CAGR[3,3]+TAB_ESTADO_CAGR[4,3]+TAB_ESTADO_CAGR[5,3]+TAB_ESTADO_CAGR[6,3], # Casos que tienen primera versi?n
  0, 2, TAB_ESTADO_CAGR[1,3], # Casos que no tienen primera versi?n.
  1, 3, TAB_ESTADO_CAGR[4,3]+TAB_ESTADO_CAGR[5,3]+TAB_ESTADO_CAGR[6,3], # Cantidad de casos archivados.
  1, 4, TAB_ESTADO_CAGR[2,3], # Casos con version final
  1, 5, TAB_ESTADO_CAGR[3,3], # LLENADOS APROBADOS 
  3, 6, TAB_ESTADO_CAGR[4,3], # APROBADOS EN REVISION
  3, 7, TAB_ESTADO_CAGR[5,3], # APROBADOS VALIDADOS
  3, 8, TAB_ESTADO_CAGR[6,3]), # APROBADOS  OBSERVADOS
  byrow = T, ncol = 3))




names(links) = c("source", "target", "value")

my_color <- 'd3.scaleOrdinal() .domain([0, 1, 2, 3, 4, 5, 6, 7, 8]) .range(["#144AA7", "#144AA7","#FFB500", "#144AA7", "#0BC7E0", "#696A6A", "#144AA7", "#696A6A", "#144AA7"])'

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA.AZUL1,
                    units = "IGAs",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100,
                    colourScale=my_color)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.7) SANKEY-CAGR.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"



# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(TAB_ESTADO_CAGR, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("white", OEFA.NARANJA)))

TAB1

#GUARDANDO TABLA
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6.7) AVANCE-AGR.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"

#####################################


# __________________________________________________________________________________________________________________________________________________________

## ################################# ###
##   7) SANKEY + TABLA: 0) General   ####
## ################################# ###

#GENERANDO LA NUEVA BD
TAB_ESTADO_TOT=as.data.frame(BD_INAF)
TAB_ESTADO_TOT$AUX=1

#CREAR TABLA RESUMEN
TAB_ESTADO_TOT=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(TAB_ESTADO_TOT),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
TAB_ESTADO_TOT$AUX2=paste(round(100*TAB_ESTADO_TOT$AUX/sum(TAB_ESTADO_TOT$AUX),digits = 1),"%",sep = "")
colnames(TAB_ESTADO_TOT)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
TAB_ESTADO_TOT$Etapa[TAB_ESTADO_TOT$Etapa=="1) REGISTRADOR"]="Registrador"
TAB_ESTADO_TOT$Etapa[TAB_ESTADO_TOT$Etapa=="2) COORDINADOR"]="Coordinador"
TAB_ESTADO_TOT$Etapa[TAB_ESTADO_TOT$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
TAB_ESTADO_TOT$Estado[TAB_ESTADO_TOT$Estado=="PENDIENTE DE REVISION"]="Incompletos"
TAB_ESTADO_TOT$Estado[TAB_ESTADO_TOT$Estado=="EN REVISION"]="En revisión"
TAB_ESTADO_TOT$Estado[TAB_ESTADO_TOT$Estado=="OBSERVADO"]="Observado"
TAB_ESTADO_TOT$Estado[TAB_ESTADO_TOT$Estado=="VALIDADO"]="Validado"


#GENERANDO TABLA AUXILIAR
M <- data.frame(
  "Etapa" = factor(c("Registrador", "Coordinador", "Coordinador", "CSIG", "CSIG", "CSIG"),levels = c("Registrador","Coordinador","CSIG")), 
  "Estado" = factor(c("Incompletos", "En revisión", "Observado", "En revisión", "Observado", "Validado"), levels = c("Incompletos","En revisión","Observado", "Validado"))#, 
)#%>%

#JUNTANDO LA NUEVA TABLA Y ORDENANDOLA
M=merge(M, TAB_ESTADO_TOT, by=c("Etapa","Estado"),all.x=T )
M <-M[ order(xtfrm(M$Etapa),xtfrm(M$Estado)), ]

#COMPLETAR VACIOS
M$Registros[is.na(M$Registros)==T]=0
M$`%del total`[is.na(M$`%del total`)==T]="0%"
rownames(M) <- 1:nrow(M)


#CAMBIAR NOMBRE Y ELIMINANDO TABLA AUXILIAR
TAB_ESTADO_TOT=M
rm(M)


# ########################### #
# ##   GRÁFICO DE SANKEY   ## #
# ########################### #

#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       "Aprobado", #Tercer nivel (3)
                       "En revisión", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       "En revisión", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, TAB_ESTADO_TOT[2,3]+TAB_ESTADO_TOT[3,3]+TAB_ESTADO_TOT[4,3]+TAB_ESTADO_TOT[5,3]+TAB_ESTADO_TOT[6,3], # Casos que tienen primera versi?n
  0, 2, TAB_ESTADO_TOT[1,3], # Casos que no tienen primera versi?n.
  1, 3, TAB_ESTADO_TOT[4,3]+TAB_ESTADO_TOT[5,3]+TAB_ESTADO_TOT[6,3], # Cantidad de casos archivados.
  1, 4, TAB_ESTADO_TOT[2,3], # Casos con version final
  1, 5, TAB_ESTADO_TOT[3,3], # Casos sin veri?n final
  3, 6, TAB_ESTADO_TOT[4,3], # Casos con revisión de ?rika
  3, 7, TAB_ESTADO_TOT[5,3], # Casos sin revisión de ?rika
  3, 8, TAB_ESTADO_TOT[6,3]), # Casos con cargo (revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

my_color <- 'd3.scaleOrdinal() .domain([0, 1, 2, 3, 4, 5, 6, 7, 8]) .range(["#144AA7", "#144AA7","#FFB500", "#144AA7", "#0BC7E0", "#696A6A", "#144AA7", "#696A6A", "#144AA7"])'

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA.AZUL1,
                    units = "IGAs",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100,
                    colourScale=my_color)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5) SANKEY-TOTAL.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"



# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #

TAB1=
  
  formattable(TAB_ESTADO_TOT, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

TAB1

#GRABANDO
html_header="
<head> 
<charset=\"UTF-8\"\r\n> 
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"> 
<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\">
</head>
<body>
"
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6) TABLA-TOTAL.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"



# ############################################# #
# ##   GRAFICANDO TABLA GENERAL COMPARATIVA  ## #
# ############################################# #


# M= as.data.frame(cbind(TAB_ESTADO_CMIN[,c(1:3)],TAB_ESTADO_CELE[,c(3)],TAB_ESTADO_CHID[,c(3)],TAB_ESTADO_CIND[,c(3)],TAB_ESTADO_CPES[,c(3)],TAB_ESTADO_CRES[,c(3)],TAB_ESTADO_CAGR[,c(3)]))
M= as.data.frame(cbind(TAB_ESTADO_CMIN[,c(1:3)],subset(TAB_ESTADO_CELE,select=c(Registros)),subset(TAB_ESTADO_CHID,select=c(Registros)),subset(TAB_ESTADO_CIND,select=c(Registros)),subset(TAB_ESTADO_CPES,select=c(Registros)),subset(TAB_ESTADO_CRES,select=c(Registros)),subset(TAB_ESTADO_CAGR,select=c(Registros)),subset(TAB_ESTADO_TOT,select=c(Registros,`%del total`))))
colnames(M)[(3:11)]<-as.list(c("Registros CMIN","Registros CELE","Registros CHID","Registros CIND","Registros CPES","Registros CRES","Registros CAGR","Registros totales","% del total"))
rownames(M) <- 1:nrow(M)

#GRAFICANDO TABLA
TAB_TOT=
  
  formattable(M, align=c("l","l","c","c","c","c","c","c","c","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   `Registros CMIN`=color_tile("white", OEFA.ROJO),
                   `Registros CELE`=color_tile("white", OEFA.ROJO),
                   `Registros CHID`=color_tile("white", OEFA.ROJO),
                   `Registros CIND`=color_tile("white", OEFA.ROJO),
                   `Registros CPES`=color_tile("white", OEFA.ROJO),
                   `Registros CRES`=color_tile("white", OEFA.ROJO),
                   `Registros CAGR`=color_tile("white", OEFA.ROJO),
                   `Registros totales`=color_tile("white", OEFA.AZUL2)))

TAB_TOT

#GRABANDO
html_header="
<head> 
<charset=\"UTF-8\"\r\n> 
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"> 
<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\">
</head>
<body>
"
write(paste(html_header, TAB_TOT, sep=""), "./TAB_TOT.html")
webshot("TAB_TOT.html", "6.0) TABLA-TOTAL_GRL.jpeg",vwidth = 800,vheight = 300,zoom = 8) #Guarda el archivo 
unlink("TAB_TOT.html") #Elimina el "temporal"


########################################


# __________________________________________________________________________________________________________________________________________________________

# ############### ###
#     Guardado    ####
# ############### ###

#Extraer el "data frame"
# TEMP <- as.data.frame(BD_INAF_CRIT)

#EXPORTANDO EN FORMATO R
#save(TEMP, file="BD_INAF.rdata")


#EXPORTANDO ARCHIVO DE TRABAJO
# write.csv(TEMP,file=archivo, na="")

#EXPORTANDO BACKUP
# write.csv(TEMP,file=paste(backup,archivo,sep = ""), na="")


#####################























