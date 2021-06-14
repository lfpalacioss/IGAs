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

###############################


# __________________________________________________________________________________________________________________________________________________________

## ################################## ###
##    II) PROCESAMIENTO DE PRE-DATA   ####
## ################################## ###

source(paste('C:/Users/',USER,'/Google Drive/4) R/1) Scripts/1) OEFA/3) IGAS/PROC_DATA_IGA.R',sep = ""))

#########################################


# __________________________________________________________________________________________________________________________________________________________

## ######################## ###
##    III) TRABAJAR DATA    ####
## ######################## ###

# FUENTE
FUENTE_IGAS<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRafB2LaFQ0YIPfK5j1ofCaBz4oRz3-1Jqt3c9UST-WnJ7j2D0tbEMsPhTDBd5qhlk6gnnCXlw_CQDy/pub?output=xlsx"
# SETEANDO LA DIRECCION DEL DRIVE
archivo='IGAS_INAF.csv'
# GENERANDO UN TEMPORAL
tp1<-tempfile()
# DESCARGAR
download.file(FUENTE_IGAS,tp1,mode ="wb")
# SELECCIONAR LA PESTAÑA DEL TEMPORAL
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
BD_INAF$SUB_SECT_0[BD_INAF$SUB_SECT=="CELE"|BD_INAF$SUB_SECT=="CMIN"|BD_INAF$SUB_SECT=="CHID"]="DSEM"
BD_INAF$SUB_SECT_0[BD_INAF$SUB_SECT=="CIND"|BD_INAF$SUB_SECT=="CAGR"|BD_INAF$SUB_SECT=="CPES"]="DSAP"
BD_INAF$SUB_SECT_0[BD_INAF$SUB_SECT=="CRES"]="DSIS"
BD_INAF$ETAPA[BD_INAF$ESTADO=="Pendiente de revisión"]="1) REGISTRADOR"
BD_INAF$ETAPA[BD_INAF$ESTADO=="En revisión [Coordinador]"|BD_INAF$ESTADO=="Observado [Coordinador]"]="2) COORDINADOR"
BD_INAF$ETAPA[BD_INAF$ESTADO=="En revisión [Especialista CSIG]"|BD_INAF$ESTADO=="Observado [Especialista CSIG]"|BD_INAF$ESTADO=="Validado [Especialista CSIG]"]="3) ESPECIALISTA CSIG"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Pendiente de revisión"]="PENDIENTE DE REVISION"
BD_INAF$ESTADO[BD_INAF$ESTADO=="En revisión [Coordinador]"]="EN REVISION"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Observado [Coordinador]"]="OBSERVADO"
BD_INAF$ESTADO[BD_INAF$ESTADO=="En revisión [Especialista CSIG]"]="EN REVISION"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Observado [Especialista CSIG]"]="OBSERVADO"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Validado [Especialista CSIG]"]="VALIDADO"

# __________________________________________________________________________________________________________________________________________________________

# #################### #
# ##     BD UFC     ## #
# #################### #

#FUENTE
FUENTE_UFC<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQeH0CEF06jjKnKpeu3mylLMi0PA5OKjubQzxKKb8ZCODBKHnyXfnExFVaywIMl5woO_HakkNQIucSI/pub?output=xlsx"

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR
download.file(FUENTE_UFC,tp1,mode ="wb")

#SELECCIONAR LA PESTAÑA DEL TEMPORAL
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

# ################################# #
# ##    II) HISTORIAL DE IGAS    ## #
# ################################# #

#FUENTE
FUENTE_HISTORIAL<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSBErlOvYBET_39N9DVvanERIeuH1FlC-xDubfnvcvKWOOTpFEFSYR_HnQx9faFZCu_J6T-CgtwjZ6W/pub?output=xlsx"

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR
download.file(FUENTE_HISTORIAL,tp1,mode ="wb")

#SELECCIONAR LA PESTAÑA DEL TEMPORAL
BD_HISTORIAL<-read_xlsx(path = tp1, sheet = "BD_HISTORIAL")
BD_HISTORIAL_DICC<-read_xlsx(path = tp1, sheet = "DIC_DATOS")

# __________________________________________________________________________________________________________________________________________________________

# ############################ #
# ##  TRABAJAR ENCABEZADOS  ## #
# ############################ #

BD_HISTORIAL_DICC= BD_HISTORIAL_DICC[BD_HISTORIAL_DICC$INFO_IRRELEV!=1, ] #Se eliminan las variables irrelevantes
BD_HISTORIAL_DICC=BD_HISTORIAL_DICC[,c(1,2)] #solo me quedo con los campos en BD y el CODIGO CSEP
BD_HISTORIAL_DICC=as.list(BD_HISTORIAL_DICC) #Defino las cabeceras
BD_HISTORIAL=subset(BD_HISTORIAL, select = BD_HISTORIAL_DICC$ENCABEZADOS) #Limito los campos con los que se trabajará
colnames(BD_HISTORIAL)=BD_HISTORIAL_DICC$COD_ENCAB #Renombro las cabeceras según CODIGO CSEP
rm(BD_HISTORIAL_DICC) #Eliminando DIC

#TRABAJANDO DATA DE HORAS
BD_HISTORIAL$HORA=format(BD_HISTORIAL$F_ESTADO_HORA, "%Y/%m/%d - %H/%M")



BD_HISTORIAL$HORA = chron(dates=F_ESTADO_HORA,times=dtparts[,2], format=c('y-m-d','h:m:s'))


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
#SELECCIONAR LA PESTAÑA DEL TEMPORAL
BD_ARCH<-read_xlsx(path = tp1, sheet = "BD_ARCH")
BD_DIC_ARCH<-read_xlsx(path = tp1, sheet = "DIC_DATOS")

# __________________________________________________________________________________________________________________________________________________________

# ################################# #
# ##     TRABAJAR ENCABEZADOS    ## #
# ################################# #

BD_DIC_ARCH= BD_DIC_ARCH[BD_DIC_ARCH$INFO_IRRELEV!=1, ] #Se eliminan las variables irrelevantes
BD_DIC_ARCH=BD_DIC_ARCH[,c(1,2)] #solo me quedo con los campos en BD y el CODIGO CSEP
BD_DIC_ARCH=as.list(BD_DIC_ARCH) #Defino las cabeceras
BD_ARCH=subset(BD_ARCH, select = BD_DIC_ARCH$ENCABEZADOS) #Limito los campos con los que se trabajará
colnames(BD_ARCH)=BD_DIC_ARCH$COD_ENCAB #Renombro las cabeceras según CODIGO CSEP
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
# ##    UNIENDO BD IGAS+HISTORIAL    ## #
# ##################################### #

#COMBINAR CON OTRAS COLUMNAS DE TABLA TAREAS
BD_HISTORIAL=merge(BD_HISTORIAL,subset(BD_INAF, select = c("Código","SUB_SECT_0","SUB_SECT","REGISTRADOR")),
                   by.x="COD_INST",
                   by.y="Código",
                   all.x=T)



# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

# ################################ #
# ##    IV) ARCHIVOS DE IGAS    ## #
# ################################ #

#FUENTE
FUENTE_ARCH<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRdvKvyxqdRnyvkFngDf9SGvBZVZt9aHygcq6JWgMUVeDxJE6QqsgTCwsSdhq0xFQvMSLYOT1-XifxV/pub?output=xlsx"
#GENERANDO UN TEMPORAL
tp1<-tempfile()
#DESCARGAR
download.file(FUENTE_ARCH,tp1,mode ="wb")
#SELECCIONAR LA PESTAÑA DEL TEMPORAL
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


###############################


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
F_MAX="2020-08-21"

##########################################


# __________________________________________________________________________________________________________________________________________________________
# __________________________________________________________________________________________________________________________________________________________

## ############################################################ ###
##    1) AVANCE TOTAL: 1) Evolución de la cantidad de IGA's     ####
## ############################################################ ###

# GENERANDO LA NUEVA BD
SUB_BD_INAF=as.data.frame(BD_INAF_CRIT)
SUB_BD_INAF$AUX=1
SUB_BD_INAF=summaryBy( AUX ~ COD_UF.x+SUB_SECT+UF_CRITICA, FUN=fun1, data =as.data.frame(SUB_BD_INAF),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF=SUB_BD_INAF[,-c(4)]
SUB_BD_INAF$AUX=1
SUB_BD_INAF=summaryBy( AUX ~ SUB_SECT+UF_CRITICA, FUN=fun1, data =as.data.frame(SUB_BD_INAF),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF <- spread(SUB_BD_INAF, UF_CRITICA, AUX) #RESHAP
names(SUB_BD_INAF)[names(SUB_BD_INAF) == 'NO'] <- 'UF_INAF'
names(SUB_BD_INAF)[names(SUB_BD_INAF) == 'SI'] <- 'UFC_INAF'
SUB_BD_INAF$UF_INAF=SUB_BD_INAF$UF_INAF+SUB_BD_INAF$UFC_INAF
SUB_BD_INAF=cbind(SUB_BD_INAF,"UNIV_UF"=c(1147, 4953, 1661, 727, 472, 413),"UNIV_UFC"=c(21, 19, 341, 41, 151, 52 ))
SUB_BD_INAF$AVANCE_UF=SUB_BD_INAF$UF_INAF/SUB_BD_INAF$UNIV_UF
SUB_BD_INAF$AVANCE_UFC=SUB_BD_INAF$UFC_INAF/SUB_BD_INAF$UNIV_UFC

# BARRAS 1
AVANCE_1=ggplot(SUB_BD_INAF,aes(x=SUB_SECT, y=AVANCE_UF,fill=SUB_SECT))+
  scale_fill_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()+
  ggeasy::easy_all_text_size(size = 20)+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
        axis.text.x=element_blank(),
        plot.title = element_text(size = 30, face = 2))+
  labs(x="",
       y="\nPorcentaje de avance",
       title = paste("Porcentaje de avance (UF/UNIVERSO)."),
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,"\n",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n1/ Cantidad de UF que tienen al menos un IGA respecto al universo de UF\n2/ Unidades Fiscalizables")+
  geom_shadowtext(aes(label=paste(round(AVANCE_UF*100,1),"%",sep = "")),
                  #family = "Times New Roman",
                  hjust=1, #????
                  nudge_y = 0, #DISTANCIA DEL NUMERO A LA GRAFICA
                  size=10, 
                  fontface=2)+
  ggeasy::easy_y_axis_labels_size(size = 20)

  
AVANCE_1


# BARRAS 2
AVANCE_2=ggplot(SUB_BD_INAF,aes(x=SUB_SECT, y=AVANCE_UFC,fill=SUB_SECT))+
  scale_fill_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()+
  ggeasy::easy_all_text_size(size = 20)+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
        axis.text.x=element_blank(),
        plot.title = element_text(size = 30, face = "bold"))+
  labs(x="",
       y="\nPorcentaje de avance",
       title = paste("Porcentaje de avance (UFC/UNIVERSO)."),
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,"\n",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n1/ Cantidad de UFC que tienen al menos un IGA respecto al universo de UFC\n2/ Unidades Fiscalizables Críticas")+
  geom_shadowtext(aes(label=paste(round(AVANCE_UFC*100,1),"%",sep = "")),
                  #family = "Times New Roman",
                  hjust=1, #????
                  nudge_y = 0, #DISTANCIA DEL NUMERO A LA GRAFICA
                  size=10, 
                  fontface=2)+
  ggeasy::easy_y_axis_labels_size(size = 20)

AVANCE_2


#UNIR LOS DOS GRAFICOS
UNION <- ggarrange(AVANCE_1, AVANCE_2,
                   #labels = c("A", "B", "C"),
                   ncol = 1, nrow = 2)
UNION

ggsave("0.1) AVANCE_1.jpg",  width = ANCHO*ESCAL_CONV, height = 2*ALTO*ESCAL_CONV, units="cm",dpi = RES)


###################################################################


# __________________________________________________________________________________________________________________________________________________________

## ############################################# ###
##    2) ERRORES: 1) Errores por coordinación    ####
## ############################################# ###

SUB_BD_HISTORIAL=merge(BD_HISTORIAL,BD_INAF[,c(1,16)],
                       by.x="COD_INST",
                       by.y="Código",
                       all.x=T)

# SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO>="2020-05-01",]  #F MIN
# SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO<="2020-07-09",]  #F MAX

SUB_BD_HISTORIAL$AUX1=1
SUB_BD_HISTORIAL$AUX2=0
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$PROC_ESTADO=="OBSERVADO"]=1
SUB_BD_HISTORIAL$MES=month(SUB_BD_HISTORIAL$F_ESTADO)
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2 ~ MES+SUB_SECT+ESTADO+COD_INST+ETAPA, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$AUX2>=1]=1
SUB_BD_HISTORIAL$AUX3=0
SUB_BD_HISTORIAL$AUX3[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"]=1
SUB_BD_HISTORIAL$AUX4=0
SUB_BD_HISTORIAL$AUX4[SUB_BD_HISTORIAL$ETAPA=="2) COORDINADOR"&SUB_BD_HISTORIAL$AUX2==1]=1
SUB_BD_HISTORIAL$AUX5=0
SUB_BD_HISTORIAL$AUX5[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"&SUB_BD_HISTORIAL$AUX2==1]=1
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2+AUX3+AUX4+AUX5 ~ MES+COD_INST+SUB_SECT+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$AUX2>=1]=1
SUB_BD_HISTORIAL$CASOS_COORD=1
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2+AUX3+AUX4+AUX5+CASOS_COORD ~ MES+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX1'] <- 'C_FLUJOS'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX2'] <- 'OBS'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX3'] <- 'CASOS_CSIG'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX4'] <- 'OBS_COORD'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX5'] <- 'OBS_CSIG'

SUB_BD_HISTORIAL$PORC_CASOS_OBS_COORD=round(100*SUB_BD_HISTORIAL$OBS_COORD/SUB_BD_HISTORIAL$CASOS_COORD,1)
SUB_BD_HISTORIAL$PORC_CASOS_OBS_CSIG=round(100*SUB_BD_HISTORIAL$OBS_CSIG/SUB_BD_HISTORIAL$CASOS_CSIG,1)

#TEMPORAL
SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[is.na(SUB_BD_HISTORIAL$SUB_SECT)==F,]


#BARRAS 1
OBS_1=ggplot(SUB_BD_HISTORIAL,aes(x=SUB_SECT, y=PORC_CASOS_OBS_COORD,fill=SUB_SECT))+
  scale_fill_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()+
  ggeasy::easy_all_text_size(size = 20)+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
        axis.text.x=element_blank(),
        plot.title = element_text(size = 30, face = 2))+
  labs(x="",
       y="\nPorcentaje de casos con observaciones",
       title = paste("Porcentaje de IGA con observaciones en la revisión de los coordinadores."),
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")\n",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n")+
    geom_shadowtext(aes(label=paste(PORC_CASOS_OBS_COORD,"%",sep = "")),
                  #family = "Times New Roman",
                  hjust=1, #????
                  nudge_y = 0, #DISTANCIA DEL NUMERO A LA GRAFICA
                  size=10, 
                  fontface=2)+
  ggeasy::easy_y_axis_labels_size(size = 20)+
  geom_hline(yintercept=10, linetype="dashed", 
               color = "red", size=1)
OBS_1

# #CREANDO GIF
# OBS_1=OBS_1 + transition_time(MES)+
#   labs(subtitle = "MES: {frame_time}")+
#   shadow_wake(wake_length = 0.1, alpha = FALSE)
# 
# animate(OBS_1, duration=2*(max(SUB_BD_HISTORIAL$MES)-min(SUB_BD_HISTORIAL$MES)+1), fps=0.5, width=ANCHO/1.2, height=ALTO/1.2, res=RES/4, renderer = gifski_renderer(loop = T))
# 
# # Save at gif:
# anim_save("001) PRUEBAS.gif")





#BARRAS 2
OBS_2=ggplot(SUB_BD_HISTORIAL,aes(x=SUB_SECT, y=PORC_CASOS_OBS_CSIG,fill=SUB_SECT))+
  scale_fill_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()+
  ggeasy::easy_all_text_size(size = 20)+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
        axis.text.x=element_blank(),
        plot.title = element_text(size = 30, face = "bold"))+
  labs(x="",
       y="\nPorcentaje de casos con observaciones",
       title = paste("Porcentaje de IGA con observaciones en la revisión de CSIG."),
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,")\n",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n")+
  geom_shadowtext(aes(label=paste(PORC_CASOS_OBS_CSIG,"%",sep = "")),
                  #family = "Times New Roman",
                  hjust=1, #????
                  nudge_y = 0, #DISTANCIA DEL NUMERO A LA GRAFICA
                  size=10, 
                  fontface=2)+
  ggeasy::easy_y_axis_labels_size(size = 20)+
  geom_hline(yintercept=10, linetype="dashed", 
             color = "red", size=1)
OBS_2

# #CREANDO GIF
# OBS_2=OBS_2 + transition_time(MES)+
#   labs(subtitle = "MES: {frame_time}")+
#   shadow_wake(wake_length = 0.1, alpha = FALSE)
# 
# animate(OBS_2, duration=2*(max(SUB_BD_HISTORIAL$MES)-min(SUB_BD_HISTORIAL$MES)+1), fps=0.5, width=ANCHO/1.2, height=ALTO/1.2, res=RES/4, renderer = gifski_renderer(loop = T))
# 
# # Save at gif:
# anim_save("002) PRUEBAS.gif")


####################################################


# __________________________________________________________________________________________________________________________________________________________

## ############################################# ###
##    2) ERRORES: 2) Errores por coordinación    ####
## ############################################# ###

SUB_BD_HISTORIAL=merge(BD_HISTORIAL,BD_INAF[,c(1,16)],
                       by.x="COD_INST",
                       by.y="Código",
                       all.x=T)

SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO>=F_MIN,]  #F MIN
SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO<=F_MAX,]  #F MAX

F_MIN=format(as.Date(F_MIN,"%Y-%m-%d"),"%d/%m/%Y")
F_MAX=format(as.Date(F_MAX,"%Y-%m-%d"),"%d/%m/%Y")

SUB_BD_HISTORIAL$AUX1=1
SUB_BD_HISTORIAL$AUX2=0
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$PROC_ESTADO=="OBSERVADO"]=1
SUB_BD_HISTORIAL$MES=month(SUB_BD_HISTORIAL$F_ESTADO)
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2 ~ SUB_SECT+ESTADO+COD_INST+ETAPA, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$AUX2>=1]=1
SUB_BD_HISTORIAL$AUX3=0
SUB_BD_HISTORIAL$AUX3[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"]=1
SUB_BD_HISTORIAL$AUX4=0
SUB_BD_HISTORIAL$AUX4[SUB_BD_HISTORIAL$ETAPA=="2) COORDINADOR"&SUB_BD_HISTORIAL$AUX2==1]=1
SUB_BD_HISTORIAL$AUX5=0
SUB_BD_HISTORIAL$AUX5[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"&SUB_BD_HISTORIAL$AUX2==1]=1
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2+AUX3+AUX4+AUX5 ~ COD_INST+SUB_SECT+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$AUX2>=1]=1
SUB_BD_HISTORIAL$CASOS_COORD=1
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2+AUX3+AUX4+AUX5+CASOS_COORD ~ SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX1'] <- 'C_FLUJOS'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX2'] <- 'OBS'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX3'] <- 'CASOS_CSIG'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX4'] <- 'OBS_COORD'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX5'] <- 'OBS_CSIG'

SUB_BD_HISTORIAL$PORC_CASOS_OBS_COORD=round(100*SUB_BD_HISTORIAL$OBS_COORD/SUB_BD_HISTORIAL$CASOS_COORD,1)
SUB_BD_HISTORIAL$PORC_CASOS_OBS_CSIG=round(100*SUB_BD_HISTORIAL$OBS_CSIG/SUB_BD_HISTORIAL$CASOS_CSIG,1)

#TEMPORAL
SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[is.na(SUB_BD_HISTORIAL$SUB_SECT)==F,]


# #BARRAS 1
# OBS_1=ggplot(SUB_BD_HISTORIAL,aes(x=SUB_SECT, y=PORC_CASOS_OBS_COORD,fill=SUB_SECT))+
#   scale_fill_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
#   geom_bar(stat = "identity")+
#   coord_flip()+
#   theme_minimal()+
#   ggeasy::easy_all_text_size(size = 15)+
#   theme(legend.position = "none", 
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
#         axis.text.x=element_blank(),
#         plot.title = element_text(size = 20, face = 2))+
#   labs(x="",
#        y="\nPorcentaje de casos con observaciones",
#        title = paste("Porcentaje de IGA con observaciones en la revisión de los coordinadores."),
#        subtitle = paste("(Del ",
#                         F_MIN,
#                         " al ",
#                         F_MAX,")\n",
#                         sep = ""),
#        caption = "Fuente: INAF\nElaboración: Propia\n")+
#   geom_shadowtext(aes(label=paste(PORC_CASOS_OBS_COORD,"%",sep = "")),
#                   #family = "Times New Roman",
#                   hjust=0.5, #????
#                   nudge_y = 0, #DISTANCIA DEL NUMERO A LA GRAFICA
#                   size=7.5, 
#                   fontface=2)+
#   ggeasy::easy_y_axis_labels_size(size = 20)+
#   geom_hline(yintercept=10, linetype="dashed", 
#              color = "red", size=1)
# 
# OBS_1



#BARRAS 2
OBS_2=ggplot(SUB_BD_HISTORIAL,aes(x=SUB_SECT, y=PORC_CASOS_OBS_CSIG,fill=SUB_SECT))+
  scale_fill_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()+
  ggeasy::easy_all_text_size(size = 15)+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
        axis.text.x=element_blank(),
        plot.title = element_text(size = 20, face = "bold"))+
  labs(x="",
       y="\nPorcentaje de casos con observaciones",
       title = paste("Porcentaje de IGA con observaciones en la revisión de CSIG."),
       subtitle = paste("(Del ",
                        F_MIN,
                        " al ",
                        F_MAX,")\n",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n")+
  geom_shadowtext(aes(label=paste(PORC_CASOS_OBS_CSIG,"%",sep = "")),
                  #family = "Times New Roman",
                  hjust=0.5, #????
                  nudge_y = 0, #DISTANCIA DEL NUMERO A LA GRAFICA
                  size=7.5, 
                  fontface=2)+
  ggeasy::easy_y_axis_labels_size(size = 20)+
  geom_hline(yintercept=10, linetype="dashed", 
             color = "red", size=1)

OBS_2


#UNIR LOS DOS GRAFICOS
OBS <- ggarrange(OBS_1, OBS_2,
                 #labels = c("A", "B", "C"),
                 ncol = 1, nrow = 2)
OBS

ggsave("0.2) OBSERVACIONES.jpg",  width = ANCHO*ESCAL_CONV, height = 2*ALTO*ESCAL_CONV, units="cm",dpi = RES)


####################################################


# __________________________________________________________________________________________________________________________________________________________

## ############################################# ###
##    2) ERRORES: 1) AVANCE Y ERRORES: ACTUAL    ####
## ############################################# ###

li=0
ls=10

#GENERANDO DATA 1
SUB_BD_HISTORIAL=merge(BD_HISTORIAL,BD_INAF[,c(1,16)],
                       by.x="COD_INST",
                       by.y="Código",
                       all.x=T)

SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO>=F_MIN,]  #F MIN
SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO<=F_MAX,]  #F MAX

F_min=format(as.Date(F_MIN,"%Y-%m-%d"),"%d/%m/%Y")
F_max=format(as.Date(F_MAX,"%Y-%m-%d"),"%d/%m/%Y")

SUB_BD_HISTORIAL$AUX1=1
SUB_BD_HISTORIAL$AUX2=0
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$PROC_ESTADO=="OBSERVADO"]=1
SUB_BD_HISTORIAL$MES=month(SUB_BD_HISTORIAL$F_ESTADO)
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2 ~ SUB_SECT+ESTADO+COD_INST+ETAPA, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$AUX2>=1]=1
SUB_BD_HISTORIAL$AUX3=0
SUB_BD_HISTORIAL$AUX3[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"]=1
SUB_BD_HISTORIAL$AUX4=0
SUB_BD_HISTORIAL$AUX4[SUB_BD_HISTORIAL$ETAPA=="2) COORDINADOR"&SUB_BD_HISTORIAL$AUX2==1]=1
SUB_BD_HISTORIAL$AUX5=0
SUB_BD_HISTORIAL$AUX5[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"&SUB_BD_HISTORIAL$AUX2==1]=1
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2+AUX3+AUX4+AUX5 ~ COD_INST+SUB_SECT+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$AUX2>=1]=1
SUB_BD_HISTORIAL$CASOS_COORD=1
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2+AUX3+AUX4+AUX5+CASOS_COORD ~ SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX1'] <- 'C_FLUJOS'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX2'] <- 'OBS'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX3'] <- 'CASOS_CSIG'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX4'] <- 'OBS_COORD'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX5'] <- 'OBS_CSIG'

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


BD_RESUMEN$MEDIA_3_4SD=BD_RESUMEN$MEDIA_IGA_UF+3*BD_RESUMEN$DS_IGA_UF/4  
BD_RESUMEN=BD_RESUMEN[!is.na(BD_RESUMEN$SUB_SECT)==T,]


BD_RESUMEN$E_UNIV_IGAS=ceiling(BD_RESUMEN$MEDIA_3_4SD*BD_RESUMEN$UNIV_UF)
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
  annotate("rect", xmin = 60, xmax = 100, ymin = 0, ymax = 10, 
           alpha = 0.4, fill = "grey") + 
  
  geom_point(aes(x=AVANCE_IGA,y=PORC_CASOS_OBS_CSIG,size=TOT_IGA_INAF,color=SUB_SECT))+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5,OEFA_7,OEFA_4))+
  ggeasy::easy_all_text_size(size = 15)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
        plot.title = element_text(size = 20, face = "bold"))+
  
  scale_x_continuous(limits = c(0, 102), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))+

  labs(x="IGAs en INAF* (%)",
       y="\nIGA con observaciones (%)",
       title = paste("IGA con observaciones en la revisión de CSIG y llenado."),
       subtitle = paste("(Revisión de CSIG del ",
                        F_min,
                        " al ",
                        F_max,")\n",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Estimado\nNota1: Los casos óptimos son aquellos con una tasa de observaciones de menos del 10% y un llenado de más de 60% de los caos, delimitado por el área de color gris\nNota2: Para CRES, debido a que la mayoría de UF no cuenta con IGA, los que han registrado ya constituye el universo. Es por ello que se ha considerado un avance del 100%")+
  scale_size(range = c(0.5,50), name="Cantidad de IGA\nregistrados en INAF")+
  geom_point(aes(x=AVANCE_IGA,y=PORC_CASOS_OBS_CSIG,size=1))+
  geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept=60, linetype="dashed", color = "red", size=1)+
  
  geom_text( x = 60, y = 100, angle = 90, label = paste("Umbral en llenado (60%)", sep=""), 
           hjust = 1,vjust = -0.5)+
  geom_text( x = 100, y = 10, angle = 0, label = paste("Umbral en observaciones (10%)", sep=""), 
             hjust = 1,vjust = -0.5)+
  
  ggeasy::easy_add_legend_title("Área")

  
BUBBLE_GRAPH

ggsave("0.3) OBSERVACIONES.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


write.csv(BD_RESUMEN,file="Tabla intermedia.csv", na="")


# ABC=ggplot(XX,aes(x=C_IGAS, y=SUB_SECT.x, fill=SUB_SECT.x,alpha=0.8))+
#   geom_density_ridges(scale= 1.5, bandwidth = 0.4, quantile_lines = TRUE, quantiles = 4,vline_size=1)+
#   theme_ridges() + 
#   scale_x_continuous(limits =  c(li, ls), breaks = seq(li, ls, by=5),minor_breaks = seq(li, ls, by=1)) +
#   theme(legend.position = "none",
#         panel.grid.minor = element_line(colour = "gray90",linetype="dashed"),
#         panel.grid.major = element_line(colour = "gray70"))+
#   scale_discrete_manual("vline_color",
#                         values = c("black", "red", "black", "white"), 
#                         breaks = c(1, 2),
#                         labels = c("1st & 3rd quartile", "median"),
#                         name = NULL)
# 
# ABC

# ABC=ggplot(XX,aes(x=log(T_AÑOS), y=C_IGAS, color=SUB_SECT.x))+
#   geom_point()
#   
# ABC


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
SUB_BD_HISTORIAL=SUB_BD_HISTORIAL[SUB_BD_HISTORIAL$F_ESTADO<=as.character(as.Date(F_MAX)-7),]  #F MAX

F_min=format(as.Date(F_MIN,"%Y-%m-%d"),"%d/%m/%Y")
F_max=format(as.Date(as.character(as.Date(F_MAX)-7),"%Y-%m-%d"),"%d/%m/%Y")

SUB_BD_HISTORIAL$AUX1=1
SUB_BD_HISTORIAL$AUX2=0
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$PROC_ESTADO=="OBSERVADO"]=1
SUB_BD_HISTORIAL$MES=month(SUB_BD_HISTORIAL$F_ESTADO)
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2 ~ SUB_SECT+ESTADO+COD_INST+ETAPA, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$AUX2>=1]=1
SUB_BD_HISTORIAL$AUX3=0
SUB_BD_HISTORIAL$AUX3[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"]=1
SUB_BD_HISTORIAL$AUX4=0
SUB_BD_HISTORIAL$AUX4[SUB_BD_HISTORIAL$ETAPA=="2) COORDINADOR"&SUB_BD_HISTORIAL$AUX2==1]=1
SUB_BD_HISTORIAL$AUX5=0
SUB_BD_HISTORIAL$AUX5[SUB_BD_HISTORIAL$ETAPA=="3) ESPECIALISTA CSIG"&SUB_BD_HISTORIAL$AUX2==1]=1
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2+AUX3+AUX4+AUX5 ~ COD_INST+SUB_SECT+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_HISTORIAL$AUX2[SUB_BD_HISTORIAL$AUX2>=1]=1
SUB_BD_HISTORIAL$CASOS_COORD=1
SUB_BD_HISTORIAL=summaryBy(AUX1+AUX2+AUX3+AUX4+AUX5+CASOS_COORD ~ SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_HISTORIAL),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX1'] <- 'C_FLUJOS'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX2'] <- 'OBS'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX3'] <- 'CASOS_CSIG'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX4'] <- 'OBS_COORD'
names(SUB_BD_HISTORIAL)[names(SUB_BD_HISTORIAL) == 'AUX5'] <- 'OBS_CSIG'

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


BD_RESUMEN$MEDIA_3_4SD=BD_RESUMEN$MEDIA_IGA_UF+3*BD_RESUMEN$DS_IGA_UF/4  
BD_RESUMEN=BD_RESUMEN[!is.na(BD_RESUMEN$SUB_SECT)==T,]


BD_RESUMEN$E_UNIV_IGAS=ceiling(BD_RESUMEN$MEDIA_3_4SD*BD_RESUMEN$UNIV_UF)
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
  annotate("rect", xmin = 60, xmax = 100, ymin = 0, ymax = 10, 
           alpha = 0.4, fill = "grey") + 
  
  geom_point(aes(x=AVANCE_IGA,y=PORC_CASOS_OBS_CSIG,size=TOT_IGA_INAF,color=SUB_SECT))+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5,OEFA_7,OEFA_4))+
  ggeasy::easy_all_text_size(size = 15)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")),
        plot.title = element_text(size = 20, face = "bold"))+
  
  scale_x_continuous(limits = c(0, 102), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))+
  
  labs(x="IGAs en INAF* (%)",
       y="\nIGA con observaciones (%)",
       title = paste("IGA con observaciones en la revisión de CSIG y llenado."),
       subtitle = paste("(Revisión de CSIG del ",
                        F_min,
                        " al ",
                        F_max,")\n",
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Estimado\nNota1: Los casos óptimos son aquellos con una tasa de observaciones de menos del 10% y un llenado de más de 60% de los caos, delimitado por el área de color gris\nNota2: Para CRES, debido a que la mayoría de UF no cuenta con IGA, los que han registrado ya constituye el universo. Es por ello que se ha considerado un avance del 100%")+
  scale_size(range = c(0.5,50), name="Cantidad de IGA\nregistrados en INAF")+
  geom_point(aes(x=AVANCE_IGA,y=PORC_CASOS_OBS_CSIG,size=1))+
  geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
  geom_vline(xintercept=60, linetype="dashed", color = "red", size=1)+
  
  geom_text( x = 60, y = 100, angle = 90, label = paste("Umbral en llenado (60%)", sep=""), 
             hjust = 1,vjust = -0.5)+
  geom_text( x = 100, y = 10, angle = 0, label = paste("Umbral en observaciones (10%)", sep=""), 
             hjust = 1,vjust = -0.5)+
  
  ggeasy::easy_add_legend_title("Área")


BUBBLE_GRAPH

ggsave("0.3) OBSERVACIONES.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


write.csv(BD_RESUMEN,file="Tabla intermedia.csv", na="")


# ABC=ggplot(XX,aes(x=C_IGAS, y=SUB_SECT.x, fill=SUB_SECT.x,alpha=0.8))+
#   geom_density_ridges(scale= 1.5, bandwidth = 0.4, quantile_lines = TRUE, quantiles = 4,vline_size=1)+
#   theme_ridges() + 
#   scale_x_continuous(limits =  c(li, ls), breaks = seq(li, ls, by=5),minor_breaks = seq(li, ls, by=1)) +
#   theme(legend.position = "none",
#         panel.grid.minor = element_line(colour = "gray90",linetype="dashed"),
#         panel.grid.major = element_line(colour = "gray70"))+
#   scale_discrete_manual("vline_color",
#                         values = c("black", "red", "black", "white"), 
#                         breaks = c(1, 2),
#                         labels = c("1st & 3rd quartile", "median"),
#                         name = NULL)
# 
# ABC

# ABC=ggplot(XX,aes(x=log(T_AÑOS), y=C_IGAS, color=SUB_SECT.x))+
#   geom_point()
#   
# ABC


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

#GRÁFICO ACUMULADO
AVANCE_REG_DIA_TOT <- ggplot(SUB_BD_INAF,aes(F_REG, cumsum(AUX))) +
  geom_line(color=OEFA_1, size=1.5) +
  #stat_smooth(se=FALSE)+
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
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
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)

AVANCE_REG_DIA_TOT

#GUARDANDO EL GRÁFICO
ggsave("1.1) Avance acumulado total.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


# #CREANDO GIF
# AVANCE_REG_DIA_TOT=AVANCE_REG_DIA_TOT + transition_reveal(F_REG)
# animate(AVANCE_REG_DIA_TOT, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("1.1) Avance acumulado total.gif")

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

#GRÁFICO ACUMULADO
AVANCE_REG_DIA_TOT <- ggplot(SUB_BD_INAF,aes(F_REG, cumsum(N_ARCH))) +
  geom_line(color=OEFA_1, size=1.5) +
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
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)

AVANCE_REG_DIA_TOT

#GUARDANDO EL GRÁFICO
ggsave("1.2) Avance ARCHIVOS acumulado total.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


# #CREANDO GIF
# AVANCE_REG_DIA_TOT=AVANCE_REG_DIA_TOT + transition_reveal(F_REG)
# animate(AVANCE_REG_DIA_TOT, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("1.2) Avance ARCHIVOS acumulado total.gif")


###################################################################


# __________________________________________________________________________________________________________________________________________________________

## ###################################################### ###
##   3) AVANCE TOTAL: 3) Evolución de la cantidad de MB   ####
######################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF=as.data.frame(BD_INAF)
SUB_BD_INAF$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF=summaryBy(ARCH_SIZE_MB ~ F_REG, FUN=fun2, data =as.data.frame(SUB_BD_INAF),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF$F_REG=as.Date(SUB_BD_INAF$F_REG)

#GRÁFICO ACUMULADO
AVANCE_REG_DIA_TOT <- ggplot(SUB_BD_INAF,aes(F_REG, cumsum(ARCH_SIZE_MB))) +
  geom_line(color=OEFA_1, size=1.5) +
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
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)

AVANCE_REG_DIA_TOT

#GUARDANDO EL GRÁFICO
ggsave("1.3) Avance MB acumulado total.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


#SUB_BD_INAF = mutate((SUB_BD_INAF),"AUX2" = cumsum(ARCH_SIZE_MB))

# #CREANDO GIF
# AVANCE_REG_DIA_TOT=AVANCE_REG_DIA_TOT + transition_reveal(F_REG)
# animate(AVANCE_REG_DIA_TOT, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("1.3) Avance MB acumulado total.gif")

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


#GRÁFICO ACUMULADO SOLO UF CRITICAS
AVANCE_REG_DIA_TOT_VAL <- ggplot(SUB_BD_INAF_X,aes(F_REG, AUX2, group = ESTADO_AUX, color = factor(ESTADO_AUX))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(OEFA_1, OEFA_4))+
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
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Los casos validados son aquellos que pasaron por la revisión de los coordinadores y de CSIG")+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  theme(legend.position = "bottom",legend.title = element_blank())

AVANCE_REG_DIA_TOT_VAL

#GUARDANDO EL GRÁFICO
ggsave("1.4) Avance por proceso.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


# #CREANDO GIF
# AVANCE_REG_DIA_TOT_VAL=AVANCE_REG_DIA_TOT_VAL + transition_reveal(F_REG)#+
# animate(AVANCE_REG_DIA_TOT_VAL, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("1.4) Avance por proceso.gif")

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


#GRÁFICO ACUMULADO SOLO UF CRITICAS
AVANCE_REG_DIA_TOT_VAL <- ggplot(SUB_BD_INAF_X,aes(F_REG, AUX2, group = ESTADO_AUX, color = factor(ESTADO_AUX))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(OEFA_1, OEFA_4))+
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
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Los casos validados son aquellos que pasaron por la revisión de los coordinadores y de CSIG")+
  ggeasy::easy_rotate_x_labels(angle = 90)+
  theme(legend.position = "bottom",legend.title = element_blank())

AVANCE_REG_DIA_TOT_VAL

#GUARDANDO EL GRÁFICO
ggsave("1.5) Avance por proceso.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


# #CREANDO GIF
# AVANCE_REG_DIA_TOT_VAL=AVANCE_REG_DIA_TOT_VAL + transition_reveal(F_REG)#+
# animate(AVANCE_REG_DIA_TOT_VAL, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("1.5) Avance por proceso.gif")


#########################################################################


# __________________________________________________________________________________________________________________________________________________________

## ################################################### ###
##   3) AVANCE TOTAL: 6) Evolución de IGA de UF y UFC  ####
## ################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF_3=as.data.frame(BD_INAF_CRIT)
SUB_BD_INAF_3$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF_3=summaryBy(AUX ~ F_REG+UF_CRITICA, FUN=fun1, data =as.data.frame(SUB_BD_INAF_3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF_3$F_REG=as.Date(SUB_BD_INAF_3$F_REG)

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF_3 = mutate(group_by(SUB_BD_INAF_3,UF_CRITICA),"AUX2" = cumsum(AUX))

#GRÁFICO ACUMULADO
AVANCE_REG_DIA_TOT_Y_UFC <- ggplot(SUB_BD_INAF_3,aes(F_REG, AUX2, group = UF_CRITICA, color = factor(UF_CRITICA))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(OEFA_1,OEFA_3),labels=c("Totales","UF Críticas"),name = "")+
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
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)
  
AVANCE_REG_DIA_TOT_Y_UFC

#GUARDANDO EL GRÁFICO
ggsave("1.6) Avance acumulado Totales y UFC.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)

# #CREANDO GIF
# AVANCE_REG_DIA_TOT_Y_UFC=AVANCE_REG_DIA_TOT_Y_UFC + transition_reveal(F_REG)#+
# animate(AVANCE_REG_DIA_TOT_Y_UFC, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("1.6) Avance acumulado Totales y UFC.gif")


##########################################################

# __________________________________________________________________________________________________________________________________________________________

## ########################################################## ###
##   3) AVANCE TOTAL: 7) Evolución de archivos de UF y UFC    ####
## ########################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF_3=as.data.frame(BD_INAF_CRIT)
SUB_BD_INAF_3$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF_3=summaryBy(N_ARCH ~ F_REG+UF_CRITICA, FUN=sum, data =as.data.frame(SUB_BD_INAF_3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF_3$F_REG=as.Date(SUB_BD_INAF_3$F_REG)

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF_3 = mutate(group_by(SUB_BD_INAF_3,UF_CRITICA),"AUX2" = cumsum(N_ARCH))

#GRÁFICO ACUMULADO
AVANCE_REG_DIA_TOT_Y_UFC <- ggplot(SUB_BD_INAF_3,aes(F_REG, AUX2, group = UF_CRITICA, color = factor(UF_CRITICA))) +
  geom_line(size=1.5) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("1 week"))+
  scale_color_manual(values=c(OEFA_1,OEFA_3),labels=c("Totales","UF Críticas"),name = "")+
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
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)

AVANCE_REG_DIA_TOT_Y_UFC

#GUARDANDO EL GRÁFICO
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
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de IGA",
       title = "Evolución de la cantidad de IGA en INAF,\npor coordinación (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)

AVANCE_REG_DIA

#GUARDANDO EL GRÁFICO
ggsave("2.1) Evolución por area.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)

# #CREANDO GIF
# AVANCE_REG_DIA=AVANCE_REG_DIA + transition_reveal(F_REG)
# animate(AVANCE_REG_DIA, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("2.1) Evolución por area.gif")


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
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  labs(x="",
       y="Cantidad de archivos",
       title = "Evolución de la cantidad de archivos en INAF,\npor coordinación (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)

AVANCE_REG_DIA

#GUARDANDO EL GRÁFICO
ggsave("2.2) Evolución archivos por area.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)


# #CREANDO GIF
# AVANCE_REG_DIA=AVANCE_REG_DIA + transition_reveal(F_REG)
# animate(AVANCE_REG_DIA, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("2.2) Evolución archivos por area.gif")



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
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
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
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)

AVANCE_REG_DIA

#GUARDANDO EL GRÁFICO
ggsave("2.3) Evolución MB por area.jpg",  width = 0.6*ANCHO*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)

# #CREANDO GIF
# AVANCE_REG_DIA=AVANCE_REG_DIA + transition_reveal(F_REG)
# animate(AVANCE_REG_DIA, duration=DURACION, fps=FPS, width=ANCHO, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# anim_save("2.3) Evolución MB por area.gif")


################################################################


# __________________________________________________________________________________________________________________________________________________________

## ################################################################### ###
##   5) AVANCE POR AREA: 1) Evolución del registro per cápita de IGA   ####
## ################################################################### ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF3=as.data.frame(BD_INAF)
SUB_BD_INAF3$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF3=summaryBy(AUX ~ F_REG+SUB_SECT+REGISTRADOR, FUN=fun1, data =as.data.frame(SUB_BD_INAF3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF3$N_REG=1
SUB_BD_INAF3=summaryBy(AUX+N_REG ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF3$REGISTROS_PERCAP=SUB_BD_INAF3$AUX/SUB_BD_INAF3$N_REG
SUB_BD_INAF3$F_REG=as.Date(SUB_BD_INAF3$F_REG)

#GRAFICAR
AVANCE_REG_PERCAP_DIA <- ggplot(SUB_BD_INAF3,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("2 weeks"))+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_2,OEFA_5, OEFA_7, OEFA_4))+
  ylim(0,40)+
  labs(x="",
       y="Cantidad de IGA por registrador",
       title = "Evolución de la cantidad de IGA per cápita* en INAF, por coordinación**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realizó la migración de información del DRIVE generando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.minor = element_blank())+
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  ggeasy::easy_rotate_x_labels(angle=90)

AVANCE_REG_PERCAP_DIA

#GUARDANDO EL GRÁFICO
ggsave("3.1) IGAs per cápita.jpg",  width = 0.6*2800*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)

# #CREANDO GIF
# AVANCE_REG_PERCAP_DIA=AVANCE_REG_PERCAP_DIA + transition_reveal(F_REG)
# animate(AVANCE_REG_PERCAP_DIA, duration=DURACION, fps=FPS, width= 2800, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# # Save at gif:
# anim_save("3.1) IGAs per cápita.gif")


##########################################################################


# __________________________________________________________________________________________________________________________________________________________

## ######################################################################## ###
##   5) AVANCE POR AREA: 2) Evolución del registro per cápita de archivos   ####
## ######################################################################## ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF3=as.data.frame(BD_INAF)
SUB_BD_INAF3$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF3=summaryBy(N_ARCH ~ F_REG+SUB_SECT+REGISTRADOR, FUN=sum, data =as.data.frame(SUB_BD_INAF3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF3$N_REG=1
SUB_BD_INAF3=summaryBy(N_ARCH+N_REG ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF3$REGISTROS_PERCAP=SUB_BD_INAF3$N_ARCH/SUB_BD_INAF3$N_REG
SUB_BD_INAF3$F_REG=as.Date(SUB_BD_INAF3$F_REG)

#GRAFICAR
AVANCE_REG_PERCAP_DIA <- ggplot(SUB_BD_INAF3,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("2 weeks"))+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_2,OEFA_5, OEFA_7, OEFA_4))+
  ylim(0,40)+
  labs(x="",
       y="Cantidad de archivos por registrador",
       title = "Evolución de la cantidad de archivos per cápita* en INAF, por coordinación**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realizó la migración de información del DRIVE generando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.minor = element_blank())+
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  ggeasy::easy_rotate_x_labels(angle=90)

AVANCE_REG_PERCAP_DIA

#GUARDANDO EL GRÁFICO
ggsave("3.2) ARCHIVOS per cápita.jpg",  width = 0.6*2800*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)

# #CREANDO GIF
# AVANCE_REG_PERCAP_DIA=AVANCE_REG_PERCAP_DIA + transition_reveal(F_REG)#+
# animate(AVANCE_REG_PERCAP_DIA, duration = DURACION, fps = FPS, width = 2800, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# # Save at gif:
# anim_save("3.2) ARCHIVOS per cápita.gif")


###############################################################################


# __________________________________________________________________________________________________________________________________________________________

## ################################################################# ###
##   5) AVANCE POR AREA: 3) Evolución de MB per cápita de archivos   ####
## ################################################################# ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF3=as.data.frame(BD_INAF)
SUB_BD_INAF3$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF3=summaryBy(ARCH_SIZE_MB ~ F_REG+SUB_SECT+REGISTRADOR, FUN=sum, data =as.data.frame(SUB_BD_INAF3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF3$N_REG=1
SUB_BD_INAF3=summaryBy(ARCH_SIZE_MB+N_REG ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF3$REGISTROS_PERCAP=SUB_BD_INAF3$ARCH_SIZE_MB/SUB_BD_INAF3$N_REG
SUB_BD_INAF3$F_REG=as.Date(SUB_BD_INAF3$F_REG)

#GRAFICAR
AVANCE_REG_PERCAP_DIA <- ggplot(SUB_BD_INAF3,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("2 weeks"))+
  #geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray70")+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_2,OEFA_5, OEFA_7, OEFA_4))+
  ylim(0,7500)+
  labs(x="",
       y="Cantidad de MB por registrador",
       title = "Evolución de la cantidad de MB per cápita* en INAF, por coordinación**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realizó la migración de información del DRIVE generando picos de llenado")+
  theme(legend.position = "bottom",  legend.title = element_blank(),
        panel.grid.minor = element_blank())+  
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  ggeasy::easy_rotate_x_labels(angle=90)

AVANCE_REG_PERCAP_DIA

#GUARDANDO EL GRÁFICO
ggsave("3.3) MB per cápita.jpg",  width = 0.6*2800*ESCAL_CONV, height = 0.6*ALTO*ESCAL_CONV, units="cm",dpi = RES)

# #CREANDO GIF
# AVANCE_REG_PERCAP_DIA=AVANCE_REG_PERCAP_DIA + transition_reveal(F_REG)#+
# animate(AVANCE_REG_PERCAP_DIA, duration = DURACION, fps = FPS, width = 2800, height=ALTO, res=RES, renderer = gifski_renderer(loop = F))
# 
# # Save at gif:
# anim_save("3.3) MB per cápita.gif")


########################################################################


# __________________________________________________________________________________________________________________________________________________________

## ################################################################### ###
##   5) AVANCE POR AREA: 4) Tendencia del registro per cápita de IGA   ####
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

#GRAFICAR
AVANCE_REG_PERCAP_DIA <- ggplot(SUB_BD_INAF5,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("2 weeks"))+
  geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray70")+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_2,OEFA_5, OEFA_7, OEFA_4))+
  #theme_minimal()+
  ylim(0,40)+
  labs(x="",
       y="Cantidad de IGA por registrador",
       title = "Evolución de la cantidad de IGA per cápita* en INAF, por coordinación**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realizó la migración de información del DRIVE\ngenerando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom",  legend.title = element_blank(),
        panel.grid.minor = element_blank())+  
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  ggeasy::easy_rotate_x_labels(angle=90)

AVANCE_REG_PERCAP_DIA

ggsave("3.4) Tendencia IGAs per cápita.jpg",  width = 12, height = 7)


##########################################################################


# __________________________________________________________________________________________________________________________________________________________

## ######################################################################## ###
##   5) AVANCE POR AREA: 5) Tendencia del registro per cápita de archivos   ####
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

#GRAFICAR
AVANCE_ARCH_PERCAP_DIA <- ggplot(SUB_BD_INAF5X,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("2 weeks"))+
  geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray70")+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_2,OEFA_5, OEFA_7, OEFA_4))+
  ylim(0,40)+
  labs(x="",
       y="Cantidad de archivos por registrador",
       title = "Evolución de la cantidad de archivos per cápita* en INAF, por coordinación**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realizó la migración de información del DRIVE\ngenerando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank())+  
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  ggeasy::easy_rotate_x_labels(angle=90)

  AVANCE_ARCH_PERCAP_DIA

ggsave("3.5) Tendencia ARCHIVOS per cápita.jpg",  width = 12, height = 7)


###############################################################################


# __________________________________________________________________________________________________________________________________________________________

## ######################################################################## ###
##   5) AVANCE POR AREA: 5) Tendencia del registro per cápita de archivos   ####
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

#GRAFICAR
AVANCE_ARCH_PERCAP_DIA <- ggplot(SUB_BD_INAF5X,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  scale_x_date(labels = date_format("%b %d"), breaks = date_breaks("2 weeks"))+
  geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray70")+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_2,OEFA_5, OEFA_7, OEFA_4))+
  ylim(0,7500)+
  labs(x="",
       y="Cantidad de MB por registrador",
       title = "Evolución de la cantidad de MB per cápita* en INAF, por coordinación**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realizó la migración de información del DRIVE\ngenerando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank())+  
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  ggeasy::easy_rotate_x_labels(angle=90)

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
  geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray70")+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_2,OEFA_5, OEFA_7, OEFA_4))+
  #theme_minimal()+
  #ylim(0,15)+
  scale_y_continuous(breaks = seq(0, 15, by = 2))+
  labs(x="",
       y="Cantidad de registradores",
       title = "Evolución de la cantidad de registradores.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboración: Propia")+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank())+  
  facet_wrap(.~SUB_SECT, 
             #scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  ggeasy::easy_rotate_x_labels(angle=90)

AVANCE_REG_PERCAP_DIA

ggsave("4) Cantidad de registradores.jpg",  width = 12, height = 7)


###########################################################


# __________________________________________________________________________________________________________________________________________________________

## ################################# ###
##   7) SANKEY + TABLA: 0) General   ####
## ################################# ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF4=as.data.frame(BD_INAF)
SUB_BD_INAF4$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF4=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF4),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF4$AUX2=paste(round(100*SUB_BD_INAF4$AUX/sum(SUB_BD_INAF4$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF4)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
SUB_BD_INAF4$Etapa[SUB_BD_INAF4$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF4$Etapa[SUB_BD_INAF4$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF4$Etapa[SUB_BD_INAF4$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF4$Estado[SUB_BD_INAF4$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF4$Estado[SUB_BD_INAF4$Estado=="EN REVISION"]="En revisión"
SUB_BD_INAF4$Estado[SUB_BD_INAF4$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF4$Estado[SUB_BD_INAF4$Estado=="VALIDADO"]="Validado"



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
  0, 1, SUB_BD_INAF4[2,3]+SUB_BD_INAF4[3,3]+SUB_BD_INAF4[4,3]+SUB_BD_INAF4[5,3]+SUB_BD_INAF4[6,3], # Casos que tienen primera versión
  0, 2, SUB_BD_INAF4[1,3], # Casos que no tienen primera versión.
  1, 3, SUB_BD_INAF4[4,3]+SUB_BD_INAF4[5,3]+SUB_BD_INAF4[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF4[2,3], # Casos con version final
  1, 5, SUB_BD_INAF4[3,3], # Casos sin verión final
  3, 6, SUB_BD_INAF4[4,3], # Casos con revisión de Érika
  3, 7, SUB_BD_INAF4[5,3], # Casos sin revisión de Érika
  3, 8, SUB_BD_INAF4[6,3]), # Casos con cargo (Revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              #colourScale=OEFA_1,
              units = "%",
              fontSize= 30, nodeWidth = 20,
              sinksRight=F,nodePadding=10,iterations=100)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5) SANKEY-TOTAL.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"



# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #

TAB1=
  
  formattable(SUB_BD_INAF4, align=c("l","l","c","r"),
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

########################################


# __________________________________________________________________________________________________________________________________________________________

## ############################## ###
##   7) SANKEY + TABLA: 1) CMIN   ####
## ############################## ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF6=as.data.frame(subset(BD_INAF,SUB_SECT=="CMIN"))
SUB_BD_INAF6$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF6=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF6),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF6$AUX2=paste(round(100*SUB_BD_INAF6$AUX/sum(SUB_BD_INAF6$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF6)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
SUB_BD_INAF6$Etapa[SUB_BD_INAF6$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF6$Etapa[SUB_BD_INAF6$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF6$Etapa[SUB_BD_INAF6$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF6$Estado[SUB_BD_INAF6$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF6$Estado[SUB_BD_INAF6$Estado=="EN REVISION"]="En revisión"
SUB_BD_INAF6$Estado[SUB_BD_INAF6$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF6$Estado[SUB_BD_INAF6$Estado=="VALIDADO"]="Validado"


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
  0, 1, SUB_BD_INAF6[2,3]+SUB_BD_INAF6[3,3]+SUB_BD_INAF6[4,3]+SUB_BD_INAF6[5,3]+SUB_BD_INAF6[6,3], # Casos que tienen primera versión
  0, 2, SUB_BD_INAF6[1,3], # Casos que no tienen primera versión.
  1, 3, SUB_BD_INAF6[4,3]+SUB_BD_INAF6[5,3]+SUB_BD_INAF6[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF6[2,3], # Casos con version final
  1, 5, SUB_BD_INAF6[3,3], # Casos sin verión final
  3, 6, SUB_BD_INAF6[4,3], # Casos con revisión de Érika
  3, 7, SUB_BD_INAF6[5,3], # Casos sin revisión de Érika
  3, 8, SUB_BD_INAF6[6,3]), # Casos con cargo (Revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.1) SANKEY-CMIN.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"



# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(SUB_BD_INAF6, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

TAB1

#GUARDANDO TABLA
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6.1) AVANCE-CMIN.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"

#####################################


# __________________________________________________________________________________________________________________________________________________________

## ############################## ###
##   7) SANKEY + TABLA: 2) CELE   ####
## ############################## ###

#GENERANDO LA NUEVA BD
SUB_BD_INAF7=as.data.frame(subset(BD_INAF,SUB_SECT=="CELE"))
SUB_BD_INAF7$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF7=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF7),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF7$AUX2=paste(round(100*SUB_BD_INAF7$AUX/sum(SUB_BD_INAF7$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF7)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
SUB_BD_INAF7$Etapa[SUB_BD_INAF7$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF7$Etapa[SUB_BD_INAF7$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF7$Etapa[SUB_BD_INAF7$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF7$Estado[SUB_BD_INAF7$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF7$Estado[SUB_BD_INAF7$Estado=="EN REVISION"]="En revisión"
SUB_BD_INAF7$Estado[SUB_BD_INAF7$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF7$Estado[SUB_BD_INAF7$Estado=="VALIDADO"]="Validado"



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
  0, 1, SUB_BD_INAF7[2,3]+SUB_BD_INAF7[3,3]+SUB_BD_INAF7[4,3]+SUB_BD_INAF7[5,3]+SUB_BD_INAF7[6,3], # Casos que tienen primera versión
  0, 2, SUB_BD_INAF7[1,3], # Casos que no tienen primera versión.
  1, 3, SUB_BD_INAF7[4,3]+SUB_BD_INAF7[5,3]+SUB_BD_INAF7[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF7[2,3], # Casos con version final
  1, 5, SUB_BD_INAF7[3,3], # Casos sin verión final
  3, 6, SUB_BD_INAF7[4,3], # Casos con revisión de Érika
  3, 7, SUB_BD_INAF7[5,3], # Casos sin revisión de Érika
  3, 8, SUB_BD_INAF7[6,3]), # Casos con cargo (Revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.2) SANKEY-CELE.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"



# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(SUB_BD_INAF7, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

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
SUB_BD_INAF8=as.data.frame(subset(BD_INAF,SUB_SECT=="CHID"))
SUB_BD_INAF8$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF8=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF8),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF8$AUX2=paste(round(100*SUB_BD_INAF8$AUX/sum(SUB_BD_INAF8$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF8)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
SUB_BD_INAF8$Etapa[SUB_BD_INAF8$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF8$Etapa[SUB_BD_INAF8$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF8$Etapa[SUB_BD_INAF8$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF8$Estado[SUB_BD_INAF8$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF8$Estado[SUB_BD_INAF8$Estado=="EN REVISION"]="En revisión"
SUB_BD_INAF8$Estado[SUB_BD_INAF8$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF8$Estado[SUB_BD_INAF8$Estado=="VALIDADO"]="Validado"



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
  0, 1, SUB_BD_INAF8[2,3]+SUB_BD_INAF8[3,3]+SUB_BD_INAF8[4,3]+SUB_BD_INAF8[5,3]+SUB_BD_INAF8[6,3], # Casos que tienen primera versión
  0, 2, SUB_BD_INAF8[1,3], # Casos que no tienen primera versión.
  1, 3, SUB_BD_INAF8[4,3]+SUB_BD_INAF8[5,3]+SUB_BD_INAF8[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF8[2,3], # Casos con version final
  1, 5, SUB_BD_INAF8[3,3], # Casos sin verión final
  3, 6, SUB_BD_INAF8[4,3], # Casos con revisión de Érika
  3, 7, SUB_BD_INAF8[5,3], # Casos sin revisión de Érika
  3, 8, SUB_BD_INAF8[6,3]), # Casos con cargo (Revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.3) SANKEY-CHID.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"




# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(SUB_BD_INAF8, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

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
SUB_BD_INAF9=as.data.frame(subset(BD_INAF,SUB_SECT=="CIND"))
SUB_BD_INAF9$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF9=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF9),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF9$AUX2=paste(round(100*SUB_BD_INAF9$AUX/sum(SUB_BD_INAF9$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF9)=c("Etapa","Estado","Registros","%del total")

#CORRIGIENDO "ERRORES" DE LA DATA
if (paste(SUB_BD_INAF9$Etapa,SUB_BD_INAF9$Estado)!="Coordinador En revisión") {SUB_BD_INAF9=rbind(SUB_BD_INAF9,c("Coordinador","En revisión",0,"0%"))}
SUB_BD_INAF9$Registros=as.numeric(SUB_BD_INAF9$Registros)

#REORDENANDO
SUB_BD_INAF9=SUB_BD_INAF9[c(1,6,2:5),]
rownames(SUB_BD_INAF9) <- 1:nrow(SUB_BD_INAF9)

#Mejorando un poco el contenido
SUB_BD_INAF9$Etapa[SUB_BD_INAF9$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF9$Etapa[SUB_BD_INAF9$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF9$Etapa[SUB_BD_INAF9$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF9$Estado[SUB_BD_INAF9$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF9$Estado[SUB_BD_INAF9$Estado=="EN REVISION"]="En revisión"
SUB_BD_INAF9$Estado[SUB_BD_INAF9$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF9$Estado[SUB_BD_INAF9$Estado=="VALIDADO"]="Validado"


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

links=as.data.frame(matrix(c(
  0, 1, SUB_BD_INAF9[2,3]+SUB_BD_INAF9[3,3]+SUB_BD_INAF9[4,3]+SUB_BD_INAF9[5,3]+SUB_BD_INAF9[6,3], # Casos que tienen primera versión
  0, 2, SUB_BD_INAF9[1,3], # Casos que no tienen primera versión.
  1, 3, SUB_BD_INAF9[4,3]+SUB_BD_INAF9[5,3]+SUB_BD_INAF9[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF9[2,3], # Casos con version final
  1, 5, SUB_BD_INAF9[3,3], # Casos sin verión final
  3, 6, SUB_BD_INAF9[4,3], # Casos con revisión de Érika
  3, 7, SUB_BD_INAF9[5,3], # Casos sin revisión de Érika
  3, 8, SUB_BD_INAF9[6,3]), # Casos con cargo (Revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")
rownames(SUB_BD_INAF11) <- 1:nrow(SUB_BD_INAF11)

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.4) SANKEY-CIND.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"




# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(SUB_BD_INAF9, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

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
SUB_BD_INAF10=as.data.frame(subset(BD_INAF,SUB_SECT=="CPES"))
SUB_BD_INAF10$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF10=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF10),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF10$AUX2=paste(round(100*SUB_BD_INAF10$AUX/sum(SUB_BD_INAF10$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF10)=c("Etapa","Estado","Registros","%del total")

#CORRIGIENDO "ERRORES" DE LA DATA
if (paste(SUB_BD_INAF10$Etapa,SUB_BD_INAF10$Estado)!="CSIG Observado") {SUB_BD_INAF10=rbind(SUB_BD_INAF10,c("CSIG","Observado",0,"0%"))}
SUB_BD_INAF10$Registros=as.numeric(SUB_BD_INAF10$Registros)

#REORDENANDO
SUB_BD_INAF10=SUB_BD_INAF10[c(1:4,6,5),]
rownames(SUB_BD_INAF10) <- 1:nrow(SUB_BD_INAF10)


#Mejorando un poco el contenido
SUB_BD_INAF10$Etapa[SUB_BD_INAF10$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF10$Etapa[SUB_BD_INAF10$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF10$Etapa[SUB_BD_INAF10$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF10$Estado[SUB_BD_INAF10$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF10$Estado[SUB_BD_INAF10$Estado=="EN REVISION"]="En revisión"
SUB_BD_INAF10$Estado[SUB_BD_INAF10$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF10$Estado[SUB_BD_INAF10$Estado=="VALIDADO"]="Validado"


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
  0, 1, SUB_BD_INAF10[2,3]+SUB_BD_INAF10[3,3]+SUB_BD_INAF10[4,3]+SUB_BD_INAF10[5,3]+SUB_BD_INAF10[6,3], # Casos que tienen primera versión
  0, 2, SUB_BD_INAF10[1,3], # Casos que no tienen primera versión.
  1, 3, SUB_BD_INAF10[4,3]+SUB_BD_INAF10[5,3]+SUB_BD_INAF10[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF10[2,3], # Casos con version final
  1, 5, SUB_BD_INAF10[3,3], # Casos sin verión final
  3, 6, SUB_BD_INAF10[4,3], # Casos con revisión de Érika
  3, 7, SUB_BD_INAF10[5,3], # Casos sin revisión de Érika
  3, 8, SUB_BD_INAF10[6,3]), # Casos con cargo (Revisión de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.5) SANKEY-CPES.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"




# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(SUB_BD_INAF10, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

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
SUB_BD_INAF11=as.data.frame(subset(BD_INAF,SUB_SECT=="CRES"))
SUB_BD_INAF11$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF11=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF11),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF11$AUX2=paste(round(100*SUB_BD_INAF11$AUX/sum(SUB_BD_INAF11$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF11)=c("Etapa","Estado","Registros","%del total")

#CORRIGIENDO "ERRORES" DE LA DATA
if (paste(SUB_BD_INAF11$Etapa,SUB_BD_INAF11$Estado)!="CSIG Observado") {SUB_BD_INAF11=rbind(SUB_BD_INAF11,c("CSIG","Observado",0,"0%"))}
if (paste(SUB_BD_INAF11$Etapa,SUB_BD_INAF11$Estado)!="Coordinador Observado") {SUB_BD_INAF11=rbind(SUB_BD_INAF11,c("Coordinador","Observado",0,"0%"))}
if (paste(SUB_BD_INAF11$Etapa,SUB_BD_INAF11$Estado)!="Coordinador En revisión") {SUB_BD_INAF11=rbind(SUB_BD_INAF11,c("Coordinador","En revisión",0,"0%"))}
SUB_BD_INAF11$Registros=as.numeric(SUB_BD_INAF11$Registros)

#Mejorando un poco el contenido
SUB_BD_INAF11$Etapa[SUB_BD_INAF11$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF11$Etapa[SUB_BD_INAF11$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF11$Etapa[SUB_BD_INAF11$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF11$Estado[SUB_BD_INAF11$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF11$Estado[SUB_BD_INAF11$Estado=="EN REVISION"]="En revisión"
SUB_BD_INAF11$Estado[SUB_BD_INAF11$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF11$Estado[SUB_BD_INAF11$Estado=="VALIDADO"]="Validado"

#REORDENANDO
SUB_BD_INAF11=SUB_BD_INAF11[c(1,6,5,2,3,4),]
rownames(SUB_BD_INAF11) <- 1:nrow(SUB_BD_INAF11)


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
  0, 1, SUB_BD_INAF11[2,3]+SUB_BD_INAF11[3,3]+SUB_BD_INAF11[4,3]+SUB_BD_INAF11[5,3]+SUB_BD_INAF11[6,3], # Casos que tienen primera versión
  0, 2, SUB_BD_INAF11[1,3], # Casos que no tienen primera versión.
  1, 3, SUB_BD_INAF11[4,3]+SUB_BD_INAF11[5,3]+SUB_BD_INAF11[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF11[2,3], # Casos con version final
  1, 5, SUB_BD_INAF11[3,3], # Casos sin verión final
  3, 6, SUB_BD_INAF11[4,3], # Casos con revisión de Érika
  3, 7, SUB_BD_INAF11[5,3], # Casos sin revisión de Érika
  3, 8, SUB_BD_INAF11[6,3]), # Casos con cargo (Revisión de CMIN/DSEM)
  byrow = T, ncol = 3))




names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 30, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1

#GRABANDO
saveNetwork(SANK1, "SANK1.html") #Crea un archivo html "temporal" (xq nosotros lo eliminaremos al final)
webshot("SANK1.html", "5.6) SANKEY-CRES.jpeg",vwidth = 1200,vheight = 900) #Guarda el archivo 
unlink("SANK1.html") #Elimina el "temporal"



# ########################## #
# ##   GRAFICANDO TABLA   ## #
# ########################## #
TAB1=
  
  formattable(SUB_BD_INAF11, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

TAB1

#GUARDANDO TABLA
write(paste(html_header, TAB1, sep=""), "./TAB1.html")
webshot("TAB1.html", "6.6) AVANCE-CRES.jpeg",vwidth = 400,vheight = 270) #Guarda el archivo 
unlink("TAB1.html") #Elimina el "temporal"

#####################################


# __________________________________________________________________________________________________________________________________________________________

# ############### ###
#     Guardado    ####
# ############### ###

#Extraer el "data frame"
TEMP <- as.data.frame(BD_INAF_CRIT)

#EXPORTANDO EN FORMATO R
#save(TEMP, file="BD_INAF.rdata")

#EXPORTANDO ARCHIVO DE TRABAJO
write.csv(TEMP,file=archivo, na="")

#EXPORTANDO BACKUP
# write.csv(TEMP,file=paste(backup,archivo,sep = ""), na="")


#####################






