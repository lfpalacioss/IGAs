###############################################################444
#########  MONITOREO DE LOS AVANCES DE LA BD IGA's  ###########
###########################  BY LF  ###########################
###############################################################


#######################
###  SETEO GENERAL  ###
#######################

#BORRAR TODO LO ANTERIOR
rm(list=ls())

#INFO DEL USUARIO
USER=Sys.info()
USER=USER[7]

#LIBRERIA
LIBRERIA=c(paste("C:/Users/",USER,"/Google Drive/R/3) Library/win-library/3.6",sep = ""))
.libPaths(LIBRERIA)

#SETEO DEL WD
setwd(paste('C:/Users/',USER,'/Google Drive/R/2) BD/1) OEFA/2) IGAs',sep = ""))

#SETEO DE RUTA PARA BACKUP
#backup=paste('C:\\Users\\',USER,'\\Google Drive\\R\\2) BD\\1) OEFA\\3) BOLETIN\\1) BACK UP\\1) 2020\\1) TRIMESTRE 1\\',sep = "")



#ACTIVAR LOS PAQUETES SIGUIENTES
library(easypackages)
pqt<- c("networkD3","data.table","formattable","fmsb","ggridges","doBy","xts","zoo","mgsub","sf","tidyr","tidyverse","readxl", "WriteXLS",
        "openxlsx", "Rserve", "reshape2", "stringr","foreign","ggplot2","forcats","rgdal",
        "mapproj", "forecast","lmtest","vars","packcircles","scales","safer","gganimate","ggeasy") 

libraries(pqt)


#FUENTE
FUENTE<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRafB2LaFQ0YIPfK5j1ofCaBz4oRz3-1Jqt3c9UST-WnJ7j2D0tbEMsPhTDBd5qhlk6gnnCXlw_CQDy/pub?output=xlsx"




######################################
###########  COLORES_OEFA  ###########
######################################

OEFA_1 = "#144AA7"
OEFA_2 = "#1d85bf"
OEFA_3 = "#0BC7E0"
OEFA_4 = "#44bfb5"
OEFA_5 = "#8CCD3A"
OEFA_6 = "#696A6A"
OEFA_7 = "#FFB500"



#############################################################################################################################################################
#############################################################################################################################################################

###########################
#####  IGAS DEL INAF  #####
###########################


#SETEANDO LA DIRECCION DEL DRIVE
archivo='IGAS_INAF.csv'

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR 
download.file(FUENTE,tp1,mode ="wb")

#SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_INAF<-read_xlsx(path = tp1, sheet = "BD_INAF")


#ELIMINAR UNA VARIABLE
#BD_INAF <- BD_INAF[ ,-(14:16)]
BD_INAF <- BD_INAF[ ,!colnames(BD_INAF)=="Certificador"]
BD_INAF <- BD_INAF[ ,!colnames(BD_INAF)=="Consultora ambiental"]
BD_INAF <- BD_INAF[ ,!colnames(BD_INAF)=="Documento de aprobaci?n"]


#RENOMBRANDO VARIABLES
names(BD_INAF)[names(BD_INAF) == 'Nombre del instrumento'] <- 'N_INST'
names(BD_INAF)[names(BD_INAF) == 'Administrado'] <- 'ADM_ACT'
names(BD_INAF)[names(BD_INAF) == 'Administrado original'] <- 'ADM_ORIG'
names(BD_INAF)[names(BD_INAF) == 'Unidad fiscalizable'] <- 'UF'
names(BD_INAF)[names(BD_INAF) == 'Subsector'] <- 'SUB_SECT'
names(BD_INAF)[names(BD_INAF) == 'Tipo de documento de aprobaci?n'] <- 'T_DOC_APROB'
names(BD_INAF)[names(BD_INAF) == 'Fecha de aprobaci?n'] <- 'F_APROB'
names(BD_INAF)[names(BD_INAF) == 'Archivos registrados'] <- 'N_ARCH'
names(BD_INAF)[names(BD_INAF) == 'Estado'] <- 'ESTADO'
names(BD_INAF)[names(BD_INAF) == 'Usuario de registro'] <- 'REGISTRADOR'
names(BD_INAF)[names(BD_INAF) == 'Fecha de registro'] <- 'F_REG'
names(BD_INAF)[names(BD_INAF) == 'COD ADMINISTRADO'] <- 'COD_ADM'
names(BD_INAF)[names(BD_INAF) == 'COD UF'] <- 'COD_UF'


#ELIMINANDO VACIOS
BD_INAF= BD_INAF[!is.na(BD_INAF$N_INST), ]



#TRANSFORMANDO VARIABLES
BD_INAF$F_APROB=as.Date(as.numeric(BD_INAF$F_APROB,na.rm=TRUE),origin="1899-12-30")
BD_INAF$ETAPA=NA
BD_INAF$SUB_SECT_0=NA
BD_INAF$ESTADO=gsub("\n", "", BD_INAF$ESTADO)


#REEMPLAZAR LOS VALORES POR VALORES UN POCO MAS SIMPLES DE LEER
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Electricidad"]="CELE"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Miner?a"]="CMIN"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Hidrocarburos"]="CHID"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Industria"]="CIND"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Agricultura"]="CAGR"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Pesquer?a"]="CPES"
BD_INAF$SUB_SECT[BD_INAF$SUB_SECT=="Residuos S?lidos"]="CRES"


BD_INAF$SUB_SECT_0[BD_INAF$SUB_SECT=="CELE"|BD_INAF$SUB_SECT=="CMIN"|BD_INAF$SUB_SECT=="CHID"]="DSEM"
BD_INAF$SUB_SECT_0[BD_INAF$SUB_SECT=="CIND"|BD_INAF$SUB_SECT=="CAGR"|BD_INAF$SUB_SECT=="CPES"]="DSAP"
BD_INAF$SUB_SECT_0[BD_INAF$SUB_SECT=="CRES"]="DSIS"


BD_INAF$ETAPA[BD_INAF$ESTADO=="Pendiente de revisi?n"]="1) REGISTRADOR"
BD_INAF$ETAPA[BD_INAF$ESTADO=="En revisi?n [Coordinador]"|BD_INAF$ESTADO=="Observado [Coordinador]"]="2) COORDINADOR"
BD_INAF$ETAPA[BD_INAF$ESTADO=="En revisi?n [Especialista CSIG]"|BD_INAF$ESTADO=="Observado [Especialista CSIG]"|BD_INAF$ESTADO=="Validado [Especialista CSIG]"]="3) ESPECIALISTA CSIG"


BD_INAF$ESTADO[BD_INAF$ESTADO=="Pendiente de revisi?n"]="PENDIENTE DE REVISION"
BD_INAF$ESTADO[BD_INAF$ESTADO=="En revisi?n [Coordinador]"]="EN REVISION"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Observado [Coordinador]"]="OBSERVADO"
BD_INAF$ESTADO[BD_INAF$ESTADO=="En revisi?n [Especialista CSIG]"]="EN REVISION"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Observado [Especialista CSIG]"]="OBSERVADO"
BD_INAF$ESTADO[BD_INAF$ESTADO=="Validado [Especialista CSIG]"]="VALIDADO"




#############################################################################################################################################################
#############################################################################################################################################################

# ###########################
# #####  BD ADMIN + UF  #####
# ###########################
# 
# #FUENTE
# FUENTE<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRXsNh99WAG8QMff7O0EFbBXg4B4JHGUnX7E08U1wom2MukzoWNn-XgEXzY3Y_y0gwkCJtrS2J2-SDJ/pub?output=xlsx"
# 
# 
# #GENERANDO UN TEMPORAL
# tp1<-tempfile()
# 
# #DESCARGAR 
# download.file(FUENTE,tp1,mode ="wb")
# 
# #SELECCIONAR LA PESTA?A DEL TEMPORAL
# UF_ADMIN<-read_xlsx(path = tp1, sheet = "UF_ADMIN")
# 
# 
# #ELIMINAR UNA VARIABLE
# UF_ADMIN <- UF_ADMIN[ ,-c(1,3:4,6:11,16:19,23)]
# 
# #RENOMBRANDO VARIABLES
# names(UF_ADMIN)[names(UF_ADMIN) == 'C?DIGO'] <- 'COD_ADM'
# names(UF_ADMIN)[names(UF_ADMIN) == 'NOMBRE O RAZ?N SOCIAL'] <- 'NOM_ADM'
# names(UF_ADMIN)[names(UF_ADMIN) == 'NOMBRE'] <- 'NOM_UF'
# names(UF_ADMIN)[names(UF_ADMIN) == 'C?DIGO OEFA'] <- 'COD_UF'
# names(UF_ADMIN)[names(UF_ADMIN) == 'DEPARTAMENTO3'] <- 'DEP'
# names(UF_ADMIN)[names(UF_ADMIN) == 'PROVINCIA4'] <- 'PROV'
# names(UF_ADMIN)[names(UF_ADMIN) == 'DISTRITO5'] <- 'DIST'




#############################################################################################################################################################
#############################################################################################################################################################

###############################
#####  BD CASOS CRITICOS  #####
###############################

#FUENTE
FUENTE<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQeH0CEF06jjKnKpeu3mylLMi0PA5OKjubQzxKKb8ZCODBKHnyXfnExFVaywIMl5woO_HakkNQIucSI/pub?output=xlsx"


#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR 
download.file(FUENTE,tp1,mode ="wb")

#SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_CRITICAS<-read_xlsx(path = tp1, sheet = "ORIGINAL")


#ELIMINAR UNA VARIABLE
BD_CRITICAS <- BD_CRITICAS[ ,-c(1,9,10)]

#RENOMBRANDO VARIABLES
names(BD_CRITICAS)[names(BD_CRITICAS) == 'ADMINISTRADO (SEGUN INAF)'] <- 'NOM_ADM'
names(BD_CRITICAS)[names(BD_CRITICAS) == 'CODIGO DEL ADMINISTRADO'] <- 'COD_ADM'
names(BD_CRITICAS)[names(BD_CRITICAS) == 'UNIDAD FISCALIZABLE (SEGUN INAF)'] <- 'NOM_UF'
names(BD_CRITICAS)[names(BD_CRITICAS) == 'CODIGO DE LA UNIDAD FISCALIZABLE'] <- 'COD_UF'
names(BD_CRITICAS)[names(BD_CRITICAS) == 'BREVE DESCRIPCI?N (OPCIONAL)'] <- 'DESCRIP'
#names(BD_CRITICAS)[names(BD_CRITICAS) == 'PROVINCIA4'] <- 'PROV'


#REEMPLAZAR LOS VALORES POR VALORES UN POCO MAS SIMPLES DE LEER
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Electricidad"]="CELE"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Miner?a"]="CMIN"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Hidrocarburos"]="CHID"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Industria"]="CIND"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Agricultura"]="CAGR"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Pesca"]="CPES"
BD_CRITICAS$SUBSECTOR[BD_CRITICAS$SUBSECTOR=="Residuos S?lidos"]="CRES"




#############################################################################################################################################################
#############################################################################################################################################################

########################################
#####  UNIENDO LAS BD EN UNA SOLA  #####
########################################


#CREAMOS AUXILIARES
BD_INAF$AUX=paste(BD_INAF$COD_UF,BD_INAF$SUB_SECT,sep = "-")
BD_CRITICAS$AUX=paste(BD_CRITICAS$COD_UF,BD_CRITICAS$SUBSECTOR,sep = "-")

#COMBINAR CON OTRAS COLUMNAS DE TABLA TAREAS
BD_INAF=merge(BD_INAF,BD_CRITICAS,
          by.x="AUX",
          by.y="AUX",
          all.x=T)





BD_INAF$UF_CRITICA=NA
BD_INAF$UF_CRITICA[is.na(BD_INAF$FUENTE)==T]="NO"
BD_INAF$UF_CRITICA[is.na(BD_INAF$FUENTE)==F]="SI"





#############################################################################################################################################################
#############################################################################################################################################################

###############################################
#####  CREANDO UNAS FORMULAS PARA CONTEO  #####
###############################################

fun1 <- function(x){
  c(cuenta=sum((x)))
}


fun <- function(x){
  c(cuenta=sum(!is.na(x)))
}




fecha_inicio=format(as.Date(min(BD_INAF$F_REG),"%Y-%m-%d"),"%d/%m/%Y")
fecha_final=format(as.Date(max(BD_INAF$F_REG),"%Y-%m-%d"),"%d/%m/%Y")




#############################################################################################################################################################
#############################################################################################################################################################

#####################################################
#####  Graficando avances o: Registros totales  #####
#####################################################

#GENERANDO LA NUEVA BD
SUB_BD_INAF=as.data.frame(BD_INAF)
SUB_BD_INAF$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF=summaryBy(AUX ~ F_REG, FUN=fun1, data =as.data.frame(SUB_BD_INAF),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T

# #GR?FICO EN NIVELES
# AVANCE_REG_DIA_TOT <- ggplot(SUB_BD_INAF,aes(F_REG, AUX)) +
#   geom_line(color=OEFA_1, size=1.5) +
#   #stat_smooth(se=FALSE)+
#   theme(legend.position = "bottom")+
#   theme_minimal()+
#   labs(x="",
#        y="Cantidad de registros",
#        title = "Evoluci?n de la cantidad de registros en INAF.",
#        subtitle = paste("(Del ",
#                         fecha_inicio,
#                         " al ",
#                         fecha_final,
#                         sep = ""),
#        caption = "Fuente: INAF\nElaboraci?n: Propia")
#   #theme(legend.position="none")
#   #scale_fill_manual(values=c(OEFA_1,OEFA_2,OEFA_3,OEFA_4,OEFA_5,OEFA_6,OEFA_7,OEFA_1,OEFA_2))+
#   
# AVANCE_REG_DIA_TOT
# 
# 
# #CREANDO GIF
# AVANCE_REG_DIA_TOT=AVANCE_REG_DIA_TOT + transition_reveal(F_REG)#+
#   #labs(title = "Year: {frame_time}")+
#   #shadow_wake(wake_length = 0.1, alpha = FALSE)
# 
# animate(AVANCE_REG_DIA_TOT, duration = 8, fps = 18, width = 2200, height = 1600, res=250, renderer = gifski_renderer())
# 
# anim_save("G1_niveles.gif")





#GR?FICO ACUMULADO
AVANCE_REG_DIA_TOT <- ggplot(SUB_BD_INAF,aes(F_REG, cumsum(AUX))) +
  geom_line(color=OEFA_1, size=1.5) +
  #stat_smooth(se=FALSE)+
  theme(legend.position = "bottom")+
  theme_minimal()+
  labs(x="",
       y="Cantidad de registros",
       title = "Evoluci?n de la cantidad de registros en INAF (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboraci?n: Propia")+
  ggeasy::easy_rotate_x_labels(angle = 90)
#theme(legend.position="none")
#scale_fill_manual(values=c(OEFA_1,OEFA_2,OEFA_3,OEFA_4,OEFA_5,OEFA_6,OEFA_7,OEFA_1,OEFA_2))+

AVANCE_REG_DIA_TOT


#CREANDO GIF
AVANCE_REG_DIA_TOT=AVANCE_REG_DIA_TOT + transition_reveal(F_REG)#+
#labs(title = "Year: {frame_time}")+
#shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(AVANCE_REG_DIA_TOT, duration = 8, fps = 15, width = 2200, height = 1600, res=250, renderer = gifski_renderer())

anim_save("1) Avance acumulado total.gif")




#############################################################################################################################################################
#############################################################################################################################################################

################################################
#####  Graficando avances o: SOLO UF_CRIT  #####
################################################


#GENERANDO LA NUEVA BD
SUB_BD_INAF_2=as.data.frame(subset(BD_INAF,UF_CRITICA=="SI"))
SUB_BD_INAF_2$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF_2=summaryBy(AUX ~ F_REG, FUN=fun1, data =as.data.frame(SUB_BD_INAF_2),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T

#GR?FICO ACUMULADO SOLO UF CRITICAS
AVANCE_REG_DIA_TOT_UF_CRIT <- ggplot(SUB_BD_INAF_2,aes(F_REG, cumsum(AUX))) +
  geom_line(color=OEFA_3, size=1.5) +
  #stat_smooth(se=FALSE)+
  theme(legend.position = "bottom")+
  theme_minimal()+
  labs(x="",
       y="Cantidad de registros",
       title = "Evoluci?n de la cantidad de registros en INAF relacionados a UFC*(Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboraci?n: Propia\n*UFC: Unidades Fiscalizables Cr?ticas")
#theme(legend.position="none")
#scale_fill_manual(values=c(OEFA_1,OEFA_2,OEFA_3,OEFA_4,OEFA_5,OEFA_6,OEFA_7,OEFA_1,OEFA_2))+

AVANCE_REG_DIA_TOT_UF_CRIT


#CREANDO GIF
AVANCE_REG_DIA_TOT_UF_CRIT=AVANCE_REG_DIA_TOT_UF_CRIT + transition_reveal(F_REG)#+
#labs(title = "Year: {frame_time}")+
#shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(AVANCE_REG_DIA_TOT_UF_CRIT, duration = 8, fps = 15, width = 2200, height = 1600, res=250, renderer = gifski_renderer())

anim_save("2) Avance acumulado UFC.gif")




#############################################################################################################################################################
#############################################################################################################################################################

###############################################################
#####  Graficando avances o: Registros totales + UF_CRIT  #####
###############################################################

#GENERANDO LA NUEVA BD
SUB_BD_INAF_3=as.data.frame(BD_INAF)
SUB_BD_INAF_3$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF_3=summaryBy(AUX ~ F_REG+UF_CRITICA, FUN=fun1, data =as.data.frame(SUB_BD_INAF_3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF_3 = mutate(group_by(SUB_BD_INAF_3,UF_CRITICA),"AUX2" = cumsum(AUX))

#GR?FICO ACUMULADO
AVANCE_REG_DIA_TOT_Y_UFC <- ggplot(SUB_BD_INAF_3,aes(F_REG, AUX2, group = UF_CRITICA, color = factor(UF_CRITICA))) +
  geom_line(size=1.5) +
  scale_color_manual(values=c(OEFA_1,OEFA_3),labels=c("Totales","UF Cr?ticas"),
                     name = "")+

  #stat_smooth(se=FALSE)+
  theme_minimal()+
  theme(legend.position="top")+
  labs(x="",
       y="Cantidad de registros",
       title = "Evoluci?n de la cantidad de registros en INAF (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboraci?n: Propia")

AVANCE_REG_DIA_TOT_Y_UFC


#CREANDO GIF
AVANCE_REG_DIA_TOT_Y_UFC=AVANCE_REG_DIA_TOT_Y_UFC + transition_reveal(F_REG)#+
#labs(title = "Year: {frame_time}")+
#shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(AVANCE_REG_DIA_TOT_Y_UFC, duration = 8, fps = 15, width = 2200, height = 1600, res=250, renderer = gifski_renderer())

anim_save("3) Avance acumulado Totales y UFC.gif")





#############################################################################################################################################################
#############################################################################################################################################################

#######################################################################
#####  Graficando avances 1: Registros totales, por coordinaci?n  #####
#######################################################################

#GENERANDO LA NUEVA BD
SUB_BD_INAF1=as.data.frame(BD_INAF)
SUB_BD_INAF1$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF1=summaryBy(AUX ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF1),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF1 = mutate(group_by(SUB_BD_INAF1,SUB_SECT),"AUX2" = cumsum(AUX))


AVANCE_REG_DIA <- ggplot(SUB_BD_INAF1,aes(F_REG, AUX2, group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1.5) +
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  theme_minimal()+
  labs(x="",
       y="Cantidad de registros",
       title = "Evoluci?n de la cantidad de registros en INAF,\npor coordinaci?n (Acumulado).",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboraci?n: Propia")+
  theme(legend.position = "bottom",legend.title = element_blank())
  
AVANCE_REG_DIA

#CREANDO GIF
AVANCE_REG_DIA=AVANCE_REG_DIA + transition_reveal(F_REG)#+
#labs(title = "Year: {frame_time}")+
#shadow_wake(wake_length = 0.1, alpha = FALSE)


animate(AVANCE_REG_DIA, duration = 8, fps = 15, width = 2200, height = 1600, res=250, renderer = gifski_renderer())

anim_save("4) Evoluci?n por area.gif")



#############################################################################################################################################################
#############################################################################################################################################################

########################################################################
#####  Graficando avances 2: AVANCE EN EL REGISTRO DE UF CON IGAS  #####
########################################################################


#GENERANDO LA NUEVA BD
SUB_BD_INAF2=as.data.frame(BD_INAF)
SUB_BD_INAF2$AUX=1

#COMO ES POR UF, QUITAMOS LOS DUPLICADOS
SUB_BD_INAF2=SUB_BD_INAF2[!duplicated(SUB_BD_INAF2$UF),]

#CREAR TABLA RESUMEN
SUB_BD_INAF2=summaryBy(AUX ~ F_REG+SUB_SECT, FUN=c(fun1), data =as.data.frame(SUB_BD_INAF2),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T

#CREANDO CAMPO CONTEO ACUMULADO
SUB_BD_INAF2 = mutate(group_by(SUB_BD_INAF2,SUB_SECT),"AUX2" = cumsum(AUX))


#GENERANDO GR?FICO EN NIVELES
AVANCE_UF_DIA <- ggplot(SUB_BD_INAF2,aes(F_REG, AUX, group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1.5) +
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  theme_minimal()+
  labs(x="",
       y="Cantidad de registros",
       title = "Evoluci?n de la cantidad de Unidades Fiscalizables registradas en INAF con al menos un IGA,\npor coordinaci?n.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboraci?n: Propia")+
  theme(legend.position = "bottom",legend.title = element_blank())
AVANCE_UF_DIA

#CREANDO GIF
AVANCE_UF_DIA=AVANCE_UF_DIA + transition_reveal(F_REG)#+
#labs(title = "Year: {frame_time}")+
#shadow_wake(wake_length = 0.1, alpha = FALSE)


animate(AVANCE_UF_DIA, duration = 8, fps = 15, width = 2200, height = 1600, res=250, renderer = gifski_renderer())

# Save at gif:
anim_save("G3_Niveles.gif")



#GENERANDO GR?FICO ACUMULADO
AVANCE_UF_DIA <- ggplot(SUB_BD_INAF2,aes(F_REG, AUX2, group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1.5) +
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_6,OEFA_5, OEFA_7, OEFA_4))+
  theme_minimal()+
  labs(x="",
       y="Cantidad de registros",
       title = "Evoluci?n de la cantidad de Unidades Fiscalizables registradas en INAF con al menos un IGA,\npor coordinaci?n. (Acumulado)",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboraci?n: Propia")+
  theme(legend.position = "bottom",legend.title = element_blank())
AVANCE_UF_DIA

#CREANDO GIF
AVANCE_UF_DIA=AVANCE_UF_DIA + transition_reveal(F_REG)#+
#labs(title = "Year: {frame_time}")+
#shadow_wake(wake_length = 0.1, alpha = FALSE)


animate(AVANCE_UF_DIA, duration = 8, fps = 15, width = 2200, height = 1600, res=250, renderer = gifski_renderer())

# Save at gif:
anim_save("G3_Acumulado.gif")



#############################################################################################################################################################
#############################################################################################################################################################

########################################################
#####  Graficando avances 3: REGISTROS PER CAPITA  #####
########################################################


#GENERANDO LA NUEVA BD
SUB_BD_INAF3=as.data.frame(BD_INAF)
SUB_BD_INAF3$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF3=summaryBy(AUX ~ F_REG+SUB_SECT+REGISTRADOR, FUN=fun1, data =as.data.frame(SUB_BD_INAF3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF3$AUX2=1
SUB_BD_INAF3=summaryBy(AUX+AUX2 ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF3),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF3$REGISTROS_PERCAP=SUB_BD_INAF3$AUX/SUB_BD_INAF3$AUX2

#GRAFICAR

AVANCE_REG_PERCAP_DIA <- ggplot(SUB_BD_INAF3,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  #geom_smooth(method = "lm", formula = y~x, se=F, color= "gray40")+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_2,OEFA_5, OEFA_7, OEFA_4))+
  #theme_minimal()+
  ylim(0,30)+
  labs(x="",
       y="Cantidad de registros por registrador",
       title = "Evoluci?n de la cantidad de registros per c?pita* en INAF, por coordinaci?n**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboraci?n: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realiz? la migraci?n de informaci?n del DRIVE generando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom",legend.title = element_blank())+
  facet_wrap(.~SUB_SECT, 
             scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  ggeasy::easy_rotate_x_labels(angle=90)


AVANCE_REG_PERCAP_DIA


#CREANDO GIF
AVANCE_REG_PERCAP_DIA=AVANCE_REG_PERCAP_DIA + transition_reveal(F_REG)#+

# gganimate specific bits:
  #labs(title = "Year: {frame_time}")+
  #shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(AVANCE_REG_PERCAP_DIA, duration = 8, fps = 15, width = 2800, height = 1600, res=250, renderer = gifski_renderer())

# Save at gif:
anim_save("5) Llenado per c?pita por ?rea.gif")


# png(filename=paste(path,"/hires.png",sep=""), width=5, height=5,
#     units="in", res=300, pointsize=12)




########################################################
#####  Graficando avances 3: REGISTROS PER CAPITA  #####
########################################################


#GENERANDO LA NUEVA BD
SUB_BD_INAF5=as.data.frame(BD_INAF)
SUB_BD_INAF5$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF5=summaryBy(AUX ~ F_REG+SUB_SECT+REGISTRADOR, FUN=fun1, data =as.data.frame(SUB_BD_INAF5),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF5$AUX2=1
SUB_BD_INAF5=summaryBy(AUX+AUX2 ~ F_REG+SUB_SECT, FUN=fun1, data =as.data.frame(SUB_BD_INAF5),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF5$REGISTROS_PERCAP=SUB_BD_INAF5$AUX/SUB_BD_INAF5$AUX2

#GRAFICAR

AVANCE_REG_PERCAP_DIA <- ggplot(SUB_BD_INAF5,aes(F_REG, (REGISTROS_PERCAP), group = SUB_SECT, color = factor(SUB_SECT))) +
  geom_line(size=1) +
  geom_smooth(method = "loess", formula = y~x, se=T, color= "firebrick",fill="gray70")+
  scale_color_manual(values=c(OEFA_1,OEFA_3,OEFA_2,OEFA_5, OEFA_7, OEFA_4))+
  #theme_minimal()+
  ylim(0,30)+
  labs(x="",
       y="Cantidad de registros por registrador",
       title = "Evoluci?n de la cantidad de registros per c?pita* en INAF, por coordinaci?n**.",
       subtitle = paste("(Del ",
                        fecha_inicio,
                        " al ",
                        fecha_final,
                        sep = ""),
       caption = "Fuente: INAF\nElaboraci?n: Propia\n*Nota 1: Cantidad promedio de registros por cada registrador\n*Nota 2: Entre la segunda y tercera semana de mayo se realiz? la migraci?n de informaci?n del DRIVE generando picos de llenado, por lo que el eje Y ha sido acotado al valor 40")+
  theme(legend.position = "bottom",legend.title = element_blank())+
  facet_wrap(.~SUB_SECT, 
             scales="free_x",
             nrow = 1,
             labeller = label_value,
             strip.position = "bottom")+
  ggeasy::easy_rotate_x_labels(angle=90)

AVANCE_REG_PERCAP_DIA

ggsave("8) Tendencia llenado per c?pita por ?rea.jpg",  width = 12, height = 7)

########################################################
#####  Graficando avances 3: REGISTROS PER CAPITA  #####
########################################################


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
SUB_BD_INAF4$Estado[SUB_BD_INAF4$Estado=="EN REVISION"]="En revisi?n"
SUB_BD_INAF4$Estado[SUB_BD_INAF4$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF4$Estado[SUB_BD_INAF4$Estado=="VALIDADO"]="Validado"



#############################################
#####  GR?FICO DE SANKEY (PORCENTAJES)  #####
#############################################


#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       
                       "Aprobado", #Tercer nivel (3)
                       "En revisi?n", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       
                       "En revisi?n", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, SUB_BD_INAF4[2,3]+SUB_BD_INAF4[3,3]+SUB_BD_INAF4[4,3]+SUB_BD_INAF4[5,3]+SUB_BD_INAF4[6,3], # Casos que tienen primera versi?n
  0, 2, SUB_BD_INAF4[1,3], # Casos que no tienen primera versi?n.
  1, 3, SUB_BD_INAF4[4,3]+SUB_BD_INAF4[5,3]+SUB_BD_INAF4[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF4[2,3], # Casos con version final
  1, 5, SUB_BD_INAF4[3,3], # Casos sin veri?n final
  3, 6, SUB_BD_INAF4[4,3], # Casos con revisi?n de ?rika
  3, 7, SUB_BD_INAF4[5,3], # Casos sin revisi?n de ?rika
  3, 8, SUB_BD_INAF4[6,3]), # Casos con cargo (Revisi?n de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              #colourScale=OEFA_1,
              units = "%",
              fontSize= 14, nodeWidth = 20,
              sinksRight=F,nodePadding=10,iterations=100)

SANK1




########################################
#####  GRAFICANDO UNA TABLITA XVR  #####
########################################
TAB1=
  
  formattable(SUB_BD_INAF4, align=c("l","l","c","r"),
            list(`Indicator Name` = formatter("span", 
                                              style = ~ style(color = "grey",
                                                              font.weight = "bold")),
                 Registros=color_tile("#DeF7E9", "#71CA97")))

TAB1



#######################################################################################################################################################
#######################################################################################################################################################


###############################################################
#####  Graficando avances 3: REGISTROS PER CAPITA (CMIN)  #####
###############################################################


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
SUB_BD_INAF6$Estado[SUB_BD_INAF6$Estado=="EN REVISION"]="En revisi?n"
SUB_BD_INAF6$Estado[SUB_BD_INAF6$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF6$Estado[SUB_BD_INAF6$Estado=="VALIDADO"]="Validado"



#############################################
#####  GR?FICO DE SANKEY (PORCENTAJES)  #####
#############################################


#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       
                       "Aprobado", #Tercer nivel (3)
                       "En revisi?n", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       
                       "En revisi?n", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, SUB_BD_INAF6[2,3]+SUB_BD_INAF6[3,3]+SUB_BD_INAF6[4,3]+SUB_BD_INAF6[5,3]+SUB_BD_INAF6[6,3], # Casos que tienen primera versi?n
  0, 2, SUB_BD_INAF6[1,3], # Casos que no tienen primera versi?n.
  1, 3, SUB_BD_INAF6[4,3]+SUB_BD_INAF6[5,3]+SUB_BD_INAF6[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF6[2,3], # Casos con version final
  1, 5, SUB_BD_INAF6[3,3], # Casos sin veri?n final
  3, 6, SUB_BD_INAF6[4,3], # Casos con revisi?n de ?rika
  3, 7, SUB_BD_INAF6[5,3], # Casos sin revisi?n de ?rika
  3, 8, SUB_BD_INAF6[6,3]), # Casos con cargo (Revisi?n de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 14, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1




########################################
#####  GRAFICANDO UNA TABLITA XVR  #####
########################################
TAB2=
  
  formattable(SUB_BD_INAF6, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

TAB2


#######################################################################################################################################################
#######################################################################################################################################################


###############################################################
#####  Graficando avances 3: REGISTROS PER CAPITA (CMIN)  #####
###############################################################


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
SUB_BD_INAF7$Estado[SUB_BD_INAF7$Estado=="EN REVISION"]="En revisi?n"
SUB_BD_INAF7$Estado[SUB_BD_INAF7$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF7$Estado[SUB_BD_INAF7$Estado=="VALIDADO"]="Validado"



#############################################
#####  GR?FICO DE SANKEY (PORCENTAJES)  #####
#############################################


#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       
                       "Aprobado", #Tercer nivel (3)
                       "En revisi?n", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       
                       "En revisi?n", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, SUB_BD_INAF7[2,3]+SUB_BD_INAF7[3,3]+SUB_BD_INAF7[4,3]+SUB_BD_INAF7[5,3]+SUB_BD_INAF7[6,3], # Casos que tienen primera versi?n
  0, 2, SUB_BD_INAF7[1,3], # Casos que no tienen primera versi?n.
  1, 3, SUB_BD_INAF7[4,3]+SUB_BD_INAF7[5,3]+SUB_BD_INAF7[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF7[2,3], # Casos con version final
  1, 5, SUB_BD_INAF7[3,3], # Casos sin veri?n final
  3, 6, SUB_BD_INAF7[4,3], # Casos con revisi?n de ?rika
  3, 7, SUB_BD_INAF7[5,3], # Casos sin revisi?n de ?rika
  3, 8, SUB_BD_INAF7[6,3]), # Casos con cargo (Revisi?n de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 14, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1




########################################
#####  GRAFICANDO UNA TABLITA XVR  #####
########################################
TAB2=
  
  formattable(SUB_BD_INAF7, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

TAB2


#######################################################################################################################################################
#######################################################################################################################################################


###############################################################
#####  Graficando avances 3: REGISTROS PER CAPITA (CMIN)  #####
###############################################################


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
SUB_BD_INAF8$Estado[SUB_BD_INAF8$Estado=="EN REVISION"]="En revisi?n"
SUB_BD_INAF8$Estado[SUB_BD_INAF8$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF8$Estado[SUB_BD_INAF8$Estado=="VALIDADO"]="Validado"



#############################################
#####  GR?FICO DE SANKEY (PORCENTAJES)  #####
#############################################


#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       
                       "Aprobado", #Tercer nivel (3)
                       "En revisi?n", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       
                       "En revisi?n", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, SUB_BD_INAF8[2,3]+SUB_BD_INAF8[3,3]+SUB_BD_INAF8[4,3]+SUB_BD_INAF8[5,3]+SUB_BD_INAF8[6,3], # Casos que tienen primera versi?n
  0, 2, SUB_BD_INAF8[1,3], # Casos que no tienen primera versi?n.
  1, 3, SUB_BD_INAF8[4,3]+SUB_BD_INAF8[5,3]+SUB_BD_INAF8[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF8[2,3], # Casos con version final
  1, 5, SUB_BD_INAF8[3,3], # Casos sin veri?n final
  3, 6, SUB_BD_INAF8[4,3], # Casos con revisi?n de ?rika
  3, 7, SUB_BD_INAF8[5,3], # Casos sin revisi?n de ?rika
  3, 8, SUB_BD_INAF8[6,3]), # Casos con cargo (Revisi?n de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 14, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1




########################################
#####  GRAFICANDO UNA TABLITA XVR  #####
########################################
TAB2=
  
  formattable(SUB_BD_INAF8, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

TAB2


#######################################################################################################################################################
#######################################################################################################################################################


###############################################################
#####  Graficando avances 3: REGISTROS PER CAPITA (CMIN)  #####
###############################################################


#GENERANDO LA NUEVA BD
SUB_BD_INAF9=as.data.frame(subset(BD_INAF,SUB_SECT=="CIND"))
SUB_BD_INAF9$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF9=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF9),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF9$AUX2=paste(round(100*SUB_BD_INAF9$AUX/sum(SUB_BD_INAF9$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF9)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
SUB_BD_INAF9$Etapa[SUB_BD_INAF9$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF9$Etapa[SUB_BD_INAF9$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF9$Etapa[SUB_BD_INAF9$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF9$Estado[SUB_BD_INAF9$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF9$Estado[SUB_BD_INAF9$Estado=="EN REVISION"]="En revisi?n"
SUB_BD_INAF9$Estado[SUB_BD_INAF9$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF9$Estado[SUB_BD_INAF9$Estado=="VALIDADO"]="Validado"



#############################################
#####  GR?FICO DE SANKEY (PORCENTAJES)  #####
#############################################


#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       
                       "Aprobado", #Tercer nivel (3)
                       "En revisi?n", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       
                       "En revisi?n", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, SUB_BD_INAF9[2,3]+SUB_BD_INAF9[3,3]+SUB_BD_INAF9[4,3]+SUB_BD_INAF9[5,3]+SUB_BD_INAF9[6,3], # Casos que tienen primera versi?n
  0, 2, SUB_BD_INAF9[1,3], # Casos que no tienen primera versi?n.
  1, 3, SUB_BD_INAF9[4,3]+SUB_BD_INAF9[5,3]+SUB_BD_INAF9[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF9[2,3], # Casos con version final
  1, 5, SUB_BD_INAF9[3,3], # Casos sin veri?n final
  3, 6, SUB_BD_INAF9[4,3], # Casos con revisi?n de ?rika
  3, 7, SUB_BD_INAF9[5,3], # Casos sin revisi?n de ?rika
  3, 8, SUB_BD_INAF9[6,3]), # Casos con cargo (Revisi?n de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 14, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1




########################################
#####  GRAFICANDO UNA TABLITA XVR  #####
########################################
TAB2=
  
  formattable(SUB_BD_INAF9, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

TAB2


#######################################################################################################################################################
#######################################################################################################################################################


###############################################################
#####  Graficando avances 3: REGISTROS PER CAPITA (CMIN)  #####
###############################################################


#GENERANDO LA NUEVA BD
SUB_BD_INAF10=as.data.frame(subset(BD_INAF,SUB_SECT=="CPES"))
SUB_BD_INAF10$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF10=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF10),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF10$AUX2=paste(round(100*SUB_BD_INAF10$AUX/sum(SUB_BD_INAF10$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF10)=c("Etapa","Estado","Registros","%del total")

#Mejorando un poco el contenido
SUB_BD_INAF10$Etapa[SUB_BD_INAF10$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF10$Etapa[SUB_BD_INAF10$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF10$Etapa[SUB_BD_INAF10$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF10$Estado[SUB_BD_INAF10$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF10$Estado[SUB_BD_INAF10$Estado=="EN REVISION"]="En revisi?n"
SUB_BD_INAF10$Estado[SUB_BD_INAF10$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF10$Estado[SUB_BD_INAF10$Estado=="VALIDADO"]="Validado"



#############################################
#####  GR?FICO DE SANKEY (PORCENTAJES)  #####
#############################################


#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       
                       "Aprobado", #Tercer nivel (3)
                       "En revisi?n", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       
                       "En revisi?n", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, SUB_BD_INAF10[2,3]+SUB_BD_INAF10[3,3]+SUB_BD_INAF10[4,3]+SUB_BD_INAF10[5,3]+SUB_BD_INAF10[6,3], # Casos que tienen primera versi?n
  0, 2, SUB_BD_INAF10[1,3], # Casos que no tienen primera versi?n.
  1, 3, SUB_BD_INAF10[4,3]+SUB_BD_INAF10[5,3]+SUB_BD_INAF10[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF10[2,3], # Casos con version final
  1, 5, SUB_BD_INAF10[3,3], # Casos sin veri?n final
  3, 6, SUB_BD_INAF10[4,3], # Casos con revisi?n de ?rika
  3, 7, SUB_BD_INAF10[5,3], # Casos sin revisi?n de ?rika
  3, 8, SUB_BD_INAF10[6,3]), # Casos con cargo (Revisi?n de CMIN/DSEM)
  byrow = T, ncol = 3))

names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 14, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1




########################################
#####  GRAFICANDO UNA TABLITA XVR  #####
########################################
TAB2=
  
  formattable(SUB_BD_INAF10, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

TAB2


#######################################################################################################################################################
#######################################################################################################################################################


###############################################################
#####  Graficando avances 3: REGISTROS PER CAPITA (CMIN)  #####
###############################################################


#GENERANDO LA NUEVA BD
SUB_BD_INAF11=as.data.frame(subset(BD_INAF,SUB_SECT=="CRES"))
SUB_BD_INAF11$AUX=1

#CREAR TABLA RESUMEN
SUB_BD_INAF11=summaryBy(AUX ~ ETAPA+ESTADO, FUN=fun1, data =as.data.frame(SUB_BD_INAF11),keep.names = T) #Para mantener el nombre de la variable usar: keep.names = T
SUB_BD_INAF11$AUX2=paste(round(100*SUB_BD_INAF11$AUX/sum(SUB_BD_INAF11$AUX),digits = 1),"%",sep = "")
colnames(SUB_BD_INAF11)=c("Etapa","Estado","Registros","%del total")

if (paste(SUB_BD_INAF11$Etapa,SUB_BD_INAF11$Estado)!="CSIG Observado") {SUB_BD_INAF11=rbind(SUB_BD_INAF11,c("CSIG","Observado",0,"0%"))}

SUB_BD_INAF11$Registros=as.numeric(SUB_BD_INAF11$Registros)

#Mejorando un poco el contenido
SUB_BD_INAF11$Etapa[SUB_BD_INAF11$Etapa=="1) REGISTRADOR"]="Registrador"
SUB_BD_INAF11$Etapa[SUB_BD_INAF11$Etapa=="2) COORDINADOR"]="Coordinador"
SUB_BD_INAF11$Etapa[SUB_BD_INAF11$Etapa=="3) ESPECIALISTA CSIG"]="CSIG"
SUB_BD_INAF11$Estado[SUB_BD_INAF11$Estado=="PENDIENTE DE REVISION"]="Incompletos"
SUB_BD_INAF11$Estado[SUB_BD_INAF11$Estado=="EN REVISION"]="En revisi?n"
SUB_BD_INAF11$Estado[SUB_BD_INAF11$Estado=="OBSERVADO"]="Observado"
SUB_BD_INAF11$Estado[SUB_BD_INAF11$Estado=="VALIDADO"]="Validado"



#############################################
#####  GR?FICO DE SANKEY (PORCENTAJES)  #####
#############################################


#library(networkD3)
nodes = data.frame("name" = 
                     c("IGAs en INAF", #Primer nivel (0). Previo a esto hay que definir todos los niveles
                       
                       "Completados", #Segundo nivel (1)
                       "Pendientes de completar", #Segundo nivel (2)
                       
                       "Aprobado", #Tercer nivel (3)
                       "En revisi?n", #Tercer nivel (4)
                       "Observados", #Tercer nivel (5)
                       
                       "En revisi?n", #Cuarto nivel (6)
                       "Observados", #Cuarto nivel (7)
                       "Validado")) #Cuarto nivel (8)

# COL=nrow(CMIN)

links=as.data.frame(matrix(c(
  0, 1, SUB_BD_INAF11[2,3]+SUB_BD_INAF11[3,3]+SUB_BD_INAF11[4,3]+SUB_BD_INAF11[5,3]+SUB_BD_INAF11[6,3], # Casos que tienen primera versi?n
  0, 2, SUB_BD_INAF11[1,3], # Casos que no tienen primera versi?n.
  1, 3, SUB_BD_INAF11[4,3]+SUB_BD_INAF11[5,3]+SUB_BD_INAF11[6,3], # Cantidad de casos archivados.
  1, 4, SUB_BD_INAF11[2,3], # Casos con version final
  1, 5, SUB_BD_INAF11[3,3], # Casos sin veri?n final
  3, 6, SUB_BD_INAF11[4,3], # Casos con revisi?n de ?rika
  3, 8, SUB_BD_INAF11[5,3], # Casos sin revisi?n de ?rika
  3, 7, SUB_BD_INAF11[6,3]), # Casos con cargo (Revisi?n de CMIN/DSEM)
  byrow = T, ncol = 3))


names(links) = c("source", "target", "value")

#links$value=round(100*links$value/COL,digits = 1)


SANK1=sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    #colourScale=OEFA_1,
                    units = "%",
                    fontSize= 14, nodeWidth = 20,
                    sinksRight=F,nodePadding=10,iterations=100)

SANK1




########################################
#####  GRAFICANDO UNA TABLITA XVR  #####
########################################
TAB2=
  
  formattable(SUB_BD_INAF11, align=c("l","l","c","r"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Registros=color_tile("#DeF7E9", "#71CA97")))

TAB2


#######################################################################################################################################################
#######################################################################################################################################################

####################
####  Guardado  ####
####################

#Extraer el "data frame"
TEMP <- as.data.frame(BD_INAF)

#EXPORTANDO EN FORMATO R
#save(TEMP, file="BD_INAF.rdata")

#EXPORTANDO ARCHIVO DE TRABAJO
write.csv(TEMP,file=archivo, na="")

#EXPORTANDO BACKUP
# write.csv(TEMP,file=paste(backup,archivo,sep = ""), na="")













