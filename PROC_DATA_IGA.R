###############################################################
#########  MONITOREO DE LOS AVANCES DE LA BD IGA's  ###########
###########################  BY LF  ###########################
###############################################################


## ##################################### ###
##      0) CARGANDO CONFIGURACIONES      ####
## ##################################### ###


# CARGANDO LAS CONEXIONES DE ORACLE
# jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="D:/Oracle/ojdbc7.jar")
# jdbcConnection =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//orclnod-cluster-scan:1534/BIOEFABD", "STGPORTAL", "aZMYYq97FkRE")


sheets_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

## #########################################




# ____________________________________________________________________________________________________________________________________

## ########################################### ###
##      I) CARGANDO LA DATA DEL HISTORIAL      ####
## ########################################### ###



# ------------------- ---
#   CARGANDO LA DATA     
# ------------------- ---

#FUENTE
NOMB_EXC='2) Historial.xlsx'
FUENTE_PRE_HISTORIAL<- paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs/1) INPUTS/',NOMB_EXC,sep = "")

#CARGAR LA DATA ORIGINAL
BD_HISTORIAL<-read_xlsx(path = FUENTE_PRE_HISTORIAL)
# BD_HISTORIAL=dbReadTable(jdbcConnection ,'BI_IGAS_HISTORIAL_CTRL_CAL')




# -------------------------------------- ---
#   AGREGANDO LOS ENCABEZADOS DEL DRIVE   
# -------------------------------------- ---

#FUENTE
FUENTE_HISTORIAL<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSBErlOvYBET_39N9DVvanERIeuH1FlC-xDubfnvcvKWOOTpFEFSYR_HnQx9faFZCu_J6T-CgtwjZ6W/pub?output=xlsx"

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR 
download.file(FUENTE_HISTORIAL,tp1,mode ="wb")

#SELECCIONAR LA PESTA?A DEL TEMPORAL
ENCABEZADOS<-as.list(read_xlsx(path = tp1, sheet = "DIC_DATOS"))




# ---------------------------- ---
#   TRANSFORMACIÓN DE LA DATA   
# ---------------------------- ---

#RENOMBRANDO
names(BD_HISTORIAL)[names(BD_HISTORIAL) == 'TXESTADOGRILLA'] <- 'PROC_ESTADO'
names(BD_HISTORIAL)[names(BD_HISTORIAL) == 'FEESTADO'] <- 'F_ESTADO'

#GENERANDO VARIABLE CONCATENADA
BD_HISTORIAL$ETAPA=NA
BD_HISTORIAL$ETAPA[BD_HISTORIAL$PROC_ESTADO=="Pendiente de revisión"]="1) REGISTRADOR"
BD_HISTORIAL$ETAPA[BD_HISTORIAL$PROC_ESTADO=="En revisión[Revisor]"|BD_HISTORIAL$PROC_ESTADO=="Observado[Revisor]"]="2) REVISOR"
BD_HISTORIAL$ETAPA[BD_HISTORIAL$PROC_ESTADO=="En revisión[Calidad]"|BD_HISTORIAL$PROC_ESTADO=="Observado[Calidad]"|BD_HISTORIAL$PROC_ESTADO=="Validado[Calidad]"]="3) CSIG (CALIDAD)"

BD_HISTORIAL$PROC_ESTADO[BD_HISTORIAL$PROC_ESTADO=="Pendiente de revisión"]="PENDIENTE DE REVISION"
BD_HISTORIAL$PROC_ESTADO[BD_HISTORIAL$PROC_ESTADO=="En revisión[Revisor]"]="EN REVISION"
BD_HISTORIAL$PROC_ESTADO[BD_HISTORIAL$PROC_ESTADO=="Observado[Revisor]"]="OBSERVADO"
BD_HISTORIAL$PROC_ESTADO[BD_HISTORIAL$PROC_ESTADO=="En revisión[Calidad]"]="EN REVISION"
BD_HISTORIAL$PROC_ESTADO[BD_HISTORIAL$PROC_ESTADO=="Observado[Calidad]"]="OBSERVADO"
BD_HISTORIAL$PROC_ESTADO[BD_HISTORIAL$PROC_ESTADO=="Validado[Calidad]"]="VALIDADO"

#CAMBIANDO FORMATO DE FECHA
BD_HISTORIAL$F_ESTADO_HORA=BD_HISTORIAL$F_ESTADO
BD_HISTORIAL$F_ESTADO=as.Date(format(as.Date(BD_HISTORIAL$F_ESTADO,"%d/%m/%Y"),"%Y-%m-%d"))

#ORDENANDO
BD_HISTORIAL=BD_HISTORIAL %>%
  subset(select=ENCABEZADOS$ENCABEZADOS)


#PAUSANDO
pause(5)




# -------------------------- ---
#   CARGANDO DATA AL DRIVE  
# -------------------------- ---

#SE PEGA EN LA NUEVA PESTA?A (PREVIAMENTE CREADA) DIRECTAMENTE EN EL DRIVE PARA JALAR LA DATA PROCESADA
sheet_write(data =BD_HISTORIAL, ss="https://docs.google.com/spreadsheets/d/1qPVdzN3MEOnBM8O5jEN_8QkZdPiRfrjcjDDqQzY1MiI/edit?usp=sharing", sheet = "BD_HISTORIAL")

#BORRANDO BDs
rm(BD_HISTORIAL)

#PAUSANDO
pause(5)

































## ###############################################




# ____________________________________________________________________________________________________________________________________

## ################################### ###
##      II) CARGANDO LA DATA IGAS      ####
## ################################### ###


# ------------------- ---
#   CARGANDO LA DATA     
# ------------------- ---

#FUENTE
NOMB_EXC='1) Igas.xlsx'
FUENTE_PRE_IGAS<- paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs/1) INPUTS/',NOMB_EXC,sep = "")

#CARGAR LA DATA ORIGINAL
PRE_BD_INAF<-read_xlsx(path = FUENTE_PRE_IGAS)
# PRE_BD_INAF=as.data.frame(dbReadTable(jdbcConnection ,'BI_IGAS'))
# PROVISIONAL
PRE_BD_INAF$FECHA_CSIG=0





# -------------------------------------- ---
#   AGREGANDO LOS ENCABEZADOS DEL DRIVE   
# -------------------------------------- ---

#FUENTE
FUENTE_IGAS<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRafB2LaFQ0YIPfK5j1ofCaBz4oRz3-1Jqt3c9UST-WnJ7j2D0tbEMsPhTDBd5qhlk6gnnCXlw_CQDy/pub?output=xlsx"

#SETEANDO LA DIRECCION DEL DRIVE
archivo='IGAS_INAF.csv'

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR 
download.file(FUENTE_IGAS,tp1,mode ="wb")

#SELECCIONAR LA PESTA?A DEL TEMPORAL
ENCABEZADOS<-as.list(read_xlsx(path = tp1, sheet = "DICC_DATOS"))





# -------------------------------------------------- ---
#   TRANSFORMACI?N DE LA DATA CSEP A FORMATO DRIVE   
# -------------------------------------------------- ---

#GENERANDO VARIABLE CONCATENADA
PRE_BD_INAF$Estado_AUX=paste(PRE_BD_INAF$TXESTADO,' [',PRE_BD_INAF$TXNIVEL,']',sep = "")
PRE_BD_INAF$AUX_BLANK=NA

#EDITANDO CATEGORÍAS
PRE_BD_INAF$Estado_AUX[PRE_BD_INAF$Estado_AUX=="NA [NA]"]="Pendiente de revisión"
PRE_BD_INAF$Estado_AUX[PRE_BD_INAF$Estado_AUX=="En revisión [Registrador]"]=paste('Observado [',PRE_BD_INAF$TXNIVELANT[PRE_BD_INAF$Estado_AUX=="En revisión [Registrador]"],']',sep = "")

#BD_INAF <- BD_INAF[ ,-(14:16)]
PRE_BD_INAF=PRE_BD_INAF %>%
  subset(select=ENCABEZADOS$COD_CSEP)

#CAMBIANDO FORMATO DE FECHA
PRE_BD_INAF$FEREG=as.Date(format(as.Date(PRE_BD_INAF$FEREG,"%d/%m/%Y"),"%Y-%m-%d"))
PRE_BD_INAF$FEAPROBACION=as.Date(format(as.Date(PRE_BD_INAF$FEAPROBACION,"%d/%m/%Y"),"%Y-%m-%d"))




# -------------------------- ---
#   CARGANDO DATA AL DRIVE  
# -------------------------- ---

#Asignar nombres
names(PRE_BD_INAF)=ENCABEZADOS$ENCABEZADOS


#SE PEGA EN LA NUEVA PESTA?A (PREVIAMENTE CREADA) DIRECTAMENTE EN EL DRIVE PARA JALAR LA DATA PROCESADA
sheet_write(data =PRE_BD_INAF, ss="https://docs.google.com/spreadsheets/d/1ulMVlD-r7fBHJfI7V72Gl7E72E_0FHHM19YlOsi5TiM/edit?usp=sharing", sheet = "BD_INAF")

#BORRAR BDs
rm(PRE_BD_INAF)

#PAUSAR
pause(5)


## #######################################




# ____________________________________________________________________________________________________________________________________

## ############################################### ###
##      III) CARGANDO LA DATA DE LOS ARCHIVOS      ####
## ############################################### ###


# ------------------- ---
#   CARGANDO LA DATA     
# ------------------- ---

#FUENTE
NOMB_EXC='3) ArchPropios.xlsx'
FUENTE_PRE_ARCH<- paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs/1) INPUTS/',NOMB_EXC,sep = "")

#CARGAR LA DATA ORIGINAL
BD_ARCH<-read_xlsx(path = FUENTE_PRE_ARCH)
# BD_ARCH=dbReadTable(jdbcConnection ,'BI_IGAS_ARCH_PROPIOS')






# -------------------------------------- ---
#   AGREGANDO LOS ENCABEZADOS DEL DRIVE   
# -------------------------------------- ---

#FUENTE
FUENTE_ARCHIVOS<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSnQ63y1JKc00rbS0N2LIYg5QyaXIhbs9km-M2uCYsWVkjftSL4BljLgLUH64gG1RlrtOBCu4Ik4CUf/pub?output=xlsx"

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR 
download.file(FUENTE_ARCHIVOS,tp1,mode ="wb")

#SELECCIONAR LA PESTA?A DEL TEMPORAL
ENCABEZADOS<-as.list(read_xlsx(path = tp1, sheet = "DIC_DATOS"))






# ---------------------------- ---
#   TRANSFORMACIÓN DE LA DATA   
# ---------------------------- ---

#RENOMBRANDO
names(BD_ARCH)[names(BD_ARCH) == 'NUSIZE'] <- 'ARCH_SIZE'
names(BD_ARCH)[names(BD_ARCH) == 'FEREG'] <- 'F_REG'

#CAMBIANDO FORMATO DE FECHA
BD_ARCH$F_REG=as.Date(format(as.Date(BD_ARCH$F_REG,"%d/%m/%Y"),"%Y-%m-%d"))






# -------------------------- ---
#   CARGANDO DATA AL DRIVE  
# -------------------------- ---

#SE PEGA EN LA NUEVA PESTA?A (PREVIAMENTE CREADA) DIRECTAMENTE EN EL DRIVE PARA JALAR LA DATA PROCESADA
sheet_write(data =BD_ARCH, ss="https://docs.google.com/spreadsheets/d/1dFj7_z0Py688Chqbh_8tcTjz9-teF9teG_s1-DpMskg/edit?usp=sharing", sheet = "BD_ARCH")

#BORRANDO BDs
rm(BD_ARCH)

#PAUSANDO
pause(5)









## ###################################################




#####
#___________________________________________________________

# ====================================== ===
#   AGREGANDO LOS ENCABEZADOS DEL RIVE   ===
# ====================================== ===
# 
# #FUENTE
# FUENTE_HISTORIAL<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRafB2LaFQ0YIPfK5j1ofCaBz4oRz3-1Jqt3c9UST-WnJ7j2D0tbEMsPhTDBd5qhlk6gnnCXlw_CQDy/pub?output=xlsx"
# 
# #SETEANDO LA DIRECCION DEL DRIVE
# archivo='IGAS_INAF.csv'
# 
# #GENERANDO UN TEMPORAL
# tp1<-tempfile()
# 
# #DESCARGAR 
# download.file(FUENTE_IGAS,tp1,mode ="wb")
# 
# #SELECCIONAR LA PESTA?A DEL TEMPORAL
# ENCABEZADOS<-as.list(read_xlsx(path = tp1, sheet = "DICC_DATOS"))
# 
# 
# #Asignar nombres
# names(PRE_BD_INAF)=ENCABEZADOS$ENCABEZADOS

#___________________________________________________________


#SE PEGA EN LA NUEVA PESTA?A (PREVIAMENTE CREADA) DIRECTAMENTE EN EL DRIVE PARA JALAR LA DATA PROCESADA



# Correo_electrónico <- "oefaencifras@oefa.pe"
# drive_auth(email = Correo_electrónico)
# gs4_auth(token = drive_token())


############################################# ###


#_______________________________________________________________________________
#_______________________________________________________________________________

# ## ############################# ###
# ##   IV) CARGANDO LA DATA IGAS   ## ##
# ## ############################# ###
# 
# 
# #FUENTE
# load("C:/Users/lfpal/Google Drive/4) R/2) BD/1) OEFA/2) IGAs/Planefa_Agosto_2020.RData")
# CRIT_PLANEFA=Planefa2020_R[ ,c(3:6,8:9,11:12,15,16,19:22)]
# 
# # Cambiar Nombres de las variables
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'ID_COORDINACION'] = 'COD_COORD'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'DES_COORDINACION'] = 'COORD'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'ID_ADMINISTRADO'] = 'COD_ADMIN'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TX_ADMINISTRADO'] = 'N_ADMIN'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'IDUF_SIG'] = 'COD_UF'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TX_SUBUNIDAD'] = 'N_UF'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TX_NIVEL_PRIORIZA'] = 'NIV_PRIOR'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TX_UBICACION_UF'] = 'UBIC_UF'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TX_ESTADO_PLANEFA_X_CUC'] = 'ESTADO_PLANIFIC'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TX_ESTADO_CUC'] = 'ESTADO_SUP'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TXUBIGEO_SUP'] = 'UBIGEO_SUP'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TXDEPARTAMENTO_SUP'] = 'DEP_SUP'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TXPROVINCIA_SUP'] = 'PROV_SUP'
# names(CRIT_PLANEFA)[names(CRIT_PLANEFA) == 'TXDISTRITO_SUP'] = 'DIST_SUP'
# 
# 
# # Cambiar categorías de variables
# CRIT_PLANEFA$COORD[CRIT_PLANEFA$COORD=="Coordinación de Seguimiento y Verificación a las Consultoras Ambientales"]="CCAMB"
# CRIT_PLANEFA$COORD[CRIT_PLANEFA$COORD=="Coordinación de Supervisión Ambiental en Agricultura"]="CAGR"
# CRIT_PLANEFA$COORD[CRIT_PLANEFA$COORD=="Coordinación de Supervisión Ambiental en Electricidad"]="CELE"
# CRIT_PLANEFA$COORD[CRIT_PLANEFA$COORD=="Coordinación de Supervisión Ambiental en Hidrocarburos"]="CHID"
# CRIT_PLANEFA$COORD[CRIT_PLANEFA$COORD=="Coordinación de Supervisión Ambiental en Industria"]="CIND"
# CRIT_PLANEFA$COORD[CRIT_PLANEFA$COORD=="Coordinación de Supervisión Ambiental en Minería"]="CMIN"
# CRIT_PLANEFA$COORD[CRIT_PLANEFA$COORD=="Coordinación de Supervisión Ambiental en Pesca"]="CPES"
# CRIT_PLANEFA$COORD[CRIT_PLANEFA$COORD=="Coordinación de Supervisión Ambiental en Residuos Solidos"]="CRES"
# 
# 
# 
# #___________________________________________________________
# 
# # ========================== ===
# #   CARGANDO DATA AL DRIVE   ===
# # ========================== ===
# 
# #SE PEGA EN LA NUEVA PESTA?A (PREVIAMENTE CREADA) DIRECTAMENTE EN EL DRIVE PARA JALAR LA DATA PROCESADA
# sheet_write(data =CRIT_PLANEFA, ss="https://docs.google.com/spreadsheets/d/1OeqGso5-KRtH7wXcK8zuvGxY_29e85-Xp7OKIU-dLWM/edit?usp=sharing", sheet = "CRITICOS")

################################# ###


#_______________________________________________________________________________
#_______________________________________________________________________________
# 
# ## ############################ ###
# ##   V) CARGANDO LA DATA IGAS   ## ##
# ## ############################# ###
# 
#   #FUENTE
#   NOMB_EXC='5) Registro de IGA.xlsx'
#   FUENTE_PRE_ADM_UF<- paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs/1) INPUTS/',NOMB_EXC,sep = "")
# 
#   #CARGAR LA DATA ORIGINAL
#   PRE_BD_ADM_UF<-read_xlsx(path = FUENTE_PRE_ADM_UF,skip = 9)
# 
# 
# #___________________________________________________________
# 
# # ====================================== ===
# #   AGREGANDO LOS ENCABEZADOS DEL RIVE   ===
# # ====================================== ===
# 
# #FUENTE
# FUENTE_IGAS<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRdvKvyxqdRnyvkFngDf9SGvBZVZt9aHygcq6JWgMUVeDxJE6QqsgTCwsSdhq0xFQvMSLYOT1-XifxV/pub?output=xlsx"
# 
# #SETEANDO LA DIRECCION DEL DRIVE
# # archivo='IGAS_INAF.csv'
# 
# #GENERANDO UN TEMPORAL
# tp1<-tempfile()
# 
# #DESCARGAR
# download.file(FUENTE_IGAS,tp1,mode ="wb")
# 
# #SELECCIONAR LA PESTA?A DEL TEMPORAL
# ENCABEZADOS<-read_xlsx(path = tp1, sheet = "DIC_DATOS")
# ENCABEZADOS=ENCABEZADOS[ENCABEZADOS$INFO_IRRELEV==0, ]
# ENCABEZADOS=as.list(ENCABEZADOS)
# PRE_BD_ADM_UF=subset(PRE_BD_ADM_UF, select = ENCABEZADOS$ENCABEZADOS) #Limito los campos con los que se trabajar?
# colnames(PRE_BD_ADM_UF)=ENCABEZADOS$COD_ENCAB
# 
# 
# #___________________________________________________________
# 
# # ========================== ===
# #   CARGANDO DATA AL DRIVE   ===
# # ========================== ===
# 
# #SE PEGA EN LA NUEVA PESTA?A (PREVIAMENTE CREADA) DIRECTAMENTE EN EL DRIVE PARA JALAR LA DATA PROCESADA
# sheet_write(data =PRE_BD_ADM_UF, ss="https://docs.google.com/spreadsheets/d/1Y_4q0ok_LOQp5WLUs3WWda7OWLUWD1YEC8nh3iYza5k/edit?usp=sharing", sheet = "BD_ADM_UF")
# 
# ################################# ##

#####






# ____________________________________________________________________________________________________________________________________

## ############################ ###
##      V) ELIMINAR LAS BD      ####
## ############################ ###

# rm(jdbcConnection)
# rm(jdbcDriver)
rm(ENCABEZADOS)


###################################

