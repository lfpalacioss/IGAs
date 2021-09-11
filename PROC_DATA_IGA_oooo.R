###############################################################
#########  MONITOREO DE LOS AVANCES DE LA BD IGA's  ###########
###########################  BY LF  ###########################
###############################################################



# ____________________________________________________________________________________________________________________________________

## ########################################### ###
##      I) CARGANDO LA DATA DEL HISTORIAL      ####
## ########################################### ###


# -------------------------------------- ---
#   AGREGANDO LOS ENCABEZADOS DEL DRIVE   
# -------------------------------------- ---

#FUENTE
DIC_DATOS<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSBErlOvYBET_39N9DVvanERIeuH1FlC-xDubfnvcvKWOOTpFEFSYR_HnQx9faFZCu_J6T-CgtwjZ6W/pub?output=xlsx"

#SETEANDO LA DIRECCION DEL DRIVE
# archivo='IGAS_INAF.csv'

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR 
download.file(DIC_DATOS,tp1,mode ="wb")

#GENERAR ENCABEZADOS_HIST
ENCABEZADOS_HIST = read_xlsx(path = tp1, sheet = "DIC_DATOS") %>% 
  subset(INFO_IRRELEV == 0) %>% 
  as.list()

#GENERAR ENCABEZADOS_HIST
ENCABEZADOS_HIST_DRIVE = read_xlsx(path = tp1, sheet = "DIC_DATOS") %>% 
  subset(is.na(COD_R) == F) %>% 
  as.list()


# ------------------- ---
#   CARGANDO LA DATA     
# ------------------- ---

#FUENTE
NOMB_EXC='2) Historial.xlsx'
FUENTE_BD_HISTORIAL<- paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs/1) INPUTS/',NOMB_EXC,sep = "")

#CARGAR LA DATA ORIGINAL
# PRE_BD_INAF=as.data.frame(dbReadTable(jdbcConnection ,'BI_IGAS'))
BD_HISTORIAL <- read_xlsx(FUENTE_BD_HISTORIAL) %>% 
  subset(select = ENCABEZADOS_HIST$COD_ORIGINAL) %>% 
  rename_at(vars(ENCABEZADOS_HIST$COD_ORIGINAL), ~ ENCABEZADOS_HIST$COD_R) %>%
  
  mutate(F_ESTADO = as.Date(format(as.Date(F_ESTADO,"%d/%m/%Y"),"%Y-%m-%d")),
         ETAPA = case_when(str_detect(ESTADO, "Revisor") == T ~ "2) REVISOR",
                           str_detect(ESTADO, "Calidad") == T ~ "3) CSIG (CALIDAD)"),
         ESTADO = case_when(str_detect(ESTADO, "En revisión") == T ~ "EN REVISION",
                            str_detect(ESTADO, "Observado") == T ~ "OBSERVADO",
                            str_detect(ESTADO, "Validado") == T ~ "VALIDADO"))
  
## ###############################################




# ____________________________________________________________________________________________________________________________________

## ################################### ###
##      II) CARGANDO LA DATA IGAS      ####
## ################################### ###


# -------------------------------------- ---
#   AGREGANDO LOS ENCABEZADOS DEL DRIVE   
# -------------------------------------- ---

#FUENTE
DIC_DATOS<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRafB2LaFQ0YIPfK5j1ofCaBz4oRz3-1Jqt3c9UST-WnJ7j2D0tbEMsPhTDBd5qhlk6gnnCXlw_CQDy/pub?output=xlsx"

#SETEANDO LA DIRECCION DEL DRIVE
# archivo='IGAS_INAF.csv'

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR 
download.file(DIC_DATOS,tp1,mode ="wb")

#GENERAR ENCABEZADOS_IGAS
ENCABEZADOS_IGAS = read_xlsx(path = tp1, sheet = "DIC_DATOS") %>% 
  subset(INFO_IRRELEV == 0) %>% 
  as.list()

#GENERAR ENCABEZADOS_IGAS
ENCABEZADOS_IGAS_DRIVE = read_xlsx(path = tp1, sheet = "DIC_DATOS") %>% 
  subset(is.na(COD_R) == F) %>% 
  as.list()

# ------------------- ---
#   CARGANDO LA DATA     
# ------------------- ---

#FUENTE
NOMB_EXC='1) Igas.xlsx'
FUENTE_BD_IGAS<- paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs/1) INPUTS/',NOMB_EXC,sep = "")

#CARGAR LA DATA ORIGINAL
# PRE_BD_INAF=as.data.frame(dbReadTable(jdbcConnection ,'BI_IGAS'))
BD_IGAS <- read_xlsx(FUENTE_BD_IGAS) %>% 
  subset(select = ENCABEZADOS_IGAS$COD_ORIGINAL) %>% 
  rename_at(vars(ENCABEZADOS_IGAS$COD_ORIGINAL), ~ ENCABEZADOS_IGAS$COD_R) %>%
  mutate(F_APROB = as.Date(F_APROB, "%d/%m/%Y"),
         F_REG_IGA = as.Date(F_REG_IGA, "%d/%m/%Y")) %>% 
  mutate(SUB_SECT=case_when(SUB_SECT=="Electricidad"~"CELE",
                            SUB_SECT=="Minería"~"CMIN",
                            SUB_SECT=="Hidrocarburos"~"CHID",
                            SUB_SECT=="Industria"~"CIND",
                            SUB_SECT=="Agricultura"~"CAGR",
                            SUB_SECT=="Pesquería"~"CPES",
                            SUB_SECT=="Residuos Sólidos"~"CRES",
                            SUB_SECT=="Hidrocarburos, Industria"~"CHID")) %>%
  mutate(SUB_SECT_0 = case_when(SUB_SECT=="CELE"|SUB_SECT=="CMIN"|SUB_SECT=="CHID"~"DSEM",
                                SUB_SECT=="CIND"|SUB_SECT=="CAGR"|SUB_SECT=="CPES"~"DSAP",
                                SUB_SECT=="CRES"~"DSIS")) %>%
  mutate(ETAPA = case_when(is.na(ESTADO_IGA) == T ~ "1) REGISTRADOR",
                             str_detect(ESTADO_IGA, "Revisor") == T ~ "2) REVISOR",
                             str_detect(ESTADO_IGA, "Calidad") == T ~ "3) CSIG (CALIDAD)")) %>%
  mutate(ESTADO = case_when(is.na(ESTADO_IGA) == T ~ "PENDIENTE DE REVISION",
                            str_detect(ESTADO_IGA, "En revisión") == T ~ "EN REVISION",
                            str_detect(ESTADO_IGA, "Observado") == T ~ "OBSERVADO",
                            str_detect(ESTADO_IGA, "Validado") == T ~ "VALIDADO"))

## #######################################




# ____________________________________________________________________________________________________________________________________

## ############################################### ###
##      III) CARGANDO LA DATA DE LOS ARCHIVOS      ####
## ############################################### ###

# -------------------------------------- ---
#   AGREGANDO LOS ENCABEZADOS DEL DRIVE   
# -------------------------------------- ---

#FUENTE
DIC_DATOS<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSnQ63y1JKc00rbS0N2LIYg5QyaXIhbs9km-M2uCYsWVkjftSL4BljLgLUH64gG1RlrtOBCu4Ik4CUf/pub?output=xlsx"

#SETEANDO LA DIRECCION DEL DRIVE
# archivo='IGAS_INAF.csv'

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR 
download.file(DIC_DATOS,tp1,mode ="wb")

#GENERAR ENCABEZADOS_ARCH
ENCABEZADOS_ARCH = read_xlsx(path = tp1, sheet = "DIC_DATOS") %>% 
  subset(INFO_IRRELEV == 0) %>% 
  as.list()

#GENERAR ENCABEZADOS_ARCH
ENCABEZADOS_ARCH_DRIVE = read_xlsx(path = tp1, sheet = "DIC_DATOS") %>% 
  subset(is.na(COD_R) == F) %>% 
  as.list()


# ------------------- ---
#   CARGANDO LA DATA     
# ------------------- ---

#FUENTE
NOMB_EXC='3) ArchPropios.xlsx'
FUENTE_BD_ARCH<- paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs/1) INPUTS/',NOMB_EXC,sep = "")

#CARGAR LA DATA ORIGINAL
# PRE_BD_INAF=as.data.frame(dbReadTable(jdbcConnection ,'BI_IGAS'))
BD_ARCH <- read_xlsx(FUENTE_BD_ARCH) %>% 
  subset(select = ENCABEZADOS_ARCH$COD_ORIGINAL) %>% 
  rename_at(vars(ENCABEZADOS_ARCH$COD_ORIGINAL), ~ ENCABEZADOS_ARCH$COD_R) %>%
  mutate(F_REG_ARCH = as.Date(format(as.Date(F_REG_ARCH,"%d/%m/%Y"),"%Y-%m-%d")),
         ARCH_SIZE_MB = round(ARCH_SIZE/1000000,2),
         ARCH_SIZE_GB = round(ARCH_SIZE/1000000,2)/1000)

## ###################################################




# ____________________________________________________________________________________________________________________________________

## ##################################### ###
##      IV) CARGANDO LA DATA ADM_UF      ####
## ##################################### ###



# -------------------------------------- ---
#   AGREGANDO LOS ENCABEZADOS DEL DRIVE   
# -------------------------------------- ---

#FUENTE
DIC_DATOS<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRdvKvyxqdRnyvkFngDf9SGvBZVZt9aHygcq6JWgMUVeDxJE6QqsgTCwsSdhq0xFQvMSLYOT1-XifxV/pub?output=xlsx"

#SETEANDO LA DIRECCION DEL DRIVE
# archivo='IGAS_INAF.csv'

#GENERANDO UN TEMPORAL
tp1<-tempfile()

#DESCARGAR 
download.file(DIC_DATOS,tp1,mode ="wb")

#GENERAR ENCABEZADOS_ADM_UF
ENCABEZADOS_ADM_UF = read_xlsx(path = tp1, sheet = "DIC_DATOS") %>% 
  subset(INFO_IRRELEV == 0) %>% 
  as.list()

#GENERAR ENCABEZADOS_ADM_UF
ENCABEZADOS_ADM_UF_DRIVE = read_xlsx(path = tp1, sheet = "DIC_DATOS") %>% 
  subset(is.na(COD_R) == F) %>% 
  as.list()


# ------------------- ---
#   CARGANDO LA DATA     
# ------------------- ---

#FUENTE
NOMB_EXC='AdminUF.RData'
FUENTE_BD_ADM_UF<- paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs/1) INPUTS/',NOMB_EXC,sep = "")

#CARGAR LA DATA ORIGINAL
BD_ADM_UF = get(load(FUENTE_BD_ADM_UF)) %>% 
  subset(select = ENCABEZADOS_ADM_UF$COD_ORIGINAL) %>% 
  rename_at(vars(ENCABEZADOS_ADM_UF$COD_ORIGINAL), ~ ENCABEZADOS_ADM_UF$COD_R) %>%
  
  mutate(SUB_SECT=case_when(SUB_SECT=="Electricidad"~"CELE",
                            SUB_SECT=="Minería"~"CMIN",
                            SUB_SECT=="Hidrocarburos"~"CHID",
                            SUB_SECT=="Industria"~"CIND",
                            SUB_SECT=="Agricultura"~"CAGR",
                            SUB_SECT=="Pesquería"~"CPES",
                            SUB_SECT=="Residuos Sólidos"~"CRES",
                            SUB_SECT=="Consultoras Ambientales"~"CAMB")) %>% 
  
  mutate(UF_CON_IGA=case_when(UF_NECESITA_IGA == "SI" ~ "SI",
                              str_detect(UF_NECESITA_IGA, "NO")~ "NO",
                              is.na(UF_NECESITA_IGA) == T ~ "PENDIENTE"))

rm(AdminUF_R)

############################################




# ____________________________________________________________________________________________________________________________________

## ############################ ###
##      V) CARGAR AL DRIVE      ####
## ############################ ###


# VERIFICAR SI EXISTE EL COMANDO DE AUTORIZACIÓN
if (exists("AUTORIZACION")==TRUE) {
  rm(AUTORIZACION) #source(paste('C:/Users/',USER,'/Google Drive/4) R/1) Scripts/1) OEFA/3) IGAS/PROC_DATA_IGA.R',sep = ""), encoding="utf-8")
} 


# SOLICITAR LA AUTORIZACIÓN
AUTORIZACION = askYesNo("¿Desea pre procesar y subir la data a Drive?", 
                        default = TRUE,
                        prompts = getOption("askYesNo", gettext(c("SÍ", "NO", "CANCELAR")))
)


#EJECUTAR COMANDO DE ACTUALIZACIÓN
if (AUTORIZACION==TRUE & is.na(AUTORIZACION)==FALSE) {
  
  
  
  # ------------------ ---
  #   CONFIGURACIÓN  
  # ------------------ ---
  
  googlesheets4::gs4_auth(
    email = gargle::gargle_oauth_email(),
    path = NULL,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL)
  
  
  
  
  # ------------------ ---
  #   DRIVE HISTORIAL  
  # ------------------ ---
  
  #Asignar nombres
  DRIVE_BD_HISTORIAL = BD_HISTORIAL %>% 
    rename_at(vars(ENCABEZADOS_HIST_DRIVE$COD_R), ~ ENCABEZADOS_HIST_DRIVE$ENCABEZADOS) %>%
    googlesheets4::sheet_write(ss="https://docs.google.com/spreadsheets/d/1qPVdzN3MEOnBM8O5jEN_8QkZdPiRfrjcjDDqQzY1MiI/edit?usp=sharing", sheet = "BD_HISTORIAL")
  
  #BORRAR BDs
  rm(DRIVE_BD_HISTORIAL)
  
  
  
  
  # ------------- ---
  #   DRIVE IGAS  
  # ------------- ---
  
  #Asignar nombres
  DRIVE_BD_IGAS = BD_IGAS %>% 
    rename_at(vars(ENCABEZADOS_IGAS_DRIVE$COD_R), ~ ENCABEZADOS_IGAS_DRIVE$ENCABEZADOS) %>%
    googlesheets4::sheet_write(ss="https://docs.google.com/spreadsheets/d/1ulMVlD-r7fBHJfI7V72Gl7E72E_0FHHM19YlOsi5TiM/edit?usp=sharing", sheet = "BD_INAF")
  
  #BORRAR BDs
  rm(DRIVE_BD_IGAS)
  
  
  
  
  # ----------------- ---
  #   DRIVE ARCHIVOS 
  # ----------------- ---
  
  #Asignar nombres
  DRIVE_BD_ARCH = BD_ARCH %>% 
    rename_at(vars(ENCABEZADOS_ARCH_DRIVE$COD_R), ~ ENCABEZADOS_ARCH_DRIVE$ENCABEZADOS) %>%
    googlesheets4::sheet_write(ss="https://docs.google.com/spreadsheets/d/1dFj7_z0Py688Chqbh_8tcTjz9-teF9teG_s1-DpMskg/edit?usp=sharing", sheet = "BD_ARCH")
  
  #BORRAR BDs
  rm(DRIVE_BD_ARCH)
  
  
  
  
  # --------------- ---
  #   DRIVE ADM-UF  
  # --------------- ---
  
  #Asignar nombres
  DRIVE_BD_ADM_UF = BD_ADM_UF %>% 
    rename_at(vars(ENCABEZADOS_ADM_UF_DRIVE$COD_R), ~ ENCABEZADOS_ADM_UF_DRIVE$ENCABEZADOS) %>%
    googlesheets4::sheet_write(ss="https://docs.google.com/spreadsheets/d/1Y_4q0ok_LOQp5WLUs3WWda7OWLUWD1YEC8nh3iYza5k/edit?usp=sharing", sheet = "BD_ADM_UF")
  
  #BORRAR BDs
  rm(DRIVE_BD_ADM_UF)
  
  
  
  
  
  
  
} else if (AUTORIZACION==FALSE& is.na(AUTORIZACION)==FALSE) {
  message("NO SE HA REALIZADO LA CARGA DE DATOS A DRIVE")
} else {
  stop("SE HA CANCELADO EL PROCESO", call. = FALSE)
}

  
  

###################################




# ____________________________________________________________________________________________________________________________________

## ############################# ###
##      VI) ELIMINAR LAS BD      ####
## ############################# ###

# rm(jdbcConnection)
# rm(jdbcDriver)
rm(FUENTE_BD_IGAS, FUENTE_BD_ARCH, FUENTE_BD_ADM_UF, FUENTE_BD_HISTORIAL, NOMB_EXC,tp1)
rm(ENCABEZADOS_ADM_UF, ENCABEZADOS_ADM_UF_DRIVE, 
   ENCABEZADOS_ARCH, ENCABEZADOS_ARCH_DRIVE, 
   ENCABEZADOS_HIST, ENCABEZADOS_HIST_DRIVE, 
   ENCABEZADOS_IGAS, ENCABEZADOS_IGAS_DRIVE, 
   DIC_DATOS
   )


####################################















