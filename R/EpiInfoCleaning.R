################################################################################
################## 0. LOAD PACKAGES AND SET UP ################################# 
################################################################################

# Ensures the package "pacman" is installed
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways
  readxl,     # read excel
  writexl,    # write excel
  openxlsx,   # Write to Excel
  strex,      #String manipulation
  #skimr,      # Exploring data
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse,  # data management and visualization,
  #gtsummary,  # summary statistics and tests,
  rstatix,    # summary statistics and statistical tests
  scales,     # easily convert proportions to percents  
  labelled,   # Variable and values labelling
  flextable,  # Format tables
  sqldf,      # Use sqlite queries
  RODBC,       # Odbc database connection,
  DiagrammeR,  # Create flowcharts and diagrams
  rsvg,       # Create images
  DiagrammeRsvg # Export diagrams
)

# use here from the here package
here <- here::here
# use clean_names from the janitor package
clean_names <- janitor::clean_names

# load functions used from R scripts
source_path <- here("R", "RScripts.R")
source(source_path,local = knitr::knit_global())

data_folder <- "data/"
image_folder <- "images/"
output_folder <- "output/"
excelLuyindu <- "luyinduipd.xls"
excelBiyela  <- "biyelaipd.xls"

################################################################################
#################   I. Importing & Exploring Data         ######################
################################################################################


luyindu_raw   <- readxl::read_excel(here(data_folder,excelLuyindu))
biyela_raw    <- readxl::read_excel(here(data_folder,excelBiyela))


################################################################################
##########################     II. Cleaning data    ############################
################################################################################


################# 1. Cleaning column names    #########################
#######################################################################

luyindu      <- luyindu_raw %>% janitor::clean_names()
biyela       <- biyela_raw %>% janitor::clean_names()

######W##### 2. Select,add and/or re order/rename columns ###W#########
#############################################################WW########

luyindu <- luyindu %>%
  
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim)) %>%
  rename(
    id_patient = numrodedossier
  )

biyela <- biyela %>%
  
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim)) %>%
  rename(
    id_patient = numrodedossier
  )

################ 3. Cleaning/replacing empty/null values     ##########
#######################################################################

luyindu[is.null(luyindu)  | luyindu == ""] <- NA
biyela[is.null(biyela)    | biyela  == ""]  <- NA

############ 5. Remove duplicate/unnecessary rows      ################
#######################################################################
luyindu_deleted <- luyindu %>%
  filter(grepl("*",id_patient,fixed = TRUE))

luyindu    <- luyindu %>%
  #Remove rows that start with * as they are deleted rows
  filter(!grepl("*",id_patient,fixed = TRUE))

biyela_deleted <- biyela %>%
  filter(grepl("*",id_patient,fixed = TRUE))

biyela    <- biyela %>%
  #Remove rows that start with * as they are deleted rows
  filter(!grepl("*",id_patient,fixed = TRUE))


############# 6. Convert columns classes/add columns  ################
######################################################################


luyindu_data <- luyindu %>%
  mutate(
    facility             = "luyindu",
    dateadm              = parse_date_time(paste(anne,mois,jour,sep = "-"),"%Y-%m-%d"),
    age                  = parse_number(as.character(age)),
    stade_oms            = parse_number(as.character(stade_oms)),
    glasgow              = parse_number(as.character(glasgow)),
    creatinine           = parse_number(as.character(creatinine)),
    date_cd4admission    = parse_date_time(as.character(date_cd4admission),"%Y-%m-%d"),
    valeur_cd4admission  = parse_number(as.character(valeur_cd4admission)),
    date_cv_admission    = parse_date_time(as.character(date_cv_admission),"%Y-%m-%d"),
    valeur_cv_admission  = parse_number(as.character(valeur_cv_admission)),
    datediagnostic       = parse_date_time(as.character(datediagnostic),"%Y-%m-%d"), # Date diagnostic VIH
    datedbutdelaligne_arv= parse_date_time(as.character(datedbutdelaligne_arv),"%Y-%m-%d"),
    datedbut_arv         = parse_date_time(as.character(datedbut_arv),"%Y-%m-%d"),
    jours_arv            = (dateadm - datedbut_arv),    
    datesortie           = parse_date_time(as.character(datesortie),"%Y-%m-%d"),
    period_adm           = parse_number(format(dateadm, "%Y.%m")),
    period_disc          = parse_number(format(datesortie, "%Y.%m")),
    quar_adm             = quarter(dateadm, with_year = T),
    quar_disc            = quarter(datesortie, with_year = T),
    diff_adm_disc        = datesortie - dateadm 
    #dateadm = as.Date(as.numeric(dateadm),origin="1970-01-01")
  ) %>%
  mutate(
    #Change variable values
    valeur_cv_admission  = if_else(valeur_cv_admission ==0,parse_number(""),valeur_cv_admission),
    valeur_cd4admission  = if_else(valeur_cd4admission ==0,parse_number(""),valeur_cd4admission),
  )%>%
  #Only select variables that are useful for analysis
  select(
    facility,id_patient,origine,dateadm,period_adm,period_disc,diff_adm_disc,quar_adm,quar_disc,sexe,age,stade_oms,maladearrivmort,lam,sang,csf,xpert,
    date_cd4admission,valeur_cd4admission,date_cv_admission,valeur_cv_admission,datediagnostic,
    histoire_arv,datedbut_arv,jours_arv,datedbutdelaligne_arv,lignedes_ar_vencours,t_bencoursdetraitement,
    datesortie,modedesortie
  )

biyela_data <- biyela %>%
  mutate(
    facility             = "biyela",
    dateadm              = parse_date_time(paste(anne,mois,jour,sep = "-"),"%Y-%m-%d"),
    age                  = parse_number(as.character(age)),
    stade_oms            = parse_number(as.character(stade_oms)),
    sang                 = crag_sang,
    csf                  = crag_csf,
    glasgow              = parse_number(as.character(glasgow)),
    creatinine           = parse_number(as.character(creatinineml)),
    date_cd4admission    = parse_date_time(as.character(date_cd4admission),"%Y-%m-%d"),
    valeur_cd4admission  = parse_number(as.character(valeur_cd4admission)),
    date_cv_admission    = parse_date_time(as.character(date_cv_admission),"%Y-%m-%d"),
    valeur_cv_admission  = parse_number(as.character(valeur_cv_admission)),
    appreciationcv       = parse_number(as.character(appreciationcv)),
    datediagnostic       = parse_date_time(as.character(datediagnostic1),"%Y-%m-%d"), # Date diagnostic VIH
    datedbutdelaligne_arv= parse_date_time(as.character(datedbutdelaligne_arv),"%Y-%m-%d"),
    datedbut_arv         = parse_date_time(as.character(datedbut_arv),"%Y-%m-%d"),
    crypo_ttt_adm        = cryptoencoursdetraitementladmission,
    fluco_proph          = prohylaxieflucoladmission,
    jours_arv            = (dateadm - datedbut_arv),    
    datesortie           = parse_date_time(as.character(datesortie),"%Y-%m-%d"),
    period_adm           = parse_number(format(dateadm, "%Y.%m")),
    period_disc          = parse_number(format(datesortie, "%Y.%m")),
    quar_adm             = quarter(dateadm, with_year = T),
    quar_disc            = quarter(datesortie, with_year = T),
    diff_adm_disc        = datesortie - dateadm 
    #dateadm = as.Date(as.numeric(dateadm),origin="1970-01-01")
  ) %>%
  mutate(
    #Change variable values
    valeur_cv_admission  = if_else(valeur_cv_admission ==0,parse_number(""),valeur_cv_admission),
    valeur_cd4admission  = if_else(valeur_cd4admission ==0,parse_number(""),valeur_cd4admission),
  )%>%
  #Only select variables that are useful for analysis
  select(
    facility,id_patient,origine,dateadm,period_adm,period_disc,diff_adm_disc,quar_adm,quar_disc,sexe,age,stade_oms,maladearrivmort,lam,sang,csf,xpert,
    date_cd4admission,valeur_cd4admission,date_cv_admission,valeur_cv_admission,appreciationcv,datediagnostic,
    histoire_arv,datedbut_arv,jours_arv,datedbutdelaligne_arv,lignedes_ar_vencours,t_bencoursdetraitement,crypo_ttt_adm,fluco_proph,
    datesortie,modedesortie
  )






