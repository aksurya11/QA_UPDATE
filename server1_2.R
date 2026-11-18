# Server Prep -------------------------------------------------------------
#touchFile("setup.R")
source("setup.R")
source("helpers.R")
source("smart_counting.R")
source("supervised_model_script.R")
# print("start server")
# source("library_load.R")
# source("setup.R")
# source("helpers.R")
# print("end sourcing")
# #SET UP SNOWFLAKE CONNECTION 
# # library(dplyr.snowflakedb)
# # require(dplyr)


#require(tinytex)
#tinytex::install_tinytex(extra_packages = c("amscls","amsfonts","amsmath","babel","bibtex","booktabs","caption","cm","dehyph","dvipdfmx","dvips","ec","etex","etoolbox","euenc","fancyvrb","float","fontspec","framed","geometry","glyphlist","graphics","graphics-cfg","graphics-def","gsftopk","helvetic","hyperref","hyphen-base","ifluatex","iftex","ifxetex","inconsolata","knuth-lib","kpathsea","l3backend","l3kernel","l3packages","latex","latex-base-dev","latex-bin","latex-fonts","latexconfig","latexmk","lm","lualibs","luaotfload","luatex","makeindex","mathspec","metafont","mfware","natbib","oberdiek","pdftex","plain","tetex","tex","tex-ini-files","texlive.infra","times","tipa","titling","tools","unicode-data","upquote","url","xetex","xetexconfig","xkeyval","xunicode","zapfding","scheme-infraonly","colortbl","environ","makecell","multirow","tabu","threeparttable","threeparttablex","trimspaces","ulem","varwidth","wrapfig","xcolor"))
# username='SVC_THEATRICAL_LIGHTHOUSE'
# account='warnerbros'
# warehouse='WH_THEATRICAL_LIGHTHOUSE'
# database='THEATRICAL_LIGHTHOUSE'
# 
# selectizeInput(inputId = "scenario_mpaa", label = "MPAA Rating", choices = levels(mms$`MPAA Rating`),
#                options = list(
#                  placeholder = 'Select MPAA     ',
#                  onInitialize = I('function() { this.setValue(""); }'),
#                  create = FALSE))
# 
# # download the .jar from https://repo1.maven.org/maven2/net/snowflake/snowflake-jdbc/3.8.6/
# #setwd("C:/Users/alkrebs/OneDrive - Capgemini/Desktop/Projects/WB/DomesticDistribution/WireFraming/Greenlight_App/Greenlight_App")
# #options(dplyr.jdbc.classpath = "snowflake-jdbc-3.8.6.jar") 
# Sys.setenv(ODBCINI="/opt/rstudio-connect/mnt/app/odbc.ini") 
# print("connect snowflake")
# my_db <- odbc::dbConnect(odbc::odbc(), dsn = "snowflake", pwd=Sys.getenv("SF_SVC_PWD")) 
# print(sort(dbListTables(my_db)))
# print("end connect snowflake")
# ######


labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

table_col <- "#3c8dbc"



#Keep the column names from mms so we can easily bind_rows:
comps_selected <- mms %>% slice(0)#%>% select(-synopsis,-writer,-cast,-director,-voice)

#QUERY - COMPLETE - LOAD IN ALL SCENARIO DATA:
#scenarios <- dbGetQuery(my_db, "SELECT APP_SCENARIO.* FROM APP_PROJECT_LIST, APP_SCENARIO WHERE APP_SCENARIO.APP_PROJ_KEY = APP_PROJECT_LIST.APP_PROJ_KEY;")

print("check db")
#tryCatch(dbGetQuery(my_db, 'ALTER WAREHOUSE WH_THEATRICAL_LIGHTHOUSE RESUME;'),error=function(x){"already running"})
print("read scenario table")

#scenarios <- readRDS("data/000_scenarios.RDS")

#print(scenarios)

#Initial read of projects/scenarios/model coefficients:
#QUERY - COMPLETE - LOAD IN ALL PROJECT DATA:
#print("read project table")
#tryCatch(dbGetQuery(my_db, 'ALTER WAREHOUSE WH_THEATRICAL_LIGHTHOUSE RESUME;'),error=function(x){"already running"})

# print(str(project_read))
# project_read <- project_read

#print("check end project read")


#QUERY - PENDING - TICKET-PRICES
start_time <- Sys.time()

avg_ticket_price <- readRDS("data/000_avg_annual_ticketprice.RDS")

end_time <- Sys.time()

print(end_time-start_time)
print("Done read avd ticket")

#QUERY - PENDING - SUPERVISED MODEL COEFFICIENTS
start_time <- Sys.time()

scen_model <- read_csv("data/model_full.csv")[,-1]


end_time <- Sys.time()

print(end_time-start_time)
print("Done read coeff")

#Import demographic data (PostTrak.xlsx): Akshay Mahale
#postTrakData<-read_csv("data/PostTrak.csv")
#postTrackData<-postTrakData%>%select(-c(Title,Studio,`Domestic Box Office`:`Survey Week`,`Comp Titles`))%>%rename(Mcode=`MMS Code`)

postTrakDataExcel<-read_excel("data/PostTrak Edited.xlsx",sheet=1) #T32
#postTrakDataExcel<-read_excel("data/PostTrak Edited_updated_02-28-25-Sou Edited3.xlsx")
#postTrakDataExcel<-read_excel("data/PostTrak Edited2_Rose.xlsx")

postTrakDataExcel<-postTrakDataExcel%>%select(-c(Title,Studio,`Domestic Box Office`:`Survey Week`,`Comp Titles`))%>%rename(MCODE=`MMS Code`)
print("PostTrak")
# print(colnames(postTrakDataExcel))
# # print(head(postTrakDataExcel))
# print("PostTrak")

#Import CinemaScore data
cinemaScoreData<-read.csv("data/CinemaScoreORPC.csv",check.names=FALSE,encoding = "ISO-8859-1") # T32 Soumen CSV UTF-8 format
cinemaScoreData<-cinemaScoreData%>%select(-c(Title:Date,Rating))
# # print(head(cinemaScoreData))
# print(colnames(cinemaScoreData))
print("Cinema score data")
#gradeCinemascore<-read_excel("data/CinemascoreNew.xlsx",sheet=3)
gradeCinemascore<-read.csv("data/Grade_Data.csv",check.names=FALSE,encoding = "ISO-8859-1") #T32 Soumen

# print(dim(gradeCinemascore))

gradeCinemascore$GradeNew<-stringr::str_trim(gradeCinemascore$Grade)
# print(dim(gradeCinemascore))

gradeCinemascore<-gradeCinemascore%>%
  mutate(GradeNew=ifelse(GradeNew=="NA"|GradeNew==""|is.na(GradeNew),NA,GradeNew))
# print(dim(gradeCinemascore))

rainbow<-c("A+","A","A-","B+","B","B-","C+","C","C-","D+","D","D-","F",NA)
gradeCinemascore$Desc. <- factor(gradeCinemascore$Desc.,levels = c("25 and Up","Under 25","50 and Up","35-49","25-34", "18-24", "Under 18", "Females","Males"))
ordered <- factor(gradeCinemascore$GradeNew, levels = rainbow)
gradeCinemascore$GradeNewNum<-as.numeric(ordered)
gradeCinemascore$`% of Audience`<-as.numeric(gradeCinemascore$`% of Audience`)

gradeCinemascore$label<-paste0(gradeCinemascore$GradeNew," (",gradeCinemascore$`% of Audience`*100,"%)")
# print(dim(gradeCinemascore))
print("Cinemaore")
# 
# Machine Learning Batch --------------------------------------------------

#sl_batch <- data.table::fread("data/sl_batch.csv")

#sl_batch_lookup <- data.table::fread("data/sl_batch_matrix_lookup.csv")

#sl_colnames <- data.table::fread(file = "data/sl_batch_matrix_colnames.csv", stringsAsFactors = F)

# ML Supervised Coefficients (for use within the sup_model() function defined in helpers.R):
#scenario <- mms %>% filter(Title=="Aquaman")
#supervised_coefficients <- read_excel("data/supervised_coefficients.xlsx")
#supervised_coefficients <- read_excel("data/supervised_coefficients_new.xlsx")


#comp_universe <- mms


# Server - Start ----------------------------------------------------------

shinyServer(function(input, output, session) {
  # observe({
  #  str(rv_currentComps$currentComps)
  #  str(rv_refTable$tbl)
  # })
  
  
  re.escape <- function(strings){
    vals <- c("\\\\", 
              "\\{", "\\}", "\\^","\\*","\\_","\\$", 
              "\\#","\\%","\\&","\\@","\\~","\\'")
    replace.vals <- paste0("\\\\", vals)
    for(i in seq_along(vals)){
      strings <- gsub(vals[i], replace.vals[i], strings)
    }
    strings
  }
  
  #my_db <- odbc::dbConnect(odbc::odbc(), dsn = "snowflake", pwd=Sys.getenv("SF_SVC_PWD"))  #T20 Soumen 23.07.24
  if(dev_env==TRUE){
    my_db <- odbc::dbConnect(odbc::odbc(), dsn = "SnowflakeDSIIDriverDEV", pwd=Sys.getenv("SF_SVC_PWD"))
    database='BOLT_STUDIOS_THEATRICAL_LIGHTHOUSE_DEV'
  }else{
    my_db <- odbc::dbConnect(odbc::odbc(), dsn = "SnowflakeDSIIDriverPROD", pwd=Sys.getenv("SF_SVC_PWD"))
    database='BOLT_STUDIOS_THEATRICAL_LIGHTHOUSE_PROD'
  }
  read_query <- paste0("SELECT APP_FIELD, APP_COEFFICIENT FROM ",database,".ODS.APP_FIELD_COEFF")# Added T21 30.8.24 T33
  supervised_coefficients <- dbGetQuery(my_db,read_query )
  names(supervised_coefficients) <- c("field","coef")
  rv_updateSelects <- reactiveValues(i = 1)
  rv_updateFilterSelects <- reactiveValues(i = 1)
  #rv_checkMMSdataRefresh<-reactiveValues(i=1)
  
  
  #Optimise dashboard:Akshay
  rv_updateMMSFilters<-reactiveValues(i=1)
  rv_updateScenarioFilters<-reactiveValues(i=1)
  
  
  observeEvent(input$btn_compFilter,ignoreInit = TRUE,{
    if(rv_updateFilterSelects$i == 1){
      #Largest filters update later (cast + producers):
      #updateSelectizeInput(session,"filter_film",choices=mms %>% select(Title) %>% distinct(), server=F, selected="")
      updateSelectizeInput(session,"filter_film",choices=unique(mms$Title), server=TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000))#Soumen added 012.05.23
      #updateSelectizeInput(session,"filter_producer",choices = mms_producers,server = F, selected="")
      updateSelectizeInput(session,"filter_producer",choices = sort(as.character(mms_producers$.)),server = TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000))#Soumen added 04.05.23)
      
      rv_updateFilterSelects$i <- 2
    }
  })
  
  output$ScenarioNameError <- renderText({
    paste("You have selected", input$var)
  })
  
  # T23  --Soumen 
  #to ensure last training date availability at every application load
  output$last_training_date <- renderText({
    paste("Last trained on: ", format(training_timestamp(), "%Y-%m-%d %H:%M:%S %Z"))
  })

  
  
  
  #Highlight Bad comps:Akshay 
  rv_bdCmp<-reactiveValues(bad=NULL)
  rv_SelectedDetails<-reactiveValues(Scen=NULL,Proj=NULL)
  
  observeEvent(ignoreInit = TRUE,input$scenario_selection_rows_selected,{
    if(length(input$scenario_selection_rows_selected)>0)
    {
      
      rv_bdCmp$bad<-NULL
      ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
      sceneName <- rv_scenarios$scens %>%
        filter(Title == ttl) %>%
        select(Name)%>%
        slice(input$scenario_selection_rows_selected)
      
      scenarioDetailsFunc(sceneName,ttl)
      # print(paste0("SCENE ID: ",rv_SelectedDetails$Scen$APP_SCENARIO_KEY))
      sce_Id=rv_SelectedDetails$Scen$APP_SCENARIO_KEY
      projectDetailsFunc(rv_projectscenarios$project_title)
      # print(paste0("PROJ ID INSERT: ",rv_SelectedDetails$Proj$APP_PROJ_KEY))
      projId<-rv_SelectedDetails$Proj$APP_PROJ_KEY
      
      #selectBadCompQuery<-paste0('Select BAD_COMPS_MCODE,SCEN_NAME,SCEN_ID FROM THEATRICAL_LIGHTHOUSE.ODS.APP_BAD_COMPS WHERE SCEN_ID = ',sce_Id,' AND PROJ_ID=',projId)   #T20 Soumen 23.07.24
      selectBadCompQuery<-paste0('Select BAD_COMPS_MCODE,SCEN_NAME,SCEN_ID FROM ',database,'.ODS.APP_BAD_COMPS WHERE SCEN_ID = ',sce_Id,' AND PROJ_ID=',projId)
      print(paste0("QUERY SELECT BAD:",selectBadCompQuery))
      badC<- dbGetQuery(my_db,selectBadCompQuery)
      rv_bdCmp$bad<-badC
      print("bad comps selected")
      print(rv_bdCmp$bad)
      print(sceneName)
      
      print("scen details")
      
      # if(is.na(sce_Id))
      # {
      #   print("scen id again")
      #   scenDetail<- dbGetQuery(my_db,paste0("SELECT APP_SCENARIO_KEY FROM  THEATRICAL_LIGHTHOUSE.ODS.APP_SCENARIO WHERE SCEN_NAME='",sceneName,"'AND PROJ_TITLE='",ttl,"';"))
      #   print(scenDetail)
      #   selectBadCompQuery<-paste0('Select BAD_COMPS_MCODE,SCEN_NAME,SCEN_ID FROM THEATRICAL_LIGHTHOUSE.ODS.APP_BAD_COMPS WHERE SCEN_ID = ',scenDetail$APP_SCENARIO_KEY,'AND PROJ_ID=',rv_projectscenarios$proj_id)
      #   
      # }
      # else
      # {
      #   print("scen id again else")
      #   scenDetail<- dbGetQuery(my_db,paste0("SELECT APP_SCENARIO_KEY FROM  THEATRICAL_LIGHTHOUSE.ODS.APP_SCENARIO WHERE SCEN_NAME='",sceneName,"'AND PROJ_TITLE='",ttl,"';"))
      #   selectBadCompQuery<-paste0('Select BAD_COMPS_MCODE,SCEN_NAME,SCEN_ID FROM THEATRICAL_LIGHTHOUSE.ODS.APP_BAD_COMPS WHERE SCEN_ID = ',scenDetail$APP_SCENARIO_KEY,'AND PROJ_ID=',rv_projectscenarios$proj_id)
      #   
      # }
      
      
    }
  })
  
  
  start_time <- Sys.time()
  #mms <- dbGetQuery(my_db,'select * from "THEATRICAL_LIGHTHOUSE"."ODS"."TOBI_FULL_TABLE"; ')       #T20 Soumen 23.07.24
  mms <- dbGetQuery(my_db,paste0('select * from ',database,'.ODS.TOBI_FULL_TABLE; '))
  print("before join")
  ## print(head(mms,2))
  mms<-mms%>%left_join(postTrakDataExcel,by="MCODE")
  mms<-mms%>%left_join(cinemaScoreData,by=c("MCODE"="MMS Code"))
  #mms<-mms%>%left_join(cinemaScoreData,by=c("MCODE"="MMS Code"),relationship= "many-to-many")  #Edited with relationship Soumen 21.08.23
  print("after join")
  ## print(head(mms,2))
  mms <- mms %>% 
    mutate(dup = duplicated(MCODE)) %>% 
    filter(dup==0) %>% 
    select(-dup) %>% 
    rename(
      Title = TITLE_NAME,
      `Rel CF Dt` = REL_CF_DT,
      `MPM Number` = MPM_NUMBER,
      Mcode = MCODE,
      `Release Date` = RELEASE_DATE,
      `BO Unscheduled Release Description` = BO_UNSCHED_REL_DT_DESC,
      `Pattern Description` = PATTERN_DESC,
      `Short Pattern Description` = PATTERN_SHORT_DESC,
      `Release Week Number` = REL_WEEK_NUM,
      `Season Code` = SEASON_CD,
      `Season CF Code` = SEASON_CF_CD,
      `Title Type NK` = TITLE_TYPE_NK,
      `Picture Category` = PICTURE_CATEGORY,
      Minutes = MINUTES,
      `MPAA Rating` = MPAA_RATING_CD,
      `MPAA Rating Desc`= MPAA_RATING_DESC,
      `Cinema Score` = CINEMA_SCORE_CD,
      `Tomato Meter` = TOMATO_METER_NUM,
      `Primary Genre` = PRIMARY_GENRE,
      `Secondary Genre` = SECONDARY_GENRE,
      `Other Genre` = OTHER_GENRE,
      `Keyword 1` = KEYWORD_1,
      `Keyword 2` = KEYWORD_2,
      `Keyword 3` = KEYWORD_3,
      Distributor = DISTRIBUTOR,
      Studio = STUDIO,
      Synopsis = SYNOPSIS,
      Director = DIRECTOR,
      `Cast (Full)` = CASTS,
      Voice = VOICE,
      `Production Design`=PRODUCTION_DESIGN,
      Songs = SONGS,
      `Sound Editing`=SOUND_EDITING,
      Lyrics= LYRICS,
      `Set Decoration` = SET_DECORATION,
      Cinematographer = CINEMATOGRAPHER,
      Makeup = MAKEUP,
      Score = SCORE,
      `Sound Mixing` = SOUND_MIXING,
      Sound = SOUND,
      Producer= PRODUCER,
      Author = AUTHOR,
      Narrator = NARRATOR,
      `Costume Design` = COSTUME_DESIGN,
      Cameo = CAMEO,
      `Music Supervisor` = MUSIC_SUPERVISOR,
      `Executive Producer` = EXEC_PRODUCER,
      Writer = WRITER,
      Editor = EDITOR,
      `Visual Effects` = VISUAL_EFFECTS,
      `Art Direction` = ART_DIRECTION,
      Publicist = PUBLICIST,
      `DBO ($M)` = DOMESTIC_BO_AMT,
      `IBO ($M)` = INTERNATIONAL_BO_AMT,
      #`WBO ($M)` = WW_BO_AMT,           #Commented T19 18.06.24 Soumen
      #`WWBO ($M)` = WEEKEND_BO_AMT      #Commented T19 18.06.24 Soumen
      `WBO ($M)` = WEEKEND_BO_AMT,       #Added T19 18.06.24 Soumen
      `WWBO ($M)` = WW_BO_AMT            #Added T19 18.06.24 Soumen
      
    ) %>% 
    mutate(`Release Date` = as.Date(`Release Date`),
           Mcode = as.character(Mcode),
           `MPAA Rating` = factor(`MPAA Rating`,levels = c("G","PG","PG13","NC17","R","NR"),ordered = T)) %>% 
    mutate(`Release Day` = day(`Release Date`),
           `Release Year` = year(`Release Date`),
           `Release Month` = month(`Release Date`)) %>% 
    mutate(`Cast (Major)` = str_extract(`Cast (Full)`,".[^,]+,.[^,]+,.[^,]+,.[^,]+,|.[^,]+,.[^,]+,.[^,]+,|.[^,]+,.[^,]+,|.[^,]+")) %>%
    mutate(`Cast (Major)` = str_replace(`Cast (Major)`,",$",""),
           MCODE_MASTER = Mcode) %>% 
    select(
      "Title",  "Release Date",  "Director",  "Cast (Major)",
      "DBO ($M)",  "IBO ($M)",
      "WBO ($M)",  "WWBO ($M)",
      "Minutes",  "MPAA Rating",  "Cinema Score",  "Tomato Meter",
      "Primary Genre",  "Secondary Genre",
      "Other Genre",  "Keyword 1",  "Keyword 2",  "Keyword 3",
      "Distributor","Release Month",  "Release Day",  "Release Year","Release Month","Release Week Number",
      "Studio",  "Producer",  "Executive Producer",  "Writer",
      "Editor", "Cast (Full)", "MPM Number",  "Mcode",  "MPAA Rating Desc",
      "Synopsis",  "BO Unscheduled Release Description",
      "Pattern Description",  "Short Pattern Description",
      #"Pattern Description Comments",  
      "Season Code",
      "Season CF Code",  "Title Type NK",
      "Picture Category",  "Voice",  "Production Design",  "Songs",
      "Sound Editing",  "Lyrics",  "Set Decoration",  "Cinematographer",
      "Makeup",  "Score",  "Sound Mixing",  "Sound",
      "Author",  "Narrator",  "Costume Design",  "Cameo",
      "Music Supervisor",  "Visual Effects",
      "Art Direction",  "Publicist",  
      #"Release Year Check",  
      #"Release Era", 
      "Rel CF Dt",
      "FINAL_GROSS_IND",
      "MCODE_MASTER","Patrons_General":"Age Brkdwn_55+","Grade - 18-24":"Respondents Un 18","Overall","A/B %") %>% 
    mutate(`Supervised Rank` = 1000,
           `Smart Rank` = 1000,
           `Demo Rank`=1000) %>% 
    select(Title, `Smart Rank`, `Supervised Rank`,`Demo Rank`, everything()) %>% 
    mutate(`Cinema Score` = factor(`Cinema Score`,ordered = T, levels = c("F","D-","D","D+","C-","C",'C+',"B-","B","B+","A-","A","A+"))) %>% 
    column_to_rownames("MCODE_MASTER")
  
  #Join mms data with demographics
  print("mms columns")
  #print(colnames(mms))
  
  print(mms%>%filter(stringr::str_detect(Title,"Call")))
  
  ## print(head(postTrakData,5))
  #mms<-mms%>%left_join(postTrackData,by="Mcode")
  # print("mmsdata")
  # # print(head(mms,2))
  # # print(mmsData.head(4))
  # end_time <- Sys.time()
  # 
  # print(end_time-start_time)
  # print("Done read mms")
  
  mmsNew<-mms%>%gather(demographicKey,ValuePercentage,Patrons_General:`Age Brkdwn_55+`)
  #Visualization change to add tomato meter slabs
  mms$`Tomato Meter Range`<-cut(mms$`Tomato Meter`, breaks = seq(0, 100, by = 10),labels = c("0-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100"))
  
  end_time <- Sys.time()
  print("tomato meter")
  # print(mms%>%select(`Tomato Meter`,`Tomato Meter Range`,`Release Month`)%>%head(10))
  
  print("Done reading MMS")
  print(end_time-start_time)
  
  print("initialize reactives")
  #scenarios <- dbGetQuery(my_db, 'SELECT * FROM "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO";')   #T20 Soumen 23.07.24
  scenarios <- dbGetQuery(my_db, paste0('SELECT * FROM ',database,'.ODS.APP_SCENARIO;'))
  # print(str(scenarios))
  # # print(head(scenarios))
  scenarios <- scenarios %>% 
    rename(Title = PROJ_TITLE,
           Name = SCEN_NAME,
           `Created By` = SCEN_CREATED_BY,
           `Last Modified` = SCEN_LAST_MODIFIED,
           Low = FCST_LOW,
           Base = FCST_BASE,
           High = FCST_HIGH,
           #Int FCST: Akshay Mahale
           `Int Low`=FCST_LOW_INTL,
           `Int Base`=FCST_BASE_INTL,
           `Int High`=FCST_HIGH_INTL,
           
           `Int FR Low`=INT_FR_LOW,
           `Int FR Mid`=INT_FR_BASE,
           `Int FR High`=INT_FR_HIGH,
           
           `Int Prints Low`=INT_PRINTS_LOW,
           `Int Prints Mid`=INT_PRINTS_BASE,
           `Int Prints High`=INT_PRINTS_HIGH,
           
           `Int Ad/Pub Low`=INT_AD_PUB_LOW,
           `Int Ad/Pub Mid`=INT_AD_PUB_BASE,
           `Int Ad/Pub High`=INT_AD_PUB_HIGH,
           
           
           #China FCST: Akshay Mahale
           `China Fcst Low`=FCST_LOW_CHINA,
           `China Fcst Mid`=FCST_BASE_CHINA,
           `China Fcst High`=FCST_HIGH_CHINA,
           
           `China FR Low`=CHINA_FR_LOW,
           `China FR Mid`=CHINA_FR_BASE,
           `China FR High`=CHINA_FR_HIGH,
           
           `China Prints Low`=CHINA_PRINTS_LOW,
           `China Prints Mid`=CHINA_PRINTS_BASE,
           `China Prints High`=CHINA_PRINTS_HIGH,
           
           `China Ad/Pub Low`=CHINA_AD_PUB_LOW,
           `China Ad/Pub Mid`=CHINA_AD_PUB_BASE,
           `China Ad/Pub High`=CHINA_AD_PUB_HIGH,
           
           
           #Film Rental / Marketing NEED UPDATING
           Notes = NOTES,
           `Release Date` = SCEN_RELEASE_DATE,
           Director = SCEN_DIRECTOR,
           `Cast (Major)` = SCEN_CAST,
           `DBO ($M)` = SCEN_DBO,
           `IBO ($M)` = SCEN_IBO,
           `WBO ($M)` = SCEN_WBO,
           `WWBO ($M)` = SCEN_WWBO,
           Minutes = SCEN_MINUTES,
           `MPAA Rating` = SCEN_MPAA_RATING,
           `Cinema Score` = SCEN_CINEMA_SCORE,
           `Tomato Meter` = SCEN_TOMATO_METER,
           `Primary Genre` = SCEN_PRI_GENRE,
           `Secondary Genre` = SCEN_SEC_GENRE,
           `Other Genre` = SCEN_OTHER_GENRE,
           `Keyword 1` = SCEN_KEYWORD_1,
           `Keyword 2` = SCEN_KEYWORD_2,
           `Keyword 3` = SCEN_KEYWORD_3,
           #Release Day/year/week#
           Studio = SCEN_PROD_COMPANY,
           Producer = SCEN_PRODUCER,
           `Executive Producer` = SCEN_EXEC_PRODUCER,
           Writer = SCEN_WRITER,
           Editor = SCEN_EDITOR,
           #season cd
           Voice = SCEN_VOICE,
           #release era
           `Selected Comps` = SELECTED_COMPS,
           `Selected Comps Mcodes` = SELECTED_COMPS_MCODES,
           `Final Comps Mcodes` = FINAL_COMPS_MCODES,
           Submitted = SUBMITTED_TO_FIA,
           #Long Weekend
           `Production Budget ($M)` = SCEN_PROD_BUDGET,
           `Date Submitted` = SCEN_DATE_SUBMITTED,
           `FR Low` = FR_LOW,
           `FR Mid` = FR_BASE,
           `FR High` = FR_HIGH,
           `Ad/Pub Low` = AD_PUB_LOW,
           `Ad/Pub Mid` = AD_PUB_BASE,
           `Ad/Pub High` = AD_PUB_HIGH,
           `Prints Low` = PRINTS_LOW,
           `Prints Mid` = PRINTS_BASE,
           `Prints High` = PRINTS_HIGH,
           `Date Created` = SCEN_CREATED_DATE,
           `ThreeD Low` = THREED_LOW,
           `ThreeD Mid` = THREED_BASE,
           `ThreeD High` = THREED_HIGH,
           `Imax Low` = IMAX_LOW,
           `Imax Mid` = IMAX_BASE,
           `Imax High`=IMAX_HIGH,
           scen_id=APP_SCENARIO_KEY
    ) %>% 
    #Marketing filed to contain Ad/Pub Mid value instead of Print Mid Value:Akshay
    mutate(`Film Rental` = `FR Mid`,
           Marketing = `Ad/Pub Mid`) %>% 
    mutate(`Release Date` = as.Date(`Release Date`),
           `Date Created` = as.Date(`Date Created`),
           `Date Submitted`=as.Date(`Date Submitted`),
           `Last Modified` = as.POSIXct(`Last Modified`)) %>% 
    select(
      Title,
      Name,
      `Created By`,
      `Last Modified`,
      Low,
      Base,
      High,
      #Int FCST: Akshay Mahale
      `Int Low`,
      `Int Base`,
      `Int High`,
      `Int FR Low`,
      `Int FR Mid`,
      `Int FR High`,
      `Int Prints Low`,
      `Int Prints Mid`,
      `Int Prints High`,
      `Int Ad/Pub Low`,
      `Int Ad/Pub Mid`,
      `Int Ad/Pub High`,
      #China FCST: Akshay Mahale
      `China Fcst Low`,
      `China Fcst Mid`,
      `China Fcst High`,
      `China FR Low`,
      `China FR Mid`,
      `China FR High`,
      `China Prints Low`,
      `China Prints Mid`,
      `China Prints High`,
      `China Ad/Pub Low`,
      `China Ad/Pub Mid`,
      `China Ad/Pub High`,
      `Film Rental`,
      Marketing,
      Notes,
      `Release Date`,
      Director,
      `Cast (Major)`,
      `DBO ($M)`,
      `IBO ($M)`,
      `WBO ($M)`,
      `WWBO ($M)`,
      Minutes,
      `MPAA Rating`,
      `Cinema Score`,
      `Tomato Meter`,
      `Primary Genre`,
      `Secondary Genre`,
      `Other Genre`,
      `Keyword 1`,
      `Keyword 2`,
      `Keyword 3`,
      Studio,
      Producer,
      `Executive Producer`,
      Writer,
      Editor,
      Voice,
      `Selected Comps`,
      `Selected Comps Mcodes`,
      `Final Comps Mcodes`,
      Submitted,
      `Production Budget ($M)`,
      `Date Submitted`,
      `FR Low`,
      `FR Mid`,
      `FR High`,
      `Ad/Pub Low`,
      `Ad/Pub Mid`,
      `Ad/Pub High`,
      `Prints Low`,
      `Prints Mid`,
      `Prints High`,
      `Date Created`,
      `ThreeD Low`,
      `ThreeD Mid`,
      `ThreeD High`,
      `Imax Low`,
      `Imax Mid`,
      `Imax High`,
      scen_id
    ) %>% 
    arrange(desc(`Last Modified`))
  
  
  end_time <- Sys.time()
  
  print(end_time-start_time)
  print("Done reading scenarios")
  print("Scenario id")
  # # print(head(scenarios))
  
  rv_scenarios <- reactiveValues(scens = scenarios)
  
  
  #Flag to indicate whether a scenario is being edited or new
  rv_flgNewOrEdit <- reactiveValues(flg = "")
  
  #Flag to indicate whether a valid project is already selected
  rv_flgValidProject <- reactiveValues(flg=FALSE)
  
  #On the scenario building page, 
  # we will have a separate textbox for the title within the popup
  # and static text on the page to signify what scenario is being worked on
  rv_currentScenario <- reactiveValues(name = "",
                                       full_detail=scenarios %>% slice(0)) 
  
  
  rv_currentComps <- reactiveValues(currentComps = mms %>% slice(0) %>% select(-`Smart Rank`,-`Supervised Rank`,-`Demo Rank`))
  rv_currentComps$finalComps <- c()
  rv_currentCompsLength <- reactiveValues(t_0 = 0, t_1 = 0)
  
  rv_refTable <- reactiveValues(tbl = mms %>% slice(0) %>% select(-`Smart Rank`,-`Supervised Rank`,-`Demo Rank`))
  
  rv_scenInputs <- reactiveValues(inputs = mms %>% slice(0) %>% select(-`Smart Rank`,-`Supervised Rank`,-`Demo Rank`))
  
  rv_model_fcst <- reactiveValues(mod_scen_inputs = 0,
                                  mod_comps = 0)
  
  
  all_mcodes <- unique(mms$Mcode)
  
  rv_newProj <- reactiveValues(title="")
  
  
  #Project List:
  #project_read <-dbGetQuery(my_db, 'SELECT * FROM "THEATRICAL_LIGHTHOUSE"."ODS"."APP_PROJECT_LIST";') %>%   # T20 Soumen 23.07.24
  project_read <-dbGetQuery(my_db, paste0( 'SELECT * FROM ',database,'."ODS"."APP_PROJECT_LIST";')) %>%
    rename(Title = PROJ_TITLE,
           `Date Created` = APP_PROJ_CREATED_DATE,
           `Last Scenario` = APP_PROJ_LAST_MODIFIED,
           Rating = PROJ_MPAA_RATING,
           Genre = PROJ_GENRE,
           Keyword = PROJ_KEYWORD,
           Synopsis=SYNOPSIS,
           Status = PROJECT_ACTIVE_STATUS,
           `At a Glance` = AT_A_GLANCE_FLAG,
           proj_id=APP_PROJ_KEY) %>% 
    select(Title,
           `Date Created`,
           `Last Scenario`,
           Rating,
           Genre,
           Keyword,
           Synopsis,
           Status,
           `At a Glance`,
           proj_id) %>% 
    mutate(`Date Created` = as.Date(`Date Created`),
           `Last Scenario` = as.POSIXct(`Last Scenario`)) %>% 
    arrange(desc(`Last Scenario`))
  
  all_projects <- reactiveValues(projs = project_read)
  #all_projects <- reactiveValues(projs=readRDS("data/000_projects.RDS"))
  rv_projectList <- reactiveValues(projs = project_read)
  #rv_projectList <- reactiveValues(projs = readRDS("data/000_projects.RDS"))
  # # print(head(project_read))
  print("end reactives")
  
  # observe({
  #   print(input$tabs)
  # })
  
  
  
  #Update defaults:
  updateSelectizeInput(session, inputId = "projfilter_title",choices = sort(project_read$Title))
  updateDateRangeInput(session, inputId = "projfilter_created", end = Sys.Date())
  updateDateRangeInput(session, inputId = "projfilter_lastscen", end = Sys.Date())
  updateDateRangeInput(session, inputId = "filter_release", end = Sys.Date())
  
  #Dynamic Selectize input Akshay
  
  updateSelectizeInput(session, inputId = "projfilter_mpaa",choices = levels(mms$`MPAA Rating`),options = list(virtualScroll=TRUE, maxOptions = 1000))
  updateSelectizeInput(session, inputId = "projfilter_genre",choices = unique(c(mms$`Primary Genre`,mms$`Secondary Genre`,mms$`Other Genre`)))
  updateSelectizeInput(session, inputId = "projfilter_keyword",choices = mms_keywords)
  updateSelectizeInput(session, inputId = "scenario_mpaa",choices = levels(mms$`MPAA Rating`))
  updateSelectizeInput(session, inputId = "scenario_cinemascore",choices = sort(unique(mms$`Cinema Score`))[-which(is.na(unique(mms$`Cinema Score`)))])
  updateSelectizeInput(session, inputId = "lookupFilm",choices = mms$Title)
  updateSelectizeInput(session, inputId = "filter_mpaa",choices = levels(mms$`MPAA Rating`))
  updateSelectizeInput(session, inputId = "filter_cinemascore",choices = rev(levels(mms$`Cinema Score`)),options = list(virtualScroll=TRUE, maxOptions = 1000))
  
  
  
  
  output$report_at_a_glance <- downloadHandler(
    filename = function() {
      if(input$ataglance_pdf == "PDF"){
        paste0("At a Glance - ",Sys.Date(),".pdf")
      }else{
        paste0("At a Glance - ",Sys.Date(),".xlsx")
      }
    },
    content = function(file) {
      included_films <- rv_projectList$projs %>% filter(`At a Glance` == "Include") %>% select(Title) %>% pull()
      df <- rv_scenarios$scens %>% filter(Title %in% included_films)
      
      
      #saveRDS(df, "current_estimate_Excel_df.RDS")
      
      #showNotification(strong(paste("You have successfully outputted a 'Current Estimates Report' in Excel.")), duration = 5)
      if(input$ataglance_pdf == "PDF"){
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("rmd_report_templates/ataglance_template.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(scens = df)
        print("params")
        
        director_rows <- df %>% 
          select(Title, Name, `Date Submitted`,Submitted, Director,`Production Budget ($M)`, `Primary Genre`, Notes, `Last Modified`, Low, Base, High,`Int Low`,`Int Base`,`Int High`) %>% 
          mutate(`Global Low`=ifelse(is.na(Low),0,Low)+ifelse(is.na(`Int Low`),0,`Int Low`),
                 `Global Base`=ifelse(is.na(Base),0,Base)+ifelse(is.na(`Int Base`),0,`Int Base`),
                 `Global High`=ifelse(is.na(High),0,High)+ifelse(is.na(`Int High`),0,`Int High`))%>%
          rename(` Low`=`Int Low`,` Base`=`Int Base`,` High`=`Int High`,`  Low`=`Global Low`,`  Base`=`Global Base`,`  High`=`Global High`)%>%
          rename(Talent = Director) %>%
          #mutate(Talent = ifelse(nchar(Talent) > 20, paste0(substr(Talent,1,20),"..."), Talent)) %>%
          rename(`Prod (\\$M)` = `Production Budget ($M)`) %>% 
          mutate(flg_dir = 1) %>%
          mutate(Talent = paste0("\\textbf{",Talent,"}"))
        #print(names(director_rows))
        
        actor_rows <- df %>%
          select(Title, Name, `Last Modified`,`Cast (Major)`) %>%
          rename(Talent = `Cast (Major)`) %>%
          #mutate(Talent = ifelse(nchar(Talent) > 20, paste0(substr(Talent,1,20),"..."), Talent)) %>%
          mutate(flg_dir = 0)
        print("............")
        
        full_dta <- bind_rows(director_rows,actor_rows) %>%
          arrange(Title,
                  #desc(`Last Modified`),
                  Name,
                  desc(flg_dir)) %>%
          #Adjust length of column:Akshay
          #mutate(Name = ifelse(nchar(Name) > 20, paste0(substr(Name,1,20),"..."), Name)) %>%
          #mutate(Title= ifelse(nchar(Title) > 25, paste0(substr(Title,1,25),"..."), Title))%>%
          
          group_by(Title) %>%
          mutate(scen_n = cumsum(flg_dir)) %>%
          group_by(Title) %>%
          mutate(flg_top = scen_n == 1 & flg_dir==1,
                 flg_bottom = scen_n == max(scen_n) & flg_dir==0) %>%
          ungroup() %>%
          rename(FIA = Submitted) %>%
          mutate(FIA = ifelse(FIA == "Submitted","X", NA)) %>%
          mutate(`Last Modified` = paste0(month(`Last Modified`),"/",day(`Last Modified`),"/",year(`Last Modified`)),
                 `Date Submitted` = ifelse(is.na(`Date Submitted`),"",paste0(month(`Date Submitted`),"/",day(`Date Submitted`),"/",year(`Date Submitted`)))
          ) %>%
          mutate(`Last Modified` = if_else(flg_dir==1, `Last Modified`, ""),
                 `Date Submitted` = if_else(flg_dir==1, `Date Submitted`, ""),
                 #`Title`=ifelse(flg_dir==1,`Title`,NA),
                 Name = ifelse(flg_dir==1, Name, NA),
                 Title = ifelse(flg_top == 1, Title,"")) %>%
          mutate(Title=re.escape(Title))%>%
          mutate(Name=re.escape(Name))%>%
          mutate(Notes=re.escape(Notes))
        
        
        
        
        #mutate(Name = str_replace_all(Name,"_"," - ")) %>%
        #mutate(Title = str_replace_all(Title,"_"," - ")) %>%
        # mutate(Notes = str_replace_all(Notes,"#","No.")) %>%
        # mutate(Notes = str_replace_all(Notes,"&","and")) %>%
        # mutate(Notes = str_replace_all(Notes,"%","pct."))
        
        bottoms <- which(full_dta$flg_bottom == 1)
        
        full_dta <- full_dta %>%
          select(1:18)
        # full_dta
        full_dta<-as.data.frame(full_dta)
        
        full_dta[is.na(full_dta)]<-""
        
        # print(full_dta[is.na(full_dta)])
        
        # Knit the document, passing in the `params` list, and evaluate it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        #file.copy(paste0("at-a-glance/out1.pdf"),file)
      }else{
        ataglance_to_excel(dta = df,filename = "out1",toPDF=input$ataglance_pdf)
        file.copy(paste0("at-a-glance/out1.xlsx"),file)
      }
    }
  )
  output$report_fulldetail <- downloadHandler(
    filename = function() {
      if(input$ataglance_pdf == "PDF"){
        paste0("Full Summary - ",Sys.Date(),".pdf")
      }else{
        paste0("Full Summary - ",Sys.Date(),".xlsx")
      }
    },
    content = function(file) {
      df <- rv_scenarios$scens
      print("colnames df")
      # print(colnames(df))
      # # print(head(df))
      
      if(input$ataglance_pdf == "PDF"){
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("rmd_report_templates/full_detail_template.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(scens = df
        )
        #print(params)
        # print(str(params))
        
        # Knit the document, passing in the `params` list, and evaluate it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        #file.copy(paste0("at-a-glance/out2.pdf"),file)
      }else{
        print("full detail")
        #write.csv(df,"data/Scenario.csv")
        fulldetail_to_excel(dta = df,filename = "out2", toPDF = input$ataglance_pdf)
        file.copy(paste0("at-a-glance/out2.xlsx"),file)
      }
    }
  )
  
  
  # Downloadable csv of selected dataset ----
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("Download", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(rv_scenarios$scens, file, row.names = FALSE)
  #   }
  # )
  
  output$report_comps1 <- downloadHandler(
    filename = function() {
      if(input$comps1_pdf == "PDF"){
        paste0("Comps Report - ",Sys.Date(),".pdf")
      }else{
        paste0("Comps Report - ",Sys.Date(),".xlsx")
      }
    },
    content = function(file) {
      #df <- rv_scenarios$scens[input$scenario_selection_rows_selected,]
      df <- rv_scenarios$scens %>% filter(Title == rv_projectscenarios$project_title &
                                            Name == rv_projectscenarios$scenario_names[input$scenario_selection_rows_selected])
      
      
      
      if(input$comps1_pdf == "PDF"){
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("rmd_report_templates/comps_template.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(report_title = df$Title[1],#rv_projectscenarios$project_title,
                       scen = df, #rv_currentScenario$full_detail,
                       directors = df$Director[1],#rv_currentScenario$full_detail$Director,
                       cast = df$`Cast (Major)`[1],#rv_currentScenario$full_detail$`Cast (Major)`,
                       genre = df$`Primary Genre`[1],#rv_currentScenario$full_detail$`Primary Genre`,
                       budget = df$`Production Budget ($M)`[1],#rv_currentScenario$full_detail$`Production Budget ($M)`,
                       mms = mms,
                       avg_ticket_price = avg_ticket_price
        )
        #print(params)
        #print(str(params))
        
        # Knit the document, passing in the `params` list, and evaluate it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        file.copy(paste0("at-a-glance/comps1.pdf"),file)
      }else{
        comps_report_to_excel(df, projects = rv_projectList$projs,avg_ticket_price=avg_ticket_price, toPDF = input$comps1_pdf)
        file.copy(paste0("at-a-glance/comps1.xlsx"),file)
      }
    }
  )
  
  
  
  
  # FIA Report Start: Akshay Mahale
  output$reports_fia <- downloadHandler(
    filename = function() {
      ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
      dta1 <- rv_scenarios$scens %>%
        filter(Title == ttl) %>%
        slice(input$scenario_selection_rows_selected)
      print("dta1 here now")
      # print(dta1)
      
      if(input$comps1_pdf == "PDF"){
        paste0("FIA Report - ",dta1$Title,".pdf")
      }else{
        paste0("FIA report - ",dta1$Title,".xlsx")
      }
    },
    content = function(file) {
      ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
      dta1 <- rv_scenarios$scens %>%
        filter(Title == ttl) %>%
        slice(input$scenario_selection_rows_selected)
      print("dta1")
      # print(dta1)
      finalCompsMcodes<- dta1 %>% select(`Final Comps Mcodes`) %>% pull() %>% str_split(.,", ") %>% unlist
      #finalcompDetails <- mms %>% select(Title,`Release Date`,`DBO ($M)`,Mcode)%>%filter(Mcode %in% finalCompsMcodes)%>%mutate(scene_name=paste0(Title," (",substr(`Release Date`,3,4),")"))%>%arrange(desc(`DBO ($M)`))%>%rename(dbo=`DBO ($M)`)
      #finalcompDetails<-finalcompDetails%>%mutate(UnadjAvgY=" ",AdjBox="-",AdjAvgY=" ")
      SelectedCompsMcodes<- dta1 %>% select(`Selected Comps Mcodes`) %>% pull() %>% str_split(.,", ") %>% unlist
      finalcompDetails <- mms %>% select(Title,`Release Date`,`DBO ($M)`,Mcode)%>%filter(Mcode %in% SelectedCompsMcodes)%>%mutate(scene_name=paste0(Title," (",substr(`Release Date`,3,4),")"))%>%arrange(desc(`DBO ($M)`))%>%rename(dbo=`DBO ($M)`)
      finalcompDetails<-finalcompDetails%>%mutate(UnadjAvgY=ifelse(Mcode %in% finalCompsMcodes," ","Y" ),AdjBox="-",AdjAvgY=" ")
      print("akshay final comps")
      # print(finalcompDetails)
      if(input$comps1_pdf == "PDF"){
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("rmd_report_templates/fia_report_template.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(df = finalcompDetails)
        #print(params)
        #print(str(params))
        
        # Knit the document, passing in the `params` list, and evaluate it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        file.copy(paste0("at-a-glance/comps1.pdf"),file)
      }else{
        
        fia_report(dta = finalcompDetails,filename = "out1",toPDF=input$ataglance_pdf)
        file.copy(paste0("at-a-glance/out1.xlsx"),file)
      }
    }
  )
  
  
  
  
  # output$report_comps2 <- downloadHandler(
  #   filename = function() {
  #     if(input$comps1_pdf == "PDF"){
  #       paste0("Comps Report - ",Sys.Date(),".pdf")
  #     }else{
  #       paste0("Comps Report - ",Sys.Date(),".xlsx")
  #     }
  #   },
  #   content = function(file) {
  #     #df <- rv_scenarios$scens[input$scenario_selection_rows_selected,]
  #     df <- rv_currentScenario$full_detail
  #     
  #     comps_report_to_excel(df, projects = rv_projectList$projs,avg_ticket_price=avg_ticket_price, toPDF = input$comps1_pdf)
  #     
  #     if(input$comps1_pdf == "PDF"){
  #       file.copy(paste0("at-a-glance/comps1.pdf"),file)
  #     }else{
  #       file.copy(paste0("at-a-glance/comps1.xlsx"),file)
  #     }
  #   }
  # )
  observe({
    toggleState("report_comps2",condition = rv_flgNewOrEdit$flg=="Edit")
  })
  
  
  
  
  observe({
    rv_projectList$projs <- all_projects$projs %>% 
      filter(Title %in% projflt_film()&
               Title %in% projflt_created()&
               Title %in% projflt_lastscen()&
               Title %in% projflt_mpaa()&
               Title %in% projflt_genre()&
               Title %in% projflt_keyword()&
               Title %in% projflt_status()&
               Title %in% projflt_ataglance()
      )
    
    print("CHECK111")
    #print(all_projects$projs)
    # print(rv_projectList$projs)
    # 
    # print("start1")
    # print(rv_projectList$projs)
    # print(rv_projectList$projs$`Last Scenario`)
    # print(date(rv_projectList$projs$`Last Scenario`))
    # print(rv_projectList$projs$Genre)
    # print(rv_projectList$projs$Rating)
    # print(rv_projectList$projs$`At a Glance`)
    # print("chck")
    # print(projflt_film())
    # print(projflt_created())
    # print(projflt_lastscen())
    # print(projflt_mpaa())
    # print(projflt_genre())
    # print(projflt_keyword())
    # print(projflt_status())
    # print(projflt_ataglance())
    print("endck")
    
  })
  
  observe({
    print("proj selection render")
    output$project_selection <- renderDataTable({
      
      print("ak485")
      out <- rv_projectList$projs%>%select(-proj_id)
      #out <- rv_projectList$projs
      # print(head(out))
      # print('ak487')
      # print(all_projects)
      
      targs_large <- match(c("Cast (Major)","Synopsis","Cast (Full)"),colnames(out)) - 1
      targs_mid <- match(c("Title","Director","Executive Producer","MPAA Rating Desc","Producer","Writer","Editor","BO Unscheduled Release Description","Pattern Description Comments"),colnames(out)) - 1
      targs_small <- match(c("DBO ($M)","IBO ($M)","WBO ($M)","WWBO ($M)","Release Week Number","Short Pattern Description","Season CF Code","Title Type NK","Release Year Check","Rel CF Dt"),colnames(out))-1
      
      
      #colnames(out) = paste0('<span style="color:white">',colnames(out),'</span>')
      
      DT::datatable(out,colnames = paste0('<span style="color:white">',colnames(out),'</span>'),
                    filter = "none",
                    selection = list(mode="single",target="row"),
                    rownames = FALSE,
                    
                    class = "cell-border stripe",
                    
                    extensions = c('FixedColumns','FixedHeader'),
                    escape = FALSE, #this argument allows for COLUMN NAMES to HAVE HTML STYLING
                    plugins = 'ellipsis',
                    options = list(
                      dom = "tp",
                      pageLength = input$n_projects,
                      #lengthMenu = list(c(5,10,-1),c('5','10','All')),
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#000'});",
                        "}"),
                      columnDefs = list(#list(width = '500px', targets = targs_large),
                        list(width = '150px', targets=c(targs_mid, targs_large)),
                        list(width = '80px', targets=targs_small),
                        list(targets = c(targs_mid, targs_large),
                             render = JS("$.fn.dataTable.render.ellipsis( 22, false )")
                        )),
                      language = list(
                        #lengthMenu = '<span style = "color:black">Projects per page: </span>_MENU_   ',
                        #search = '<span style = "color:black"></span>', #placeholder below saves space
                        #searchPlaceholder = 'Search',
                        info = '<span style = "color:black">Showing _START_ to _END_ of _TOTAL_ entries</span>',
                        paginate = list(previous = '<span style = "color:black">Previous</span>', `next` = '<span style = "color:black">Next</span>')
                      ))) %>% 
        formatDate(.,~`Last Scenario`+`Date Created`,method = "toLocaleDateString")#%>% formatStyle(columns=c(1,2,5,6), width = '200px')
    })
    print("end proj selection render")
  })
  
  observeEvent(ignoreInit=TRUE,input$btn_loadData,{
    print("In Load Data")
  })
  
  # Modal for new project entry-----
  observeEvent(ignoreInit=TRUE,input$btn_newProject,{
    showModal(modal_newProject)
  })
  modal_newProject <- modalDialog(title = "New Project Entry", size = "l",easyClose=T, footer = NULL,
                                  #actionButton(inputId = "btn_new_reset", label = "Reset Inputs", icon = icon("refresh")),
                                  #insert_breaks(2),
                                  fluidRow(
                                    column(width = 6,
                                           # [AK - DISABLE THIS ORIGINALLY TO VERIFY TITLE IS NOT BLANK AND DOESN'T ALREADY EXIST]
                                           textInput(inputId = "new_title", label = labelMandatory("Title"))
                                    )),   
                                  br(),
                                  #the project name they have typed already exists: Akshay
                                  fluidRow(
                                    column(width = 6,
                                           shinyjs::hidden(p(id="error_title","The project title already exists.",style="color: red;"))
                                    )  
                                  ),
                                  fluidRow(
                                    column(width = 6,
                                           #shinyjs::hidden(p(id="error_titleChr","The project title should not end with $ or '.",style="color: red;"))
                                           shinyjs::hidden(p(id="error_titleChr","The project title should not end with $",style="color: red;"))
                                           
                                    )  
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width = 6,
                                           selectizeInput(inputId = "new_mpaa",
                                                          label = "Anticipated Rating",
                                                          choices = c("G","PG","PG13","R","NC17","NR"),
                                                          options = list(
                                                            placeholder = 'Select Rating     ',
                                                            onInitialize = I('function() { this.setValue(""); }'),
                                                            create = FALSE)
                                           )
                                    ),
                                    column(width = 6,
                                           selectizeInput(inputId = "new_genre",
                                                          label = "Genre(s) (Max. 3)",
                                                          choices = mms_genres,
                                                          options = list(
                                                            maxItems = 3,
                                                            placeholder = 'Select Genre     ',
                                                            onInitialize = I('function() { this.setValue(""); }'),
                                                            create = FALSE)
                                           )
                                    )
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width = 6,
                                           selectizeInput(inputId = "new_keyword",
                                                          label = "Keyword(s) (Max. 3)",
                                                          choices = mms_keywords,
                                                          options = list(
                                                            maxItems = 3,
                                                            placeholder = 'Select Keyword(s)     ',
                                                            onInitialize = I('function() { this.setValue(""); }'),
                                                            create = FALSE)
                                           )
                                    ),
                                    column(width = 6,
                                           selectizeInput(inputId = "new_activeFlg",
                                                          label="Active/Inactive",
                                                          choices=c("Active","Inactive"),
                                                          width =NULL)
                                    )
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width = 6,
                                           selectizeInput(inputId = "new_ataglance",
                                                          label = "Include in At-a-Glance Report?",
                                                          choices=c("Include","Do Not Include"))
                                    )
                                  ),
                                  br(),
                                  fluidRow(
                                    column(width = 12,
                                           textAreaInput("new_synopsis",label = "Synopsis",height = NULL,width = "250%", resize = "both")
                                    )
                                  ),
                                  insert_breaks(2),
                                  # [AK - MAKE SURE WHEN A FILM SHARES THE SAME NAME THAT A WARNING/CONFIRM BOX POPS UP TO EITHER CANCEL OR OVERWRITE EXISTING NAME
                                  shinyjs::disabled(actionButton("btn_saveNewProject", label = HTML("Save Project"), icon = icon("save")))
                                  
  )
  
  
  
  #Only allow user to save project when a VALID (new) Title is entered
  observe({
    #the project name they have typed already exists: Akshay
    #shinyjs::toggle("error_title",condition = input$new_title%in%rv_projectList$projs$Title==TRUE)
    #shinyjs::toggle("error_title",condition = str_trim(tolower(input$new_title))%in%unlist(lapply(lapply(rv_projectList$projs$Title,tolower),str_trim))==TRUE)   #Commented Soumen 20.07.23 Ticket:T12
    shinyjs::toggle("error_title",condition = str_trim(tolower(input$new_title))%in%unlist(lapply(lapply(all_projects$projs$Title,tolower),str_trim))==TRUE)      #Added Soumen 20.07.23 Ticket:T12
    shinyjs::toggle("error_titleChr",condition = checkSpChar(input$new_title)==TRUE)
    
    #shinyjs::toggleState("btn_saveNewProject",condition = !is.null(input$new_title) & input$new_title!=""& str_trim(tolower(input$new_title))%in%unlist(lapply(lapply(rv_projectList$projs$Title,tolower),str_trim)) ==FALSE & checkSpChar(input$new_title)==FALSE)  # Commented Soumen 08.08.23 Ticket:T12
    shinyjs::toggleState("btn_saveNewProject",condition = !is.null(input$new_title) & input$new_title!=""& str_trim(tolower(input$new_title))%in%unlist(lapply(lapply(all_projects$projs$Title,tolower),str_trim)) ==FALSE & checkSpChar(input$new_title)==FALSE)     #Added Soumen 08.08.23 Ticket:T12
    #shinyjs::toggle("error_scenario",condition =endsWith(input$scenario_name,"$") | endsWith(input$scenario_name,"'")==TRUE)
    
  })
  #Save new project
  observeEvent(ignoreInit=TRUE,input$btn_saveNewProject,{
    shinyjs::disable("btn_saveNewProject")
    tmp <- data.frame(Title = input$new_title,
                      `Date Created` = Sys.Date(),
                      `Last Scenario`= Sys.time(),
                      Rating = input$new_mpaa,
                      Genre = paste(input$new_genre,collapse = ", "),
                      Keyword = paste(input$new_keyword,collapse = ", "),
                      Synopsis = input$new_synopsis,
                      Status = input$new_activeFlg,
                      `At a Glance` = input$new_ataglance,
                      stringsAsFactors = F, check.names = F) %>% mutate(`Last Scenario`=NA)
    # print(input$new_activeFlg)
    #QUERY - COMPLETE - WRITE NEW PROJECT-----
    print("save new 1")
    #print(paste0('INSERT INTO "THEATRICAL_LIGHTHOUSE"."ODS"."APP_PROJECT_LIST"',"(PROJ_TITLE,PROJ_MPAA_RATING,PROJ_GENRE,PROJ_KEYWORD,PROJECT_ACTIVE_STATUS,AT_A_GLANCE_FLAG,SYNOPSIS,APP_PROJ_CREATED_DATE,APP_PROJ_LAST_MODIFIED,APP_PROJ_CREATED_BY) SELECT $$",  #T20 Soumen 23.07.24
    print(paste0('INSERT INTO ',database,'."ODS"."APP_PROJECT_LIST"',"(PROJ_TITLE,PROJ_MPAA_RATING,PROJ_GENRE,PROJ_KEYWORD,PROJECT_ACTIVE_STATUS,AT_A_GLANCE_FLAG,SYNOPSIS,APP_PROJ_CREATED_DATE,APP_PROJ_LAST_MODIFIED,APP_PROJ_CREATED_BY) SELECT $$",
                 input$new_title,"$$, '",
                 input$new_mpaa,"',$$",
                 paste(input$new_genre,collapse = ", "),"$$, $$",
                 paste(input$input$new_keyword,collapse = ", "),"$$, '",
                 input$new_activeFlg,"', '",
                 input$new_ataglance,"', $$",
                 input$new_synopsis,"$$, '",
                 Sys.time(),"', '",
                 Sys.time(),"', '",
                 session$user,"';"))
    dbGetQuery(my_db, 
               #paste0('INSERT INTO "THEATRICAL_LIGHTHOUSE"."ODS"."APP_PROJECT_LIST"',"(PROJ_TITLE,PROJ_MPAA_RATING,PROJ_GENRE,PROJ_KEYWORD,PROJECT_ACTIVE_STATUS,AT_A_GLANCE_FLAG,SYNOPSIS,APP_PROJ_CREATED_DATE,APP_PROJ_LAST_MODIFIED,APP_PROJ_CREATED_BY) SELECT $$",   #T20 Soumen 23.07.24
               paste0('INSERT INTO ',database,'."ODS"."APP_PROJECT_LIST"',"(PROJ_TITLE,PROJ_MPAA_RATING,PROJ_GENRE,PROJ_KEYWORD,PROJECT_ACTIVE_STATUS,AT_A_GLANCE_FLAG,SYNOPSIS,APP_PROJ_CREATED_DATE,APP_PROJ_LAST_MODIFIED,APP_PROJ_CREATED_BY) SELECT $$",
                      input$new_title,"$$, '",
                      input$new_mpaa,"', $$",
                      paste(input$new_genre,collapse = ", "),"$$, $$",
                      paste(input$input$new_keyword,collapse = ", "),"$$, '",
                      input$new_activeFlg,"', '",
                      input$new_ataglance,"', $$",
                      input$new_synopsis,"$$, '",
                      Sys.time(),"', '",
                      Sys.time(),"', '",
                      session$user,"';"))
    print("save new 2")
    rv_projectscenarios$project_title <- input$new_title
    print("genre22")
    rv_projectscenarios$project_genre <- unlist(strsplit(tmp$Genre,", "))
    print("rating22")
    rv_projectscenarios$project_rating <- input$new_mpaa
    print("kwd22")
    rv_projectscenarios$project_keywords<- unlist(strsplit(tmp$Keyword,", "))
    print("sn22")
    rv_projectscenarios$scenario_names <- ""
    print("ttl22")
    rv_newProj$title <- input$new_title
    #Delete Project :Akshay
    # projectDetailsFunc(input$new_title)
    # tmp$projId<-rv_SelectedDetails$Proj$APP_PROJ_KEY
    #all_projects$projs <- readRDS("data/000_projects.RDS")
    all_projects$projs <- bind_rows(tmp,all_projects$projs)
    #Delete Project :Akshay
    #rv_projectList$projs <- bind_rows(tmp,rv_projectList$projs)
    
    #print(all_projects$projs)
    print("New project added")
    # print(head(rv_projectList$projs))
    #saveRDS(all_projects$projs,"data/000_projects.RDS")
    
    # updatePickerInput(session,"projfilter_title",choices = sort((all_projects$projs$Title)))
    updateSelectizeInput(session,"projfilter_title",choices = sort((all_projects$projs$Title)))
    updateDateRangeInput(session, "projfilter_created",start = min(all_projects$projs$`Date Created`),end = max(all_projects$projs$`Date Created`))
    shinyjs::enable("btn_saveNewProject")
    removeModal()
    shinyWidgets::confirmSweetAlert(session,inputId = "new_success",
                                    title = "New Project Successfully Created",
                                    type = "success",btn_labels = c("Return to Project Dashboard","Build New Scenario"),
                                    closeOnClickOutside = T)
  })
  
  # Activate button to edit existing project (sel1()) AND navigate to scenario input page (sel1 and sel2)----
  sel1 <- reactive({!is.null(input$project_selection_rows_selected)})
  sel2 <- reactive({!is.null(input$scenario_selection_rows_selected)})
  
  ###Delete Project
  #Delete Scenario: Akshay Mahale
  observeEvent(ignoreInit = TRUE,input$btn_deleteProject,{
    shinyWidgets::confirmSweetAlert(session,inputId = "confrim_delete_project",
                                    title = "Are you sure you want to delete this project?",
                                    text="Deleting this project will also delete all Scenarios for this project.",
                                    type = "question",btn_labels = c("No","Yes"),
                                    closeOnClickOutside = T)
    
  })
  
  
  #Delete Project: Akshay Mahale
  observeEvent(ignoreInit=TRUE,input$confrim_delete_project,{
    if(input$confrim_delete_project == TRUE){
      if(length(input$project_selection_rows_selected)>0){
        ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
        projId <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(proj_id) %>% pull()
        print("projId before")
        # print(projId)
        if(is.na(projId)){
          projectDetailsFunc(ttl)
          projId<-rv_SelectedDetails$Proj$APP_PROJ_KEY
        }
        print("projId after")
        # print(projId)
        #myqueryDelProj<-paste0('DELETE FROM THEATRICAL_LIGHTHOUSE.ODS.APP_PROJECT_LIST WHERE PROJ_TITLE LIKE ',"$$",ttl,"$$",' AND APP_PROJ_KEY = ',projId)   #T20 Soumen 23.07.24
        myqueryDelProj<-paste0('DELETE FROM ',database,'.ODS.APP_PROJECT_LIST WHERE PROJ_TITLE LIKE ',"$$",ttl,"$$",' AND APP_PROJ_KEY = ',projId)
        print(paste0("Delete query",myqueryDelProj))
        dbGetQuery(my_db,myqueryDelProj)
        print("in delet project")
        # # print(head(rv_projectList$projs))
        #rv_projectList$projs<-rv_projectList$projs%>%filter(!Title %in% c(ttl))
        all_projects$projs<-all_projects$projs%>%filter(!Title %in% c(ttl))
        
        #myqueryDel<-paste0('DELETE FROM THEATRICAL_LIGHTHOUSE.ODS.APP_SCENARIO WHERE PROJ_TITLE LIKE ',"$$",ttl,"$$")  #T20 Soumen 23.07.24
        myqueryDel<-paste0('DELETE FROM ',database,'.ODS.APP_SCENARIO WHERE PROJ_TITLE LIKE ',"$$",ttl,"$$") 
        # print(paste0("Delete query",myqueryDel))
        dbGetQuery(my_db,myqueryDel)
        #rv_projectList$projs<-rv_projectList$projs%>%filter(!Title %in% c(ttl))
        rv_scenarios$scens<-rv_scenarios$scens%>%filter(!Title %in% c(ttl))
        updateProjectScenarios()
        # print(projId)
        print("idsss")
        print(rv_projectList$projs%>%filter(!proj_id %in% c(projId))%>%select(proj_id))
        
        # sceneName <- rv_scenarios$scens %>%
        #   filter(Title == ttl) %>%
        #   select(Name)%>%
        #   slice(input$scenario_selection_rows_selected)
        # print(rv_scenarios$scens)
        # start_time=Sys.time()
        # myqueryDel<-paste0('DELETE FROM THEATRICAL_LIGHTHOUSE.ODS.APP_SCENARIO WHERE SCEN_NAME LIKE ',"$$",sceneName,"$$",' AND PROJ_TITLE LIKE ',"$$",ttl,"$$")
        # print(paste0("Delete query",myqueryDel))
        # dbGetQuery(my_db,myqueryDel)
        # end_time=Sys.time()
        # print("Done Deleting Scenario")
        # print(end_time-start_time)
        # rv_scenarios$scens<-rv_scenarios$scens%>%filter(!Name %in% c(sceneName))
        # updateProjectScenarios()
        
        
      }
    }
    else
    {
      shinyWidgets::closeSweetAlert(session)
    }
    
    
  })
  
  
  
  
  #Delete Scenario: Akshay Mahale
  observeEvent(ignoreInit = TRUE,input$btn_deleteScenario,{
    shinyWidgets::confirmSweetAlert(session,inputId = "confrim_delete",
                                    title = "Are you sure you want to delete this scenario?",
                                    type = "question",btn_labels = c("No","Yes"),
                                    closeOnClickOutside = T)
    
  })
  
  
  
  #Delete Scenario: Akshay Mahale
  observeEvent(ignoreInit=TRUE,input$confrim_delete,{
    print(input$confrim_delete)
    if(input$confrim_delete == TRUE){
      if(length(input$scenario_selection_rows_selected)>0 & length(input$project_selection_rows_selected)>0){
        ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
        sceneName <- rv_scenarios$scens %>%
          filter(Title == ttl) %>%
          select(Name)%>%
          slice(input$scenario_selection_rows_selected)
        # print(rv_scenarios$scens)
        start_time=Sys.time()
        #myqueryDel<-paste0('DELETE FROM THEATRICAL_LIGHTHOUSE.ODS.APP_SCENARIO WHERE SCEN_NAME LIKE ',"$$",sceneName,"$$",' AND PROJ_TITLE LIKE ',"$$",ttl,"$$")   #T20 Soumen 23.07.24
        myqueryDel<-paste0('DELETE FROM ',database,'.ODS.APP_SCENARIO WHERE SCEN_NAME LIKE ',"$$",sceneName,"$$",' AND PROJ_TITLE LIKE ',"$$",ttl,"$$")
        print(paste0("Delete query",myqueryDel))
        dbGetQuery(my_db,myqueryDel)
        end_time=Sys.time()
        print("Done Deleting Scenario")
        print(end_time-start_time)
        rv_scenarios$scens<-rv_scenarios$scens%>%filter(!Name %in% c(sceneName))
        updateProjectScenarios()
        
        
      }
    }
    else
    {
      shinyWidgets::closeSweetAlert(session)
    }
    
    
  })
  
  observeEvent(ignoreInit=TRUE,input$project_selection_rows_selected,{
    if(!is.null(input$project_selection_rows_selected)){
      print("title check 1")
      # print(rv_projectList$projs$Title)
      # print(input$project_selection_rows_selected)
      rv_newProj$title <- rv_projectList$projs$Title[input$project_selection_rows_selected]
      # print(rv_newProj$title)
    }
  })
  
  
  # Modal for EDITING a project-----
  observe({
    shinyjs::toggleState("btn_editProject",condition = sel1())
  })
  
  ###Delete Project
  observe({
    shinyjs::toggleState("btn_deleteProject",condition = sel1())
  })
  
  observe({
    shinyjs::toggleState("btn_filterProjects",condition = !sel1())
  })
  
  # For AI console 
  #Soumen 05.09.24  T23
  observe({   
    shinyjs::toggleState("btn_train_supervised_model",condition = !sel1())
  })
  observeEvent(ignoreInit=TRUE,input$btn_train_supervised_model,{
   shinyjs::disable("btn_train_supervised_model")
   shinyjs::hide("training_status")
   shinyjs::show("loading_indicator")
   print("Training Started")
   
   supervised_model_train()   #call the training module from supervised_model_script.R
   
   print("Training Completed") 
   shinyjs::enable("btn_train_supervised_model")
   shinyjs::hide("loading_indicator")
   shinyjs::show("training_status")
   output$last_training_date <- renderText({paste("Last trained on: ", format(training_timestamp(), "%Y-%m-%d %H:%M:%S %Z"))})
  })

  observeEvent(ignoreInit=TRUE,input$btn_editProject,{
    showModal(modal_editProject)
    updateTextInput(session,"edit_title",value = rv_projectscenarios$project_title)
    updateSelectizeInput(session,"edit_mpaa",selected = rv_projectList$projs$Rating[input$project_selection_rows_selected])
    updateSelectizeInput(session,"edit_genre",selected =   unlist(strsplit(rv_projectList$projs$Genre[input$project_selection_rows_selected],", ")))
    updateSelectizeInput(session,"edit_keyword",selected = unlist(strsplit(rv_projectList$projs$Keyword[input$project_selection_rows_selected],", ")))
    updateSelectizeInput(session,"edit_ataglance",selected = rv_projectList$projs$`At a Glance`[input$project_selection_rows_selected])
    updateSelectizeInput(session,"edit_activeFlg",selected = rv_projectList$projs$Status[input$project_selection_rows_selected])
    updateTextAreaInput(session,"edit_synopsis",value = rv_projectList$projs$Synopsis[input$project_selection_rows_selected])
    
    output$edit_project_txt <- renderText(paste0("Editing ", rv_projectscenarios$project_title))
    
  })
  modal_editProject <- modalDialog(title = textOutput("edit_project_txt"), size = "l",easyClose=T, footer = NULL,
                                   #actionButton(inputId = "btn_new_reset", label = "Reset Inputs", icon = icon("refresh")),
                                   #insert_breaks(2),
                                   fluidRow(
                                     column(width = 6,
                                            # [AK - DISABLE THIS ORIGINALLY TO VERIFY TITLE IS NOT BLANK AND DOESN'T ALREADY EXIST]
                                            textInput(inputId = "edit_title", label = labelMandatory("Title"))
                                     )
                                   ),
                                   br(),
                                   #the project name they have typed already exists: Akshay
                                   fluidRow(
                                     column(width = 6,
                                            shinyjs::hidden(p(id="error_title_edit","The project title already exists.",style="color: red;"))
                                     )  
                                   ),
                                   fluidRow(
                                     column(width = 6,
                                            #shinyjs::hidden(p(id="error_titleChr","The project title should not end with $ or '.",style="color: red;"))
                                            shinyjs::hidden(p(id="error_titleChr_edit","The project title should not end with $",style="color: red;"))
                                            
                                     )  
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            selectizeInput(inputId = "edit_mpaa",
                                                           label = "Anticipated Rating",
                                                           choices = c("G","PG","PG13","NC17","R"),
                                                           options = list(
                                                             placeholder = 'Select Rating     ',
                                                             onInitialize = I('function() { this.setValue(""); }'),
                                                             create = FALSE)
                                            )
                                     ),
                                     column(width = 6,
                                            selectizeInput(inputId = "edit_genre",
                                                           label = "Genre(s) (Max. 3)",
                                                           choices = mms_genres,
                                                           options = list(
                                                             maxItems = 3,
                                                             placeholder = 'Select Genre     ',
                                                             onInitialize = I('function() { this.setValue(""); }'),
                                                             create = FALSE)
                                            )
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            selectizeInput(inputId = "edit_keyword",
                                                           label = "Keyword(s) (Max. 3)",
                                                           choices = mms_keywords,
                                                           options = list(
                                                             maxItems = 3,
                                                             placeholder = 'Select Keyword(s)     ',
                                                             onInitialize = I('function() { this.setValue(""); }'),
                                                             create = FALSE)
                                            )
                                     ),
                                     column(width = 6,
                                            selectizeInput(inputId = "edit_activeFlg",
                                                           label="Active/Inactive",
                                                           choices=c("Active","Inactive"),
                                                           width =NULL)
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            selectizeInput(inputId = "edit_ataglance",
                                                           label = "Include in At-a-Glance Report?",
                                                           choices=c("Include","Do Not Include")))
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 12,
                                            textAreaInput("edit_synopsis",label = "Synopsis",height = NULL,width = "250%", resize = "both")
                                     )
                                   ),
                                   insert_breaks(2),
                                   # [AK - MAKE SURE WHEN A FILM SHARES THE SAME NAME THAT A WARNING/CONFIRM BOX POPS UP TO EITHER CANCEL OR OVERWRITE EXISTING NAME
                                   shinyjs::disabled(actionButton("btn_saveEditedProject", label = HTML("Save Project"), icon = icon("save")))
                                   
  )
  
  #Only allow user to save project when a VALID (new) Title is entered
  observe({
    #print("check1")
    shinyjs::toggle("error_titleChr_edit",condition = checkSpChar(input$edit_title)==TRUE)
    #shinyjs::toggle("error_title_edit",condition = (input$edit_title %in% rv_projectList$projs$Title ==TRUE))
    #shinyjs::toggle("error_title_edit",condition = input$edit_title != rv_projectscenarios$project_title & str_trim(tolower(input$edit_title))%in%unlist(lapply(lapply(rv_projectList$projs$Title,tolower),str_trim))==TRUE)    #Commented Soumen 18.08.23 Ticket:T14
    shinyjs::toggle("error_title_edit",condition = input$edit_title != rv_projectscenarios$project_title & str_trim(tolower(input$edit_title))%in%unlist(lapply(lapply(all_projects$projs$Title,tolower),str_trim))==TRUE)   #Added Soumen 18.08.23 Ticket:T14
    
    # shinyjs::toggleState("btn_saveEditedProject",
    #                      condition = !is.null(input$edit_title) && !is.na(input$edit_title) &
    #                        input$edit_title!=""&
    #                        (input$edit_title == rv_projectscenarios$project_title | input$edit_title %in% rv_projectList$projs$Title ==FALSE & checkSpChar(input$edit_title)==FALSE))
    #                     
    # shinyjs::toggleState("btn_saveEditedProject",condition = !is.null(input$edit_title) && !is.na(input$edit_title) &  #Commented Soumen 18.08.23 Ticket:T14 
    #                                             input$edit_title!=""&
    #                                           (input$edit_title == rv_projectscenarios$project_title | str_trim(tolower(input$edit_title))%in%unlist(lapply(lapply(rv_projectList$projs$Title,tolower),str_trim)) ==FALSE & checkSpChar(input$edit_title)==FALSE))                 
    shinyjs::toggleState("btn_saveEditedProject",condition = !is.null(input$edit_title) && !is.na(input$edit_title) &
                           input$edit_title!=""&
                           (input$edit_title == rv_projectscenarios$project_title | str_trim(tolower(input$edit_title))%in%unlist(lapply(lapply(all_projects$projs$Title,tolower),str_trim)) ==FALSE & checkSpChar(input$edit_title)==FALSE))      #Added Soumen 18.08.23 Ticket:T14
    
    #print("check2")
  })
  #Save new project
  observeEvent(ignoreInit=TRUE,input$btn_saveEditedProject,{
    shinyjs::disable("btn_saveEditedProject")
    old_title <- rv_projectscenarios$project_title
    print(old_title)
    #QUERY - COMPLETE - UPDATE EXISTING PROJECT
    #all_projects$projs <- readRDS("data/000_projects.RDS")
    print("edit proj 1")
    all_projects$projs <- all_projects$projs %>% mutate(Rating = ifelse(Title==old_title,input$edit_mpaa, Rating),
                                                        Genre = ifelse(Title==old_title,paste(input$edit_genre,collapse = ", "), Genre),
                                                        Keyword = ifelse(Title==old_title,paste(input$edit_keyword,collapse = ", "), Keyword),
                                                        Synopsis = ifelse(Title==old_title,input$edit_synopsis, Synopsis),
                                                        Status = ifelse(Title==old_title,input$edit_activeFlg, Status),
                                                        `At a Glance` = ifelse(Title==old_title,input$edit_ataglance, `At a Glance`),
                                                        Title = ifelse(Title==old_title,input$edit_title, Title)
    )
    
    #Delete project:Akshay
    all_projects$projs <- rv_projectList$projs %>% mutate(Rating = ifelse(Title==old_title,input$edit_mpaa, Rating),
                                                          Genre = ifelse(Title==old_title,paste(input$edit_genre,collapse = ", "), Genre),
                                                          Keyword = ifelse(Title==old_title,paste(input$edit_keyword,collapse = ", "), Keyword),
                                                          Synopsis = ifelse(Title==old_title,input$edit_synopsis, Synopsis),
                                                          Status = ifelse(Title==old_title,input$edit_activeFlg, Status),
                                                          `At a Glance` = ifelse(Title==old_title,input$edit_ataglance, `At a Glance`),
                                                          Title = ifelse(Title==old_title,input$edit_title, Title)
    )
    dbGetQuery(my_db, 
               #paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_PROJECT_LIST" ',"SET PROJ_TITLE = $$",input$edit_title,   #T20 Soumen 23.07.24
               paste0('UPDATE ',database,'."ODS"."APP_PROJECT_LIST" ',"SET PROJ_TITLE = $$",input$edit_title,
                      "$$,PROJ_MPAA_RATING = '",input$edit_mpaa,
                      "',PROJ_GENRE = $$",paste(input$edit_genre,collapse = ", "),
                      "$$,PROJ_KEYWORD = $$",paste(input$edit_keyword,collapse = ", "),
                      "$$,PROJECT_ACTIVE_STATUS = '",input$edit_activeFlg,
                      "',AT_A_GLANCE_FLAG = '",input$edit_ataglance,
                      "',SYNOPSIS = $$",input$edit_synopsis,
                      "$$,APP_PROJ_LAST_MODIFIED ='",Sys.time(),
                      "', APP_PROJ_MODIFIED_BY ='",session$user,
                      "' WHERE PROJ_TITLE = $$",rv_projectscenarios$project_title,"$$;")
    )
    print("edit proj 2")
    #saveRDS(all_projects$projs,"data/000_projects.RDS")
    #print("q2chk")
    #observeEvent(ignoreInit=TRUE,input$project_selection_rows_selected,{
    #  print(rv_projectList$projs[input$project_selection_rows_selected])
    #})
    #print("?")
    
    
    # ALSO CHANGE THE TITLE IN THE CORRESPONDING SCENARIOS:
    # QUERY - COMPLETE - UPDATE PROJECT TITLE IN SCENARIO TABLE WHEN EDITED
    #rv_scenarios$scens <- readRDS("data/000_scenarios.RDS")
    rv_scenarios$scens <- rv_scenarios$scens %>% mutate(Title = ifelse(Title == old_title,input$edit_title, Title))
    #print(paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO" ',"SET PROJ_TITLE = $$",input$edit_title,   # T20 Soumen 23.07.24
    print(paste0('UPDATE ',database,'."ODS"."APP_SCENARIO" ',"SET PROJ_TITLE = $$",input$edit_title,   
                 #"$$",' WHERE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO".PROJ_TITLE = ',"$$",rv_projectscenarios$project_title,"$$;"))    # T20 Soumen 23.07.24
                 "$$",' WHERE ',database,'."ODS"."APP_SCENARIO".PROJ_TITLE = ',"$$",rv_projectscenarios$project_title,"$$;"))
    #Akshay Mahale Update Bad Comps table 
    # projectDetailsFunc(input$edit_title)
    print(paste0("PROJ ID INSERT: ",rv_SelectedDetails$Proj$APP_PROJ_KEY))
    projId<-rv_SelectedDetails$Proj$APP_PROJ_KEY
    #print(paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS" '," SET PROJ_TITLE = $$",input$edit_title,"$$",     # T20 Soumen 23.07.24
    print(paste0('UPDATE ',database,'."ODS"."APP_BAD_COMPS" '," SET PROJ_TITLE = $$",input$edit_title,"$$",    
                 " WHERE PROJ_ID=",projId,";"))
    
    
    dbGetQuery(my_db, 
               #paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS" '," SET PROJ_TITLE = $$",input$edit_title,"$$",   # T20 Soumen 23.07.24
               paste0('UPDATE ',database,'."ODS"."APP_BAD_COMPS" '," SET PROJ_TITLE = $$",input$edit_title,"$$",
                      " WHERE PROJ_ID=",projId,";"))
    # 
    # THOMAS -----
    print("update proj name in scen 1")
    dbGetQuery(my_db, 
               #paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO" ',"SET PROJ_TITLE = $$",input$edit_title,    # T20 Soumen 23.07.24
               paste0('UPDATE ',database,'."ODS"."APP_SCENARIO" ',"SET PROJ_TITLE = $$",input$edit_title,
                      "$$",' WHERE PROJ_TITLE = ',"$$",rv_projectscenarios$project_title,"$$;"))
    print("update proj name in scen 2")
    #saveRDS(rv_scenarios$scens,"data/000_scenarios.RDS")
    
    
    #print("here??")
    updateSelectizeInput(session,"projfilter_title",choices = sort((rv_projectList$projs$Title)))
    #print("or here?")
    shinyjs::enable("btn_saveEditedProject")
    removeModal()
    shinyWidgets::confirmSweetAlert(session,inputId = "edit_success",title = "Project Successfully Edited",type = "success",closeOnClickOutside = T)
  })
  
  
  #Update project-selection filters when a new/edited project is saved
  projflt_film <- reactive({
    if(is.null(input$projfilter_title)||input$projfilter_title==""){
      all_projects$projs$Title
    }else{
      all_projects$projs %>% 
        filter(Title %in% input$projfilter_title) %>% 
        select(Title) %>% 
        pull()
      # mms %>% 
      #   filter(Title %in% input$filter_film) %>% 
      #   select(Mcode) %>% 
      #   pull()
    }
  })
  projflt_created <- reactive({
    all_projects$projs %>% 
      filter(input$projfilter_created[1] <= `Date Created` & `Date Created` <= input$projfilter_created[2]) %>% 
      select(Title) %>% 
      pull()
  })
  projflt_lastscen <- reactive({
    all_projects$projs %>% 
      filter((input$projfilter_lastscen[1] <= date(`Last Scenario`) & date(`Last Scenario`) <= input$projfilter_lastscen[2])|is.na(`Last Scenario`)) %>% 
      select(Title) %>% 
      pull()
  })
  projflt_mpaa <- reactive({
    if(is.null(input$projfilter_mpaa)||input$projfilter_mpaa==""){
      all_projects$projs$Title
    }else{
      all_projects$projs %>% 
        filter(Rating %in% input$projfilter_mpaa) %>% 
        select(Title) %>% 
        pull()
    }
  })
  projflt_genre <- reactive({
    if(is.null(input$projfilter_genre)||input$projfilter_genre==""){
      all_projects$projs$Title
    }else{
      all_projects$projs %>%
        filter(str_detect(Genre, paste(input$projfilter_genre,collapse = "|"))) %>%
        select(Title) %>%
        pull()
    }
  })
  projflt_keyword <- reactive({
    if(is.null(input$projfilter_keyword)||input$projfilter_keyword==""){
      all_projects$projs$Title
    }else{
      all_projects$projs %>% 
        filter(str_detect(Keyword, paste(input$projfilter_keyword,collapse = "|"))) %>% 
        select(Title) %>% 
        pull()
    }
  })
  projflt_status <- reactive({
    if(is.null(input$projfilter_status)||input$projfilter_status==""){
      all_projects$projs$Title
    }else{
      all_projects$projs %>% 
        filter(Status %in% input$projfilter_status) %>% 
        select(Title) %>% 
        pull()
    }
  })
  projflt_ataglance <- reactive({
    if(is.null(input$projfilter_ataglance)||input$projfilter_ataglance==""){
      all_projects$projs$Title
    }else{
      all_projects$projs %>% 
        filter(`At a Glance` %in% input$projfilter_ataglance) %>% 
        select(Title) %>% 
        pull()
    }
  })
  
  
  observe({
    # print(input$scenarioOption)
    # print(input$project_selection_rows_selected)
    toggleState("btn_pg_enterNewScenario",
                condition = sel1()
    )
    
    toggleState("btn_pg_enterExistingScenario",
                condition=sel2()
    )
    
    #Delete Scenario: Akshay Mahale
    toggleState("btn_deleteScenario",
                condition=sel2()
    )
    
    toggleState("btn_copyScenario",
                condition=sel2()
    )
    toggleState("report_comps1",
                condition = sel2())
    
    #FIA report Start: Akshay Mahale
    toggleState("reports_fia",
                condition = sel2())
  })
  
  
  observeEvent(ignoreInit=TRUE,input$btn_copyScenario,{
    
    #new updated way to handle issue of save scenario copy:Akshay
    updateProjectScenarios()
    
    ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
    
    modal_copyScenario <- modalDialog(title = "Name Copy of Scenario",size = "s",easyClose = T,
                                      textInput("copyScen_name", 
                                                label = "What is the name for this scenario?",
                                                value = rv_scenarios$scens %>%
                                                  filter(Title == ttl) %>% 
                                                  slice(input$scenario_selection_rows_selected) %>% 
                                                  select(Name) %>% 
                                                  pull() %>% paste0(.," - Copy")
                                      ),
                                      actionButton("btn_saveScenCopy", label = "Save Copy", icon = icon("save"))
    )
    
    
    showModal(modal_copyScenario)
    #shinyjs::enable("btn_saveScenCopy")
    # print("Project scens")
    # print(rv_projectscenarios$scenario_names)
    # print(input$copyScen_name)
    # print("bool result")
    # print(!is.null(input$copyScen_name))
    
  })
  # observe({
  #   toggleState("btn_saveScenCopy", condition = !is.null(input$copyScen_name) & 
  #                 input$copyScen_name %in% c("","<NO SCENARIO SELECTED>", rv_projectscenarios$scenario_names)==FALSE)
  # })
  
  #new updated way to handle issue of save scenario copy:Akshay
  # observeEvent(ignoreInit = TRUE, input$btn_saveScenCopy, {
  #   #shinyjs::disable("btn_saveScenCopy")
  #   #To solve invoke toggleState of save copy issue
  #   
  #   if (!is.null(input$copyScen_name) & input$copyScen_name!="")
  #   {
  #     if ((input$copyScen_name %in% rv_projectscenarios$scenario_names) == FALSE)
  #     {
  #       shinyjs::disable("btn_saveScenCopy")
  #       #rv_bdCmp$bad<-NULL
  #       ttl <-
  #         rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
  #       tmp <- rv_scenarios$scens %>%
  #         filter(Title == ttl) %>%
  #         slice(input$scenario_selection_rows_selected) %>%
  #         mutate(
  #           Name = input$copyScen_name,
  #           `Created By` = session$user,
  #           `Last Modified` = Sys.time()
  #         )
  #       
  #       #QUERY - COMPLETE - SAVE COPY OF SCENARIO
  #       #rv_scenarios$scens <- readRDS("data/000_scenarios.RDS")
  #       rv_scenarios$scens <- bind_rows(tmp, rv_scenarios$scens)
  #       #saveRDS(rv_scenarios$scens,"data/000_scenarios.RDS")
  #       start_time <- Sys.time()
  #       
  #       copy_query <-
  #         paste0(
  #           'INSERT INTO "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO"',
  #           "(PROJ_TITLE,SCEN_NAME,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,FCST_LOW,FCST_BASE,FCST_HIGH,FCST_LOW_INTL,FCST_BASE_INTL,FCST_HIGH_INTL,FR_LOW,FR_BASE,FR_HIGH,AD_PUB_LOW,AD_PUB_BASE,AD_PUB_HIGH,PRINTS_LOW,PRINTS_BASE,PRINTS_HIGH,IMAX_LOW,IMAX_BASE,IMAX_HIGH,THREED_LOW,THREED_BASE,THREED_HIGH,NOTES,SUBMITTED_TO_FIA,SCEN_DATE_SUBMITTED,SELECTED_COMPS,SELECTED_COMPS_MCODES,FINAL_COMPS_MCODES,SCEN_CREATED_DATE,SCEN_LAST_MODIFIED,SCEN_CREATED_BY,APP_PROJ_KEY,TITLE_DIM_KEY,INT_FR_LOW,INT_FR_BASE,INT_FR_HIGH,INT_PRINTS_LOW,INT_PRINTS_BASE,INT_PRINTS_HIGH,INT_AD_PUB_LOW,INT_AD_PUB_BASE,INT_AD_PUB_HIGH) SELECT PROJ_TITLE,$$",
  #           input$copyScen_name,
  #           "$$,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,FCST_LOW,FCST_BASE,FCST_HIGH,FCST_LOW_INTL,FCST_BASE_INTL,FCST_HIGH_INTL,FR_LOW,FR_BASE,FR_HIGH,AD_PUB_LOW,AD_PUB_BASE,AD_PUB_HIGH,PRINTS_LOW,PRINTS_BASE,PRINTS_HIGH,IMAX_LOW,IMAX_BASE,IMAX_HIGH,THREED_LOW,THREED_BASE,THREED_HIGH,NOTES,SUBMITTED_TO_FIA,SCEN_DATE_SUBMITTED,SELECTED_COMPS,SELECTED_COMPS_MCODES,FINAL_COMPS_MCODES,'",
  #           Sys.time(),
  #           "','",
  #           Sys.time(),
  #           "','",
  #           session$user,
  #           "',APP_PROJ_KEY,TITLE_DIM_KEY,INT_FR_LOW,INT_FR_BASE,INT_FR_HIGH,INT_PRINTS_LOW,INT_PRINTS_BASE,INT_PRINTS_HIGH,INT_AD_PUB_LOW,INT_AD_PUB_BASE,INT_AD_PUB_HIGH FROM ",
  #           '"THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO"',
  #           " WHERE SCEN_NAME = $$",
  #           rv_projectscenarios$scenario_names[input$scenario_selection_rows_selected],
  #           "$$ AND PROJ_TITLE = $$",
  #           rv_projectscenarios$project_title,
  #           "$$;"
  #         )
  #       dbGetQuery(my_db,
  #                  copy_query)
  #       end_time <- Sys.time()
  #       print("Done insert scenario")
  #       print(end_time - start_time)
  #       print(copy_query)
  #       print(rv_scenarios$scens$Name[input$scenario_selection_rows_selected])
  #       print(rv_scenarios$scens$Name)
  #       print(input$scenario_selection_rows_selected)
  #       #print(rv_projectscenarios)
  #       #shinyjs::enable("btn_saveScenCopy")
  #       removeModal()
  #       shinyWidgets::confirmSweetAlert(
  #         session,
  #         inputId = "copy_success",
  #         title = "Scenario Successfully Copied",
  #         type = "success",
  #         closeOnClickOutside = T
  #       )
  #     }
  #     else
  #     {
  #       shinyWidgets::sendSweetAlert(session, title = "Scenario name already exists within the selected project", type = "error")
  #       
  #     }
  #   }
  #   else
  #   {
  #     shinyWidgets::sendSweetAlert(session, title = "Scenario name cannot be blank", type = "error")
  #   }
  # })
  
  
  observeEvent(ignoreInit=TRUE,input$btn_saveScenCopy,{
    if(!is.null(input$copyScen_name) & input$copyScen_name %in% c("","<NO SCENARIO SELECTED>", rv_projectscenarios$scenario_names)==FALSE){
      shinyjs::disable("btn_saveScenCopy")
      #rv_bdCmp$bad<-NULL
      ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
      tmp <- rv_scenarios$scens %>%
        filter(Title == ttl) %>% 
        slice(input$scenario_selection_rows_selected) %>% 
        mutate(Name = input$copyScen_name,
               `Created By` = session$user,
               `Last Modified` = Sys.time())
      
      #QUERY - COMPLETE - SAVE COPY OF SCENARIO
      #rv_scenarios$scens <- readRDS("data/000_scenarios.RDS")
      rv_scenarios$scens <- bind_rows(tmp,rv_scenarios$scens)
      #saveRDS(rv_scenarios$scens,"data/000_scenarios.RDS")
      start_time <- Sys.time()
      
      #copy_query <- paste0('INSERT INTO "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO"',"(PROJ_TITLE,SCEN_NAME,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,FCST_LOW,FCST_BASE,FCST_HIGH,FCST_LOW_INTL,FCST_BASE_INTL,FCST_HIGH_INTL,FR_LOW,FR_BASE,FR_HIGH,AD_PUB_LOW,AD_PUB_BASE,AD_PUB_HIGH,PRINTS_LOW,PRINTS_BASE,PRINTS_HIGH,IMAX_LOW,IMAX_BASE,IMAX_HIGH,THREED_LOW,THREED_BASE,THREED_HIGH,NOTES,SUBMITTED_TO_FIA,SCEN_DATE_SUBMITTED,SELECTED_COMPS,SELECTED_COMPS_MCODES,FINAL_COMPS_MCODES,SCEN_CREATED_DATE,SCEN_LAST_MODIFIED,SCEN_CREATED_BY,APP_PROJ_KEY,TITLE_DIM_KEY,INT_FR_LOW,INT_FR_BASE,INT_FR_HIGH,INT_PRINTS_LOW,INT_PRINTS_BASE,INT_PRINTS_HIGH,INT_AD_PUB_LOW,INT_AD_PUB_BASE,INT_AD_PUB_HIGH,FCST_LOW_CHINA,FCST_BASE_CHINA,FCST_HIGH_CHINA,CHINA_FR_LOW,CHINA_FR_BASE,CHINA_FR_HIGH,CHINA_PRINTS_LOW,CHINA_PRINTS_BASE,CHINA_PRINTS_HIGH,CHINA_AD_PUB_LOW,CHINA_AD_PUB_BASE,CHINA_AD_PUB_HIGH) SELECT PROJ_TITLE,$$",   # T20 Soumen 23.07.24
      copy_query <- paste0('INSERT INTO ',database,'."ODS"."APP_SCENARIO"',"(PROJ_TITLE,SCEN_NAME,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,FCST_LOW,FCST_BASE,FCST_HIGH,FCST_LOW_INTL,FCST_BASE_INTL,FCST_HIGH_INTL,FR_LOW,FR_BASE,FR_HIGH,AD_PUB_LOW,AD_PUB_BASE,AD_PUB_HIGH,PRINTS_LOW,PRINTS_BASE,PRINTS_HIGH,IMAX_LOW,IMAX_BASE,IMAX_HIGH,THREED_LOW,THREED_BASE,THREED_HIGH,NOTES,SUBMITTED_TO_FIA,SCEN_DATE_SUBMITTED,SELECTED_COMPS,SELECTED_COMPS_MCODES,FINAL_COMPS_MCODES,SCEN_CREATED_DATE,SCEN_LAST_MODIFIED,SCEN_CREATED_BY,APP_PROJ_KEY,TITLE_DIM_KEY,INT_FR_LOW,INT_FR_BASE,INT_FR_HIGH,INT_PRINTS_LOW,INT_PRINTS_BASE,INT_PRINTS_HIGH,INT_AD_PUB_LOW,INT_AD_PUB_BASE,INT_AD_PUB_HIGH,FCST_LOW_CHINA,FCST_BASE_CHINA,FCST_HIGH_CHINA,CHINA_FR_LOW,CHINA_FR_BASE,CHINA_FR_HIGH,CHINA_PRINTS_LOW,CHINA_PRINTS_BASE,CHINA_PRINTS_HIGH,CHINA_AD_PUB_LOW,CHINA_AD_PUB_BASE,CHINA_AD_PUB_HIGH) SELECT PROJ_TITLE,$$",
                           input$copyScen_name,
                           #"$$,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,FCST_LOW,FCST_BASE,FCST_HIGH,FCST_LOW_INTL,FCST_BASE_INTL,FCST_HIGH_INTL,FR_LOW,FR_BASE,FR_HIGH,AD_PUB_LOW,AD_PUB_BASE,AD_PUB_HIGH,PRINTS_LOW,PRINTS_BASE,PRINTS_HIGH,IMAX_LOW,IMAX_BASE,IMAX_HIGH,THREED_LOW,THREED_BASE,THREED_HIGH,NOTES,SUBMITTED_TO_FIA,SCEN_DATE_SUBMITTED,SELECTED_COMPS,SELECTED_COMPS_MCODES,FINAL_COMPS_MCODES,'",Sys.time(),"','",Sys.time(),"','",session$user,"',APP_PROJ_KEY,TITLE_DIM_KEY,INT_FR_LOW,INT_FR_BASE,INT_FR_HIGH,INT_PRINTS_LOW,INT_PRINTS_BASE,INT_PRINTS_HIGH,INT_AD_PUB_LOW,INT_AD_PUB_BASE,INT_AD_PUB_HIGH,FCST_LOW_CHINA,FCST_BASE_CHINA,FCST_HIGH_CHINA,CHINA_FR_LOW,CHINA_FR_BASE,CHINA_FR_HIGH,CHINA_PRINTS_LOW,CHINA_PRINTS_BASE,CHINA_PRINTS_HIGH,CHINA_AD_PUB_LOW,CHINA_AD_PUB_BASE,CHINA_AD_PUB_HIGH FROM ",'"THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO"'," WHERE SCEN_NAME = $$", rv_projectscenarios$scenario_names[input$scenario_selection_rows_selected],  #T20 Soumen 23.07.24  
                           "$$,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,FCST_LOW,FCST_BASE,FCST_HIGH,FCST_LOW_INTL,FCST_BASE_INTL,FCST_HIGH_INTL,FR_LOW,FR_BASE,FR_HIGH,AD_PUB_LOW,AD_PUB_BASE,AD_PUB_HIGH,PRINTS_LOW,PRINTS_BASE,PRINTS_HIGH,IMAX_LOW,IMAX_BASE,IMAX_HIGH,THREED_LOW,THREED_BASE,THREED_HIGH,NOTES,SUBMITTED_TO_FIA,SCEN_DATE_SUBMITTED,SELECTED_COMPS,SELECTED_COMPS_MCODES,FINAL_COMPS_MCODES,'",Sys.time(),"','",Sys.time(),"','",session$user,"',APP_PROJ_KEY,TITLE_DIM_KEY,INT_FR_LOW,INT_FR_BASE,INT_FR_HIGH,INT_PRINTS_LOW,INT_PRINTS_BASE,INT_PRINTS_HIGH,INT_AD_PUB_LOW,INT_AD_PUB_BASE,INT_AD_PUB_HIGH,FCST_LOW_CHINA,FCST_BASE_CHINA,FCST_HIGH_CHINA,CHINA_FR_LOW,CHINA_FR_BASE,CHINA_FR_HIGH,CHINA_PRINTS_LOW,CHINA_PRINTS_BASE,CHINA_PRINTS_HIGH,CHINA_AD_PUB_LOW,CHINA_AD_PUB_BASE,CHINA_AD_PUB_HIGH FROM ",database,'."ODS"."APP_SCENARIO"'," WHERE SCEN_NAME = $$", rv_projectscenarios$scenario_names[input$scenario_selection_rows_selected],
                           "$$ AND PROJ_TITLE = $$",rv_projectscenarios$project_title,"$$;")
      dbGetQuery(my_db, 
                 copy_query)
      end_time<-Sys.time()
      print("Done insert scenario")
      print(end_time-start_time)
      print(copy_query)
      print(rv_scenarios$scens$Name[input$scenario_selection_rows_selected])
      print(rv_scenarios$scens$Name)
      print(input$scenario_selection_rows_selected)
      #print(rv_projectscenarios)
      #shinyjs::enable("btn_saveScenCopy")
      removeModal()
      shinyWidgets::confirmSweetAlert(session,inputId = "copy_success",title = "Scenario Successfully Copied",type = "success",closeOnClickOutside = T)
      
    }
    else{
      sendSweetAlert(session, title = "Scenario Name is blank or it already exists",type = "warning")
      #shinyWidgets::confirmSweetAlert(session,inputId = "error_msg_save",title = "Scenario Name is blank or it already exists",type = "success",closeOnClickOutside = T)
    }
  })
  
  
  
  
  #Update table of historical scenarios and project visuals when a project is selected----
  
  rv_projectscenarios <- reactiveValues(project_title = NULL, scenario_names = NULL)
  
  observe({
    print("CHECK HERE")
    print(input$project_selection_rows_selected)
    if(!is.null(input$project_selection_rows_selected)){
      rv_projectscenarios$project_title <- rv_projectList$projs$Title[input$project_selection_rows_selected]
      rv_projectscenarios$proj_id<-rv_projectList$projs$proj_id[input$project_selection_rows_selected]
      print(rv_projectscenarios$project_title)
      # USE the following to auto-populate scenario details when creating a new project
      rv_projectscenarios$project_genre <- unlist(strsplit(rv_projectList$projs$Genre[input$project_selection_rows_selected],", "))
      rv_projectscenarios$project_rating <- rv_projectList$projs$Rating[input$project_selection_rows_selected]
      print("genre")
      print(rv_projectscenarios$project_genre)
      rv_projectscenarios$project_keywords<-unlist(strsplit(rv_projectList$projs$Keyword[input$project_selection_rows_selected],", "))
      print("kwd")
      print(rv_projectscenarios$project_keywords)
      print("new here?")
      updateProjectScenarios()
      # rv_projectscenarios$scenario_names <- rv_scenarios$scens %>%
      #   filter(Title == rv_projectscenarios$project_title) %>%
      #   select(Name) %>%
      #   pull()
    }
  })
  
  observe({
    if(rv_flgValidProject$flg == FALSE){
      #js$collapse("box_scenarios") 
    }else{
      #js$collapse("box_projects")
      #js$collapse("box_scenarios")
      #ZM - UNCOMMENT BELOW WHEN VIZ IN PLACE
      #js$collapse("box_vis_scencomps")
      #js$collapse("box_vis_history")
    }
    
  })
  
  
  observe({
    if(rv_currentCompsLength$t_1 > 0 & rv_currentCompsLength$t_0==0){
      #js$collapse("box_final_comps")
      #js$collapse("box_category_cts")
      #js$collapse("box_numeric_analysis")
    }else if(rv_currentCompsLength$t_1 == 0 & rv_currentCompsLength$t_0 > 0){
      #js$collapse("box_final_comps")
      #js$collapse("box_category_cts")
      #js$collapse("box_numeric_analysis")
    }
  })
  
  observe({
    #observeEvent(ignoreInit=TRUE,c(sel1(),input$btn_saveScenCopy),{
    print("new here2")
    if(is.null(input$project_selection_rows_selected)){
      ttl <- ""
      rv_flgValidProject$flg <- FALSE
      out <- rv_scenarios$scens %>% slice(0) %>% select(Name, `Created By`, `Last Modified`,#`Final Comps`,
                                                        Notes, `Production Budget ($M)`,Low:Marketing,`Cast (Major)`, Submitted)
    }else{
      print("new here 3")
      ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
      rv_flgValidProject$flg <- TRUE
      out <- rv_scenarios$scens %>%
        filter(Title == ttl) %>%
        #Int Fcst: Akshay Mahale
        mutate(`Global Low`=ifelse(is.na(Low),0,Low)+ifelse(is.na(`Int Low`),0,`Int Low`),
               `Global Base`=ifelse(is.na(Base),0,Base)+ifelse(is.na(`Int Base`),0,`Int Base`),
               `Global High`=ifelse(is.na(High),0,High)+ifelse(is.na(`Int High`),0,`Int High`))%>%
        select(Name, `Created By`, `Last Modified`,`Selected Comps`,#`Final Comps`,
               Notes, `Production Budget ($M)`,Low:`Int High`,`Global Low`,`Global Base`,`Global High`,`Film Rental`,Marketing,`Cast (Major)`,Submitted)
      
    }
    #print(input$project_selection_rows_selected)
    #print(ttl)
    
    #Update Project Name:
    output$project_name <- renderText({
      if(!is.null(input$project_selection_rows_selected)){
        rv_projectscenarios$project_title
      }else{
        ""
        #"<SELECT PROJECT FROM TABLE ABOVE>"
      }
      
    })
    print("new here 4")
    print(str(out))
    print(out)
    # restricted_scenarios <- reactive({
    #   rv_scenarios$scens %>%
    #     filter(Title == ttl) %>%
    #     select(Name, `Created By`, `Last Modified`,`Selected Comps`,`Final Comps`,Notes,Low:Actor,Submitted)
    # })
    
    
    #Update Scenario Table:
    tryCatch(
      output$scenario_selection <- renderDataTable({
        targs_large <- match(c("Selected Comps","Cast (Major)","Synopsis","Cast (Full)"),colnames(out)) - 1
        targs_mid <- match(c("Title","Director","Executive Producer","MPAA Rating Desc","Producer","Writer","Editor","BO Unscheduled Release Description","Pattern Description Comments"),colnames(out)) - 1
        targs_small <- match(c("DBO ($M)","IBO ($M)","WBO ($M)","WWBO ($M)","Release Week Number","Short Pattern Description","Season CF Code","Title Type NK","Release Year Check","Rel CF Dt"),colnames(out))-1
        targs_ranks <- match(c("Smark Rank", "Supervised Rank","Demo Rank"),colnames(out)) - 1
        
        # if(sel1()){
        #   out <- restricted_scenarios()
        # }else{
        #   out <- rv_scenarios$scens %>% slice(0) %>% select(Name, `Created By`, `Last Modified`,`Final Comps`,Notes,Low:Actor)
        # }
        
        #colnames(out) = paste0('<span style="color:white">',colnames(out),'</span>')
        
        DT::datatable(out, colnames = paste0('<span style="color:white">',colnames(out),'</span>'),
                      filter = "none",
                      selection = list(mode="single",target="row"),
                      rownames = FALSE,
                      
                      class = "cell-border stripe",
                      
                      extensions = c('FixedColumns','FixedHeader'),
                      plugins = "ellipsis",
                      escape = FALSE, #this argument allows for COLUMN NAMES to HAVE HTML STYLING
                      
                      options = list(
                        dom="tp",
                        pageLength = input$n_scens,
                        lengthMenu = list(c(5,10,-1),c('5','10','All')),
                        initComplete = JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#000'});",
                          "}"),
                        columnDefs = list(#list(width = '500px', targets = targs_large),
                          list(width = '150px', targets=c(targs_mid, targs_large)),
                          list(width = '80px', targets=targs_small),
                          list(targets = c(targs_mid, targs_large),
                               render = JS("$.fn.dataTable.render.ellipsis( 22, false )")
                          )),
                        language = list(
                          #lengthMenu = '<span style = "color:black">Scenarios per page: </span>_MENU_   ',
                          #search = '<span style = "color:black"></span>', #placeholder below saves space
                          #searchPlaceholder = 'Search',
                          info = '<span style = "color:black">Showing _START_ to _END_ of _TOTAL_ entries</span>',
                          paginate = list(previous = '<span style = "color:black">Previous</span>', `next` = '<span style = "color:black">Next</span>')
                        )
                      )
        )%>% formatDate(.,~`Last Modified`,method = "toLocaleDateString")%>% 
          formatStyle("Submitted",
                      target = "row",
                      backgroundColor = styleEqual("Submitted", "lightgreen"),
                      fontWeight = styleEqual("Submitted","bold")
          )
      }),
      error=function(x){paste("error: scen_table_render",x)}
    )
    
    #Update Visual of Scenarios:
    #pm_forecast_history_plot
    # output$pm_forecast_history_plot <- renderPlotly({
    #   if(sel1() &&
    #      nrow(rv_scenarios$scens %>%
    #           filter(Title==rv_projectscenarios$project_title))>0
    #   ){
    #     # print(nrow(rv_scenarios$scens %>%
    #     #              filter(Title==rv_projectscenarios$project_title)))
    #     dta <- rv_scenarios$scens %>%
    #       filter(Title == ttl) %>%
    #       select(`Last Modified`,Low:High) %>%
    #       gather(key = "Type", value = "Forecast", -`Last Modified`) %>%
    #       mutate(Type = factor(Type,levels = c("High","Base","Low"),ordered = TRUE)) %>%
    #       arrange(Type,`Last Modified`)
    #     
    #     #save(dta, file = "dta.RDA")
    # 
    #     dta %>%
    #       distinct(`Last Modified`,Type, Forecast) %>%
    #       spread(Type, Forecast) %>%
    #       filter_at(vars(High,Base,Low),all_vars(!is.na(.))) %>%
    #       plot_ly(x = ~`Last Modified`, y = ~High, name = 'High',  type = 'scatter', mode = 'lines+markers',
    #               line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'),
    #               marker = list(size = 7,
    #                             color = 'rgba(205, 12, 24, .9)',
    #                             line = list(color = 'rgba(0, 0, 0, .8)',
    #                                         width = 2))) %>%
    #       add_trace(y = ~Base, name = 'Base', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = "solid"),
    #                 marker = list(size = 7,
    #                               color = 'rgba(22, 96, 167, .9)',
    #                               line = list(color = 'rgba(0, 0, 0, .8)',
    #                                           width = 2))) %>%
    #       add_trace(y = ~Low, name = 'Low', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'),
    #                 marker = list(size = 7,
    #                               color = 'rgba(205, 12, 24, .9)',
    #                               line = list(color = 'rgba(0, 0, 0, .8)',
    #                                           width = 2))) %>%
    #       layout(title = "Domestic Box Office Revenue ($M) User Forecasts Over Time",
    #              xaxis = list(title = "Forecast Date"), 
    #              yaxis = list(title = "Forecasted Domestic Box Office Revenue ($M)"))
    #   }
    # 
    # })
    tryCatch(output$pm_forecast_history_plot <- renderPlotly({
      plt <- rv_scenarios$scens %>%
        filter(Title == ttl) %>% 
        filter(Submitted=="Submitted")
      
      if(nrow(plt)>0){
        
        scen_fcsts <- plt %>% select(Name, Low:High)
        scen_fcsts <- gather(scen_fcsts,key = "Val","DBO ($M)",-Name)
        
        comps_bo_list <- data.frame()
        
        for(i in 1:nrow(plt)){
          #fcsts <- aqua %>% select(Low:High) %>% slice(i)
          comps <- unlist(str_split(plt$`Selected Comps Mcodes`[i], pattern = ", "))
          comps <- mms %>% filter(Mcode %in% comps) %>% select(Title,Mcode,`DBO ($M)`) %>% mutate(Name = plt$Name[i])
          comps_bo_list <- bind_rows(comps_bo_list, comps)
        }
        
        
        p <- ggplot(data = scen_fcsts, aes(x=Name, y = `DBO ($M)`,label=Val))
        p <- p + geom_point(shape = 3, size = 3)
        p <- p + geom_line()
        p <- p + geom_point(data = comps_bo_list, aes(x=Name, y = `DBO ($M)`, label=Title), shape = 20, size = 3.5,colour="#3C8DBC80")
        p <- p + theme_bw()
        p <- p + theme(panel.grid.minor = element_blank())
        p <- p + scale_y_continuous(limits = c(min(c(scen_fcsts$`DBO ($M)`,comps_bo_list$`DBO ($M)`),na.rm=T),
                                               max(c(scen_fcsts$`DBO ($M)`,comps_bo_list$`DBO ($M)`),na.rm=T)))
        p <- p + labs(x = "")
        
        p <- p + coord_flip()
        
      }else{
        p <- ggplot()
      }
      
      ggplotly(p)
    }),
    error=function(x){paste("plotly error,",x)}
    )
    
  })
  
  
  # Proceed to scenario inputs when button clicked,
  # **auto-filling details when radio button "Existing" is active, and
  # resetting details when radio option "New" is active
  
  #If they proceed to enter a new scenario,
  # 1) Reset all inputs:
  # 2) If sel2() is TRUE, then import every detail for that scenario EXCEPT THE SCENARIO NAME
  #     and Update the table of comps to reflect the selected scenario! -- they will still need to enter a valid scenario name to save new scenario
  # 3) If sel2() is FALSE, just continue (no else necessary)
  
  
  observeEvent(ignoreInit=TRUE,input$new_success,{
    print("try new success")
    print(input$new_success)
    rv_bdCmp$bad<-NULL
    
    if(input$new_success == TRUE){
      
      #rv_newProj$title <- input$
      # Add project name to Scenario builder: Start Akshay Mahale
      output$proj_name <- renderText(rv_projectscenarios$project_title)
      # Add project name to Scenario builder: End Akshay Mahale
      rv_currentCompsLength$t_1 <- 0
      rv_currentCompsLength$t_0 <- 0
      updateTabItems(session, inputId="sbm", selected = "build")
      rv_scenInputs$inputs <- mms %>% slice(0)
      rv_flgNewOrEdit$flg <- "New"
      rv_currentComps$currentComps <- mms %>% slice(0) %>% select(-`Smart Rank`,-`Supervised Rank`,-`Demo Rank`)
      rv_currentComps$finalComps <- c()
      rv_scenInputs$inputs <- mms %>% slice(0) %>% select(-`Smart Rank`,-`Supervised Rank`,-`Demo Rank`)
      
      rv_currentScenario$name <- "<NO SCENARIO SELECTED>"
      shinyjs::reset("scenario_name")
      #shinyjs::reset("scenario_date")
      
      updateSelectizeInput(session, "lookupFilm",selected = "")
    }else{
      shinyWidgets::closeSweetAlert(session)
    }
  })
  
  
  #Need to keep input$new_success above/separate b/c it requires the extra layer of new_success==TRUE
  observeEvent(ignoreInit=TRUE,input$btn_pg_enterNewScenario,{
    
    rv_bdCmp$bad<-NULL
    
    # Add project name to Scenario builder: Start Akshay Mahale
    output$proj_name <- renderText(rv_projectscenarios$project_title)
    # Add project name to Scenario builder: End Akshay Mahale
    
    #print(rv_projectList$projs[input$project_selection_rows_selected])
    print("genre")
    print(rv_projectscenarios$project_genre)
    print(rv_projectscenarios$project_rating)
    print(str(rv_projectscenarios))
    rv_currentCompsLength$t_1 <- 0
    rv_currentCompsLength$t_0 <- 0
    updateTabItems(session, inputId="sbm", selected = "build")
    rv_scenInputs$inputs <- mms %>% slice(0)
    rv_flgNewOrEdit$flg <- "New"
    rv_currentComps$currentComps <- mms %>% slice(0) %>% select(-`Smart Rank`,-`Supervised Rank`,-`Demo Rank`)
    rv_currentComps$finalComps <- c()
    rv_scenInputs$inputs <- mms %>% slice(0) %>% select(-`Smart Rank`,-`Supervised Rank`,-`Demo Rank`)
    
    updateTextInput(session,"fcst_low",value = "")
    updateTextInput(session,"fcst_mid",value = "")
    updateTextInput(session,"fcst_high",value = "")
    
    #Int fcst: Akshay Mahale
    updateTextInput(session,"int_fcst_low",value = "")
    updateTextInput(session,"int_fcst_mid",value = "")
    updateTextInput(session,"int_fcst_high",value = "")
    
    
    
    updateTextInput(session,"imax_low",value="")
    updateTextInput(session,"imax_mid",value="")
    updateTextInput(session,"imax_high",value="")
    updateTextInput(session,"threed_low",value="")
    updateTextInput(session,"threed_mid",value="")
    updateTextInput(session,"threed_high",value="")
    updateTextInput(session,"adpub_low",value = "")
    
    
    print("chkchk")
    updateTextInput(session,"adpub_mid",value = "")
    updateTextInput(session,"adpub_high",value = "")
    updateTextInput(session,"fr_low",value="")
    updateTextInput(session,"fr_mid",value="")
    updateTextInput(session,"fr_high",value="")
    updateTextInput(session,"prints_low",value="")
    updateTextInput(session,"prints_mid",value="")
    updateTextInput(session,"prints_high",value="")
    updateTextAreaInput(session,"fcst_notes",value="")
    
    #Add new Int fields : Akshay Mahale
    updateTextInput(session,"int_adpub_low",value = "")
    updateTextInput(session,"int_adpub_mid",value = "")
    updateTextInput(session,"int_adpub_high",value = "")
    updateTextInput(session,"int_prints_low",value="")
    updateTextInput(session,"int_prints_mid",value="")
    updateTextInput(session,"int_prints_high",value="")
    updateTextInput(session,"int_fr_low",value="")
    updateTextInput(session,"int_fr_mid",value="")
    updateTextInput(session,"int_fr_high",value="")
    
    #China Fcst fields 
    updateTextInput(session,"china_fcst_low",value = "")
    updateTextInput(session,"china_fcst_mid",value = "")
    updateTextInput(session,"china_fcst_high",value = "")
    updateTextInput(session,"china_fr_low",value="")
    updateTextInput(session,"china_fr_mid",value="")
    updateTextInput(session,"china_fr_high",value="")
    updateTextInput(session,"china_adpub_low",value="")
    updateTextInput(session,"china_adpub_mid",value="")
    updateTextInput(session,"china_adpub_high",value="")
    updateTextInput(session,"china_prints_low",value="")
    updateTextInput(session,"china_prints_mid",value="")
    updateTextInput(session,"china_prints_high",value="")
    
    
    rv_currentScenario$name <- "<NO SCENARIO SELECTED>"
    shinyjs::reset("scenario_name")
    #shinyjs::reset("scenario_date")
    
    updateSelectizeInput(session, "lookupFilm",selected = "")
    
    #IDEA: option to have a pop-up from the project selection page that allows them to duplicate the scenario
    # and asks that they just type in a new valid name!!
    #if(sel2()){
    #   updateSelectizeInput()
    #}
  })
  
  observeEvent(ignoreInit=TRUE,input$new_success,{
    updateTextInput(session,"fcst_low",value="")
    updateTextInput(session,"fcst_mid",value="")
    updateTextInput(session,"fcst_high",value="")
    #Int fcst: Akshay Mahale
    updateTextInput(session,"int_fcst_low",value = "")
    updateTextInput(session,"int_fcst_mid",value = "")
    updateTextInput(session,"int_fcst_high",value = "")
    updateTextInput(session,"imax_low",value="")
    updateTextInput(session,"imax_mid",value="")
    updateTextInput(session,"imax_high",value="")
    updateTextInput(session,"threed_low",value="")
    updateTextInput(session,"threed_mid",value="")
    updateTextInput(session,"threed_high",value="")
    updateTextInput(session,"adpub_low",value="")
    updateTextInput(session,"adpub_mid",value="")
    updateTextInput(session,"adpub_high",value="")
    updateTextInput(session,"fr_low",value="")
    updateTextInput(session,"fr_mid",value="")
    updateTextInput(session,"fr_high",value="")
    updateTextInput(session,"prints_low",value="")
    updateTextInput(session,"prints_mid",value="")
    updateTextInput(session,"prints_high",value="")
    updateTextAreaInput(session,"fcst_notes",value="")
    
    #Add new Int fields : Akshay Mahale
    updateTextInput(session,"int_adpub_low",value = "")
    updateTextInput(session,"int_adpub_mid",value = "")
    updateTextInput(session,"int_adpub_high",value = "")
    updateTextInput(session,"int_prints_low",value="")
    updateTextInput(session,"int_prints_mid",value="")
    updateTextInput(session,"int_prints_high",value="")
    updateTextInput(session,"int_fr_low",value="")
    updateTextInput(session,"int_fr_mid",value="")
    updateTextInput(session,"int_fr_high",value="")
    
    #China Fcst fields 
    updateTextInput(session,"china_fcst_low",value = "")
    updateTextInput(session,"china_fcst_mid",value = "")
    updateTextInput(session,"china_fcst_high",value = "")
    updateTextInput(session,"china_fr_low",value="")
    updateTextInput(session,"china_fr_mid",value="")
    updateTextInput(session,"china_fr_high",value="")
    updateTextInput(session,"china_adpub_low",value="")
    updateTextInput(session,"china_adpub_mid",value="")
    updateTextInput(session,"china_adpub_high",value="")
    updateTextInput(session,"china_prints_low",value="")
    updateTextInput(session,"china_prints_mid",value="")
    updateTextInput(session,"china_prints_high",value="")
  })
  
  observeEvent(ignoreInit=TRUE,c(input$btn_pg_enterNewScenario, input$btn_scenarioReset, input$new_success),{
    rv_model_fcst$mod_scen_inputs <- 0
    rv_model_fcst$mod_comps <- 0
    #updateSelectizeInput(session, "scenario_mpaa",selected = "")
    #updateSelectizeInput(session, "scenario_",selected = "")
    updateSelectizeInput(session, "scenario_submitted", selected = "Not Submitted")
    updateSelectizeInput(session, "scenario_mpaa",selected = "")
    if(length(rv_projectscenarios$project_rating)>0){
      updateSelectizeInput(session, "scenario_mpaa", selected = rv_projectscenarios$project_rating)
    }
    updateTextInput(session,"scenario_prodbudget", value = "")
    updateSelectizeInput(session, "scenario_director", selected = "")
    updateSelectizeInput(session, "scenario_cast", selected = "")
    updateSelectizeInput(session, "scenario_voice", selected = "")
    #updateSelectizeInput(session, "scenario_distributor", selected = "")
    updateSelectizeInput(session, "scenario_producer", selected = "")
    updateSelectizeInput(session, "scenario_execproducer", selected = "")
    updateSelectizeInput(session, "scenario_genre1", selected = "")
    updateSelectizeInput(session, "scenario_genre2", selected = "")
    updateSelectizeInput(session, "scenario_genre3", selected = "")
    if(length(rv_projectscenarios$project_genre) > 0){
      updateSelectizeInput(session, "scenario_genre1", selected = rv_projectscenarios$project_genre[1])
    }
    if(length(rv_projectscenarios$project_genre) > 1){
      updateSelectizeInput(session, "scenario_genre2", selected = rv_projectscenarios$project_genre[2])
    }
    if(length(rv_projectscenarios$project_genre) > 2){
      updateSelectizeInput(session, "scenario_genre3", selected = rv_projectscenarios$project_genre[3])
    }
    updateSelectizeInput(session, "scenario_keyword1", selected = "")
    updateSelectizeInput(session, "scenario_keyword2", selected = "")
    updateSelectizeInput(session, "scenario_keyword3", selected = "")
    if(length(rv_projectscenarios$project_keywords) > 0){
      updateSelectizeInput(session, "scenario_keyword1", selected = rv_projectscenarios$project_keywords[1])
    }
    if(length(rv_projectscenarios$project_keywords) > 1){
      updateSelectizeInput(session, "scenario_keyword2", selected = rv_projectscenarios$project_keywords[2])
    }
    if(length(rv_projectscenarios$project_keywords) > 2){
      updateSelectizeInput(session, "scenario_keyword3", selected = rv_projectscenarios$project_keywords[3])
    }
    updateSelectizeInput(session, "scenario_studio", selected = "")
    updateSelectizeInput(session, "scenario_editor", selected = "")
    updateSelectizeInput(session, "scenario_writer", selected = "")
    updateSelectizeInput(session, "scenario_longweekend", selected = "")
    updateSelectizeInput(session, "scenario_cinemascore", selected = "")
    updateDateInput(session, inputId = "scenario_release", value = as.Date(NA))
    updateDateInput(session, inputId = "scenario_dateSubmitted", value = Sys.Date())
    updateNumericRangeInput(session, inputId = "scenario_minutes", value = c(0,400), label = "Minutes")
    updateNumericRangeInput(session, inputId = "scenario_tomatometer", value = c(0,100), label = "Tomato Meter")
    updateNumericRangeInput(session, inputId = "scenario_domesticbo", value = c(1,1000), label = "Domestic Box Office ($M)")
    updateNumericRangeInput(session, inputId = "scenario_intlbo", value = c(1,4000), label = "International Box Office ($M)")
    updateNumericRangeInput(session, inputId = "scenario_weekendbo", value = c(1,1000), label = "Weekend Box Office ($M)")
    updateNumericRangeInput(session, inputId = "scenario_wwbo", value = c(1,4000), label = "WW Box Office ($M)")
  })
  
  
  
  #If they proceed to EDIT an existing scenario,
  # 1) Reset all inputs:
  # 2) Import all inputs INCLUDING the scenario name (if they change the name from here, it will overwrite the name along with the other details)
  observeEvent(ignoreInit=TRUE,input$btn_pg_enterExistingScenario,{
    print("also check here")
    rv_model_fcst$mod_scen_inputs <- 0
    rv_model_fcst$mod_comps <- 0
    updateTabItems(session, inputId = "sbm", selected = "build")
    
    rv_flgNewOrEdit$flg <- "Edit"
    print("check11")
    #rv_currentScenario$name <- restricted_scenarios()$Name[input$scenario_selection_rows_selected]
    rv_currentScenario$name <- rv_projectscenarios$scenario_names[input$scenario_selection_rows_selected]
    #print(rv_currentScenario$name)
    #print(rv_projectscenarios$project_title)
    print("check22")
    rv_currentScenario$full_detail <- rv_scenarios$scens %>% filter(Title == rv_projectscenarios$project_title &
                                                                      Name == rv_currentScenario$name)
    print("check33")
    print(rv_currentScenario$full_detail)
    
    rv_scenInputs$inputs <- rv_currentScenario$full_detail %>% mutate(Title = paste0("SCENARIO: ",Name))
    
    if(rv_flgNewOrEdit$flg == "Edit"){
      #output$page <- renderUI(ui5)
      #print(rv_currentScenario$full_detail %>% select(Low) %>% pull())
      
      # low <- rv_currentScenario$full_detail$Low# %>% select(Low) %>% pull()
      # #mid <- rv_currentScenario$full_detail %>% select(Mid) %>% pull()
      # mid <- rv_currentScenario$full_detail$Mid
      # high <- rv_currentScenario$full_detail$High
      # print(low)
      
      #print(rv_currentScenario$full_detail$`Ad/Pub Mid`)
      
      updateTextInput(session,"fcst_low",value = rv_currentScenario$full_detail$Low)
      updateTextInput(session,"fcst_mid",value = rv_currentScenario$full_detail$Base)
      updateTextInput(session,"fcst_high",value = rv_currentScenario$full_detail$High)
      #Int fcst: Akshay Mahale
      updateTextInput(session,"int_fcst_low",value = rv_currentScenario$full_detail$`Int Low`)
      updateTextInput(session,"int_fcst_mid",value = rv_currentScenario$full_detail$`Int Base`)
      updateTextInput(session,"int_fcst_high",value = rv_currentScenario$full_detail$`Int High`)
      
      updateTextInput(session,"imax_low",value=rv_currentScenario$full_detail$`Imax Low`)
      updateTextInput(session,"imax_mid",value=rv_currentScenario$full_detail$`Imax Mid`)
      updateTextInput(session,"imax_high",value=rv_currentScenario$full_detail$`Imax High`)
      updateTextInput(session,"threed_low",value=rv_currentScenario$full_detail$`ThreeD Low`)
      updateTextInput(session,"threed_mid",value=rv_currentScenario$full_detail$`ThreeD Mid`)
      updateTextInput(session,"threed_high",value=rv_currentScenario$full_detail$`ThreeD High`)
      updateTextInput(session,"fr_low", value = rv_currentScenario$full_detail$`FR Low`)
      updateTextInput(session,"fr_mid", value = rv_currentScenario$full_detail$`FR Mid`)
      updateTextInput(session,"fr_high", value = rv_currentScenario$full_detail$`FR High`)
      updateTextInput(session,"prints_low", value = rv_currentScenario$full_detail$`Prints Low`)
      updateTextInput(session,"prints_mid", value = rv_currentScenario$full_detail$`Prints Mid`)
      updateTextInput(session,"prints_high", value = rv_currentScenario$full_detail$`Prints High`)
      updateTextInput(session,"adpub_low", value = rv_currentScenario$full_detail$`Ad/Pub Low`)
      print(rv_currentScenario$full_detail$`Ad/Pub Low`)
      print(rv_currentScenario$full_detail$`Ad/Pub Mid`)
      updateTextInput(session,"adpub_mid", value = rv_currentScenario$full_detail$`Ad/Pub Mid`)
      updateTextInput(session,"adpub_high", value = rv_currentScenario$full_detail$`Ad/Pub High`)
      updateTextAreaInput(session,"fcst_notes",value=rv_currentScenario$full_detail$Notes)
      
      #Add new Intl fields:Akshay Mahale
      updateTextInput(session,"int_fr_low", value = rv_currentScenario$full_detail$`Int FR Low`)
      updateTextInput(session,"int_fr_mid", value = rv_currentScenario$full_detail$`Int FR Mid`)
      updateTextInput(session,"int_fr_high", value = rv_currentScenario$full_detail$`Int FR High`)
      updateTextInput(session,"int_prints_low", value = rv_currentScenario$full_detail$`Int Prints Low`)
      updateTextInput(session,"int_prints_mid", value = rv_currentScenario$full_detail$`Int Prints Mid`)
      updateTextInput(session,"int_prints_high", value = rv_currentScenario$full_detail$`Int Prints High`)
      updateTextInput(session,"int_adpub_low", value = rv_currentScenario$full_detail$`Int Ad/Pub Low`)
      updateTextInput(session,"int_adpub_mid", value = rv_currentScenario$full_detail$`Int Ad/Pub Mid`)
      updateTextInput(session,"int_adpub_high", value = rv_currentScenario$full_detail$`Int Ad/Pub High`)
      
      #China Fcst fields 
      updateTextInput(session,"china_fcst_low",value = rv_currentScenario$full_detail$`China Fcst Low`)
      updateTextInput(session,"china_fcst_mid",value = rv_currentScenario$full_detail$`China Fcst Mid`)
      updateTextInput(session,"china_fcst_high",value = rv_currentScenario$full_detail$`China Fcst High`)
      updateTextInput(session,"china_fr_low",value=rv_currentScenario$full_detail$`China FR Low`)
      updateTextInput(session,"china_fr_mid",value=rv_currentScenario$full_detail$`China FR Mid`)
      updateTextInput(session,"china_fr_high",value=rv_currentScenario$full_detail$`China FR High`)
      updateTextInput(session,"china_adpub_low",value=rv_currentScenario$full_detail$`China Ad/Pub Low`)
      updateTextInput(session,"china_adpub_mid",value=rv_currentScenario$full_detail$`China Ad/Pub Mid`)
      updateTextInput(session,"china_adpub_high",value=rv_currentScenario$full_detail$`China Ad/Pub High`)
      updateTextInput(session,"china_prints_low",value=rv_currentScenario$full_detail$`China Prints Low`)
      updateTextInput(session,"china_prints_mid",value=rv_currentScenario$full_detail$`China Prints Mid`)
      updateTextInput(session,"china_prints_high",value=rv_currentScenario$full_detail$`China Prints High`)
      
      
    }
    print("check44")
    #print(rv_currentScenario$full_detail)
    #print(rv_currentScenario$full_detail$`Selected Comps Mcodes`)
    comps <- strsplit(rv_currentScenario$full_detail$`Selected Comps Mcodes`,", ")[[1]]
    #print(comps)
    print("check55")
    #shinyjs::enable("btn_pg_ScenarioToForecast")
    
    rv_currentComps$currentComps <- mms %>% filter(Mcode %in% comps) %>% select(-`Smart Rank`,-`Supervised Rank`,-`Demo Rank`)
    rv_currentComps$finalComps <- strsplit(rv_currentScenario$full_detail$`Final Comps Mcodes`,", ")[[1]]
    
    rv_currentCompsLength$t_1 <- 0
    rv_currentCompsLength$t_0 <- nrow(rv_currentComps$currentComps)
    
    #print(rv_currentComps$currentComps$Title)
    print(rv_flgNewOrEdit$flg)
    #print(rv_currentScenario$name)
    #print(rv_currentScenario$full_detail)
    #shinyjs::reset("input1")
    #shinyjs::reset("input2")
    #shinyjs::reset("input3")
    
    IMPORTED_DIRECTOR <- unlist(strsplit(rv_currentScenario$full_detail$`Director`,split = ", "))
    IMPORTED_CAST <- unlist(strsplit(rv_currentScenario$full_detail$`Cast (Major)`,split = ", "))
    IMPORTED_VOICE <- unlist(strsplit(rv_currentScenario$full_detail$`Voice`,split = ", "))
    #IMPORTED_DISTRIBUTOR <- unlist(strsplit(rv_currentScenario$full_detail$`Distributor`,split = ", "))
    IMPORTED_PRODUCER <- unlist(strsplit(rv_currentScenario$full_detail$`Producer`,split = ", "))
    IMPORTED_EXECPROD <- unlist(strsplit(rv_currentScenario$full_detail$`Executive Producer`,split = ", "))
    IMPORTED_EDITOR <- unlist(strsplit(rv_currentScenario$full_detail$`Editor`,split = ", "))
    IMPORTED_WRITER <- unlist(strsplit(rv_currentScenario$full_detail$`Writer`,split = ", "))
    
    IMPORTED_MINUTES <- unlist(strsplit(rv_currentScenario$full_detail$`Minutes`,split = "-"))
    IMPORTED_TOMATO <- unlist(strsplit(rv_currentScenario$full_detail$`Tomato Meter`,split = "-"))
    IMPORTED_DOMESTIC <- unlist(strsplit(rv_currentScenario$full_detail$`DBO ($M)`,split = "-"))
    IMPORTED_INTL <- unlist(strsplit(rv_currentScenario$full_detail$`IBO ($M)`,split = "-"))
    IMPORTED_WEEKEND <- unlist(strsplit(rv_currentScenario$full_detail$`WBO ($M)`,split = "-"))
    IMPORTED_WW <- unlist(strsplit(rv_currentScenario$full_detail$`WWBO ($M)`,split = "-"))
    
    print(IMPORTED_CAST)
    print(rv_currentScenario$full_detail$`Cast (Major)`)
    print("Update modal")
    print(rv_currentScenario$full_detail)
    #updateSelectizeInput()
    updateTextInput(session, inputId = "scenario_name", value = rv_currentScenario$name)
    #updateDateInput(session, inputId = "scenario_date", value = rv_currentScenario$full_detail$`Last Modified`)
    updateSelectizeInput(session, inputId = "scenario_submitted",selected = rv_currentScenario$full_detail$Submitted)
    updateDateInput(session, inputId = "scenario_dateSubmitted",value = rv_currentScenario$full_detail$`Date Submitted`)
    updateSelectizeInput(session, inputId = "scenario_mpaa", selected = rv_currentScenario$full_detail$`MPAA Rating`)
    updateTextInput(session, inputId = "scenario_prodbudget", value = rv_currentScenario$full_detail$`Production Budget ($M)`)#selected = rv_currentScenario$full_detail$`Production Budget ($M)`)
    updateSelectizeInput(session, inputId = "scenario_director", selected = IMPORTED_DIRECTOR )
    updateSelectizeInput(session, inputId = "scenario_cast", selected = IMPORTED_CAST )
    updateSelectizeInput(session, inputId = "scenario_voice", selected =IMPORTED_VOICE )
    #updateSelectizeInput(session, inputId = "scenario_distributor", selected = IMPORTED_DISTRIBUTOR )
    updateSelectizeInput(session, inputId = "scenario_producer", selected =IMPORTED_PRODUCER )
    updateSelectizeInput(session, inputId = "scenario_execproducer", selected= IMPORTED_EXECPROD )
    updateSelectizeInput(session, inputId = "scenario_genre1", selected = rv_currentScenario$full_detail$`Primary Genre`)
    updateSelectizeInput(session, inputId = "scenario_genre2", selected = rv_currentScenario$full_detail$`Secondary Genre`)
    updateSelectizeInput(session, inputId = "scenario_genre3", selected = rv_currentScenario$full_detail$`Other Genre`)
    updateSelectizeInput(session, inputId = "scenario_keyword1", selected = rv_currentScenario$full_detail$`Keyword 1`)
    updateSelectizeInput(session, inputId = "scenario_keyword2", selected = rv_currentScenario$full_detail$`Keyword 2`)
    updateSelectizeInput(session, inputId = "scenario_keyword3", selected = rv_currentScenario$full_detail$`Keyword 3`)
    updateSelectizeInput(session, inputId = "scenario_studio", selected = rv_currentScenario$full_detail$`Studio`)
    updateSelectizeInput(session, inputId = "scenario_editor", selected = IMPORTED_EDITOR )
    updateSelectizeInput(session, inputId = "scenario_writer", selected = IMPORTED_WRITER )
    updateDateInput(session, inputId = "scenario_release", value = rv_currentScenario$full_detail$`Release Date`)
    updateSelectizeInput(session, inputId = "scenario_longweekend", selected = rv_currentScenario$full_detail$`Long Weekend`)
    updateSelectizeInput(session, inputId = "scenario_cinemascore", selected = rv_currentScenario$full_detail$`Cinema Score`)
    updateNumericRangeInput(session, inputId = "scenario_minutes", value = IMPORTED_MINUTES, label = "Minutes")
    updateNumericRangeInput(session, inputId = "scenario_tomatometer", value = IMPORTED_TOMATO, label = "Tomato Meter")
    updateNumericRangeInput(session, inputId = "scenario_domesticbo", value = IMPORTED_DOMESTIC, label = "Domestic Box Office ($M)")
    updateNumericRangeInput(session, inputId = "scenario_intlbo", value = IMPORTED_INTL, label = "International Box Office ($M)")
    updateNumericRangeInput(session, inputId = "scenario_weekendbo", value = IMPORTED_WEEKEND, label = "Weekend Box Office ($M)")
    updateNumericRangeInput(session, inputId = "scenario_wwbo", value = IMPORTED_WW, label = "WW Box Office ($M)")
    
    #Elements to display as static text for the user to see outside of the modal:
    # Add project name to Scenario builder: Start Akshay Mahale
    output$proj_name <- renderText(rv_projectscenarios$project_title)
    # Add project name to Scenario builder: End Akshay Mahale
    output$prod_budget <- renderText(input$scenario_prodbudget)
    output$mpaa <- renderText(paste(input$scenario_mpaa, collapse = ", "))
    output$director <- renderText(paste(input$scenario_director, collapse = ", "))
    output$cast <- renderText(paste(input$scenario_cast, collapse = ", "))
    output$voice <- renderText(paste(input$scenario_voice, collapse = ", "))
    output$producer <- renderText(paste(input$scenario_producer, collapse = ", "))
    output$execprod <- renderText(paste(input$scenario_execproducer, collapse = ", "))
    
  })
  
  
  #Return to project selection from the scenario building page:
  # observeEvent(ignoreInit=TRUE,input$btn_pg_ScenarioToProjectSel,{
  #   output$page <- renderUI(ui3)
  # })
  
  
  
  #SCENARIO BUILDING SERVER:-----
  
  #When a user wants to edit an existing scenario
  # or just import the inputs from an existing scenario,
  # Update all inputs:
  # observeEvent(ignoreInit=TRUE,input$scenario_change,{
  #   updateTextInput(session, inputId = "scenario_name", value = input$scenario_change)
  #   #[AK - ADD REMAINING UPDATE INPUTS HERE]
  # })
  
  
  
  
  
  observe({
    output$txt_currentScenario <- renderText(rv_currentScenario$name)
  })
  
  
  #Update the btn_saveSelectedComps to say different things when a user has not yet entered a valid scenario name
  # and only enable the button when they are in a valid scenario
  # observe({
  #   if(is.null(rv_currentScenario$name)||rv_currentScenario$name == "<NO SCENARIO SELECTED>"){
  #     output$txt_saveCompsBtn <- renderText("Edit scenario name to save comps.")
  #     #shinyjs::disable("btn_saveSelectedComps")
  #     #shinyjs::disable("btn_pg_ScenarioToForecast")
  # 
  #   }else{
  #     output$txt_saveCompsBtn <- renderText("Save Comps")
  #     shinyjs::enable("btn_saveSelectedComps")
  #     #shinyjs::enable("btn_pg_ScenarioToForecast")
  #     #shinyjs::toggleState("btn_saveSelectedComps", condition = (!is.null(rv_currentScenario$name) & rv_currentScenario$name != "<NO SCENARIO SELECTED>")|!rv_flgNewOrEdit$flg=="Edit")
  #   }
  # })
  
  #When the btn_saveSelectedComps is clicked,
  # 1) Reload the most recent scenario data
  # 2) Inject the selected comps and mcodes into row with project and scenario name
  # 3) Export
  # observeEvent(ignoreInit=TRUE,input$btn_saveSelectedComps,{
  # 
  #   titles <- paste(rv_currentComps$currentComps$Title,collapse = ", ")
  #   mcodes <- paste(rv_currentComps$currentComps$Mcode,collapse = ", ")
  # 
  #   #Read/Overwrite/Save Scenarios
  #   rv_scenarios$scens <- readRDS("data/000_scenarios.RDS")
  # 
  #   rv_scenarios$scens <- rv_scenarios$scens %>%
  #     mutate(`Selected Comps` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, titles, `Selected Comps`),
  #            `Selected Comps Mcodes` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, mcodes, `Selected Comps Mcodes`))
  # 
  #   saveRDS(rv_scenarios$scens, "data/000_scenarios.RDS")
  #   
  #   rv_projectList$projs <- readRDS("data/000_projects.RDS")
  #   rv_projectList$projs <- rv_projectList$projs %>% mutate(`Last Scenario` = if_else(Title == rv_projectscenarios$project_title, Sys.time(), `Last Scenario`))
  #   saveRDS(rv_projectList$projs, "data/000_projects.RDS")
  # 
  #   sendSweetAlert(session, title = "Successfully Saved Selected Comps", type = "success")
  # 
  # })
  
  
  
  #When in smart counting pop-up, allow user to switch all on/off
  observeEvent(ignoreInit=TRUE,input$switch_ALL,{
    updateSwitchInput(session, inputId = "switch_director", input$switch_ALL)
    updateSwitchInput(session, inputId = "switch_cast", input$switch_ALL)
    updateSwitchInput(session, inputId = "switch_execprod", input$switch_ALL)
    updateSwitchInput(session, inputId = "switch_writer", input$switch_ALL)
    updateSwitchInput(session, inputId = "switch_genre", input$switch_ALL)
    updateSwitchInput(session, inputId = "switch_keyword", input$switch_ALL)
    updateSwitchInput(session, inputId = "switch_rating", input$switch_ALL)
    #updateSwitchInput(session, inputId = "switch_releaseDay", input$switch_ALL)
    updateSwitchInput(session, inputId = "switch_releaseMonth", input$switch_ALL)
    #updateSwitchInput(session, inputId = "switch_releaseWeekend", input$switch_ALL)
  })
  
  #Modal - Comp Visuals -----
  #Barplot on project management page:
  output$pm_comp_selection_plot <- renderPlotly({
    if(length(input$scenario_selection_rows_selected) != 0){
      print("new here 5")
      ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
      print(ttl)
      tmp <- rv_scenarios$scens %>%
        filter(Title == ttl) %>% 
        slice(input$scenario_selection_rows_selected) %>%
        select(Name,`Selected Comps Mcodes`,Low,Base,High)
      print(tmp)
      
      comma_count <- max(str_count(tmp$`Selected Comps Mcodes`,","),na.rm = T) + 1
      
      df2 <- tmp %>%
        separate(`Selected Comps Mcodes`, into = paste0(1:comma_count)) %>%
        gather(key, Mcode, -Name,-Low,-Base,-High) %>%
        select(-key) %>%
        left_join(
          mms %>%
            select(Title, Mcode, `DBO ($M)`,`IBO ($M)`),
          by = "Mcode"
        )
      
      df2 <- df2 %>% arrange(`DBO ($M)`)
      df2 <- df2 %>% mutate(Title = factor(Title, levels = unique(df2$Title),ordered = TRUE)) %>% 
        mutate(col = ifelse(`DBO ($M)` <= Low, "red",
                            ifelse(`DBO ($M)`<=Base,"orange","darkgreen"))) %>% 
        mutate(col = factor(col,levels = c("red","orange","darkgreen"),ordered = TRUE))
      
      print(df2)
      print(str(df2))
      p <- ggplot(df2, aes(x = Title, y = `DBO ($M)`, fill=col))
      p <- p+ geom_bar(stat="identity")
      p <- p + labs(x="")
      p <- p + theme(legend.position = "none")
      print(df2)
      vals <- levels(df2$col)[which(levels(df2$col) %in% df2$col)]
      #vals <- unique(df2$col)[!is.na(unique(df2$col))]
      #print(vals)
      #print(all(vals %in% c("red","orange")))
      #vals <- ifelse(all(vals %in% c("red","orange","darkgreen")),c("red","orange","darkgreen"),
      #               ifelse(all(vals %in% c("red","orange")), c("red","orange"),
      #                      ifelse(all(c("orange","darkgreen") %in% vals),c("orange","darkgreen"),
      #                             ifelse(all(c("red","darkgreen") %in% vals),c("red",'darkgreen'),vals))))
      print(vals)
      p <- p + scale_fill_manual(values = vals)
      p <- p + geom_hline(yintercept = unique(df2$Low), linetype = "dashed", color = "red", lwd = 1.25)
      p <- p + geom_hline(yintercept = unique(df2$Base), linetype = "dashed", color = "orange", lwd = 1.25)
      p <- p + geom_hline(yintercept = unique(df2$High), linetype = "dashed", color = "darkgreen", lwd = 1.25)
      p <- p + coord_flip()
      ggplotly(p)
      #print("new here 6")
      
      # df2 %>% 
      #   plot_ly() %>%
      #   add_trace(x = ~Title, y = ~`DBO ($M)`, type = 'bar', 
      #             name = "Domestic",
      #             text = paste("$",round(df2$`DBO ($M)`, 2),"M"), textposition = 'auto',
      #             marker = list(color = 'rgb(158,202,225)',
      #                           line = list(color = 'rgb(0,0,0)', width = 1.5))) %>%
      #   add_trace(x = ~Title, y = ~`IBO ($M)`, type = 'bar', 
      #             name = "International",
      #             text = paste("$",round(df2$`IBO ($M)`, 2),"M"), textposition = 'auto',
      #             marker = list(color = 'rgb(240,128,128)',
      #                           line = list(color = 'rgb(0,0,0)', width = 1.5))) %>%
      #   layout(title = "Domestic & International Box Office Revenue ($M) of Comps",
      #          barmode = 'group',
      #          xaxis = list(title = "Title"), 
      #          yaxis = list(title = "Box Office Revenue ($M)"))
      
    }
  })
  
  #Barplot:
  observe({
    output$plot_comps <- renderPlotly({
      
      if(nrow(rv_currentComps$currentComps)==0){
        p<-ggplot()
      }else{
        #using selected comps data
        
        dta <- rv_currentComps$currentComps %>% 
          group_by_at(input$select_vis) %>% 
          summarize(Count = n()) %>% 
          ungroup()%>% 
          mutate_at(input$select_vis, as.factor)
        
        dta<- na.omit(dta)
        
        dta<-dta%>%  mutate(`% of value`=(Count/sum(Count))*100)%>% mutate(`% of value text`=paste0(as.character(round(`% of value`,1))," %"))
        
        #If displaying a date on the x-axis, keep it in the proper chronological order
        #If displaying any other categorical field, arrange in the order from lowest freq to highest freq
        if(names(dta)[1] %in% c("Release Year")){
          dta <- dta %>% arrange(`Release Year`)
        }else{
          dta <- dta %>% arrange(Count)
        }
        
        var1 <- sym(input$select_vis)
        var2 <- sym("Count")
        
        print("category count")
        print(dta)
        
        p <- ggplot(data = dta, aes(x = (!!var1), y = (!!var2)))
        p <- p + geom_bar(stat = "identity", fill = "#0064b0")
        p<- p+geom_text(aes(label=`% of value text`),size=4,color="black",vjust=0,hjust=0.5)
        p <- p + scale_y_continuous(breaks = seq(0,max(dta$Count),2), limits = c(0,max(dta$Count+1)))
        p <- p + theme_bw()
        p <- p + theme(panel.border = element_blank(),
                       axis.line = element_line(color="black"),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),
                       axis.text.x =element_text(angle = 12, hjust = 1),
                       axis.text=element_text(size=11,face="bold"),
                       plot.margin=unit(c(2,0,0,0),"cm"))
        p <- p + labs(title = paste0("Distribution of ",input$select_vis))
        #p <- p + xlab(input$select_vis)
        #p <- p + ylab("Count")
      }
      ggplotly(p)
    })
  })
  
  observe({
    output$plot_demo3<-renderPlot({
      if(nrow(rv_currentComps$currentComps)==0){
        f<-ggplot()
        e<-ggplot()
      }else{
        gradeCine<-gradeCinemascore%>%filter(`MMS Code` %in% rv_currentComps$currentComps$Mcode== TRUE)
        print("in cine viz")
        print(rv_currentComps$currentComps)
        print(dim(gradeCine))
        tabDemoData<-mms%>%filter(Mcode %in% rv_currentComps$currentComps$Mcode== TRUE & Mcode %in% gradeCine$`MMS Code`)%>%
          select(Mcode,Title,`DBO ($M)`,`WBO ($M)`,`Release Date`)%>%
          mutate(`FSS Mult`=round(as.numeric(`DBO ($M)`)/as.numeric(`WBO ($M)`),2),`DBO ($M)`=paste0("$",as.character(round(`DBO ($M)`,1)),"M"),`WBO ($M)`=paste0("$",as.character(round(`WBO ($M)`,1)),"M"))%>%
          rename("Open Wkn"=`WBO ($M)`,"Cume"=`DBO ($M)`) %>%
          mutate(`Release Date` = as.Date(`Release Date`, "%d-%m-%Y")) 
        print("arrange release")
        # print(head(tabDemoData))
        tabDemoData<-tabDemoData%>%gather(Desc.,Value,Cume:`FSS Mult`)
        tabDemoData$Desc. <- factor(tabDemoData$Desc.,levels = c( "FSS Mult", "Cume","Open Wkn"))
        print("tabdemoData")
        print(tabDemoData)
        f<-ggplot(data = gradeCine, aes(x = Title, y = `Desc.`)) +
          geom_tile(aes(fill = GradeNewNum), colour = "white")+
          geom_text(aes(label=label),size=5,color="white")+
          scale_fill_gradient(low = "darkgreen",  high = "red")+
          labs(title = paste0("Grade % distribution across demographic attr. for selected comps"))+
          xlab("Comps used for forecasts")+
          ylab("Demographic Attributes")+
          theme_classic()+
          theme(
            legend.title = element_blank(),
            legend.position = "none",
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text=element_text(size=13),
            plot.title = element_text(size = 16))
        
        e<-ggplot(data = tabDemoData, aes(x = Title, y = `Desc.`)) +
          geom_tile(colour = "black",fill="white")+
          geom_text(aes(label=Value),size=5,color="black")+
          theme_classic()+
          theme(axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.x =element_text(angle = 12, hjust = 1),
                axis.text=element_text(size=13),
                axis.title=element_text(size=15))
        
      }
      gridExtra::grid.arrange(f, e, nrow=2,  heights = c(3,1))
      
      
    })
  })
  
  
  
  #Barplot: demographic Akshay Mahale
  observe({
    output$plot_demo1 <- renderPlot({
      if(nrow(rv_currentComps$currentComps)==0){
        f<-ggplot()
      }else{
        #using selected comps data
        
        choice<-""
        dataDemoBar<-mmsNew%>%filter(Mcode %in% rv_currentComps$currentComps$Mcode== FALSE)
        if(input$select_metric=="Age Breakdown")
        {
          choice="Age Brkdwn_"
          data1<-rv_currentComps$currentComps%>%gather(demographicKey,ValuePercentage,Patrons_General:`Age Brkdwn_55+`)%>%
            select(demographicKey,ValuePercentage)%>%
            filter(str_detect(demographicKey,"Brkdwn"))
          data2<-dataDemoBar%>%
            select(demographicKey,ValuePercentage)%>%
            filter(str_detect(demographicKey,"Brkdwn"))
          
        }
        else if(input$select_metric=="Age Split")
        {
          choice="Age Split_"
          data1<-rv_currentComps$currentComps%>%gather(demographicKey,ValuePercentage,Patrons_General:`Age Brkdwn_55+`)%>%
            select(demographicKey,ValuePercentage)%>%
            filter(str_detect(demographicKey,"Split"))
          data2<-dataDemoBar%>%
            select(demographicKey,ValuePercentage)%>%
            filter(str_detect(demographicKey,"Split"))
        }
        else
        {
          choice="Ethnicity_"
          data1<-rv_currentComps$currentComps%>%gather(demographicKey,ValuePercentage,Patrons_General:`Age Brkdwn_55+`)%>%
            select(demographicKey,ValuePercentage)%>%
            filter(str_detect(demographicKey,"Ethnicity"))
          print("Data 1")
          print(data1)
          data2<-dataDemoBar%>%
            select(demographicKey,ValuePercentage)%>%
            filter(str_detect(demographicKey,"Ethnicity"))
        }
        
        c<-data1%>%select(demographicKey,ValuePercentage)%>%
          mutate(demographicKey=str_replace(demographicKey,choice,""))%>%
          group_by(demographicKey)%>%
          #summarise(avgValueBreak=mean(ValuePercentage,na.rm = T)*100)
     #     summarise(`Selected Comps`=mean(ValuePercentage,na.rm = T)*100) #T32 09.03.25 Soumen
           summarise(`Selected Comps`=mean(as.numeric(ValuePercentage),na.rm = T)*100)#T32 09.03.25 Soumen   
      
        d<-data2%>%select(demographicKey,ValuePercentage)%>%
          mutate(demographicKey=str_replace(demographicKey,choice,""))%>%
          group_by(demographicKey)%>%
          #summarise(avgValue=round(mean(ValuePercentage,na.rm = T)*100,0))
          #summarise(`All Comps`=round(mean(ValuePercentage,na.rm = T)*100,0))  #T32 09.03.25 Soumen
          summarise(`All Comps`=round(mean(as.numeric(ValuePercentage),na.rm = T)*100,0))  #T32 09.03.25 Soumen
        
        
        e<-merge(c, d, by = "demographicKey")
        e
        
        e<-e%>%gather(key,value,`Selected Comps`:`All Comps`)
        
        print("Combined data")
        print(e)
        if(input$select_metric=="Age Breakdown")
        {
          e$demographicKey <- factor(e$demographicKey,levels = c("10<", "10-12", "13-17",
                                                                 "18-24", "25-34", "35-44",
                                                                 "45-54", "55+"))
          
        }
        else if(input$select_metric=="Age Split")
        {
          e$demographicKey <- factor(e$demographicKey,levels = c("25<", "25+"))
          
        }
        
        # f<-ggplot(data=e,aes(x=demographicKey))+
        #   geom_bar(aes(y=avgValue),stat = "identity",alpha=.2,fill='green',color='green')+
        #   geom_bar(aes(y=avgValueBreak),stat = "identity",alpha=.6,fill='lightblue',color='lightblue4',width=0.4)+
        #   geom_text(aes(label=paste0(round(avgValue,1),"%"),y=avgValue), position=position_dodge(width=0.9), size=5,vjust=-0.2,hjust=-1.3)+
        #   geom_text(aes(label=paste0(round(avgValueBreak,1),"%"),y=avgValueBreak), position=position_dodge(width=0.5),size=5,vjust=-0.5)+
        #   theme(axis.title.y=element_blank(),
        #         axis.text.y=element_blank(),
        #         axis.ticks.y=element_blank())+
        #   labs(title = paste0("% Distribution of viewership wrt ",input$select_metric))+
        #       xlab(input$select_metric)+
        #       ylab("Avg Viewership (%)")+
        #   theme_classic()+
        #   theme(axis.text=element_text(size=13),
        #         axis.title=element_text(size=15),
        #         plot.title = element_text(size = 16))
        
        f<-ggplot(data=e,aes(x=demographicKey,y=value,fill = factor(key)))+
          geom_bar(position = "dodge",stat = "identity",alpha=0.6,color="gray")+
          geom_text(aes(label=paste0(round(value,1),"%"),y=value),position=position_dodge(width=1),vjust=0,hjust=0.5,size=5)+
          scale_fill_manual("", values = c("Selected Comps" = "green", "All Comps" = "lightblue"))+
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())+
          labs(title = paste0("% Distribution of viewership wrt ",input$select_metric))+
          xlab(input$select_metric)+
          ylab("Avg Viewership (%)")+
          theme_classic()+
          theme(axis.text=element_text(size=13),
                axis.title=element_text(size=15),
                legend.text=element_text(size=13),
                plot.title = element_text(size = 16),
                legend.position="bottom")
        
        
        
        
        
        
        # d<- data1%>%
        #    filter(str_detect(demographicKey,input$select_metric))%>%
        #    group_by(demographicKey)%>%
        #    summarise(avgValue=mean(ValuePercentage,na.rm = T)*100)%>%
        #    arrange(desc(avgValue))%>%
        #   ggplot(aes(x=reorder(demographicKey,-avgValue),y=avgValue))+
        #    geom_bar(stat = "identity", fill = "#0064b0")+
        #    geom_text(aes(label=paste0(round(avgValue,1),"%")), position=position_dodge(width=0.9), vjust=-0.25)+
        #    theme_bw()+
        #    theme(panel.border = element_blank(),
        #        axis.line = element_line(color="black"),
        #        plot.margin=unit(c(2,0,0,0),"cm"),axis.text.x = element_text(angle = 45))+
        #    labs(title = paste0("% Distribution of viewership wrt ",input$select_metric))+
        #    xlab(input$select_metric)+
        #    ylab("Percentage viewership")
        
      }
      f
      #ggplotly(f)
    })
  })
  
  observe({
    output$plot_demo2 <- renderPlot({
      if(nrow(rv_currentComps$currentComps)==0){
        u<-ggplot()
        v<-ggplot()
      }else{
        #using selected comps data
        
        data1<-rv_currentComps$currentComps%>%gather(demographicKey,ValuePercentage,Patrons_General:`Age Brkdwn_55+`)%>%select(demographicKey,ValuePercentage)
        data2<-mmsNew%>%filter(Mcode %in% rv_currentComps$currentComps$Mcode== FALSE)%>%select(demographicKey,ValuePercentage)
        data3<-data1%>%
          filter((str_detect(demographicKey,"s_F")|
                    str_detect(demographicKey,"s_M")) &
                   (!str_detect(demographicKey,"Parents") & !str_detect(demographicKey,"Gend")))
        print("Demo data selected")
        # print(head(data3))
        
        data4<-data2%>%
          filter((str_detect(demographicKey,"s_F")|
                    str_detect(demographicKey,"s_M")) &
                   (!str_detect(demographicKey,"Parents") & !str_detect(demographicKey,"Gend")))
        print("Demo data MMS")
        # print(head(data4))
        data4
        
        y<-data3%>%select(demographicKey,ValuePercentage)%>%
          group_by(demographicKey)%>%
          #summarise(avgValueBreak=round(mean(ValuePercentage,na.rm = T)*100,0))%>% #T32 09.03.25 Soumen
          summarise(avgValueBreak=round(mean(as.numeric(ValuePercentage),na.rm = T)*100,0))%>%  #T32 09.03.25 Soumen
          mutate(demographicKey=str_replace(demographicKey,"Quads_",""))%>%
          mutate(demographicKey=str_replace(demographicKey,"Child",""))%>%
          mutate(demographicKey=str_replace(demographicKey,"Adult",""))%>%
          mutate(gender=ifelse(str_detect(demographicKey,"F"),"Female","Male"))%>%
          mutate(demographicKey=str_replace(demographicKey,"F",""))%>%
          mutate(demographicKey=str_replace(demographicKey,"M",""))%>%
          mutate(demographicKey=str_trim(demographicKey,side = "both"))%>%
          arrange(gender)
        
        z<-data4%>%select(demographicKey,ValuePercentage)%>%
          group_by(demographicKey)%>%
          #summarise(avgValue=round(mean(ValuePercentage,na.rm = T)*100,0))%>%  #T32 09.03.25 Soumen
          summarise(avgValue=round(mean(as.numeric(ValuePercentage),na.rm = T)*100,0))%>%  #T32 09.03.25 Soumen
          mutate(demographicKey=str_replace(demographicKey,"Quads_",""))%>%
          mutate(demographicKey=str_replace(demographicKey,"Child",""))%>%
          mutate(demographicKey=str_replace(demographicKey,"Adult",""))%>%
          mutate(gender=ifelse(str_detect(demographicKey,"F"),"Female","Male"))%>%
          mutate(demographicKey=str_replace(demographicKey,"F",""))%>%
          mutate(demographicKey=str_replace(demographicKey,"M",""))%>%
          mutate(demographicKey=str_trim(demographicKey,side = "both"))%>%
          arrange(gender)
        
        w<-merge(y,z,by=c("demographicKey","gender"))
        w
        
        m<-w%>%filter(gender=="Male")
        f<-w%>%filter(gender=="Female")
        m$demographicKey<-factor(m$demographicKey,levels = c("10<","10+","25<","25+"))
        f$demographicKey<-factor(f$demographicKey,levels = c("10<","10+","25<","25+"))
        
        u<- ggplot(data=m,aes(x=demographicKey))+
          #geom_bar(aes(y=avgValue),stat = "identity",alpha=.2,fill='green',color='green')+                        #Commented Soumen 20th April 2023                      
          #geom_bar(aes(y=avgValue,fill ='Male Selected Comps'),stat = "identity",alpha=.2,color='green')+               #Added Soumen 20th April 2023                      
          geom_bar(aes(y=avgValue,fill ='Male All Comps'),stat = "identity",alpha=.2,color='green')+               #Edited Soumen 30.05.25 T42                      
          #geom_bar(aes(y=avgValueBreak),stat = "identity",alpha=.6,fill='lightblue',color='lightblue4',width=0.4)+ #Commented Soumen 20th April 2023
          #geom_bar(aes(y=avgValueBreak, fill='Male All Comps'),stat = "identity",alpha=.6,color='lightblue4',width=0.4)+  #Added Soumen 20th April 2023                      
          geom_bar(aes(y=avgValueBreak, fill='Male Selected Comps'),stat = "identity",alpha=.6,color='lightblue4',width=0.4)+  #Edited Soumen 30.05.25 T42
          geom_text(aes(label=paste0(round(avgValue,1),"%"),y=avgValue), position=position_dodge(width=0.9), size=5,vjust=-2,fontface = "bold")+
          geom_text(aes(label=paste0(round(avgValueBreak,1),"%"),y=avgValueBreak), position=position_dodge(width=0.5), size=5,hjust=0)+
          ggtitle("Male") +
          ylab("Avg Viewership (%)")+xlab("Age group")+
          theme(axis.text=element_text(size=13,face="bold"),
                axis.title=element_text(size=15),
                #plot.title = element_text(size = 16,hjust = 0.5))+     #Commented Soumen 20th April 2023
                plot.title = element_text(size = 16,hjust = 0.5),       #Added Soumen 20th April 2023
                legend.position = "bottom")+                            #Added Soumen 20th April 2023
          coord_flip()+
          #scale_y_reverse()      #Commented Soumen 20th April 2023
          scale_y_reverse()+      #Added Soumen 20th April 2023
          #scale_fill_manual(name = "", values = c('Male Selected Comps' = "lightgreen", 'Male All Comps' = "lightblue"))#Added Soumen 20th April  2023
          scale_fill_manual(name = "", values = c('Male All Comps' = "lightgreen", 'Male Selected Comps' = "lightblue"))#Edited Soumen 30.05.25 T42
          
        v<-ggplot(data=f,aes(x=demographicKey))+
          #geom_bar(aes(y=avgValue),stat = "identity",alpha=.2,fill='green',color='green')+              #Commented Soumen 20th April 2023
          #geom_bar(aes(y=avgValue,fill='Female Selected Comps'),stat = "identity",alpha=.2,color='green')+    #Added Soumen 20th April 2023                        
          geom_bar(aes(y=avgValue,fill='Female All Comps'),stat = "identity",alpha=.2,color='green')+    #Edited Soumen 30.05.25 T42                        
          #geom_bar(aes(y=avgValueBreak),stat = "identity",alpha=.6,fill='lightpink',color='lightpink4',width=0.4)+           #Commented Soumen 20th April 2023
          #geom_bar(aes(y=avgValueBreak,fill='Female All Comps'),stat = "identity",alpha=.6,color='lightpink4',width=0.4)+ #Added Soumen 20th April 2023
          geom_bar(aes(y=avgValueBreak,fill='Female Selected Comps'),stat = "identity",alpha=.6,color='lightpink4',width=0.4)+ #Edited Soumen 30.05.25 T42
          geom_text(aes(label=paste0(round(avgValue,1),"%"),y=avgValue), position=position_dodge(width=0.9), size=5,vjust=-2,fontface = "bold")+
          geom_text(aes(label=paste0(round(avgValueBreak,1),"%"),y=avgValueBreak), position=position_dodge(width=0.5),size=5,hjust=0)+
          ggtitle("Female") +
          ylab("Avg Viewership (%)")+
          coord_flip()+
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.text.x=element_text(size=13,face="bold"),
                axis.title=element_text(size=15),
                #plot.title = element_text(size = 16,hjust = 0.5))      #Commented Soumen 20th April 2023
                plot.title = element_text(size = 16,hjust = 0.5),        #Added Soumen 20th April 2023
                legend.position = "bottom")+      #Added Soumen 20th April 2023
          #scale_fill_manual(name = "", values = c('Female Selected Comps' = "lightgreen", 'Female All Comps' = "lightpink"))#Added Soumen 20th April 2023
          scale_fill_manual(name = "", values = c('Female All Comps' = "lightgreen", 'Female Selected Comps' = "lightpink"))#Edited Soumen 30.05.25 T42
          
        
        ######################################################################################
        
        
        
        
        
        
        # f<- data1%>%select(demographicKey,ValuePercentage)%>%
        #   filter(str_detect(demographicKey,"s_F")|str_detect(demographicKey,"s_M") & (!str_detect(demographicKey,"Parents") & !str_detect(demographicKey,"Gend")))%>%
        #   group_by(demographicKey)%>%
        #   summarise(avgValue=mean(ValuePercentage,na.rm = T)*100)%>%
        #   arrange(desc(avgValue))%>%
        #   mutate(gender=ifelse(str_detect(demographicKey,"_F"),"Female","Male")) %>%
        #   ggplot(aes(x=reorder(demographicKey,-avgValue),y=avgValue,fill=gender))+
        #   geom_bar(stat = "identity",position = position_dodge(width = 0.5))+
        #   geom_text(aes(label=paste0(round(avgValue,1),"%")), position=position_dodge(width=0.9), vjust=-0.25)+
        #   theme_bw()+
        #   theme(panel.border = element_blank(),
        #         axis.line = element_line(color="black"),
        #         plot.margin=unit(c(2,0,0,0),"cm"),axis.text.x = element_text(angle = 45))+
        #   labs(title = paste0("% Distribution of viewership of Gender across diff age groups",input$select_metric))+
        #   xlab("Gender/Age Group")+
        #   ylab("Percentage viewership")
        
        
      }
      # ply1 <- ggplotly(u)
      # ply2 <- ggplotly(v)
      
      #subplot(ply1, ply2, nrows=1)
      title1=textGrob("% Viewership by Gender and Age Group", gp=gpar(fontsize=15))
      #legend_grob <- cowplot::get_legend(v)
      #title2=textGrob("Green: All Comps, Light Blue/Pink: Selected Comps", gp=gpar(fontsize=15))  #Added Soumen 20.04.23
      gridExtra::grid.arrange(u, v, ncol=2,top=title1)                                           #Commented Soumen 20.04.23
      #gridExtra::grid.arrange(u, v, legend_grob, ncol=2,top=title1)                             #Added Soumen 20.04.23
      #ggplotly(x)
    })
  })
  
  
  
  #Scatterplot:
  observe(
    output$plot_comps2 <- renderPlotly({
      
      if(nrow(rv_currentComps$currentComps)==0){
        p<-ggplot()
      }else{
        #using selected comps data
        dta <- rv_currentComps$currentComps
        names(dta)[names(dta) == 'Tomato Meter'] <- 'Tomato Meter (%)'
        
        
        
        var1 <- sym(input$select_scatter1)
        var2 <- sym(input$select_scatter2)
        var3 <- sym("Title")
        
        # print("print plot")
        # print(input$select_scatter1)
        # print(var1)
        # percenatgeCal <- function(x, na.rm=FALSE) (x*100)
        # 
        # dta <- mutate_all(dta[,7:14],percenatgeCal)
        # # print(head(dta))
        
        
        p <- ggplot(data = dta, aes(x = (!!var1), y = (!!var2), text=(!!var3)))
        p <- p + geom_point(color = "#0064b0")
        p <- p + theme_bw()
        p <- p + xlab(input$select_scatter1)
        p <- p + ylab(input$select_scatter2)
        p <- p + theme(panel.border = element_blank(),
                       axis.line = element_line(color="black"),
                       #axis.title.x=element_blank(),
                       #axis.title.y=element_blank(),
                       axis.text.x =element_text(angle = 15),
                       axis.text.y =element_text(angle = 15))
        #p <- p + labs(title = paste0("\nDistribution of ",input$select_vis,"\n\n"))
        if(input$select_scatter1=="Release Month"){            ## Condition will ensure quantization of x axis only in the said condition. T16 --Soumen
          month_labels<-substr(month.name[seq(1,12,by=1)],1,3) ########## Added new line for declare the month name --Soumen, 18th April,23
          p  <- p + scale_x_continuous(breaks=seq(1,12,by=1),labels=month_labels)#Soumen, 18th April,23
        }  # end T16
        if(input$select_scatter2=="Release Month"){             ## Condition will ensure quantization of y axis only in the said condition. T17 --Soumen
          month_labels<-substr(month.name[seq(1,12,by=1)],1,3) ########## Added new line for declare the month name --Soumen, 24th Nov,23
          p  <- p + scale_y_continuous(breaks=seq(1,12,by=1),labels=month_labels)#Soumen, 24th Nov,23
        }   # end T17
      }
      ggplotly(p)
    })
  )
  
  #Update universe for filters/columns selected-----
  
  #Function for putting action buttons within a cell of table
  # each button has a unique ID that we can
  buttonInput_select <- function(FUN, len, id, label,icon="plus",background = "green",color="white", ...) {
    paste0("<button style = background:",background,";color:",color," id = ", id, ' type = button class = btn btn-default action-button onclick = "Shiny.onInputChange(',"'select_button',",' this.id)"','><i class="fa fa-',icon,'"></i>',label,"</button>")
  }
  buttonInput_smart <- function(FUN, len, id, label,icon="plus",background = "green",color="white", ...) {
    paste0("<button style = background:",background,";color:",color," id = ", id, ' type = button class = btn btn-default action-button onclick = "Shiny.onInputChange(',"'smart_button',",' this.id)"','><i class="fa fa-',icon,'"></i>',label,"</button>")
    #paste0("<button style = background:",background,";color:",color," id = ", paste0(id,1:len), ' type = button class = btn btn-default action-button onclick = "Shiny.onInputChange("smart_button", this.id)"><i class="fa fa-',icon,'"></i>',label,"</button>")
  }
  buttonInput_delete <- function(FUN, len, id, label,icon="plus",background = "green",color="white", ...) {
    paste0("<button style = background:",background,";color:",color," id = ", id, ' type = button class = btn btn-default action-button onclick = "Shiny.onInputChange(',"'delete_button',",' this.id)"','><i class="fa fa-',icon,'"></i>',label,"</button>")
    #paste0("<button style = background:",background,";color:",color," id = ", paste0(id,1:len), ' type = button class = btn btn-default action-button onclick = "Shiny.onInputChange("delete_button", this.id)"><i class="fa fa-',icon,'"></i>',label,"</button>")
  }
  buttonInput_thumbdown <- function(FUN, len, id, label,icon="plus",background = "green",color="white", ...) {
    paste0("<button style = background:",background,";color:",color," id = ", id, ' type = button class = btn btn-default action-button onclick = "Shiny.onInputChange(',"'thumbdown_button',",' this.id)"','><i class="fa fa-',icon,'"></i>',label,"</button>")
    #paste0("<button style = background:",background,";color:",color," id = ", paste0(id,1:len), ' type = button class = btn btn-default action-button onclick = "Shiny.onInputChange("delete_button", this.id)"><i class="fa fa-',icon,'"></i>',label,"</button>")
  }
  
  buttonInput_undo <- function(FUN, len, id, label,icon="plus",background = "green",color="white", ...) {
    paste0("<button style = background:",background,";color:",color," id = ", id, ' type = button class = btn btn-default action-button onclick = "Shiny.onInputChange(',"'undo_button',",' this.id)"','><i class="fa fa-',icon,'"></i>',label,"</button>")
    #paste0("<button style = background:",background,";color:",color," id = ", paste0(id,1:len), ' type = button class = btn btn-default action-button onclick = "Shiny.onInputChange("delete_button", this.id)"><i class="fa fa-',icon,'"></i>',label,"</button>")
  }
  
  checkboxColumn <- function(len, id, chk){ #should the label argument be "condition" checking for whether to be checked or not?
    chk <- ifelse(chk == TRUE, 'checked = "checked"', "")
    paste0('<input id="',paste0(id,1:len),'" type="checkbox"',chk,'onclick = "Shiny.onInputChange(',"'final_chk',",' this.id)">')
  }
  
  
  #QUERY - COMPLETE - WHEN USER THUMBS-DOWN A COMP, WRITE IT TO THE BAD-COMP TABLE-----
  observeEvent(ignoreInit=TRUE,input$thumbdown_button,{
    
    #js$pageCol("test")
    print(input$thumbdown_button)
    print(input$scenario_release)
    
    print(length(input$scenario_release)==0)
    bad_mcode <- as.numeric(strsplit(input$thumbdown_button, "_")[[1]][2])
    print("scenario ID insert")
    print(rv_currentScenario$full_detail$scen_id)
    
    # print(paste0("INSERT INTO APP_BAD_COMPS(APP_BAD_COMPS_KEY,SCEN_NAME, SCEN_PROD_BUDGET, SCEN_MPAA_RATING, SCEN_DIRECTOR, SCEN_CAST, SCEN_VOICE, SCEN_PRODUCER, SCEN_EXEC_PRODUCER, SCEN_PROD_COMPANY, SCEN_EDITOR, SCEN_WRITER, SCEN_RELEASE_DATE, SCEN_CINEMA_SCORE, SCEN_MINUTES, SCEN_TOMATO_METER, SCEN_DBO, SCEN_IBO, SCEN_WBO, SCEN_WWBO, SCEN_PRI_GENRE, SCEN_SEC_GENRE, SCEN_OTHER_GENRE, SCEN_KEYWORD_1, SCEN_KEYWORD_2, SCEN_KEYWORD_3)",
    #        "SELECT '",bad_mcode,"','",rv_currentScenario$name,"','",input$scenario_prodbudget,"','",input$scenario_mpaa,"','",
    #        input$scenario_director,"','",input$scenario_cast,"','",input$scenario_voice,"','",
    #        input$scenario_producer,"','",input$scenario_execproducer,"','",input$scenario_studio,"','",
    #        input$scenario_editor,"','",input$scenario_writer,"','",input$scenario_release,"','",
    #        input$scenario_cinemascore,"','",paste(input$scenario_minutes, collapse = "-"),"','",paste(input$scenario_tomatometer, collapse = "-"),"','",
    #        paste(input$scenario_domesticbo, collapse = "-"),"','",paste(input$scenario_intlbo, collapse = "-"),"','",
    #        paste(input$scenario_weekendbo, collapse = "-"),"','",paste(input$scenario_wwbo, collapse = "-"),"','",
    #        input$scenario_genre1,"','",input$scenario_genre2,"','",input$scenario_genre3,"','",
    #        input$scenario_keyword1,"','",input$scenario_keyword2,"','",input$scenario_keyword3,"';"))
    if(bad_mcode %in% rv_bdCmp$bad$BAD_COMPS_MCODE ==FALSE)
    {
      print("in first if")
      newBad<-NULL
      scenarioDetailsFunc(input$scenario_name,rv_projectscenarios$project_title)
      print(paste0("SCENE ID INSERT: ",rv_SelectedDetails$Scen$APP_SCENARIO_KEY," ",rv_SelectedDetails$Scen$Name))
      
      projectDetailsFunc(rv_projectscenarios$project_title)
      print(paste0("PROJ ID INSERT: ",rv_SelectedDetails$Proj$APP_PROJ_KEY))
      projId<-rv_SelectedDetails$Proj$APP_PROJ_KEY
      
      #query_write_bad <- paste0('INSERT INTO "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS"',"(BAD_COMPS_MCODE,SCEN_NAME, SCEN_PROD_BUDGET, SCEN_MPAA_RATING, SCEN_DIRECTOR, SCEN_CAST, SCEN_VOICE, SCEN_PRODUCER, SCEN_EXEC_PRODUCER, SCEN_PROD_COMPANY, SCEN_EDITOR, SCEN_WRITER, SCEN_RELEASE_DATE, SCEN_CINEMA_SCORE, SCEN_MINUTES, SCEN_TOMATO_METER, SCEN_DBO, SCEN_IBO, SCEN_WBO, SCEN_WWBO, SCEN_PRI_GENRE, SCEN_SEC_GENRE, SCEN_OTHER_GENRE, SCEN_KEYWORD_1, SCEN_KEYWORD_2, SCEN_KEYWORD_3,BAD_COMPS_CREATED_BY,PROJ_TITLE,SCEN_ID,PROJ_ID)",   #T20 Soumen 23.07.24  
      query_write_bad <- paste0('INSERT INTO ',database,'."ODS"."APP_BAD_COMPS"',"(BAD_COMPS_MCODE,SCEN_NAME, SCEN_PROD_BUDGET, SCEN_MPAA_RATING, SCEN_DIRECTOR, SCEN_CAST, SCEN_VOICE, SCEN_PRODUCER, SCEN_EXEC_PRODUCER, SCEN_PROD_COMPANY, SCEN_EDITOR, SCEN_WRITER, SCEN_RELEASE_DATE, SCEN_CINEMA_SCORE, SCEN_MINUTES, SCEN_TOMATO_METER, SCEN_DBO, SCEN_IBO, SCEN_WBO, SCEN_WWBO, SCEN_PRI_GENRE, SCEN_SEC_GENRE, SCEN_OTHER_GENRE, SCEN_KEYWORD_1, SCEN_KEYWORD_2, SCEN_KEYWORD_3,BAD_COMPS_CREATED_BY,PROJ_TITLE,SCEN_ID,PROJ_ID)",   
                                " SELECT '",bad_mcode,"',$$",
                                input$scenario_name,"$$,",
                                ifelse(input$scenario_prodbudget=="","NULL,'",paste0("'",input$scenario_prodbudget,"','")),
                                input$scenario_mpaa,"',$$",
                                paste(input$scenario_director, collapse = ", "),"$$,$$",
                                paste(input$scenario_cast, collapse = ", "),"$$,$$",
                                paste(input$scenario_voice, collapse = ", "),"$$,$$",
                                paste(input$scenario_producer, collapse = ", "),"$$,$$",
                                paste(input$scenario_execproducer, collapse = ", "),"$$,$$",
                                input$scenario_studio,"$$,$$",
                                input$scenario_editor,"$$,$$",
                                paste(input$scenario_writer, collapse = ", "),"$$,",
                                ifelse(length(input$scenario_release)==0,"NULL,'",paste0("'",input$scenario_release,"','")),
                                input$scenario_cinemascore,"','",
                                paste(input$scenario_minutes, collapse = "-"),"','",
                                paste(input$scenario_tomatometer, collapse = "-"),"','",
                                paste(input$scenario_domesticbo, collapse = "-"),"','",
                                paste(input$scenario_intlbo, collapse = "-"),"','",
                                paste(input$scenario_weekendbo, collapse = "-"),"','",
                                paste(input$scenario_wwbo, collapse = "-"),"',$$",
                                input$scenario_genre1,"$$,$$",
                                input$scenario_genre2,"$$,$$",
                                input$scenario_genre3,"$$,$$",
                                input$scenario_keyword1,"$$,$$",
                                input$scenario_keyword2,"$$,$$",
                                input$scenario_keyword3,"$$,'",
                                session$user,"',$$",
                                rv_projectscenarios$project_title,"$$,",
                                rv_SelectedDetails$Scen$APP_SCENARIO_KEY,",",
                                projId,";" )
      
      
      
      # if(is.na(rv_currentScenario$full_detail$scen_id))
      # {
      #   print("in bad comps scen if")
      #   # print(input$scenario_name)
      #   # scenDetail<- dbGetQuery(my_db,paste0("SELECT APP_SCENARIO_KEY FROM  THEATRICAL_LIGHTHOUSE.ODS.APP_SCENARIO WHERE SCEN_NAME='",input$scenario_name,"' AND PROJ_TITLE='",rv_projectscenarios$project_title,"';"))
      #   # print(paste0("SELECT APP_SCENARIO_KEY FROM  THEATRICAL_LIGHTHOUSE.ODS.APP_SCENARIO WHERE SCEN_NAME='",input$scenario_name,"' AND PROJ_TITLE='",rv_projectscenarios$project_title,"';"))
      #   # print(scenDetail)
      #   
      #   query_write_bad <- paste0('INSERT INTO "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS"',"(BAD_COMPS_MCODE,SCEN_NAME, SCEN_PROD_BUDGET, SCEN_MPAA_RATING, SCEN_DIRECTOR, SCEN_CAST, SCEN_VOICE, SCEN_PRODUCER, SCEN_EXEC_PRODUCER, SCEN_PROD_COMPANY, SCEN_EDITOR, SCEN_WRITER, SCEN_RELEASE_DATE, SCEN_CINEMA_SCORE, SCEN_MINUTES, SCEN_TOMATO_METER, SCEN_DBO, SCEN_IBO, SCEN_WBO, SCEN_WWBO, SCEN_PRI_GENRE, SCEN_SEC_GENRE, SCEN_OTHER_GENRE, SCEN_KEYWORD_1, SCEN_KEYWORD_2, SCEN_KEYWORD_3,BAD_COMPS_CREATED_BY,PROJ_TITLE,SCEN_ID,PROJ_ID)",
      #                             " SELECT '",bad_mcode,"',$$",
      #                             input$scenario_name,"$$,",
      #                             ifelse(input$scenario_prodbudget=="","NULL,'",paste0("'",input$scenario_prodbudget,"','")),
      #                             input$scenario_mpaa,"',$$",
      #                             paste(input$scenario_director, collapse = ", "),"$$,$$",
      #                             paste(input$scenario_cast, collapse = ", "),"$$,$$",
      #                             paste(input$scenario_voice, collapse = ", "),"$$,$$",
      #                             paste(input$scenario_producer, collapse = ", "),"$$,$$",
      #                             paste(input$scenario_execproducer, collapse = ", "),"$$,$$",
      #                             input$scenario_studio,"$$,$$",
      #                             input$scenario_editor,"$$,$$",
      #                             paste(input$scenario_writer, collapse = ", "),"$$,",
      #                             ifelse(length(input$scenario_release)==0,"NULL,'",paste0("'",input$scenario_release,"','")),
      #                             input$scenario_cinemascore,"','",
      #                             paste(input$scenario_minutes, collapse = "-"),"','",
      #                             paste(input$scenario_tomatometer, collapse = "-"),"','",
      #                             paste(input$scenario_domesticbo, collapse = "-"),"','",
      #                             paste(input$scenario_intlbo, collapse = "-"),"','",
      #                             paste(input$scenario_weekendbo, collapse = "-"),"','",
      #                             paste(input$scenario_wwbo, collapse = "-"),"',$$",
      #                             input$scenario_genre1,"$$,$$",
      #                             input$scenario_genre2,"$$,$$",
      #                             input$scenario_genre3,"$$,$$",
      #                             input$scenario_keyword1,"$$,$$",
      #                             input$scenario_keyword2,"$$,$$",
      #                             input$scenario_keyword3,"$$,'",
      #                             session$user,"','",
      #                             rv_projectscenarios$project_title,"',",
      #                             rv_SelectedDetails$Scen$APP_SCENARIO_KEY,",",
      #                             rv_projectscenarios$proj_id,";" )
      #   
      #   newBad<-data.frame(BAD_COMPS_MCODE=bad_mcode,SCEN_NAME= rv_currentScenario$name,SCEN_ID=scenDetail$APP_SCENARIO_KEY,stringsAsFactors=FALSE)
      #   
      #   
      # }
      # else{
      #   print("in bad comps scen else")
      #   
      #   query_write_bad <- paste0('INSERT INTO "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS"',"(BAD_COMPS_MCODE,SCEN_NAME, SCEN_PROD_BUDGET, SCEN_MPAA_RATING, SCEN_DIRECTOR, SCEN_CAST, SCEN_VOICE, SCEN_PRODUCER, SCEN_EXEC_PRODUCER, SCEN_PROD_COMPANY, SCEN_EDITOR, SCEN_WRITER, SCEN_RELEASE_DATE, SCEN_CINEMA_SCORE, SCEN_MINUTES, SCEN_TOMATO_METER, SCEN_DBO, SCEN_IBO, SCEN_WBO, SCEN_WWBO, SCEN_PRI_GENRE, SCEN_SEC_GENRE, SCEN_OTHER_GENRE, SCEN_KEYWORD_1, SCEN_KEYWORD_2, SCEN_KEYWORD_3,BAD_COMPS_CREATED_BY,PROJ_TITLE,SCEN_ID,PROJ_ID)",
      #                             " SELECT '",bad_mcode,"',$$",
      #                             input$scenario_name,"$$,",
      #                             ifelse(input$scenario_prodbudget=="","NULL,'",paste0("'",input$scenario_prodbudget,"','")),
      #                             input$scenario_mpaa,"',$$",
      #                             paste(input$scenario_director, collapse = ", "),"$$,$$",
      #                             paste(input$scenario_cast, collapse = ", "),"$$,$$",
      #                             paste(input$scenario_voice, collapse = ", "),"$$,$$",
      #                             paste(input$scenario_producer, collapse = ", "),"$$,$$",
      #                             paste(input$scenario_execproducer, collapse = ", "),"$$,$$",
      #                             input$scenario_studio,"$$,$$",
      #                             input$scenario_editor,"$$,$$",
      #                             paste(input$scenario_writer, collapse = ", "),"$$,",
      #                             ifelse(length(input$scenario_release)==0,"NULL,'",paste0("'",input$scenario_release,"','")),
      #                             input$scenario_cinemascore,"','",
      #                             paste(input$scenario_minutes, collapse = "-"),"','",
      #                             paste(input$scenario_tomatometer, collapse = "-"),"','",
      #                             paste(input$scenario_domesticbo, collapse = "-"),"','",
      #                             paste(input$scenario_intlbo, collapse = "-"),"','",
      #                             paste(input$scenario_weekendbo, collapse = "-"),"','",
      #                             paste(input$scenario_wwbo, collapse = "-"),"',$$",
      #                             input$scenario_genre1,"$$,$$",
      #                             input$scenario_genre2,"$$,$$",
      #                             input$scenario_genre3,"$$,$$",
      #                             input$scenario_keyword1,"$$,$$",
      #                             input$scenario_keyword2,"$$,$$",
      #                             input$scenario_keyword3,"$$,'",
      #                             session$user,"','",
      #                             rv_projectscenarios$project_title,"',",
      #                             rv_SelectedDetails$Scen$APP_SCENARIO_KEY,",",
      #                             rv_projectscenarios$proj_id,";" )
      #   newBad<-data.frame(BAD_COMPS_MCODE=bad_mcode,SCEN_NAME= rv_currentScenario$name,SCEN_ID=rv_currentScenario$full_detail$scen_id,stringsAsFactors=FALSE)
      #   
      # }
      
      print("bad comp new")
      print(query_write_bad)
      
      #SQL Query:
      dbGetQuery(my_db,
                 query_write_bad
      )
      
      #Highlighting bad comps: Akshay
      print("rbind")
      newBad<-data.frame(BAD_COMPS_MCODE=bad_mcode,SCEN_NAME= rv_currentScenario$name,SCEN_ID=rv_SelectedDetails$Scen$APP_SCENARIO_KEY,stringsAsFactors=FALSE)
      print(newBad)
      rv_bdCmp$bad<-rv_bdCmp$bad%>%rbind(newBad,stringsAsFactors=FALSE)
      print(rv_bdCmp$bad)
      
    }
    #js$pageCol()
    #print(rv_bdCmp()%>%rbind(newBad,stringsAsFactors=FALSE))
    #print(rv_bdCmp())
    
    #showNotification(ui=paste0(bad_mcode," bad comp information saved"),duration = 1,session = session)
    
  })
  
  #Need to save out as a new field in the scenario table
  observeEvent(ignoreInit=TRUE,input$final_chk, {
    chkrow <- as.numeric(strsplit(input$final_chk, "_")[[1]][2])
    print(chkrow)
    print(input$final_chk)
    if(chkrow %in% rv_currentComps$finalComps){
      rv_currentComps$finalComps <- setdiff(rv_currentComps$finalComps,chkrow)
    }else{
      rv_currentComps$finalComps <- c(rv_currentComps$finalComps,chkrow)
    }
    #print(rv_currentComps$currentComps)
  })
  
  #FILTERS (all separate so only the one that is updated actually changes):-----
  flt_film <- reactive({
    if(is.null(input$filter_film)||input$filter_film==""){
      all_mcodes
    }else{
      mms %>% 
        filter(Title %in% input$filter_film) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_release <- reactive({
    mms %>% 
      filter((input$filter_release[1] <= `Release Date` & `Release Date` <= input$filter_release[2])) %>% 
      select(Mcode) %>% 
      pull()
  })
  flt_genre <- reactive({
    if(is.null(input$filter_genre)||input$filter_genre == ""){
      all_mcodes
    }else{
      mms %>% 
        filter((`Primary Genre` %in% input$filter_genre | `Secondary Genre` %in% input$filter_genre | `Other Genre` %in% input$filter_genre)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_mpaa <- reactive({
    if(is.null(input$filter_mpaa)||input$filter_mpaa == ""){
      all_mcodes
    }else{
      mms %>% 
        filter(`MPAA Rating` %in% input$filter_mpaa) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_director <- reactive({
    if(is.null(input$filter_director)||input$filter_director == ""){
      all_mcodes
    }else{
      ptrn <- paste(input$filter_director,collapse = "|")
      ptrn <- str_replace_all(ptrn, pattern = "\\(|\\)",replacement = "")
      mms %>% 
        mutate(Director = str_replace_all(Director, pattern = "\\(|\\)",replacement = "")) %>% 
        filter(str_detect(string = Director, pattern = ptrn)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_cast <- reactive({
    if(is.null(input$filter_cast)||input$filter_cast == ""){
      all_mcodes
    }else{
      ptrn <- paste(input$filter_cast,collapse = "|")
      ptrn <- str_replace_all(ptrn, pattern = "\\(|\\)",replacement = "")
      mms %>% 
        mutate(`Cast (Full)` = str_replace_all(`Cast (Full)`, pattern = "\\(|\\)",replacement = "")) %>% 
        filter(str_detect(string = `Cast (Full)`, pattern = ptrn)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_voice <- reactive({
    if(is.null(input$filter_voice)||input$filter_voice == ""){
      all_mcodes
    }else{
      ptrn <- paste(input$filter_voice,collapse = "|")
      ptrn <- str_replace_all(ptrn, pattern = "\\(|\\)",replacement = "")
      mms %>% 
        mutate(Voice = str_replace_all(Voice, pattern = "\\(|\\)",replacement = "")) %>% 
        filter(str_detect(string = Voice, pattern = ptrn)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_distributor <- reactive({
    if(is.null(input$filter_distributor)||input$filter_distributor == ""){
      all_mcodes
    }else{
      mms %>% 
        filter(str_detect(string = Distributor, pattern = paste(input$filter_distributor,collapse="|"))) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_producer <- reactive({
    if(is.null(input$filter_producer)||input$filter_producer == ""){
      all_mcodes
    }else{
      ptrn <- paste(input$filter_producer,collapse = "|")
      ptrn <- str_replace_all(ptrn, pattern = "\\(|\\)|\\\\",replacement = "")
      mms %>% 
        mutate(Producer = str_replace_all(Producer, "\\(|\\)|\\\\", replacement = "")) %>% 
        filter(str_detect(string = Producer, pattern = ptrn)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_execprod <- reactive({
    if(is.null(input$filter_execprod)||input$filter_execprod == ""){
      all_mcodes
    }else{
      ptrn <- paste(input$filter_execprod,collapse = "|")
      ptrn <- str_replace_all(ptrn, pattern = "\\(|\\)",replacement = "")
      mms %>% 
        mutate(`Executive Producer` = str_replace_all(`Executive Producer`, "\\(|\\)", replacement = "")) %>% 
        filter(str_detect(string = `Executive Producer`, pattern = ptrn)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_keyword <- reactive({
    if(is.null(input$filter_keyword)||input$filter_keyword == ""){
      all_mcodes
    }else{
      mms %>% 
        filter((`Keyword 1` %in% input$filter_keyword | `Keyword 2` %in% input$filter_keyword | `Keyword 3` %in% input$filter_keyword)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_studio <- reactive({
    if(is.null(input$filter_studio)||input$filter_studio == ""){
      all_mcodes
    }else{
      mms %>% 
        filter(Studio %in% input$filter_studio) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_editor <- reactive({
    if(is.null(input$filter_editor)||input$filter_editor == ""){
      all_mcodes
    }else{
      ptrn <- paste(input$filter_editor,collapse = "|")
      ptrn <- str_replace_all(ptrn, pattern = "\\(|\\)",replacement = "")
      mms %>% 
        mutate(Editor = str_replace(Editor, "\\(|\\)", "")) %>% 
        filter(str_detect(string = Editor, pattern = ptrn)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_writer <- reactive({
    if(is.null(input$filter_writer)||input$filter_writer == ""){
      all_mcodes
    }else{
      ptrn <- paste(input$filter_writer,collapse = "|")
      ptrn <- str_replace_all(ptrn, pattern = "\\(|\\)",replacement = "")
      mms %>% 
        mutate(Writer = str_replace(Writer, "\\(|\\)", "")) %>% 
        filter(str_detect(string = Writer, pattern = ptrn)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_cinemascore <- reactive({
    if(is.null(input$filter_cinemascore)||input$filter_cinemascore == ""){
      all_mcodes
    }else{
      mms %>% 
        filter(`Cinema Score` %in% input$filter_cinemascore) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_minutes <- reactive({
    if(input$filter_minutes[1]==0 & input$filter_minutes[2]==400){
      all_mcodes
    }else{
      mms %>% 
        filter((input$filter_minutes[1] <= Minutes & Minutes <= input$filter_minutes[2])) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_tomato <- reactive({
    if(input$filter_tomato[1]==0 & input$filter_tomato[2]==100){
      all_mcodes
    }else{
      mms %>% 
        filter((input$filter_tomato[1] <= `Tomato Meter` & `Tomato Meter` <= input$filter_tomato[2])) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_dombo <- reactive({
    if(input$filter_dombo[1]==0 & input$filter_dombo[2]==1000){
      all_mcodes
    }else{
      mms %>% 
        filter((input$filter_dombo[1] <= `DBO ($M)` & `DBO ($M)` <= input$filter_dombo[2])) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_intlbo <- reactive({
    if(input$filter_intlbo[1]==0 & input$filter_intlbo[2]==4000){
      all_mcodes
    }else{
      mms %>% 
        filter((input$filter_intlbo[1] <= `IBO ($M)` & `IBO ($M)` <= input$filter_intlbo[2])) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_weekendbo <- reactive({
    if(input$filter_weekendbo[1]==0 & input$filter_weekendbo[2]==1000){
      all_mcodes
    }else{
      mms %>% 
        filter((input$filter_weekendbo[1] <= `WBO ($M)` & `WBO ($M)` <= input$filter_weekendbo[2])) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  flt_wwbo <- reactive({
    if(input$filter_wwbo[1]==0 & input$filter_wwbo[2]==4000){
      all_mcodes
    }else{
      mms %>% 
        filter((input$filter_wwbo[1] <= `WWBO ($M)` & `WWBO ($M)` <= input$filter_wwbo[2])|is.na(`WWBO ($M)`)) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  #---------------------------------------------------------------------------------
  #print("Soumen-X")
  #res<- mms %>% 
  #  rownames_to_column("MCODE_MASTER")%>%                       
  #  select(-`Supervised Rank`,-`Smart Rank`) %>%                
  #  left_join(ranks(),
  #            by = c("MCODE_MASTER" = "Mcode")) %>%
  #  select(Mcode) %>% 
  #  pull()
  #print("Soumen-Y") 
  #---------------------------------------------------------------------------------
  flt_smartrank <- reactive({                   #Added Soumen 24.07.23 smart rank filter start T4
    #if(input$filter_smartrank[1]==1 & input$filter_smartrank[2]==10){
    if(input$filter_smartrank[1]==1 & input$filter_smartrank[2]==100){ #scale enhenced Soumen 21.12.23 T18
      all_mcodes
    }else{
      mms %>% 
        rownames_to_column("MCODE_MASTER")%>%                       
        select(-`Supervised Rank`,-`Smart Rank`) %>%                #mms does not hold real ranks
        left_join(ranks(),
                  by = c("MCODE_MASTER" = "Mcode")) %>%
        filter((input$filter_smartrank[1] <= `Smart Rank` & `Smart Rank` <= input$filter_smartrank[2])) %>% 
        select(Mcode) %>% 
        pull()
    }
  })                                      #Added Soumen 24.07.23 smart rank filter ends T4
  
  flt_supervrank <- reactive({                   #Added Soumen 24.07.23 smart rank filter start T4
    #if(input$filter_supervrank[1]==1 & input$filter_supervrank[2]==10){
    if(input$filter_supervrank[1]==1 & input$filter_supervrank[2]==100){ #scale enhenced Soumen 21.12.23 T18
      all_mcodes
    }else{
      mms %>% 
        rownames_to_column("MCODE_MASTER")%>%                       
        select(-`Supervised Rank`,-`Smart Rank`) %>%                #mms does not hold real ranks
        left_join(ranks(),
                  by = c("MCODE_MASTER" = "Mcode")) %>%
        filter((input$filter_supervrank[1] <= `Supervised Rank` & `Supervised Rank` <= input$filter_supervrank[2])) %>% 
        select(Mcode) %>% 
        pull()
    }
  })
  #End Filters -----
  # demoRanks<-reactive({
  #   if(nrow(rv_refTable$tbl) == 0){
  #     data.frame(Mcode = character(1),`Smart Rank`=NA, `Supervised Rank` = NA, check.names = FALSE)
  #   }
  #   else if((nrow(rv_refTable$tbl) == 1 || length(input$smart_button)==0){
  #     a <- data%>%mutate(sum = select(., Patrons_General:`Age Brkdwn_55+`) %>% apply(1, sum, na.rm=TRUE))%>%select(Mcode,sum)%>%arrange(desc(sum))
  #     print("Demooo fundtion")
  #     print(a)
  # })
  str(mms)
  typeof(mms)
  #typeof(dta_mms)
  
  demo_ranks<-reactive({
    print("in reactive demo")
    # print(rv_refTable$tbl)
    # print(nrow(rv_refTable$tbl))
    # print(length(input$smart_button))
    #print(rv_currentComps$currentComps)
    if(nrow(rv_refTable$tbl) == 0){
      print("in if")
      #data.frame(Mcode = character(1),`Demo Rank`=NA,check.names = FALSE)
      #b<-demo_ranksFunc(scenario = rv_currentComps$currentComps,comp_universe = mms)
      data.frame(Mcode = character(1),`Demographic Rank`=NA, check.names = FALSE)
    }
    else if(nrow(rv_refTable$tbl) == 1 || length(input$smart_button)==0){
      print("in elif")
      b<-demo_ranksFunc(scenario = rv_currentComps$currentComps,comp_universe = mms)
    }
    else if(length(input$smart_button)>0){
      smart_row <- as.numeric(unlist(str_split(input$smart_button,"_"))[2])
      print("Smart rank demo")
      print(rv_refTable$tbl %>% slice(smart_row))
      b<-demo_ranksFunc(scenario = rv_refTable$tbl %>% slice(smart_row),comp_universe = mms)
    }
    else{
      data.frame(Mcode = character(1),`Demo Rank`=NA,check.names = FALSE)
    }
    
  })
  
  # observe({
  #   demR<-demo_ranks()
  #   # print(head(demR))
  # })
  #Run Supervised Ranking Model whenever a new row is selected as the reference film
  ranks <- reactive({
    print("supervised reactive")
    #mmsComp<-mms%>%filter((mms$`DBO ($M)`>4) & (mms$`IBO ($M)`>4) & (mms$`WBO ($M)`>4) & (mms$`WWBO ($M)`>4))
    mmsComp<-mms%>%filter(mms$`DBO ($M)`>4)
    if(nrow(rv_refTable$tbl) == 0){
      data.frame(Mcode = character(1),`Smart Rank`=NA, `Supervised Rank` = NA, check.names = FALSE)
    }else if(nrow(rv_refTable$tbl) == 1 || length(input$smart_button)==0){
      #[((mms$DOMESTIC_BO_AMT>4) & (mms$INTERNATIONAL_BO_AMT>4) & (mms$WW_BO_AMT>4) & (mms$WEEKEND_BO_AMT>4)),]
      a <- rank_models(scenario = rv_refTable$tbl %>% slice(1),
                       comp_universe = mmsComp, 
                       model_coefficients = supervised_coefficients,
                       smart_weights = list(director = input$switch_director,
                                            cast = input$switch_cast, 
                                            exec_prod = input$switch_execprod,
                                            writer = input$switch_writer,
                                            genre= input$switch_genre,
                                            keyword=input$switch_keyword,
                                            rating = input$switch_rating,
                                            release_month = input$switch_releaseMonth))
      print("smaartt ranksss")
      # print(head(a))
      a
      
    }else if(length(input$smart_button)>0){
      smart_row <- as.numeric(unlist(str_split(input$smart_button,"_"))[2])
      print(smart_row)
      rank_models(scenario = rv_refTable$tbl %>% slice(smart_row),
                  comp_universe = mmsComp, 
                  model_coefficients = supervised_coefficients)
    }else{
      data.frame(Mcode = character(1),`Smart Rank`=NA, `Supervised Rank` = NA, check.names = FALSE)
    }
  })
  
  
  filtered_universe <- reactive({
    
    #[ADD OPTION: IF ALL SWITCHES FALSE, SHOW FULL UNIVERSE (with filters applied but not smart ranked)]
    print("Filtered MMS")
    print(input$columnList1_order$text)
    print(mms)
    dta <- mms %>%
      rownames_to_column("MCODE_MASTER")%>%
      filter(
        Mcode %in% flt_film() &
          Mcode %in% flt_release() &
          Mcode %in% flt_genre() &
          Mcode %in% flt_mpaa() &
          Mcode %in% flt_director() &
          Mcode %in% flt_cast() &
          Mcode %in% flt_voice() &
          Mcode %in% flt_distributor() &
          Mcode %in% flt_producer() &
          Mcode %in% flt_execprod() &
          Mcode %in% flt_keyword() &
          Mcode %in% flt_studio() &
          Mcode %in% flt_editor() &
          Mcode %in% flt_writer() &
          Mcode %in% flt_cinemascore() &
          Mcode %in% flt_minutes() &
          Mcode %in% flt_tomato() &
          Mcode %in% flt_dombo() &
          Mcode %in% flt_intlbo() &
          Mcode %in% flt_weekendbo() &
          Mcode %in% flt_wwbo()&
          Mcode %in% flt_smartrank()&#Added Soumen 24.07.23 T4
          Mcode %in% flt_supervrank()#Added Soumen 08.03.23 T4
      )%>%
      #select(everything())
      select(match(input$columnList1_order$text,names(.)),MCODE_MASTER)
    
    #ZM END
    print("hello filtered mms")
    print("MMS new fields")
    print(dta%>%filter(stringr::str_detect(Title,"Call")))
    
    
    #ZM BEG
    #inputs
    #lookup film is accessing the row selected from the table of multiple possible references (including custom scenario)
    print(input$dta_reference_rows_selected)
    
    #join results to mms
    dta <- dta %>% 
      select(-`Supervised Rank`,-`Smart Rank`,-`Demo Rank`) %>% 
      left_join(ranks(),
                by = c("MCODE_MASTER" = "Mcode")) %>%
      left_join(demo_ranks(),
                by = c("MCODE_MASTER" = "Mcode"))%>%
      select(Title, `Smart Rank`,`Supervised Rank`,`Demo Rank`, everything()) %>%
      
      #mutate(`Supervised Rank` = as.numeric(X1)) %>%
      #select(-X1) %>%
      #Arrange and final processing
      arrange(`Smart Rank`) %>% #next line removes it from universe if it's already selected
      filter(MCODE_MASTER %in% rv_currentComps$currentComps$Mcode == FALSE)%>%
      column_to_rownames("MCODE_MASTER")
    
    
    #ZM END
    print("MMS new fields new")
    print(dta%>%filter(stringr::str_detect(Title,"Call")))
    print(class(dta$`Smart Rank`))
    ## print(head(dta))
    ## print(head(demo_ranks()))
    
    if(nrow(dta)==0){
      a<-mms %>%
        rownames_to_column("MCODE_MASTER") %>%
        mutate(`Smart Rank` = "", `Supervised Rank`="",`Demo Rank`="") %>% #next line removes it from universe if it's already selected Soumen
        filter(MCODE_MASTER %in% rv_currentComps$currentComps$Mcode == FALSE)%>%
        filter(
          #( (input$filter_release[1] <= `Release Date` & `Release Date` <= input$filter_release[2])|is.na(`Release Date`) )&
          #( (`Primary Genre` %in% input$filter_genre | `Secondary Genre` %in% input$filter_genre | `Other Genre` %in% input$filter_genre) | (is.na(`Primary Genre`)&is.na(`Secondary Genre`)&is.na(`Other Genre`)) )
          Mcode %in% flt_film() &
            Mcode %in% flt_release() &
            Mcode %in% flt_genre() &
            Mcode %in% flt_mpaa() &
            Mcode %in% flt_director() &
            Mcode %in% flt_cast() &
            Mcode %in% flt_voice() &
            Mcode %in% flt_distributor() &
            Mcode %in% flt_producer() &
            Mcode %in% flt_execprod() &
            Mcode %in% flt_keyword() &
            Mcode %in% flt_studio() &
            Mcode %in% flt_editor() &
            Mcode %in% flt_writer() &
            Mcode %in% flt_cinemascore() &
            Mcode %in% flt_minutes() &
            Mcode %in% flt_tomato() &
            Mcode %in% flt_dombo() &
            Mcode %in% flt_intlbo() &
            Mcode %in% flt_weekendbo() &
            Mcode %in% flt_wwbo()&
            Mcode %in% flt_smartrank()& #Added Soumen24.07.23
            Mcode %in% flt_supervrank() #Added Soumen03.08.23
        )  %>%
        #select(match(input$columnList1_order,names(.)),MCODE_MASTER) %>%
        select(match(input$columnList1_order$text,names(.)),MCODE_MASTER)%>%
        column_to_rownames("MCODE_MASTER")
    }else{
      a<-dta
    }
    a
    
  })
  
  
  
  #Reference/Scenario Films -----
  
  
  
  
  #1) The scenario's inputs should always be the first row of the list
  
  #If scenario name already exists, or if it is NULL/Empty/<NO SCENARIO SELECTED> then keep save button disabled (& change title text to display a specific message)
  #ELSE if the scenario name is NEW, then change
  # observeEvent(ignoreInit=TRUE,input$scenario_name,{
  #   #print(rv_projectscenarios$scenario_names)
  #
  #   print(setdiff(rv_projectscenarios$scenario_names,
  #                 c(rv_currentScenario$name,"<NO SCENARIO SELECTED>","")))
  #
  #   shinyjs::toggleState("btn_saveScenario", condition = input$scenario_name %in% setdiff(c(rv_currentScenario$name,"<NO SCENARIO SELECTED>",""),
  #                                                                                         rv_projectscenarios$scenario_names))
  # })
  
  observeEvent(ignoreInit=TRUE,input$scenario_name,{
    print(is.null(rv_projectscenarios$scenario_names))
    if(!is.null(rv_projectscenarios$scenario_names)){
      if(rv_flgNewOrEdit$flg == "New"){
        print(rv_flgNewOrEdit$flg)
        #DO NOT ALLOW TO SAVE IF SCENARIO NAME ALREADY EXISTS
        # shinyjs::toggleState("btn_saveScenario",
        #                      condition = !is.null(input$scenario_name) &
        #                        input$scenario_name %in% c("","<NO SCENARIO SELECTED>", rv_projectscenarios$scenario_names)==FALSE & checkSpChar(input$scenario_name)==FALSE) 
        # #DO NOT ALLOW TO SAVE IF SCENARIO NAME ALREADY EXISTS
        # shinyjs::toggleState("btn_saveScenario1",
        #                      condition = !is.null(input$scenario_name) &
        #                        input$scenario_name %in% c("","<NO SCENARIO SELECTED>", rv_projectscenarios$scenario_names)==FALSE & checkSpChar(input$scenario_name)==FALSE) 
        # 
        shinyjs::toggleState("btn_saveScenario",
                             condition = !is.null(input$scenario_name) &
                               str_trim(tolower(input$scenario_name)) %in% c("","<NO SCENARIO SELECTED>",unlist(lapply(lapply(rv_projectscenarios$scenario_names,tolower),str_trim)))==FALSE & checkSpChar(input$scenario_name)==FALSE) 
        
        shinyjs::toggleState("btn_saveScenario1",
                             condition = !is.null(input$scenario_name) &
                               str_trim(tolower(input$scenario_name)) %in% c("","<NO SCENARIO SELECTED>",unlist(lapply(lapply(rv_projectscenarios$scenario_names,tolower),str_trim)))==FALSE & checkSpChar(input$scenario_name)==FALSE) 
        
        
        
        shinyjs::toggle("error_scenario",condition = checkSpChar(input$scenario_name)==TRUE)
        shinyjs::toggle("error_scenarioExist",condition = str_trim(tolower(input$scenario_name)) %in% unlist(lapply(lapply(rv_projectscenarios$scenario_names,tolower),str_trim))==TRUE)
        
        
        
      }else{#now, handle for if we are EDITING. DO NOT ALLOW TO SAVE UNLESS THE SCENARIO NAME IS NEW OR EXACTLY THE SAME AS CURRENT SCEN
        #print("?")
        #print(rv_flgNewOrEdit$flg)
        shinyjs::toggleState("btn_saveScenario",
                             condition = (!is.null(input$scenario_name) & str_trim(tolower(input$scenario_name)) %in% c("","<NO SCENARIO SELECTED>",unlist(lapply(lapply(rv_projectscenarios$scenario_names,tolower),str_trim)))==FALSE) && (checkSpChar(input$scenario_name)==FALSE) |
                               input$scenario_name == rv_currentScenario$name)
        shinyjs::toggleState("btn_saveScenario1",
                             condition = (!is.null(input$scenario_name) & str_trim(tolower(input$scenario_name)) %in% c("","<NO SCENARIO SELECTED>",unlist(lapply(lapply(rv_projectscenarios$scenario_names,tolower),str_trim)))==FALSE) && (checkSpChar(input$scenario_name)==FALSE) |
                               input$scenario_name == rv_currentScenario$name)
        
        shinyjs::toggle("error_scenario",condition = checkSpChar(input$scenario_name)==TRUE)
        shinyjs::toggle("error_scenarioExist",condition =input$scenario_name != rv_currentScenario$name & str_trim(tolower(input$scenario_name)) %in% unlist(lapply(lapply(rv_projectscenarios$scenario_names,tolower),str_trim))==TRUE)
        
        
        
      }
    }else{
      #DO NOT ALLOW TO SAVE IF SCENARIO NAME ALREADY EXISTS
      shinyjs::toggleState("btn_saveScenario",
                           condition = !is.null(input$scenario_name) &
                             str_trim(tolower(input$scenario_name)) %in% c("","<NO SCENARIO SELECTED>",unlist(lapply(lapply(rv_projectscenarios$scenario_names,tolower),str_trim)))==FALSE & checkSpChar(input$scenario_name)==FALSE)
      #DO NOT ALLOW TO SAVE IF SCENARIO NAME ALREADY EXISTS
      shinyjs::toggleState("btn_saveScenario1",
                           condition = !is.null(input$scenario_name) &
                             str_trim(tolower(input$scenario_name)) %in% c("","<NO SCENARIO SELECTED>",unlist(lapply(lapply(rv_projectscenarios$scenario_names,tolower),str_trim)))==FALSE & checkSpChar(input$scenario_name)==FALSE)
      #shinyjs::toggleState("btn_saveScenario",condition = checkSpChar(input$scenario_name)==FALSE)
      #shinyjs::toggleState("btn_saveScenario1",condition = checkSpChar(input$scenario_name)==FALSE)
      # shinyjs::toggleState("btn_saveScenario1",condition = endsWith(input$scenario_name,"'")==FALSE)
      # shinyjs::toggleState("btn_saveScenario",condition = endsWith(input$scenario_name,"'")==FALSE)
      
      #shinyjs::toggle("error_scenario",condition = checkSpChar(input$scenario_name)==TRUE)
      shinyjs::toggle("error_scenario",condition = checkSpChar(input$scenario_name)==TRUE)
      
      
      
      
    }
  })
  
  #Do not allow user to save forecast until they are editing a valid scenario
  observe({
    toggleState("btn_saveForecasts",condition = rv_flgNewOrEdit$flg == "Edit")
  })
  
  
  #OBSERVING EITHER SAVE EVENT:
  observeEvent(ignoreInit=TRUE,c(input$btn_saveScenario,input$btn_saveScenario1),{
    
    shinyjs::disable("btn_saveScenario")
    shinyjs::disable("btn_saveScenario1")
    print("Update modalsss")
    print(rv_currentScenario$full_detail)
    
    print("entering")
    
    print("title check 2")
    print(rv_projectscenarios$project_title)
    print(rv_newProj$title)
    
    #Elements to display as static text for the user to see outside of the modal:
    # Add project name to Scenario builder: Start Akshay Mahale
    output$proj_name <- renderText(rv_projectscenarios$project_title)
    # Add project name to Scenario builder: End Akshay Mahale
    output$prod_budget <- renderText(input$scenario_prodbudget)
    output$mpaa <- renderText(paste(input$scenario_mpaa, collapse = ", "))
    output$director <- renderText(paste(input$scenario_director, collapse = ", "))
    output$cast <- renderText(paste(input$scenario_cast, collapse = ", "))
    output$voice <- renderText(paste(input$scenario_voice, collapse = ", "))
    output$producer <- renderText(paste(input$scenario_producer, collapse = ", "))
    output$execprod <- renderText(paste(input$scenario_execproducer, collapse = ", "))
    
    
    #rv_scenarios$scens <- readRDS("data/000_scenarios.RDS")
    print("button check")
    print(rv_flgNewOrEdit$flg)
    if(rv_flgNewOrEdit$flg ==""){
      print("clear")
    }else if(rv_flgNewOrEdit$flg == "New"){
      print("check1")
      #If the project / scenario already exists, warn them that they will overwrite
      # The only time this will happen is if a separate user has saved out a new scenario with THE EXACT SAME NAME in the time that this user was in the app...
      check1 <- rv_scenarios$scens %>% filter(Title == rv_projectscenarios$project_title & Name == input$scenario_name) %>% nrow()
      print(check1)
      print(input$scenario_name)
      rv_currentScenario$name <- input$scenario_name
      print("check2")
      #If a scnario already exists, receive confirmation from user to overwrite
      if(check1 == 0){
        scen_rel_chk <- (length(input$scenario_release)==0)
        #print(length(as.numeric(input$scenario_prodbudget)))
        print(scen_rel_chk)
        print(input$scenario_release)
        print(str(input$scenario_release))
        print(is.null(input$scenario_release))
        print(as.character(input$scenario_release))
        #if the scenario is TRULY NEW,
        #1) make a new row in the rv_scenarios$scens table
        #2) keep necessary details for the reference film
        #3) Import current state of the full scenarios data
        #4) Append new scenario
        #5) save back out the full scenario list
        #6) Import current state of projects list, append "last scenario", then save out
        #7) Change flag status to "editing"
        rv_scenInputs$inputs <- data.frame(`Title` = paste("SCENARIO:",input$scenario_name),
                                           `Release Date` = ifelse(scen_rel_chk,NA,as.character(input$scenario_release)),
                                           `Director` = paste(input$scenario_director,collapse = ", "),
                                           `Production Budget ($M)` = as.numeric(input$scenario_prodbudget),
                                           `Cast (Major)` = paste(input$scenario_cast,collapse = ", "),
                                           `DBO ($M)` = paste0(input$scenario_domesticbo[1],"-",input$scenario_domesticbo[2]),
                                           `IBO ($M)` = paste0(input$scenario_intlbo[1],"-",input$scenario_intlbo[2]),
                                           `WBO ($M)` = paste0(input$scenario_weekendbo[1],"-",input$scenario_weekendbo[2]),
                                           `WWBO ($M)` = paste0(input$scenario_wwbo[1],"-",input$scenario_wwbo[2]),
                                           `Minutes`  = paste0(input$scenario_minutes[1],"-",input$scenario_minutes[2]),
                                           `MPAA Rating` = input$scenario_mpaa,
                                           `Cinema Score` = input$scenario_cinemascore,
                                           `Tomato Meter` = paste0(input$scenario_tomatometer[1],"-",input$scenario_tomatometer[2]),
                                           `Primary Genre` = input$scenario_genre1,
                                           `Secondary Genre` = input$scenario_genre2,
                                           `Other Genre` = input$scenario_genre3,
                                           `Keyword 1` = input$scenario_keyword1,
                                           `Keyword 2` = input$scenario_keyword2,
                                           `Keyword 3` = input$scenario_keyword3,
                                           `Distributor` = NA,#paste(input$scenario_distributor,collapse=", "),
                                           `Studio` = input$scenario_studio,
                                           `Producer` = paste(input$scenario_producer,collapse = ", "),
                                           `Executive Producer` = paste(input$scenario_execproducer,collapse=", "),
                                           `Writer` = paste(input$scenario_writer,collapse=", "),
                                           `Editor` = input$scenario_editor,
                                           `Voice` = paste(input$scenario_voice,collapse=", "),
                                           #`Notes` = input$scenario_notes,
                                           `Long Weekend` = input$scenario_longweekend,
                                           #[AK - ADD THE REST OF THE FIELDS HERE]
                                           stringsAsFactors = FALSE,check.names = FALSE) %>% 
          mutate(`Release Date`=as.Date(`Release Date`,origin = "1970-01-01"))
        
        tmp1 <- rv_scenInputs$inputs %>% rename(Name = Title) %>%
          mutate(Title = rv_projectscenarios$project_title,
                 Name = str_replace(Name,"^SCENARIO: ",""),
                 `Created By`= session$user,
                 `Date Created` = Sys.Date(),
                 `Last Modified` = Sys.time()
          )
        print(tmp1)
        print(tmp1$Title)
        print("check3")
        
        #ZM BEG
        #save(tmp1, file = "tmp1.RDA")
        #print("MMS LONG UPDATE")
        # mms_long <- mms_long %>%
        #   #mms_long_update: converts tmp1 into a long frame to add to mms_long (for smart rank counting)
        #   filter(!Title %in% paste0("SCENARIO: ",tmp1$Name)) %>% #filter out old scenario entry
        #   bind_rows(mms_long_update(tmp1)) #add new/updated entry
        #ZM END
        
        print("check2")
        rv_scenarios$scens <- bind_rows(rv_scenarios$scens %>% mutate_at(vars(-`Production Budget ($M)`,-`Release Date`,-`Date Created`, -`Last Modified`,-`Date Submitted`,-`Low`,-Base,-High,-`Int Low`,-`Int Base`,-`Int High`),as.character),
                                        tmp1)
        print("check3")
        print(rv_scenarios$scens %>% slice(n()))
        
        rv_currentScenario$full_detail <- rv_scenarios$scens %>% slice(n())
        
        rv_scenarios$scens <- rv_scenarios$scens %>% arrange(desc(`Last Modified`))
        
        #print(paste0('INSERT INTO "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO"',"(PROJ_TITLE,SCEN_NAME,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,SCEN_CREATED_DATE,SCEN_LAST_MODIFIED,SCEN_CREATED_BY,APP_PROJ_KEY) SELECT $$",   # T20 Soumen 23.07.24
        print(paste0('INSERT INTO ',database,'."ODS"."APP_SCENARIO"',"(PROJ_TITLE,SCEN_NAME,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,SCEN_CREATED_DATE,SCEN_LAST_MODIFIED,SCEN_CREATED_BY,APP_PROJ_KEY) SELECT $$",
                     #rv_projectscenarios$project_title,"','",
                     rv_newProj$title,"$$,$$",
                     input$scenario_name,"$$,",
                     ifelse(input$scenario_prodbudget=="","NULL,'",paste0("'",input$scenario_prodbudget,"','")),
                     input$scenario_mpaa,"',$$",
                     paste(input$scenario_director,collapse = ", "),"$$,$$",
                     paste(input$scenario_cast,collapse = ", "),"$$,$$",
                     paste(input$scenario_voice,collapse=", "),"$$,$$",
                     paste(input$scenario_producer,collapse=", "),"$$,$$",
                     paste(input$scenario_execproducer,collapse=", "),"$$,$$",
                     input$scenario_genre1,"$$,$$",
                     input$scenario_genre2,"$$,$$",
                     input$scenario_genre3,"$$,$$",
                     input$scenario_keyword1,"$$,$$",
                     input$scenario_keyword2,"$$,$$",
                     input$scenario_keyword3,"$$,$$",
                     input$scenario_studio,"$$,$$",
                     input$scenario_editor,"$$,$$",
                     paste(input$scenario_writer,collapse=", "),"$$,",
                     ifelse(length(input$scenario_release)==0,"NULL,",paste0("'",input$scenario_release,"',")),
                     ifelse(length(input$scenario_longweekend)==0|input$scenario_longweekend=="","NULL,'",paste0(input$scenario_longweekend,",'")),
                     #input$scenario_longweekend,",'",
                     paste(input$scenario_minutes, collapse = "-"),"','",
                     paste(input$scenario_tomatometer, collapse = "-"),"',",
                     ifelse(length(input$scenario_cinemascore)==0|input$scenario_cinemascore=="","NULL,'",paste0(input$scenario_cinemascore,",'")),
                     paste(input$scenario_domesticbo, collapse = "-"),"','",
                     paste(input$scenario_intlbo, collapse = "-"),"','",
                     paste(input$scenario_weekendbo, collapse = "-"),"','",
                     paste(input$scenario_wwbo, collapse = "-"),"','",
                     Sys.time(),"','",
                     Sys.time(),"','",
                     session$user,
                     #"',APP_PROJ_KEY FROM ",'"THEATRICAL_LIGHTHOUSE"."ODS"."APP_PROJECT_LIST" ',"WHERE PROJ_TITLE = $$",    #T20 Soumen 23.07.24
                     "',APP_PROJ_KEY FROM ",database,'."ODS"."APP_PROJECT_LIST" ',"WHERE PROJ_TITLE = $$",    
                     rv_projectscenarios$project_title,"$$;"))
        
        #QUERY - COMPLETE - SAVE NEW SCENARIO-----
        start_time=Sys.time()
        dbGetQuery(my_db, 
                   #paste0('INSERT INTO "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO"',"(PROJ_TITLE,SCEN_NAME,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,SCEN_CREATED_DATE,SCEN_LAST_MODIFIED,SCEN_CREATED_BY,APP_PROJ_KEY) SELECT $$",   #T20 Soumen 23.07.24
                   paste0('INSERT INTO ',database,'."ODS"."APP_SCENARIO"',"(PROJ_TITLE,SCEN_NAME,SCEN_PROD_BUDGET,SCEN_MPAA_RATING,SCEN_DIRECTOR,SCEN_CAST,SCEN_VOICE,SCEN_PRODUCER,SCEN_EXEC_PRODUCER,SCEN_PRI_GENRE,SCEN_SEC_GENRE,SCEN_OTHER_GENRE,SCEN_KEYWORD_1,SCEN_KEYWORD_2,SCEN_KEYWORD_3,SCEN_PROD_COMPANY,SCEN_EDITOR,SCEN_WRITER,SCEN_RELEASE_DATE,THREE_DAY_WEEKEND,SCEN_MINUTES,SCEN_TOMATO_METER,SCEN_CINEMA_SCORE,SCEN_DBO,SCEN_IBO,SCEN_WBO,SCEN_WWBO,SCEN_CREATED_DATE,SCEN_LAST_MODIFIED,SCEN_CREATED_BY,APP_PROJ_KEY) SELECT $$",      
                          rv_projectscenarios$project_title,"$$,$$",
                          input$scenario_name,"$$,",
                          ifelse(input$scenario_prodbudget=="","NULL,'",paste0("'",input$scenario_prodbudget,"','")),
                          input$scenario_mpaa,"',$$",
                          paste(input$scenario_director,collapse = ", "),"$$,$$",
                          paste(input$scenario_cast,collapse = ", "),"$$,$$",
                          paste(input$scenario_voice,collapse=", "),"$$,$$",
                          paste(input$scenario_producer,collapse=", "),"$$,$$",
                          paste(input$scenario_execproducer,collapse=", "),"$$,$$",
                          input$scenario_genre1,"$$,$$",
                          input$scenario_genre2,"$$,$$",
                          input$scenario_genre3,"$$,$$",
                          input$scenario_keyword1,"$$,$$",
                          input$scenario_keyword2,"$$,$$",
                          input$scenario_keyword3,"$$,$$",
                          input$scenario_studio,"$$,$$",
                          input$scenario_editor,"$$,$$",
                          paste(input$scenario_writer,collapse=", "),"$$,",
                          ifelse(length(input$scenario_release)==0,"NULL,",paste0("'",input$scenario_release,"',")),
                          ifelse(length(input$scenario_longweekend)==0|input$scenario_longweekend=="","NULL,'",paste0(input$scenario_longweekend,",'")),
                          #input$scenario_longweekend,",'",
                          paste(input$scenario_minutes, collapse = "-"),"','",
                          paste(input$scenario_tomatometer, collapse = "-"),"',",
                          ifelse(length(input$scenario_cinemascore)==0|input$scenario_cinemascore=="","NULL,'",paste0(input$scenario_cinemascore,",'")),
                          paste(input$scenario_domesticbo, collapse = "-"),"','",
                          paste(input$scenario_intlbo, collapse = "-"),"','",
                          paste(input$scenario_weekendbo, collapse = "-"),"','",
                          paste(input$scenario_wwbo, collapse = "-"),"','",
                          Sys.time(),"','",
                          Sys.time(),"','",
                          session$user,
                          #"',APP_PROJ_KEY FROM ",'"THEATRICAL_LIGHTHOUSE"."ODS"."APP_PROJECT_LIST" ',"WHERE PROJ_TITLE = $$",  # T20 Soumen 23.07.24
                          "',APP_PROJ_KEY FROM ",database,'."ODS"."APP_PROJECT_LIST" ',"WHERE PROJ_TITLE = $$",  
                          rv_projectscenarios$project_title,"$$;"))
        end_time=Sys.time()
        print("Done Insert Scenario")
        print(end_time-start_time)
        #saveRDS(rv_scenarios$scens, "data/000_scenarios.RDS")
        
        #QUERY - PENDING - UPDATE LAST SCENARIO DATE IN PROJECT TABLE WHEN A NEW SCENARIO IS MADE
        
        # rv_projectList$projs <- readRDS("data/000_projects.RDS")
        rv_projectList$projs <- rv_projectList$projs %>% mutate(`Last Scenario` = if_else(Title == rv_projectscenarios$project_title, Sys.time(), `Last Scenario`))
        # saveRDS(rv_projectList$projs, "data/000_projects.RDS")
        
        
        
        rv_flgNewOrEdit$flg <- "Edit"
        
        updateDateRangeInput(session, "projfilter_lastscen",end = Sys.Date())
        
        sendSweetAlert(session, title = paste0('Successfully Saved New Scenario: "',input$scenario_name,'"'),type = "success")
        
        
      }else{ #
        confirmSweetAlert(session, inputId = "alert_sameName",
                          title = "A scenario with this name already exists.",
                          text = "Someone has recently created this scenario name. Please change the scenario name.",
                          type = "warning")
        print("check3")
        
      }
    }else{#HERE, WE WILL BE EDITING/OVERWRITING AN EXISTING SCENARIO
      #first need to check that the new name does not already exist (but can still be the current name if unchanged)
      check2 <- rv_scenarios$scens %>% 
        filter(Title == rv_projectscenarios$project_title & Name != rv_currentScenario$name) %>% 
        select(Name) %>% 
        pull()
      
      
      if(input$scenario_name %in% check2 == FALSE){
        scen_rel_chk <- length(input$scenario_release)==0
        
        old_fcst <- rv_scenarios$scens %>% 
          filter(Title==rv_projectscenarios$project_title & Name==rv_currentScenario$name)
        
        print("chk0")
        rv_scenInputs$inputs <- data.frame(`Title` = paste("SCENARIO:",input$scenario_name), 
                                           `Release Date` = ifelse(scen_rel_chk,NA,as.character(input$scenario_release)),                                            
                                           `Director` = paste(input$scenario_director,collapse = ", "),
                                           `Production Budget ($M)` = as.numeric(input$scenario_prodbudget),
                                           `Cast (Major)` = paste(input$scenario_cast,collapse = ", "),
                                           `DBO ($M)` = paste0(input$scenario_domesticbo[1],"-",input$scenario_domesticbo[2]),
                                           `IBO ($M)` = paste0(input$scenario_intlbo[1],"-",input$scenario_intlbo[2]),
                                           `WBO ($M)` = paste0(input$scenario_weekendbo[1],"-",input$scenario_weekendbo[2]),
                                           `WWBO ($M)` = paste0(input$scenario_wwbo[1],"-",input$scenario_wwbo[2]),
                                           `Minutes`  = paste0(input$scenario_minutes[1],"-",input$scenario_minutes[2]),
                                           `MPAA Rating` = input$scenario_mpaa,
                                           `Cinema Score` = input$scenario_cinemascore,
                                           `Tomato Meter` = paste0(input$scenario_tomatometer[1],"-",input$scenario_tomatometer[2]),
                                           `Primary Genre` = input$scenario_genre1,
                                           `Secondary Genre` = input$scenario_genre2,
                                           `Other Genre` = input$scenario_genre3,
                                           `Keyword 1` = input$scenario_keyword1,
                                           `Keyword 2` = input$scenario_keyword2,
                                           `Keyword 3` = input$scenario_keyword3,
                                           `Distributor` = NA,#paste(input$scenario_distributor,collapse=", "),
                                           `Studio` = input$scenario_studio,
                                           `Producer` = paste(input$scenario_producer,collapse = ", "),
                                           `Executive Producer` = paste(input$scenario_execproducer,collapse=", "),
                                           `Writer` = paste(input$scenario_writer,collapse=", "),
                                           `Editor` = input$scenario_editor,
                                           `Voice` = paste(input$scenario_voice,collapse=", "),
                                           #`Notes` = input$scenario_notes,
                                           Low = old_fcst$Low,
                                           Base = old_fcst$Base,
                                           High = old_fcst$High,
                                           #Int Fcst:Akshay Mahale
                                           `Int Low`=old_fcst$`Int Low`,
                                           `Int Base`=old_fcst$`Int Base`,
                                           `Int High`=old_fcst$`Int High`,
                                           `Int FR Low`=old_fcst$`Int FR Low`,
                                           `Int FR Mid`=old_fcst$`Int FR Mid`,
                                           `Int FR High`=old_fcst$`Int FR High`,
                                           `Int Prints Low`=old_fcst$`Int Prints Low`,
                                           `Int Prints Mid`=old_fcst$`Int Prints Mid`,
                                           `Int Prints High`=old_fcst$`Int Prints High`,
                                           `Int Ad/Pub Low`=old_fcst$`Int Ad/Pub Low`,
                                           `Int Ad/Pub Mid`=old_fcst$ `Int Ad/Pub Mid`,
                                           `Int Ad/Pub High`=old_fcst$`Int Ad/Pub High`,
                                           
                                           #China Fcst:Akshay Mahale
                                           `China Fcst Low`=old_fcst$`China Fcst Low`,
                                           `China Fcst Mid`=old_fcst$`China Fcst Mid`,
                                           `China Fcst High`=old_fcst$`China Fcst High`,
                                           `China FR Low`=old_fcst$`China FR Low`,
                                           `China FR Mid`=old_fcst$`China FR Mid`,
                                           `China FR High`=old_fcst$`China FR High`,
                                           `China Prints Low`=old_fcst$`China Prints Low`,
                                           `China Prints Mid`=old_fcst$`China Prints Mid`,
                                           `China Prints High`=old_fcst$`China Prints High`,
                                           `China Ad/Pub Low`=old_fcst$`China Ad/Pub Low`,
                                           `China Ad/Pub Mid`=old_fcst$ `China Ad/Pub Mid`,
                                           `China Ad/Pub High`=old_fcst$`China Ad/Pub High`,
                                           
                                           Submitted = old_fcst$Submitted,
                                           `Date Submitted` = old_fcst$`Date Submitted`,
                                           `FR Low` = old_fcst$`FR Low`,
                                           `FR Mid` = old_fcst$`FR Mid`,
                                           `FR High` = old_fcst$`FR High`,
                                           `Ad/Pub Low` = old_fcst$`Ad/Pub Low`,
                                           `Ad/Pub Mid` = old_fcst$`Ad/Pub Mid`,
                                           `Ad/Pub High` = old_fcst$`Ad/Pub High`,
                                           `Prints Low` = old_fcst$`Prints Low`,
                                           `Prints Mid` = old_fcst$`Prints Mid`,
                                           `Prints High` = old_fcst$`Prints High`,
                                           #[AK - ADD ANY ADDITIONAL FORECAST-BOX FIELDS HERE]
                                           stringsAsFactors = FALSE,check.names = FALSE) %>% 
          mutate(`Release Date` = as.Date(`Release Date`,origin = "1970-01-01"))
        print("chk1")
        tmp1 <- rv_scenInputs$inputs %>% rename(Name = Title) %>% 
          mutate(Title = rv_projectscenarios$project_title, 
                 Name = str_replace(Name,"^SCENARIO: ",""),
                 `Created By`=session$user,
                 `Last Modified` = Sys.time(),
                 `Selected Comps`= rv_currentScenario$full_detail$`Selected Comps`,
                 `Selected Comps Mcodes` = rv_currentScenario$full_detail$`Selected Comps Mcodes`,
                 `Final Comps Mcodes` = rv_currentScenario$full_detail$`Final Comps Mcodes`
          )
        print("check2")
        
        #ZM BEG
        #save(tmp1, file = "tmp12.RDA")
        print("MMS LONG UPDATE")
        # mms_long <- mms_long %>%
        #   #mms_long_update: converts tmp1 into a long frame to add to mms_long (for smart rank counting)
        #   filter(!Title %in% paste0("SCENARIO: ",tmp1$Name)) %>% #filter out old scenario entry
        #   bind_rows(mms_long_update(tmp1)) #add new/updated entry
        #ZM END
        
        print("binding")
        print(str(rv_scenarios$scens))
        print(str(tmp1))
        print("bind trial")
        
        #Int Fcst: Akshay Mahale
        rv_scenarios$scens <- bind_rows(rv_scenarios$scens %>%  
                                          mutate_at(vars(-`Production Budget ($M)`,-`Date Submitted`,-`Date Created`,-`Release Date`, -`Last Modified`,-`Low`,-Base,-High,
                                                         -`Int Low`,
                                                         -`Int Base`,
                                                         -`Int High`,
                                                         -`Int FR Low`,
                                                         -`Int FR Mid`,
                                                         -`Int FR High`,
                                                         -`Int Prints Low`,
                                                         -`Int Prints Mid`,
                                                         -`Int Prints High`,
                                                         -`Int Ad/Pub Low`,
                                                         -`Int Ad/Pub Mid`,
                                                         -`Int Ad/Pub High`,
                                                         
                                                         -`China Fcst Low`,
                                                         -`China Fcst Mid`,
                                                         -`China Fcst High`,
                                                         -`China FR Low`,
                                                         -`China FR Mid`,
                                                         -`China FR High`,
                                                         -`China Prints Low`,
                                                         -`China Prints Mid`,
                                                         -`China Prints High`,
                                                         -`China Ad/Pub Low`,
                                                         -`China Ad/Pub Mid`,
                                                         -`China Ad/Pub High`,
                                                         
                                                         -`FR Low`,
                                                         -`FR Mid`,
                                                         -`FR High`,
                                                         -`Ad/Pub Low`,
                                                         -`Ad/Pub Mid`,
                                                         -`Ad/Pub High`,
                                                         -`Prints Low`,
                                                         -`Prints Mid`,
                                                         -`Prints High`,
                                                         -`Date Created`,
                                                         -`ThreeD Low`,
                                                         -`ThreeD Mid`,
                                                         -`ThreeD High`,
                                                         -`Imax Low`,
                                                         -`Imax Mid`,
                                                         -`Imax High`),as.character) %>% 
                                          filter((Title==rv_projectscenarios$project_title & Name==rv_currentScenario$name)==FALSE),
                                        tmp1) %>% 
          mutate(`Film Rental` = `FR Mid`,
                 Marketing = `Ad/Pub Mid`)#Marketing filed to contain Ad/Pub Mid value instead of Print Mid Value:Akshay
        print("done binding")
        print(rv_scenarios$scens %>% slice(n()))
        
        rv_currentScenario$full_detail <- rv_scenarios$scens %>% slice(n())
        
        print("Slice full detail")
        print(old_fcst$scen_id)
        
        rv_scenarios$scens <- rv_scenarios$scens %>% arrange(desc(`Last Modified`))
        #QUERY - COMPLETE - UPDATE SCENARIO WHEN EDITING DETAILS-----
        #saveRDS(rv_scenarios$scens,"data/000_scenarios.RDS")
        print("write to snowflake")
        #print(paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO" ',"SET SCEN_NAME = $$",re.escape(input$scenario_name),   # T20 Soumen 23.07.24
        print(paste0('UPDATE ',database,'."ODS"."APP_SCENARIO" ',"SET SCEN_NAME = $$",re.escape(input$scenario_name), 
                     "$$,SCEN_PROD_BUDGET = ",ifelse(input$scenario_prodbudget=="","NULL",input$scenario_prodbudget),
                     ",SCEN_MPAA_RATING = '",input$scenario_mpaa,
                     "',SCEN_DIRECTOR = $$",paste(input$scenario_director,collapse=", "),
                     "$$,SCEN_CAST = $$",paste(input$scenario_cast,collapse=", "),
                     "$$,SCEN_VOICE = $$",paste(input$scenario_voice,collapse=", "),
                     "$$,SCEN_PRODUCER = $$",paste(input$scenario_producer,collapse=", "),
                     "$$,SCEN_EXEC_PRODUCER = $$",paste(input$scenario_execproducer,collapse=", "),
                     "$$,SCEN_PRI_GENRE = $$",input$scenario_genre1,
                     "$$,SCEN_SEC_GENRE = $$",input$scenario_genre2,
                     "$$,SCEN_OTHER_GENRE = $$",input$scenario_genre3,
                     "$$,SCEN_KEYWORD_1 = $$",input$scenario_keyword1,
                     "$$,SCEN_KEYWORD_2 = $$",input$scenario_keyword2,
                     "$$,SCEN_KEYWORD_3 = $$",input$scenario_keyword3,
                     "$$,SCEN_PROD_COMPANY = $$",input$scenario_studio,
                     "$$,SCEN_EDITOR = $$",input$scenario_editor,
                     "$$,SCEN_WRITER = $$",paste(input$scenario_writer,collapse=", "),
                     "$$,SCEN_RELEASE_DATE = ", ifelse(length(input$scenario_release)==0,"NULL",paste0("'",input$scenario_release,"'")),
                     ",THREE_DAY_WEEKEND = ", ifelse(length(input$scenario_longweekend)==0|input$scenario_longweekend=="","NULL",input$scenario_longweekend),
                     ",SCEN_MINUTES = '",paste(input$scenario_minutes, collapse = "-"),
                     "',SCEN_TOMATO_METER = '",paste(input$scenario_tomatometer, collapse = "-"),
                     "',SCEN_CINEMA_SCORE = '",ifelse(length(input$scenario_cinemascore)==0|input$scenario_cinemascore=="","NULL",input$scenario_cinemascore),
                     "',SCEN_DBO = '",  paste(input$scenario_domesticbo, collapse = "-"),
                     "',SCEN_IBO = '",  paste(input$scenario_intlbo, collapse = "-"),
                     "',SCEN_WBO = '",  paste(input$scenario_weekendbo, collapse = "-"),
                     "',SCEN_WWBO = '", paste(input$scenario_wwbo, collapse = "-"),
                     "',SCEN_LAST_MODIFIED = '",Sys.time(),
                     "',SCEN_MODIFIED_BY = '",session$user,
                     "' WHERE SCEN_NAME = $$",rv_currentScenario$name,
                     "$$ AND PROJ_TITLE = $$",rv_projectscenarios$project_title,"$$;"))
        dbGetQuery(my_db, 
                   #paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO" ',"SET SCEN_NAME = $$",input$scenario_name,  # T20 Soumen 23.07.24
                   paste0('UPDATE ',database,'."ODS"."APP_SCENARIO" ',"SET SCEN_NAME = $$",input$scenario_name,
                          "$$,SCEN_PROD_BUDGET = ",ifelse(input$scenario_prodbudget=="","NULL",input$scenario_prodbudget),
                          ",SCEN_MPAA_RATING = '",input$scenario_mpaa,
                          "',SCEN_DIRECTOR = $$",paste(input$scenario_director,collapse=", "),
                          "$$,SCEN_CAST = $$",paste(input$scenario_cast,collapse=", "),
                          "$$,SCEN_VOICE = $$",paste(input$scenario_voice,collapse=", "),
                          "$$,SCEN_PRODUCER = $$",paste(input$scenario_producer,collapse=", "),
                          "$$,SCEN_EXEC_PRODUCER = $$",paste(input$scenario_execproducer,collapse=", "),
                          "$$,SCEN_PRI_GENRE = $$",input$scenario_genre1,
                          "$$,SCEN_SEC_GENRE = $$",input$scenario_genre2,
                          "$$,SCEN_OTHER_GENRE = $$",input$scenario_genre3,
                          "$$,SCEN_KEYWORD_1 = $$",input$scenario_keyword1,
                          "$$,SCEN_KEYWORD_2 = $$",input$scenario_keyword2,
                          "$$,SCEN_KEYWORD_3 = $$",input$scenario_keyword3,
                          "$$,SCEN_PROD_COMPANY = $$",input$scenario_studio,
                          "$$,SCEN_EDITOR = $$",input$scenario_editor,
                          "$$,SCEN_WRITER = $$",paste(input$scenario_writer,collapse=", "),
                          "$$,SCEN_RELEASE_DATE = ", ifelse(length(input$scenario_release)==0,"NULL",paste0("'",input$scenario_release,"'")),
                          ",THREE_DAY_WEEKEND = ", ifelse(length(input$scenario_longweekend)==0|input$scenario_longweekend=="","NULL",input$scenario_longweekend),
                          ",SCEN_MINUTES = '",paste(input$scenario_minutes, collapse = "-"),
                          "',SCEN_TOMATO_METER = '",paste(input$scenario_tomatometer, collapse = "-"),
                          "',SCEN_CINEMA_SCORE = '",ifelse(length(input$scenario_cinemascore)==0|input$scenario_cinemascore=="","NULL",input$scenario_cinemascore),
                          "',SCEN_DBO = '",  paste(input$scenario_domesticbo, collapse = "-"),
                          "',SCEN_IBO = '",  paste(input$scenario_intlbo, collapse = "-"),
                          "',SCEN_WBO = '",  paste(input$scenario_weekendbo, collapse = "-"),
                          "',SCEN_WWBO = '", paste(input$scenario_wwbo, collapse = "-"),
                          "',SCEN_LAST_MODIFIED = '",Sys.time(),
                          "',SCEN_MODIFIED_BY = '",session$user,
                          "' WHERE SCEN_NAME = $$",rv_currentScenario$name,
                          "$$ AND PROJ_TITLE = $$",rv_projectscenarios$project_title,"$$;"))
        
        #Akshay Mahale Update Bad Comps table 
        #print(paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS" '," SET SCEN_NAME = $$",input$scenario_name,"$$"," WHERE SCEN_ID=",old_fcst$scen_id," AND PROJ_ID=",rv_projectscenarios$proj_id,";"))  # T20 Soumen 23.07.24
        print(paste0('UPDATE ',database,'."ODS"."APP_BAD_COMPS" '," SET SCEN_NAME = $$",input$scenario_name,"$$"," WHERE SCEN_ID=",old_fcst$scen_id," AND PROJ_ID=",rv_projectscenarios$proj_id,";"))
        
        projectDetailsFunc(rv_projectscenarios$project_title)
        print(paste0("PROJ ID INSERT: ",rv_SelectedDetails$Proj$APP_PROJ_KEY))
        projId<-rv_SelectedDetails$Proj$APP_PROJ_KEY
        
        #print(paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS" '," SET SCEN_NAME = $$",input$scenario_name,"$$",   # T20 Soumen 23.07.24
        print(paste0('UPDATE ',database,'."ODS"."APP_BAD_COMPS" '," SET SCEN_NAME = $$",input$scenario_name,"$$",  
                     "WHERE SCEN_NAME=$$",rv_currentScenario$name,"$$ AND PROJ_ID=",projId,";"))
        dbGetQuery(my_db,
                   #paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS" '," SET SCEN_NAME = $$",input$scenario_name,"$$",   # T20 Soumen 23.07.24
                   paste0('UPDATE ',database,'."ODS"."APP_BAD_COMPS" '," SET SCEN_NAME = $$",input$scenario_name,"$$",   
                          "WHERE SCEN_NAME=$$",rv_currentScenario$name,"$$ AND PROJ_ID=",projId,";"))
        
        #  if(is.na(old_fcst$scen_id)) 
        # {
        #   print("in old fcst")
        #   
        #   print(paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS" '," SET SCEN_NAME = '",input$scenario_name,"'",
        #                "WHERE SCEN_NAME='",rv_currentScenario$name,"' AND PROJ_ID=",projId,";"))
        #   dbGetQuery(my_db,
        #              paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS" '," SET SCEN_NAME = '",input$scenario_name,"'",
        #                     "WHERE SCEN_NAME='",rv_currentScenario$name,"' AND PROJ_ID=",projId,";"))
        #   
        #   
        # }
        # else
        # {
        #   print("in new fcst")
        #   dbGetQuery(my_db,
        #              paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_BAD_COMPS" '," SET SCEN_NAME = '",input$scenario_name,"'",
        #                     "WHERE SCEN_ID=",old_fcst$scen_id," AND PROJ_ID=",rv_projectscenarios$proj_id,";"))
        #   
        #   
        # }
        
        print("write successful")
        
        #QUERY - PENDING - UPDATE PROJECT WHEN EDITING DETAILS-----
        # rv_projectList$projs <- readRDS("data/000_projects.RDS")
        rv_projectList$projs <- rv_projectList$projs %>% mutate(`Last Scenario` = if_else(Title == rv_projectscenarios$project_title, Sys.time(), `Last Scenario`))
        # saveRDS(rv_projectList$projs, "data/000_projects.RDS")
        #all_projects$projs <- readRDS("data/000_projects.RDS")
        all_projects$projs <- all_projects$projs %>% mutate(`Last Scenario` = if_else(Title == rv_projectscenarios$project_title, Sys.time(), `Last Scenario`))
        #saveRDS(all_projects$projs,"data/000_projects.RDS")
        
        rv_flgNewOrEdit$flg <- "Edit"
        
        updateDateRangeInput(session, "projfilter_lastscen",end = Sys.Date())
        
        sendSweetAlert(session, title = paste0('Successfully Edited Scenario: "',input$scenario_name,'"'),type = "success")
        
        #Finally, update the current scenario
        rv_currentScenario$name <- input$scenario_name
        
        
      }else{ #if some other user has JUST gone in and saved while this user was working
        confirmSweetAlert(session, inputId = "alert_sameName",
                          title = "A scenario with this name already exists.",
                          text = "Someone has recently created this scenario name. Please change the scenario name.",
                          type = "warning")
        print("check3")
        
      }
      
    }
    
    shinyjs::enable("btn_saveScenario")
    shinyjs::enable("btn_saveScenario1")
    
    
    #print(rv_scenInputs$inputs)
    #sendSweetAlert(session, title ="Successfully Saved Scenario")
  })
  
  #ALSO, when either scenario save event is observed OR when they edit an existing scenario,
  #  update the scenario model:
  observeEvent(ignoreInit=TRUE,c(input$btn_saveScenario,input$btn_saveScenario1,input$btn_pg_enterExistingScenario),{
    print(rv_currentScenario$full_detail)
    print("also check here")
    rv_model_fcst$mod_scen_inputs <- 0
    rv_model_fcst$mod_comps <- 0
    
    vals <- c()
    for(i in unique(scen_model$label)){
      #For embedded model, first extract the relevant information from the current scenario
      tmp1 <- rv_currentScenario$full_detail %>% select(i) %>% pull() %>% str_split(., ", ") %>% unlist()
      
      #Now, find the relevant information from the aggregate list:
      coeff_i <- scen_model %>% filter(label == i) 
      
      overall_avg_i <- mean(coeff_i$mean,na.rm=T)
      
      #Find average of scenario inputs:
      coeff_i <- coeff_i %>% 
        filter(name %in% tmp1) %>% 
        select(mean) %>% 
        pull()
      
      #If not all inputs are found in the aggregate, repeat the overall avg as inputs for those not found
      if(length(coeff_i) < length(tmp1)){
        val_i <- mean(c(coeff_i, rep(overall_avg_i, length(tmp1)-length(coeff_i))),na.rm=T )
        #If everything is found, take the mean of them
      }else{
        val_i <- mean(coeff_i,na.rm=T)
      }
      vals <- c(vals, val_i)
    }
    #After all inputs are accounted for, take the average of all fields:
    rv_model_fcst$mod_scen_inputs <- mean(vals,na.rm=T)
    
    #Comps Model
    finals <- rv_currentComps$currentComps %>% filter(Mcode %in% rv_currentComps$finalComps)
    finals_adj <- left_join(finals, avg_ticket_price, by = c("Release Year" = "yr"))
    current_price<-avg_ticket_price %>% filter(yr==max(yr)) %>% select(price) %>% pull()
    
    
    finals_adj <- finals_adj %>% 
      mutate(dbo_adj = `DBO ($M)`*1000000/price * current_price/1000000)
    
    
    rv_model_fcst$mod_comps <- mean(finals_adj$dbo_adj,na.rm=T)
    print(paste0("Comp Avg",mean(finals_adj$dbo_adj,na.rm=T)))
    print(paste0("Scene Avg",mean(vals,na.rm=T)))
    
  })
  
  #Print forecast model outputs as a weighted avg of the scenario model + the comp model
  observe({
    # out <- weighted.mean( x=c(rv_model_fcst$mod_scen_inputs, rv_model_fcst$mod_comps), w = c(3,1), na.rm=T)
    #change Weights of components summing up for DBO forecast: Akshay 
    print(paste0("Comp Avg in weighted ",rv_model_fcst$mod_comps))
    print(paste0("Scene Avg in weighted ",rv_model_fcst$mod_scen_inputs))
    
    out <- weighted.mean( x=c(rv_model_fcst$mod_scen_inputs, rv_model_fcst$mod_comps), w = c(0.25,0.75), na.rm=T)
    print(paste0("Total weighted ",out))
    
    output$txt_fcst_low <- renderText(paste0("$",round(max(c(out*0.8, 0.5),na.rm = T),1)))
    output$txt_fcst_base <- renderText(paste0("$",round(max(c(out, 0.5),na.rm = T),1)))
    output$txt_fcst_high <- renderText(paste0("$",round(max(c(out*1.2, 0.5),na.rm = T),1)))
  })
  
  #For an edited scenario, upon confirming the sweetAlert,
  # update the row in which we see the original project/scenario names
  
  
  
  #2) Lookup Films will be reactive() -- not reactiveValues
  #   Because they only need to adjust when the dropdown is engaged
  lookup_films <- reactive({
    mms %>%
      filter(Title %in% input$lookupFilm) %>%
      mutate_at(vars(-`Release Date`),as.character)
  })
  
  observe({
    print("lookup")
    print(lookup_films())
  })
  #3) The final reference table will be a reactive val listening for
  # a - the scenario input row, and
  # b - the lookup films
  
  observe({
    print(rv_scenInputs$inputs$`DBO ($M)`)
    rv_refTable$tbl <- bind_rows(rv_scenInputs$inputs %>% 
                                   #ZM BEG
                                   mutate_at(vars(-`Release Date`),as.character) %>%
                                   mutate(scen_add = TRUE),
                                 
                                 lookup_films() %>%
                                   mutate(scen_add = FALSE))
    #ZM END
  })
  
  #4) IN THE OBSERVE BELOW: feed the reference table into renderDataTable,
  #     displaying only the user-specified visible columns
  
  #When the filters or column orders are adjusted, update the universe table and reference row
  observe(#Event({
    #    input$btn_updateFilters
    #   input$btn_updateColumns
    #  },
    {
      # print(input$columnList1_order)
      
      
      #Universe:
      output$dta_mms <- DT::renderDataTable({
        
        out <- filtered_universe()%>%
          mutate(
            `  <br/>  ` = buttonInput_select(
              FUN = actionButton,
              icon = "thumbs-up",
              len = nrow(.),
              id = paste0('button_',rownames(.),"_", round(rnorm(n = 1, mean = 1000000, sd = 200000))),
              label = ""
            ),
            ` <br/> ` = buttonInput_thumbdown(
              FUN = actionButton,
              icon = "thumbs-down",
              background = "red",
              color="white",
              len = nrow(.),
              id = paste0('button_',rownames(.),"_",round(rnorm(n=1,mean=1000000,sd=200000))),
              label = ""
            ),
            `<br/>   ` = buttonInput_undo(
              FUN = actionButton,
              icon = "undo",
              background = "teal",
              color="white",
              len = nrow(.),
              id = paste0('button_',rownames(.),"_",round(rnorm(n=1,mean=1000000,sd=200000))),
              label = ""
            ),
            
            isBad=ifelse(rownames(filtered_universe()) %in% rv_bdCmp$bad$BAD_COMPS_MCODE, 1, 0)
            
            # #Highlight Bad comps:Akshay
            #isBad = ifelse((rv_currentScenario$name %in% rv_scenarios$badcomps$SCEN_NAME) & (rownames(filtered_universe()) %in% rv_scenarios$badcomps$BAD_COMPS_MCODE), 1, 0)
            
          ) %>% select(`  <br/>  `,` <br/> `,`<br/>   `,everything())
        
        #Highlight Bad comps:Akshay
        print("bad compppsss")
        #out<-out%>%select(-Producer,-isBad)
        print(out%>%filter(stringr::str_detect(Title,"Call")))
        
        #print(colnames(out))
        ## print(head(out))
        print(rv_bdCmp$bad$BAD_COMPS_MCODE)
        style_col = "isBad"
        ## print(head(out$Add))
        ## print(head(filtered_universe()))
        #print(rownames(filtered_universe()))
        targs_large <- match(c("Cast (Major)","Synopsis","Cast (Full)"),colnames(out)) - 1
        targs_mid <- match(c("Title","Director","Executive Producer","MPAA Rating Desc","Producer","Writer","Editor","BO Unscheduled Release Description","Pattern Description Comments"),colnames(out)) - 1
        targs_small <- match(c("DBO ($M)","IBO ($M)","WBO ($M)","WWBO ($M)","Release Week Number","Short Pattern Description","Season CF Code","Title Type NK","Release Year Check","Rel CF Dt"),colnames(out))-1
        targs_ranks <- match(c("Smark Rank", "Supervised Rank","Demo Rank"),colnames(out)) - 1
        targs_icons<-match(c("  <br/>  "," <br/> ","<br/>   "),colnames(out))-1
        print("here 1")
        #print(targs_large)
        
        currency_fields <- which(str_detect(colnames(out),"\\(\\$M\\)"))
        percentage_fields<-which(str_detect(colnames(out),"\\_"))
        per_fields<-which(str_detect(colnames(out),"\\%"))
        print("here 2")
        print(percentage_fields)
        
        #colnames(out) <- paste0('<span style="color:white">',colnames(out),'</span>')
        DT::datatable(out,colnames = paste0('<span style="color:white">',colnames(out),'</span>'),
                      class = "cell-border stripe",
                      #selection = list(mode = "single", target = "row"),
                      selection = list(mode = "none"),
                      rownames = FALSE,
                      #class="nowrap display",
                      #extensions = 'Buttons',
                      extensions = c('FixedColumns','FixedHeader'),
                      plugins = "ellipsis",
                      escape = FALSE, #this argument allows for COLUMN NAMES to HAVE HTML STYLING
                      options = list(
                        initComplete = JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#000'});",
                          "}"),
                        #  dom = 'Bfrtip', buttons = I('colvis'),
                        dom='ftp',
                        #dom='lftp',
                        #rowCallback = JS("function(r,d){$(r).attr('height','100px')}"),
                        fixedColumns = list(leftColumns = 3, rightColumns = 0),
                        #lengthMenu = c(5,10,15),
                        #pageLength = 20,
                        #lengthMenu = c(5, 10, 15, 20),
                        # lengthChange=FALSE,
                        pageLength = input$n_mms,
                        scrollX=TRUE,
                        autoWidth = TRUE,
                        search = list(smart=FALSE),
                        columnDefs = list(#list(width = '500px', targets = targs_large),
                          list(width = '50px', targets=targs_ranks),
                          list(width = '150px', targets=c(targs_mid, targs_large)),
                          list(width = '70px', targets=targs_small),
                          list(width = '30px', targets=targs_icons),
                          list(targets = c(targs_mid, targs_large),
                               render = JS("$.fn.dataTable.render.ellipsis( 22, false )")
                          )),
                        language = list(
                          #lengthMenu = '<span style = "color:black">Films per page: </span>_MENU_   ',
                          search = '<span style = "color:black"></span>', #placeholder below saves space
                          searchPlaceholder = 'Search',
                          info = '<span style = "color:black">Showing _START_ to _END_ of _TOTAL_ entries</span>',
                          paginate = list(previous = '<span style = "color:black">Previous</span>', `next` = '<span style = "color:black">Next</span>')
                        )
                      )
                      #,callback = JS('table.page(table.page.info().page).draw(false);')
        )%>%formatRound(columns = currency_fields, digits = 2)%>%
          formatDate(.,~`Release Date`,method = "toLocaleDateString") %>%
          formatStyle(
            style_col,
            target = 'row',
            #fontWeight = styleEqual(1,"bold"),
            backgroundColor = styleEqual(1, c('#f4d4d8'))
          )
        #%>%
        #formatPercentage(percentage_fields,1)
        #%>%
        # formatPercentage(per_fields,1)
      }
      #,server=FALSE
      )
      # print("in mms refresh")
      # rv_checkMMSdataRefresh$i<-rv_checkMMSdataRefresh$i+1
      # print(rv_checkMMSdataRefresh$i)
      #js$pageCol("test")
      
      
      # print(paste0("before delay",input$too))
      # delay(5000,session$sendCustomMessage("pageChange", input$too))
      # print("delay executed")
      
    })
  
  proxy<-dataTableProxy('dta_mms')
  #separate observe for reference table since it only relies on the column selection, not filters
  observe({
    output$dta_reference <- renderDataTable({
      out <- rv_refTable$tbl %>%
        select(match(input$columnList1_order$text,names(.)))%>%
        #select(-synopsis,-writer,-cast,-director,-voice) %>%
        #mutate(`Supervised Rank` = NA,`Smart Rank`=NA) 
        select(-`Supervised Rank`,-`Smart Rank`,-`Demo Rank`)
      
      
      
      
      
      if(nrow(out)>0){
        out <- out %>% 
          mutate(SRB = buttonInput_smart(
            FUN = actionButton,
            len = nrow(.),
            background = "#d44500",
            icon = "gears",
            id = paste0('srb_',1:nrow(.),"_",round(rnorm(n = 1, mean = 1000000, sd = 200000))),
            label = ""
          )) %>% 
          select(SRB, everything())
      }
      
      currency_fields <- which(str_detect(colnames(out),"\\(\\$M\\)"))
      targs_large <- match(c("Cast (Major)","Synopsis","Cast (Full)"),colnames(out)) - 1
      targs_mid <- match(c("Title","Director","Executive Producer","MPAA Rating Desc","Producer","Writer","Editor","BO Unscheduled Release Description","Pattern Description Comments"),colnames(out)) - 1
      targs_small <- match(c("DBO ($M)","IBO ($M)","WBO ($M)","WWBO ($M)","Release Week Number","Short Pattern Description","Season CF Code","Title Type NK","Release Year Check","Rel CF Dt"),colnames(out))-1
      targs_ranks <- match(c("Smark Rank", "Supervised Rank"),colnames(out)) - 1
      print("col nmes in ref tab")
      print(colnames(out))
      #colnames(out) <- paste0('<span style="color:white">',colnames(out),'</span>')
      
      DT::datatable(out,colnames = paste0('<span style="color:white">',colnames(out),'</span>'),
                    class = "cell-border stripe #000000",
                    selection = list(mode="none"),
                    rownames = FALSE,
                    escape = FALSE,
                    # extensions = "Buttons",
                    extensions = 'FixedColumns',
                    plugins = "ellipsis", 
                    options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#000'});",
                        "}"),
                      fixedColumns = list(leftColumns = 2, rightColumns = 0),
                      scrollX=TRUE,
                      autoWidth = TRUE,
                      columnDefs = list(list(width = '70px', targets=targs_ranks),
                                        list(width = '150px', targets=c(targs_mid, targs_large)),
                                        list(width = '80px', targets=targs_small),
                                        list(targets = c(targs_mid, targs_large),
                                             render = JS("$.fn.dataTable.render.ellipsis( 22, false )")
                                        )),
                      
                      dom = 't'#, buttons = I('colvis')
                    )
      ) %>%
        formatStyle(columns = colnames(out),backgroundColor = "#fca77e") %>%
        #formatRound(columns = currency_fields, digits = 2) %>%
        formatDate(.,~`Release Date`,method = "toLocaleDateString")
    })
    
    
  })
  
  
  #Reset Filters
  observeEvent(ignoreInit=TRUE,c(input$btn_newSuccess,input$btn_pg_enterExistingScenario, input$btn_pg_enterNewScenario,input$btn_resetFilters),
               {
                 
                 if(input$switch_ALL == FALSE){updateSwitchInput(session,"switch_ALL", value=TRUE)}
                 if(input$switch_director == FALSE){updateSwitchInput(session,"switch_director", value=TRUE)}
                 if(input$switch_cast == FALSE){updateSwitchInput(session,"switch_cast", value=TRUE)}
                 if(input$switch_execprod == FALSE){updateSwitchInput(session,"switch_execprod", value=TRUE)}
                 if(input$switch_writer == FALSE){updateSwitchInput(session,"switch_writer", value=TRUE)}
                 if(input$switch_genre == FALSE){updateSwitchInput(session,"switch_genre", value=TRUE)}
                 if(input$switch_keyword == FALSE){updateSwitchInput(session,"switch_keyword", value=FALSE)}
                 if(input$switch_rating == FALSE){updateSwitchInput(session,"switch_rating", value=FALSE)}
                 #if(input$switch_releaseDay == FALSE){updateSwitchInput(session,"switch_releaseDay", value=FALSE)}
                 if(input$switch_releaseMonth == FALSE){updateSwitchInput(session,"switch_releaseMonth", value=FALSE)}
                 #if(input$switch_releaseWeekend == FALSE){updateSwitchInput(session,"switch_releaseWeekend", value=FALSE)}
                 
                 shinyjs::reset(id = "filter_release")
                 shinyjs::reset(id = "filter_minutes")
                 shinyjs::reset(id = "filter_tomato")
                 shinyjs::reset(id = "filter_dombo")
                 shinyjs::reset(id = "filter_intlbo")
                 shinyjs::reset(id = "filter_weekendbo")
                 shinyjs::reset(id = "filter_wwbo")
                 shinyjs::reset(id = "filter_smartrank")#Added Soumen 24.07.23
                 shinyjs::reset(id = "filter_supervrank")#Added Soumen 24.07.23
                 
                 if(!is.null(input$filter_film)||any(input$filter_film != "")){
                   updateSelectizeInput(session,"filter_film",selected="")
                 }
                 if(!is.null(input$filter_genre)||any(input$filter_genre !="")){
                   updateSelectizeInput(session,"filter_genre",selected = "")
                 }
                 if(!is.null(input$filter_mpaa)||any(input$filter_mpaa !="")){
                   updateSelectizeInput(session,"filter_mpaa",selected = "")
                 }
                 if(!is.null(input$filter_director)||any(input$filter_director !="")){
                   updateSelectizeInput(session,"filter_director",selected = "")
                 }
                 if(!is.null(input$filter_cast)||any(input$filter_cast !="")){
                   updateSelectizeInput(session,"filter_cast",selected = "")
                 }
                 if(!is.null(input$filter_voice)||any(input$filter_voice !="")){
                   updateSelectizeInput(session,"filter_voice",selected = "")
                 }
                 if(!is.null(input$filter_distributor)||any(input$filter_distributor !="")){
                   updateSelectizeInput(session,"filter_distributor",selected = "")
                 }
                 if(!is.null(input$filter_producer)||any(input$filter_producer !="")){
                   updateSelectizeInput(session,"filter_producer",selected = "")
                 }
                 if(!is.null(input$filter_execprod)||any(input$filter_execprod !="")){
                   updateSelectizeInput(session,"filter_execprod",selected = "")
                 }
                 if(!is.null(input$filter_keyword)||any(input$filter_keyword !="")){
                   updateSelectizeInput(session,"filter_keyword",selected = "")
                 }
                 if(!is.null(input$filter_studio)||any(input$filter_studio !="")){
                   updateSelectizeInput(session,"filter_studio",selected = "")
                 }
                 if(!is.null(input$filter_editor)||any(input$filter_editor !="")){
                   updateSelectizeInput(session,"filter_editor",selected = "")
                 }
                 if(!is.null(input$filter_writer)||any(input$filter_writer !="")){
                   updateSelectizeInput(session,"filter_writer",selected = "")
                 }
                 if(!is.null(input$filter_cinemascore)||any(input$filter_cinemascore !="")){
                   updateSelectizeInput(session,"filter_cinemascore",selected = "")
                 }
                 
                 #[AK TO ADD REMAINING FILTERS WHEN IN PLACE]
                 
               })
  
  
  
  
  #Table of selected comps-----
  sel3 <- reactive({!is.null(input$dta_mms_rows_selected)})
  # [AK - ALLOW FOR IMPORT OF PREVIOUS COMPS FROM EXISTING SCENARIO IF USER OPTS FOR THIS PATH]
  
  observeEvent(ignoreInit=TRUE,{input$undo_button},
               #input$dta_mms_rows_selected},
               {
                 #selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
                 print(input$undo_button)
                 print("check!! undo")
                 # print(head(mms))
                 selectedRow <- as.numeric(strsplit(input$undo_button, "_")[[1]][2])
                 print(selectedRow)
                 print(mms%>%filter(Mcode==selectedRow))
                 print("End row undo")
                 #print(rv_currentComps$currentComps)
                 #if(sel3()){
                 #Convert bad comp to good comp:Akshay Mahale
                 print(rv_bdCmp$bad)
                 
                 projId<-rv_projectscenarios$proj_id
                 
                 projectDetailsFunc(rv_projectscenarios$project_title)
                 print(paste0("PROJ ID INSERT: ",rv_SelectedDetails$Proj$APP_PROJ_KEY))
                 projId<-rv_SelectedDetails$Proj$APP_PROJ_KEY
                 
                 if(selectedRow %in% rv_bdCmp$bad$BAD_COMPS_MCODE)
                 {
                   a<-rv_bdCmp$bad%>%filter(BAD_COMPS_MCODE==selectedRow)
                   print("in if bad to good undo")
                   #myqueryDel<-paste0('DELETE FROM THEATRICAL_LIGHTHOUSE.ODS.APP_BAD_COMPS WHERE BAD_COMPS_MCODE = ',"'",a$BAD_COMPS_MCODE,"'",' AND SCEN_ID = ',"'",a$SCEN_ID,"'",'AND PROJ_ID=',projId)   #T20 Soumen 23.07.24
                   myqueryDel<-paste0('DELETE FROM ',database,'.ODS.APP_BAD_COMPS WHERE BAD_COMPS_MCODE = ',"'",a$BAD_COMPS_MCODE,"'",' AND SCEN_ID = ',"'",a$SCEN_ID,"'",'AND PROJ_ID=',projId)
                   print(paste0("Delete query ",myqueryDel))
                   dbGetQuery(my_db,myqueryDel)
                   print("After query undo")
                   rv_bdCmp$bad <- rv_bdCmp$bad %>% filter(BAD_COMPS_MCODE != selectedRow)
                   print("bad to good")
                   print(rv_bdCmp$bad)
                 }
                 
               }
  )
  
  
  
  
  #When a new selection is made, append to existing comps selected
  observeEvent(ignoreInit=TRUE,{input$select_button},
               #input$dta_mms_rows_selected},
               {
                 #selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
                 print(input$select_button)
                 print("check!!")
                 ## print(head(mms))
                 selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
                 print(mms%>%filter(Mcode==selectedRow))
                 print("End row")
                 #print(rv_currentComps$currentComps)
                 #if(sel3()){
                 #Convert bad comp to good comp:Akshay Mahale
                 print(rv_bdCmp$bad)
                 
                 projectDetailsFunc(rv_projectscenarios$project_title)
                 print(paste0("PROJ ID INSERT: ",rv_SelectedDetails$Proj$APP_PROJ_KEY))
                 projId<-rv_SelectedDetails$Proj$APP_PROJ_KEY
                 
                 if(selectedRow %in% rv_bdCmp$bad$BAD_COMPS_MCODE)
                 {
                   a<-rv_bdCmp$bad%>%filter(BAD_COMPS_MCODE==selectedRow)
                   print("in if bad to good")
                   #myqueryDel<-paste0('DELETE FROM THEATRICAL_LIGHTHOUSE.ODS.APP_BAD_COMPS WHERE BAD_COMPS_MCODE = ',"'",a$BAD_COMPS_MCODE,"'",' AND SCEN_ID = ',"'",a$SCEN_ID,"'",'AND PROJ_ID=',projId)   #T20 Soumen 23.07.24
                   myqueryDel<-paste0('DELETE FROM ',database,'.ODS.APP_BAD_COMPS WHERE BAD_COMPS_MCODE = ',"'",a$BAD_COMPS_MCODE,"'",' AND SCEN_ID = ',"'",a$SCEN_ID,"'",'AND PROJ_ID=',projId)   
                   print(paste0("Delete query ",myqueryDel))
                   dbGetQuery(my_db,myqueryDel)
                   rv_bdCmp$bad <- rv_bdCmp$bad %>% filter(BAD_COMPS_MCODE != selectedRow)
                   print("bad to good")
                   print(rv_bdCmp$bad)
                 }
                 
                 rv_currentComps$currentComps <- bind_rows(rv_currentComps$currentComps,
                                                           mms %>%
                                                             filter(Mcode == selectedRow)#rownames(filtered_universe())[selectedRow])
                                                           #slice(selectedRow)
                                                           #  filter(Mcode %in% rownames(filtered_universe())[input$dta_mms_rows_selected])#%>%
                                                           #filter(Mcode %in% rownames(filtered_universe())[selectedRow])#%>%
                                                           #select(-synopsis,-writer,-cast,-director,-voice)
                 )%>%
                   select(-`Supervised Rank`,-`Smart Rank`,-`Demo Rank`) %>%
                   distinct()
                 
                 rv_currentComps$finalComps <- c(rv_currentComps$finalComps, selectedRow)
                 
                 print("new row")
                 print(rv_currentComps$currentComps)
                 print(rv_currentComps$finalComps)
                 print("replaceData")
                 print(selectedRow)
                 # print(colnames(filtered_universe()))
                 # print(dim(filtered_universe()))
                 # newTable=filtered_universe()[-which(as.integer(rownames(filtered_universe()))==selectedRow),]
                 # print(dim(newTable))
                 # session$sendCustomMessage('unbind-DT', 'dta_mms')
                 # DT::replaceData(proxy,newTable,resetPaging = FALSE)
                 
                 # 
                 # print(mms %>% filter(Mcode %in% rownames(filtered_universe())[input$dta_mms_rows_selected]))
                 # print(rv_currentComps$currentComps)
                 # print(input$columnList1_order)
                 # print(match(input$columnList1_order,names(rv_currentComps$currentComps)))
                 #print( rownames(filtered_universe()))
                 #}
               }
  )
  
  #Text Outputs for Model and Numeric Averages
  observe({
    
    #Numeric Averages:
    #print(names(rv_currentComps$currentComps))
    finals <- rv_currentComps$currentComps %>% filter(Mcode %in% rv_currentComps$finalComps)
    mn_dbo <- mean(finals %>% select(`DBO ($M)`) %>% pull(), na.rm=T)
    mn_ibo <- mean(finals %>% select(`IBO ($M)`) %>% pull(), na.rm=T)
    mn_wwbo <- mean(finals %>% select(`WWBO ($M)`) %>% pull(), na.rm=T)
    mn_fss <- mean(finals %>% select(`WBO ($M)`) %>% pull(), na.rm=T)
    output$txt_mn_dbo <- renderText(paste0("$",round(mn_dbo, digits = 1)))
    output$txt_mn_ibo <- renderText(paste0("$",round(mn_ibo, digits = 1)))
    output$txt_mn_wwbo <- renderText(paste0("$",round(mn_wwbo, digits = 1)))
    output$txt_mn_fss <- renderText(paste0("$",round(mn_fss, digits = 1)))
    #n <- sum(!is.na(rv_currentComps$currentComps%>% filter(Mcode %in% rv_currentComps$finalComps) %>% select(`DBO ($M)`) %>% pull()))
    #compute standard deviation of selected comps (recall, R's default sd() computes the sample sd w/ Bessel's Correction n-1 )
    #sd_dbo <- ifelse(n>0, sd(rv_currentComps$currentComps%>% filter(Mcode %in% rv_currentComps$finalComps) %>% select(`DBO ($M)`) %>% pull(),na.rm=T)*(n-1)/n ,10)
    #output$txt_mid <- renderText(paste0("Base: $",max(c(0.5, round(mn,digits = 1)), na.rm=T)))
    #output$txt_low <- renderText(paste0("Low: $",max(c(0.5, round(mn - 2*sd_dbo, digits = 1)), na.rm = TRUE)))
    #output$txt_high <- renderText(paste0("High: $",max(c(0.5, round(mn + 2*sd_dbo, digits = 1), round(mn,digits = 1)),na.rm=T)))
    
    
    #For the forecasting model:
    finals_adj <- left_join(finals, avg_ticket_price, by = c("Release Year" = "yr"))
    current_price<-avg_ticket_price %>% filter(yr==max(yr)) %>% select(price) %>% pull()
    
    
    finals_adj <- finals_adj %>% 
      mutate(dbo_adj = `DBO ($M)`*1000000/price * current_price/1000000)
    
    
    rv_model_fcst$mod_comps <- mean(finals_adj$dbo_adj,na.rm=T)
    print(paste0("Comp Avg",mean(finals_adj$dbo_adj,na.rm=T)))
    
    
    
  })
  
  #update the current comps table whenever a comp is added/removed/checked as final
  observe({
    #DO NOT REMOVE OR COMMENT OUT THE FOLLOWING PRINTS:
    print("head of current comps")
    # print(head(rv_currentComps$currentComps$Title))
    print(input$final_chk)
    
    output$dta_currentComps <- renderDataTable({
      out <- rv_currentComps$currentComps %>%
        mutate(MCODE_MASTER = Mcode) %>% 
        select(match(input$columnList1_order$text[-which(input$columnList1_order$text %in% c("Smart Rank","Supervised Rank","Demo Rank"))],names(.)), MCODE_MASTER)
      
      if(nrow(out) > 0){
        
        chk_flg <- out$MCODE_MASTER %in% rv_currentComps$finalComps
        
        out <- out%>% 
          mutate(Final = checkboxColumn(len = nrow(.),
                                        id = paste0('chkbx_',MCODE_MASTER,"_",round(rnorm(n=1, mean = 1000000, sd = 200000))),
                                        chk = chk_flg))%>% 
          mutate(` </br> ` = buttonInput_delete(
            FUN = actionButton,
            background = "red",
            len = nrow(.),
            id = paste0('del_',MCODE_MASTER,"_", round(rnorm(n = 1, mean = 1000000, sd = 200000))),
            icon = "times",
            label = "")
          )  %>% 
          select(` </br> `, Final, everything()) %>% 
          mutate(chkfinal = ifelse(MCODE_MASTER %in% rv_currentComps$finalComps, 1, 0)) %>% 
          select(-MCODE_MASTER)
        
        style_col = "chkfinal"
      }else{
        style_col = "MCODE_MASTER"
      }
      print(rv_currentComps$finalComps)
      print(rv_currentScenario$full_detail$`Final Comps Mcodes`)
      print(rv_currentScenario$full_detail$`Selected Comps Mcodes`)
      #print(out$chk_final)
      
      targs_large <- match(c("Cast (Major)","Synopsis","Cast (Full)"),colnames(out)) - 1
      targs_mid <- match(c("Title","Director","Executive Producer","MPAA Rating Desc","Producer","Writer","Editor","BO Unscheduled Release Description","Pattern Description Comments"),colnames(out)) - 1
      targs_small <- match(c("DBO ($M)","IBO ($M)","WBO ($M)","WWBO ($M)","Release Week Number","Short Pattern Description","Season CF Code","Title Type NK","Release Year Check","Rel CF Dt"),colnames(out))-1
      targs_ranks <- match(c("Smark Rank", "Supervised Rank","Demo Rank"),colnames(out)) - 1
      currency_fields <- which(str_detect(colnames(out),"\\(\\$M\\)"))
      percentage_fields<-which(str_detect(colnames(out),"\\_"))
      per_fields<-which(str_detect(colnames(out),"\\%"))
      print("percentage fields")
      print(percentage_fields)
      
      # percentage_fieldsAge<-which(str_detect(colnames(out),"Age"))
      # percentage_fieldsEthn<-which(str_detect(colnames(out),"Ethn"))
      # 
      
      
      #if(sel3()){
      #  colnames(out) <- paste0('<span style="color:white">',colnames(out),'</span>')
      #}
      DT::datatable(out,colnames = paste0('<span style="color:white">',colnames(out),'</span>'),
                    class = "cell-border stripe",
                    #selection = list(mode = 'multiple', target = 'row'),
                    selection = list(mode = 'none'),
                    rownames = FALSE,
                    escape = FALSE,
                    extensions = 'FixedColumns',
                    plugins = "ellipsis",
                    options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#000'});",
                        "}"),
                      dom="t",
                      paging = FALSE,
                      fixedColumns = list(leftColumns = 3, rightColumns = 0),
                      #lengthMenu = list(c(5, 10, -1),c('5','10','all')),
                      scrollX=TRUE,
                      autoWidth = TRUE,
                      columnDefs = list(list(width = '70px', targets=targs_ranks),
                                        list(width = '150px', targets=c(targs_mid, targs_large)),
                                        list(width = '80px', targets=targs_small),
                                        list(targets = c(targs_mid, targs_large),
                                             render = JS("$.fn.dataTable.render.ellipsis( 22, false )")
                                        )),
                      language = list(
                        zeroRecords = '<span style=float:left;margin-left:570px;color:red>NO COMPS SELECTED.<br/>SELECT COMPS BY CLICKING IN THE TABLE OF ALL COMPS BELOW.</span>',
                        #lengthMenu = '<span style = "color:black">Comps per page: </span>_MENU_   ',
                        #search = '<span style = "color:black"></span>', #placeholder below saves space
                        searchPlaceholder = 'Search',
                        info = '<span style = "color:black">Showing _START_ to _END_ of _TOTAL_ entries</span>'
                        #paginate = list(previous = '<span style = "color:black">Previous</span>', `next` = '<span style = "color:black">Next</span>')
                      )
                    )
      )%>%
        formatRound(columns = currency_fields, digits = 2) %>%
        formatDate(.,~`Release Date`,method = "toLocaleDateString")%>% 
        formatStyle(
          style_col,
          target = 'row',
          fontWeight = styleEqual(1,"bold"),
          backgroundColor = styleEqual(1, c('#ddf4d4'))
        )
      #%>%formatPercentage(percentage_fields,1)
      #%>%formatPercentage(per_fields,1)
    })
  })
  
  
  
  #When btn_removeSelectedComps activated, remove rows from table
  # sel4 <- reactive({!is.null(input$dta_currentComps_rows_selected)})
  # observe({
  #   shinyjs::toggleState("btn_removeSelectedComps",condition = sel4())
  # })
  # observeEvent(ignoreInit=TRUE,input$btn_removeSelectedComps,{
  #   #modal to confirm they will delete rows:
  #   confirmSweetAlert(session,
  #                     inputId = "alert_remove_comps",
  #                     type = "info",
  #                     danger_mode = T,
  #                     text = "Are you sure you want to remove the selected comps?")
  # })
  
  observeEvent(ignoreInit=TRUE,input$delete_button, {
    selectedRow <- as.numeric(strsplit(input$delete_button, "_")[[1]][2])
    rv_currentComps$currentComps <- rv_currentComps$currentComps %>% filter(Mcode != selectedRow)
    print(rv_currentComps$finalComps)
    rv_currentComps$finalComps <- setdiff(rv_currentComps$finalComps, selectedRow)
    print(rv_currentComps$finalComps)
    print("bad comps in delete")
    print(rv_bdCmp$bad)
  })
  
  ##### AK - UPDATE THIS TO REVISE FINAL COMPS SELECTED
  ##  ALSO NEED TO ADD THIS AS A THIRD LISTENER FOR THE OBSERVEEVENT THAT FOLLOWS
  # observeEvent(ignoreInit=TRUE,input$checkBoxes,{
  #   
  # })
  
  
  #MAKE SURE THE USER IS ONLY ABLE To SAVE THE SELECTION OF COMPS WHEN A VALID SCENARIO NAME IS PROVIDED
  observeEvent(ignoreInit=TRUE,c(input$select_button,
                                 input$delete_button,
                                 input$final_chk
  ),
  {
    #print("!!!!")
    print(rv_flgNewOrEdit$flg)
    if(rv_flgNewOrEdit$flg =="Edit"){
      #   shinyjs::enable("btn_saveSelectedComps")
      
      rv_currentCompsLength$t_1 <- rv_currentCompsLength$t_0
      rv_currentCompsLength$t_0 <- nrow(rv_currentComps$currentComps)
      
      titles <- paste(rv_currentComps$currentComps$Title,collapse = ", ")
      mcodes <- paste(rv_currentComps$currentComps$Mcode,collapse = ", ")
      final_mcodes <- paste(rv_currentComps$finalComps, collapse=", ")
      
      print(rv_currentComps$currentComps)
      print("OUT CODES")
      print(titles)
      print(mcodes)
      print(final_mcodes)
      print(length(mcodes))
      print(length(final_mcodes))
      
      #update the selected scenario so our reports update appropriately
      print(rv_currentScenario$full_detail)
      rv_currentScenario$full_detail$`Selected Comps`[1] <- titles
      rv_currentScenario$full_detail$`Selected Comps Mcodes`[1] <- mcodes
      rv_currentScenario$full_detail$`Final Comps Mcodes`[1] <- final_mcodes
      
      
      #QUERY - PENDING - UPDATE SCENARIO TABLE WHEN COMP IS ADDED/REMOVED/CHECKED FINAL-----
      dbGetQuery(my_db, 
                 #paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO" ',"SET SELECTED_COMPS= $$",titles,  #T20 Soumen 23.07.24
                 paste0('UPDATE ',database,'."ODS"."APP_SCENARIO" ',"SET SELECTED_COMPS= $$",titles, 
                        "$$,SELECTED_COMPS_MCODES= '",mcodes,
                        "',FINAL_COMPS_MCODES= '",final_mcodes,
                        "',SCEN_LAST_MODIFIED= '",Sys.time(),
                        "',SCEN_MODIFIED_BY = '",session$user,
                        "'WHERE SCEN_NAME = $$",rv_currentScenario$name,
                        "$$ AND PROJ_TITLE = $$",rv_projectscenarios$project_title,"$$;"))
      #Read/Overwrite/Save Scenarios
      #rv_scenarios$scens <- readRDS("data/000_scenarios.RDS")
      
      rv_scenarios$scens <- rv_scenarios$scens %>%
        mutate(`Selected Comps` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, titles, `Selected Comps`),
               `Selected Comps Mcodes` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, mcodes, `Selected Comps Mcodes`),
               `Final Comps Mcodes` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, final_mcodes, `Final Comps Mcodes`)
        )
      
      #saveRDS(rv_scenarios$scens, "data/000_scenarios.RDS")
      
      #QUERY - PENDING - UPDATE PROJECT LAST SCENARIO DATE WHEN UPDATES MADE
      # rv_projectList$projs <- readRDS("data/000_projects.RDS")
      rv_projectList$projs <- rv_projectList$projs %>% mutate(`Last Scenario` = if_else(Title == rv_projectscenarios$project_title, Sys.time(), `Last Scenario`))
      # saveRDS(rv_projectList$projs, "data/000_projects.RDS")
      #all_projects$projs <- readRDS("data/000_projects.RDS")
      all_projects$projs <- all_projects$projs %>% mutate(`Last Scenario` = if_else(Title == rv_projectscenarios$project_title, Sys.time(), `Last Scenario`))
      #saveRDS(all_projects$projs,"data/000_projects.RDS")
      
      #sendSweetAlert(session, title = "Successfully Saved Selected Comps", type = "success")
    }                 
  }
  )
  
  #FORECAST SERVER-----
  #Repeat table of selected comps for the forecast page-----
  # output$dta_fcstAllComps <- renderDataTable({
  #   out <- rv_currentComps$currentComps
  # 
  #   targs_large <- match(c("Cast (Major)","Synopsis","Cast (Full)"),colnames(out)) - 1
  #   targs_mid <- match(c("Title","Director","Executive Producer","MPAA Rating Desc","Producer","Writer","Editor","BO Unscheduled Release Description","Pattern Description Comments"),colnames(out)) - 1
  #   targs_small <- match(c("Domestic Box Office ($M)","International Box Office ($M)","Weekend Box Office ($M)","WW Box Office ($M)","Release Week Number","Short Pattern Description","Season CF Code","Title Type NK","Release Year Check","Rel CF Dt"),colnames(out))-1
  #   #print(targs_large)
  # 
  #   currency_fields <- which(str_detect(colnames(out),"\\(\\$M\\)"))
  # 
  # 
  #   #if(sel3()){
  #   #  colnames(out) <- paste0('<span style="color:white">',colnames(out),'</span>')
  #   #}
  #   DT::datatable(out,colnames = paste0('<span style="color:white">',colnames(out),'</span>'),
  #                 class = "cell-border stripe",
  #                 selection = list(mode = 'multiple', target = 'row'),
  #                 rownames = FALSE,
  #                 escape = FALSE,
  #                 extensions = 'FixedColumns',
  #                 options = list(
  #                   initComplete = JS(
  #                     "function(settings, json) {",
  #                     "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#000'});",
  #                     "}"),
  #                   #dom="t",
  #                   fixedColumns = list(leftColumns = 1, rightColumns = 0),
  #                   lengthMenu = list(c(5, 10, -1),c('5','10','all')),
  #                   scrollX=TRUE,
  #                   autoWidth = TRUE,
  #                   columnDefs = list(list(width = '500px', targets = targs_large),
  #                                     list(width = '200px', targets=targs_mid),
  #                                     list(width = '150px', targets=targs_small)),
  #                   language = list(
  #                     lengthMenu = '<span style = "color:white">Comps per page: </span>_MENU_   ',
  #                     search = '<span style = "color:white"></span>', #placeholder below saves space
  #                     searchPlaceholder = 'Search',
  #                     info = '<span style = "color:white">Showing _START_ to _END_ of _TOTAL_ entries</span>',
  #                     paginate = list(previous = '<span style = "color:white">Previous</span>', `next` = '<span style = "color:white">Next</span>')
  #                   )
  #                 )
  #   )%>%
  #     formatRound(columns = currency_fields, digits = 2) %>%
  #     formatDate(.,~`Release Date`,method = "toDateString")
  # })
  
  
  #Forecast Visual 1 - Boxplot of Box Office revenues w/ points overlaid
  # observe({
  #   output$plot_fcst1 <- renderPlotly({
  #     print(input$dta_fcstAllComps_rows_selected)
  # 
  #     dta <- rv_currentComps$currentComps %>% slice(input$dta_fcstAllComps_rows_selected) %>% mutate(measure="Domestic Box Office")
  # 
  #     var1 <- sym("measure")#the placeholder
  #     var2 <- sym("Domestic Box Office ($M)")#the data
  # 
  #     ggplot(dta, aes(x = (!!var1), y = (!!var2), text = Title))+
  #       geom_boxplot()+
  #       geom_jitter(position = position_jitter(width = .02,height = 0))+
  #       xlab(label = "")
  #   })
  # })
  
  
  
  #Report-----
  output$report_comps2 <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0("Comps Report - ",Sys.Date(),".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      # # src <- normalizePath('rmd_report_templates/report1.Rmd')
      # # owd <- setwd(tempdir())
      # # on.exit(setwd(owd))
      # 
      # # tempReport <- file.path(tempdir(), "report.Rmd")
      # # file.copy("report1.Rmd", tempReport, overwrite = TRUE)
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("rmd_report_templates/comps_template.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(report_title = rv_projectscenarios$project_title,
                     scen = rv_currentScenario$full_detail,
                     directors = rv_currentScenario$full_detail$Director,
                     cast = rv_currentScenario$full_detail$`Cast (Major)`,
                     genre = rv_currentScenario$full_detail$`Primary Genre`,
                     budget = rv_currentScenario$full_detail$`Production Budget ($M)`,
                     comps = rv_currentComps$currentComps$Title,
                     mms = mms,
                     avg_ticket_price = avg_ticket_price
      )
      #print(params)
      #print(str(params))
      
      # Knit the document, passing in the `params` list, and evaluate it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      #file.rename(out, file)
    }
  )
  
  
  #Button to proceed to forecasts page-----
  # 
  #   observeEvent(ignoreInit=TRUE,ignoreInit=TRUE,input$btn_pg_ScenarioToForecast,{
  #     print(rv_flgNewOrEdit$flg)
  #     if(rv_flgNewOrEdit$flg == "Edit"){
  #       output$page <- renderUI(ui5)
  #       #print(rv_currentScenario$full_detail %>% select(Low) %>% pull())
  # 
  #       low <- rv_currentScenario$full_detail$Low# %>% select(Low) %>% pull()
  #       #mid <- rv_currentScenario$full_detail %>% select(Mid) %>% pull()
  #       mid <- rv_currentScenario$full_detail$Mid
  #       high <- rv_currentScenario$full_detail$High
  #       print(low)
  # 
  #       updateNumericInput(session,"fcst_low",value = rv_currentScenario$full_detail$Low)
  #       updateNumericInput(session,"fcst_mid",value = rv_currentScenario$full_detail %>% select(Mid) %>% pull())
  #       updateNumericInput(session,"fcst_high",value = rv_currentScenario$full_detail %>% select(High) %>% pull())
  #     }else{
  #       sendSweetAlert(session, title = "Cannot proceed to forecasts",text = "Enter a valid scenario name and any additional details before proceeding.",type = "warning")
  #     }
  # 
  #   })
  
  #Button to return from Forecasts back to Scenario Inputs-----
  # observeEvent(ignoreInit=TRUE,ignoreInit=TRUE,input$btn_pg_ForecastToScenario,{
  #   output$page <- renderUI(ui4)
  #   #[AK - likely that we will need to again update inputs on the Scenario page]
  #   #[much like we are doing when they click "edit existing scenario"]
  #   #[the selected comps already stay in place though!!! so we would really just need to
  #   # keep the scenario inputs intact]
  # })
  
  
  
  #FORECAST PAGE SERVER:-----
  #Reference Table for how what % the F/R value will take on based on the forecast value
  fr_ref <- data.frame(bound_low = c(0,  30,  40,  50,  60,  75,  90,  110,  120,  130,  140,  150,  175,  200,  225,  250,  275,  300,  350),
                       bound_hi = c(29.999,39.999,49.999,59.999,74.999,89.999,109.999,119.999,129.999,139.999,149.999,174.999,199.999,224.999,249.999,274.999,299.999,349.999,1000000000),
                       desc = c("0-30",  "30-40",  "40-50",  "50-60",  "60-75",  "75-90",  "90-110",  "110-120",  "120-130",  "130-140",  "140-150",  "150-175",  "175-200",  "200-225",  "225-250",  "250-275",  "275-300",  "300-350",  "350+"),
                       pct = c(.4700,  .4800,  .4850,  .4900,  .5000,  .5000,  .5050,  .5050,  .5050,  .5100,  .5200,  .5200,  .5300,  .5350,  .5400,  .5450,  .5500,  .5600,  .5600),stringsAsFactors = F)
  
  observeEvent(ignoreInit=TRUE,c(input$fcst_low,input$threed_low,input$imax_low),{
    print("here1")
    #fr_low <- fr_ref %>% mutate(flg = abs(as.numeric(input$fcst_low) - bound)) %>% arrange(flg) %>% slice(1) %>% mutate(fr = round(pct *  as.numeric(input$fcst_low)/.5)*.5) %>% select(fr) %>% pull()
    fr_low <- fr_ref %>% filter(bound_low <= as.numeric(input$fcst_low) & as.numeric(input$fcst_low) < bound_hi) %>% mutate(fr = round(pct *  as.numeric(input$fcst_low)/.5)*.5) %>% select(fr) %>% pull()
    print(fr_low)
    print(fr_ref)
    print("here2")
    updateTextInput(session,"fr_low",placeholder = paste0("*",round(as.numeric(fr_low),1) ))
    print(paste0("*",round(as.numeric(fr_low),1) ))
    
    scen_yr <- ifelse(length(input$scenario_release)==0||is.null(input$scenario_release)||is.na(input$scenario_release),2020,as.numeric(year(input$scenario_release)))
    
    print_low <- data.frame(type = c("Low"),
                            g_gross = as.numeric(c(input$fcst_low)),
                            yr = scen_yr,
                            a_printct = c(3700),
                            b_avg_cost_per_print = ifelse(scen_yr <= 2020, 822, 800),
                            c_paid_vpf_pct = ifelse(scen_yr <= 2018, .7,
                                                    ifelse(scen_yr %in% c(2019:2021), .63,
                                                           ifelse(scen_yr == 2022, .59, .24))),
                            stringsAsFactors = F
    ) %>% 
      mutate(d_vpf_cost = a_printct * b_avg_cost_per_print * c_paid_vpf_pct / 1000000) %>% 
      mutate(e_dc_drive_cost = d_vpf_cost*0.1) %>% 
      mutate(f_total_vpf_drive_cost = d_vpf_cost + e_dc_drive_cost) %>% 
      mutate(h_imax_pct = as.numeric(input$imax_low)/100) %>% 
      mutate(i_imax_gross = g_gross * h_imax_pct) %>% 
      mutate(j_total_imax_dmr = i_imax_gross*0.125) %>% 
      mutate(k_3d_gross = g_gross - i_imax_gross) %>% 
      mutate(l_3d_glass_pct = as.numeric(input$threed_low)/100) %>% 
      mutate(m_avg_3d_ticket_price = 11.25) %>% 
      mutate(n_glasses_cost = .35) %>% 
      mutate(o_total_3d_glasses = k_3d_gross*l_3d_glass_pct*n_glasses_cost/m_avg_3d_ticket_price) %>% 
      mutate(p_other = 0.3) %>% 
      mutate(prints = d_vpf_cost + e_dc_drive_cost + j_total_imax_dmr + o_total_3d_glasses + p_other) %>% 
      select(prints) %>% 
      pull()
    
    #updateTextInput(session, "prints_low", placeholder = HTML(paste0("**",round(as.numeric(print_low),1) )))
  })
  
  observeEvent(ignoreInit=TRUE,c(input$fcst_mid,input$threed_mid, input$imax_mid),{
    print("here1")
    print(as.numeric(input$fcst_mid))
    #fr_mid <- fr_ref %>% mutate(flg = abs(as.numeric(input$fcst_mid) - bound)) %>% arrange(flg) %>% slice(1) %>% mutate(fr = round(pct *  as.numeric(input$fcst_mid)/.5)*.5) %>% select(fr) %>% pull()
    fr_mid <- fr_ref %>% filter(bound_low <= as.numeric(input$fcst_mid) & as.numeric(input$fcst_mid) < bound_hi) %>% mutate(fr = round(pct *  as.numeric(input$fcst_mid)/.5)*.5) %>% select(fr) %>% pull()
    print(fr_mid)
    print("here2")
    updateTextInput(session,"fr_mid",placeholder = HTML(paste0("*",round(as.numeric(fr_mid),1) )))
    
    scen_yr <- ifelse(length(input$scenario_release)==0||is.null(input$scenario_release)||is.na(input$scenario_release),2020,as.numeric(year(input$scenario_release)))
    
    print_mid <- data.frame(type = c("Base"),
                            g_gross = as.numeric(c(input$fcst_mid)),
                            yr = scen_yr,
                            a_printct = c(4000),
                            b_avg_cost_per_print = ifelse(scen_yr <= 2020, 822, 800),
                            c_paid_vpf_pct = ifelse(scen_yr <= 2018, .7,
                                                    ifelse(scen_yr %in% c(2019:2021), .63,
                                                           ifelse(scen_yr == 2022, .59, .24))),
                            stringsAsFactors = F
    ) %>% 
      mutate(d_vpf_cost = a_printct * b_avg_cost_per_print * c_paid_vpf_pct / 1000000) %>% 
      mutate(e_dc_drive_cost = d_vpf_cost*0.1) %>% 
      mutate(f_total_vpf_drive_cost = d_vpf_cost + e_dc_drive_cost) %>% 
      mutate(h_imax_pct = as.numeric(input$imax_mid)/100) %>% 
      mutate(i_imax_gross = g_gross * h_imax_pct) %>% 
      mutate(j_total_imax_dmr = i_imax_gross*0.125) %>% 
      mutate(k_3d_gross = g_gross - i_imax_gross) %>% 
      mutate(l_3d_glass_pct = as.numeric(input$threed_mid)/100) %>% 
      mutate(m_avg_3d_ticket_price = 11.25) %>% 
      mutate(n_glasses_cost = .35) %>% 
      mutate(o_total_3d_glasses = k_3d_gross*l_3d_glass_pct*n_glasses_cost/m_avg_3d_ticket_price) %>% 
      mutate(p_other = 0.3) %>% 
      mutate(prints = d_vpf_cost + e_dc_drive_cost + j_total_imax_dmr + o_total_3d_glasses + p_other) %>% 
      select(prints) %>% 
      pull()
    
    #updateTextInput(session, "prints_mid", placeholder = HTML(paste0("**",round(as.numeric(print_mid),1) )))
  })
  
  observeEvent(ignoreInit=TRUE,c(input$fcst_high,input$threed_high,input$imax_high),{
    print("here1?")
    print(as.numeric(input$fcst_high))
    #fr_high <- fr_ref %>% mutate(flg = abs(as.numeric(input$fcst_high) - bound)) %>% arrange(flg) %>% slice(1) %>% mutate(fr = round(pct *  as.numeric(input$fcst_high)/.5)*.5) %>% select(fr) %>% pull()
    fr_high <- fr_ref %>% filter(bound_low <= as.numeric(input$fcst_high) & as.numeric(input$fcst_high) < bound_hi) %>% mutate(fr = round(pct *  as.numeric(input$fcst_high)/.5)*.5) %>% select(fr) %>% pull()
    print("here2")
    updateTextInput(session,"fr_high", placeholder = HTML(paste0("*",round(as.numeric(fr_high),1) )))
    
    print(input$scenario_release)
    scen_yr <- ifelse(length(input$scenario_release)==0||is.null(input$scenario_release)||is.na(input$scenario_release),2020,as.numeric(year(input$scenario_release)))
    print(scen_yr)
    
    print_high <- data.frame(type = c("High"),
                             g_gross = as.numeric(c(input$fcst_high)),
                             yr = scen_yr,
                             a_printct = c(4300),
                             b_avg_cost_per_print = ifelse(scen_yr <= 2020|is.na(scen_yr), 822, 800),
                             c_paid_vpf_pct = ifelse(scen_yr <= 2018, .7,
                                                     ifelse(scen_yr %in% c(2019:2021), .63,
                                                            ifelse(scen_yr == 2022, .59, .24))),
                             stringsAsFactors = F
    ) %>% 
      mutate(d_vpf_cost = a_printct * b_avg_cost_per_print * c_paid_vpf_pct / 1000000) %>% 
      mutate(e_dc_drive_cost = d_vpf_cost*0.1) %>% 
      mutate(f_total_vpf_drive_cost = d_vpf_cost + e_dc_drive_cost) %>% 
      mutate(h_imax_pct = as.numeric(input$imax_high)/100) %>% 
      mutate(i_imax_gross = g_gross * h_imax_pct) %>% 
      mutate(j_total_imax_dmr = i_imax_gross*0.125) %>% 
      mutate(k_3d_gross = g_gross - i_imax_gross) %>% 
      mutate(l_3d_glass_pct = as.numeric(input$threed_high)/100) %>% 
      mutate(m_avg_3d_ticket_price = 11.25) %>% 
      mutate(n_glasses_cost = .35) %>% 
      mutate(o_total_3d_glasses = k_3d_gross*l_3d_glass_pct*n_glasses_cost/m_avg_3d_ticket_price) %>% 
      mutate(p_other = 0.3) %>% 
      mutate(prints = d_vpf_cost + e_dc_drive_cost + j_total_imax_dmr + o_total_3d_glasses + p_other)
    print(print_high)
    print_high <- print_high %>% 
      select(prints) %>% 
      pull()
    print("print test")
    print(print_high)
    
    #updateTextInput(session, "prints_high", placeholder = HTML(paste0("**",round(as.numeric(print_high),1) )))
  })
  
  observeEvent(ignoreInit=TRUE,input$btn_SaveForecasts,{
    print("check1")
    #Read/Overwrite/Save Scenarios
    
    # print(fr_low)
    # print(fr_mid)
    # print(fr_high)
    print("check2")
    print(input$scenario_release)
    print(str(input$scenario_release))
    print("check3")
    #print(str(rv_scenarios$scens))
    
    
    print("check5")
    print("check6")
    
    #print(ifelse(rv_scenarios$scens$Title == rv_projectscenarios$project_title & rv_scenarios$scens$Name == rv_currentScenario$name, as.numeric(input$adpub_mid), "ERR"))
    #print(rv_scenarios$scens %>% filter(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name))
    
    #QUERY - PENDING - UPDATE SCENARIO TABLE WHEN A FORECAST IS SUBMITTED-----
    #rv_scenarios$scens <- readRDS("data/000_scenarios.RDS")
    rv_scenarios$scens <- rv_scenarios$scens %>%
      mutate(`Notes` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, input$fcst_notes,`Notes`),
             `Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$fcst_low), `Low`),
             `Base` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$fcst_mid), `Base`),
             `High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$fcst_high), `High`),
             #Int Fcst: Akshay Mahale
             `Int Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$int_fcst_low), as.numeric(`Int Low`)),
             `Int Base` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$int_fcst_mid), as.numeric(`Int Base`)),
             `Int High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$int_fcst_high), as.numeric(`Int High`)),
             
             Submitted = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, input$scenario_submitted, Submitted),
             `Imax Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$imax_low), `Imax Low`),
             `Imax Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$imax_mid), `Imax Mid`),
             `Imax High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$imax_high), `Imax High`),
             `ThreeD Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$threed_low), `ThreeD Low`),
             `ThreeD Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$threed_mid), `ThreeD Mid`),
             `ThreeD High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$threed_high), `ThreeD High`),
             `FR Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$fr_low), as.numeric(`FR Low`)),
             `FR Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$fr_mid), as.numeric(`FR Mid`)),
             `FR High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$fr_high), as.numeric(`FR High`)),
             `Ad/Pub Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$adpub_low), as.numeric(`Ad/Pub Low`)),
             `Ad/Pub Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$adpub_mid), as.numeric(`Ad/Pub Mid`)),
             `Ad/Pub High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$adpub_high), as.numeric(`Ad/Pub High`)),
             `Prints Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, round(as.numeric(input$prints_low),1), as.numeric(`Prints Low`)),
             `Prints Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, round(as.numeric(input$prints_mid),1), as.numeric(`Prints Mid`)),
             `Prints High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, round(as.numeric(input$prints_high),1), as.numeric(`Prints High`)),
             
             `Int FR Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$int_fr_low), as.numeric(`Int FR Low`)),
             `Int FR Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$int_fr_mid), as.numeric(`Int FR Mid`)),
             `Int FR High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$int_fr_high), as.numeric(`Int FR High`)),
             `Int Ad/Pub Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$int_adpub_low), as.numeric(`Int Ad/Pub Low`)),
             `Int Ad/Pub Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$int_adpub_mid), as.numeric(`Int Ad/Pub Mid`)),
             `Int Ad/Pub High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$int_adpub_high), as.numeric(`Int Ad/Pub High`)),
             `Int Prints Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, round(as.numeric(input$int_prints_low),1), as.numeric(`Int Prints Low`)),
             `Int Prints Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, round(as.numeric(input$int_prints_mid),1), as.numeric(`Int Prints Mid`)),
             `Int Prints High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, round(as.numeric(input$int_prints_high),1), as.numeric(`Int Prints High`)),
             
             
             `China Fcst Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$china_fcst_low), as.numeric(`China Fcst Low`)),
             `China Fcst Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$china_fcst_mid), as.numeric(`China Fcst Mid`)),
             `China Fcst High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$china_fcst_high), as.numeric(`China Fcst High`)),
             
             `China FR Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$china_fr_low), as.numeric(`China FR Low`)),
             `China FR Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$china_fr_mid), as.numeric(`China FR Mid`)),
             `China FR High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$china_fr_high), as.numeric(`China FR High`)),
             `China Ad/Pub Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$china_adpub_low), as.numeric(`China Ad/Pub Low`)),
             `China Ad/Pub Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$china_adpub_mid), as.numeric(`China Ad/Pub Mid`)),
             `China Ad/Pub High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, as.numeric(input$china_adpub_high), as.numeric(`China Ad/Pub High`)),
             `China Prints Low` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, round(as.numeric(input$china_prints_low),1), as.numeric(`China Prints Low`)),
             `China Prints Mid` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, round(as.numeric(input$china_prints_mid),1), as.numeric(`China Prints Mid`)),
             `China Prints High` = ifelse(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, round(as.numeric(input$china_prints_high),1), as.numeric(`China Prints High`))
             
             
             
             
             
      ) %>% 
      mutate(`Last Modified` = if_else(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name, Sys.time(), `Last Modified`)) %>% 
      arrange(desc(`Last Modified`)) %>% 
      mutate(`Film Rental` = `FR Mid`,
             Marketing = `Ad/Pub Mid`) #Marketing filed to contain Ad/Pub Mid value instead of Print Mid Value:Akshay
    
    print(input$scenario_dateSubmitted)  
    if(!is.null(input$scenario_dateSubmitted) & length(input$scenario_dateSubmitted)>0 & input$scenario_submitted == "Submitted"){
      rv_scenarios$scens <- rv_scenarios$scens %>%
        mutate(`Date Submitted` = dplyr::if_else(Title == rv_projectscenarios$project_title & Name == rv_currentScenario$name & Submitted == "Submitted", input$scenario_dateSubmitted, `Date Submitted`)
        )
    }
    #print("check2")
    #saveRDS(rv_scenarios$scens, "data/000_scenarios.RDS")
    #Int Fcst Akshay 
    
    print("rv_scenarios$scens")
    print(rv_scenarios$scens)
    
    dbGetQuery(my_db, 
               #paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO" ',"SET FCST_LOW= ",ifelse(input$fcst_low=="","NULL",as.numeric(input$fcst_low)),   # T20 Soumen 23.07.24
               paste0('UPDATE ',database,'."ODS"."APP_SCENARIO" ',"SET FCST_LOW= ",ifelse(input$fcst_low=="","NULL",as.numeric(input$fcst_low)),
                      ",FCST_BASE= ",ifelse(input$fcst_mid=="","NULL",as.numeric(input$fcst_mid)),
                      ",FCST_HIGH= ",ifelse(input$fcst_high=="","NULL",as.numeric(input$fcst_high)),
                      ",FR_LOW= ",ifelse(input$fr_low=="","NULL",as.numeric(input$fr_low)),
                      ",FR_BASE= ",ifelse(input$fr_mid=="","NULL",as.numeric(input$fr_mid)),
                      ",FR_HIGH= ",ifelse(input$fr_high=="","NULL",as.numeric(input$fr_high)),
                      ",INT_FR_LOW= ",ifelse(input$int_fr_low=="","NULL",as.numeric(input$int_fr_low)),
                      ",INT_FR_BASE= ",ifelse(input$int_fr_mid=="","NULL",as.numeric(input$int_fr_mid)),
                      ",INT_FR_HIGH= ",ifelse(input$int_fr_high=="","NULL",as.numeric(input$int_fr_high)),
                      ",FCST_LOW_INTL= ",ifelse(input$int_fcst_low=="","NULL",as.numeric(input$int_fcst_low)),
                      ",FCST_BASE_INTL= ",ifelse(input$int_fcst_mid=="","NULL",as.numeric(input$int_fcst_mid)),
                      ",FCST_HIGH_INTL= ",ifelse(input$int_fcst_high=="","NULL",as.numeric(input$int_fcst_high)),
                      ",AD_PUB_LOW= ",ifelse(input$adpub_low=="","NULL",as.numeric(input$adpub_low)),
                      ",AD_PUB_BASE= ",ifelse(input$adpub_mid=="","NULL",as.numeric(input$adpub_mid)),
                      ",AD_PUB_HIGH= ",ifelse(input$adpub_high=="","NULL",as.numeric(input$adpub_high)),
                      ",PRINTS_LOW= ",ifelse(input$prints_low=="","NULL",as.numeric(input$prints_low)),
                      ",PRINTS_BASE= ",ifelse(input$prints_mid=="","NULL",as.numeric(input$prints_mid)),
                      ",PRINTS_HIGH= ",ifelse(input$prints_high=="","NULL",as.numeric(input$prints_high)),
                      
                      ",INT_AD_PUB_LOW= ",ifelse(input$int_adpub_low=="","NULL",as.numeric(input$int_adpub_low)),
                      ",INT_AD_PUB_BASE= ",ifelse(input$int_adpub_mid=="","NULL",as.numeric(input$int_adpub_mid)),
                      ",INT_AD_PUB_HIGH= ",ifelse(input$int_adpub_high=="","NULL",as.numeric(input$int_adpub_high)),
                      ",INT_PRINTS_LOW= ",ifelse(input$int_prints_low=="","NULL",as.numeric(input$int_prints_low)),
                      ",INT_PRINTS_BASE= ",ifelse(input$int_prints_mid=="","NULL",as.numeric(input$int_prints_mid)),
                      ",INT_PRINTS_HIGH= ",ifelse(input$int_prints_high=="","NULL",as.numeric(input$int_prints_high)),
                      
                      ",IMAX_LOW= ",ifelse(input$imax_low=="","NULL",as.numeric(input$imax_low)),
                      ",IMAX_BASE= ",ifelse(input$imax_mid=="","NULL",as.numeric(input$imax_mid)),
                      ",IMAX_HIGH= ",ifelse(input$imax_high=="","NULL",as.numeric(input$imax_high)),
                      ",THREED_LOW= ",ifelse(input$threed_low=="","NULL",as.numeric(input$threed_low)),
                      ",THREED_BASE= ",ifelse(input$threed_mid=="","NULL",as.numeric(input$threed_mid)),
                      ",THREED_HIGH= ",ifelse(input$threed_high=="","NULL",as.numeric(input$threed_high)),
                      
                      ",FCST_LOW_CHINA= ",ifelse(input$china_fcst_low=="","NULL",as.numeric(input$china_fcst_low)),
                      ",FCST_BASE_CHINA= ",ifelse(input$china_fcst_mid=="","NULL",as.numeric(input$china_fcst_mid)),
                      ",FCST_HIGH_CHINA= ",ifelse(input$china_fcst_high=="","NULL",as.numeric(input$china_fcst_high)),
                      ",CHINA_FR_LOW= ",ifelse(input$china_fr_low=="","NULL",as.numeric(input$china_fr_low)),
                      ",CHINA_FR_BASE= ",ifelse(input$china_fr_mid=="","NULL",as.numeric(input$china_fr_mid)),
                      ",CHINA_FR_HIGH= ",ifelse(input$china_fr_high=="","NULL",as.numeric(input$china_fr_high)),
                      ",CHINA_PRINTS_LOW= ",ifelse(input$china_prints_low=="","NULL",as.numeric(input$china_prints_low)),
                      ",CHINA_PRINTS_BASE= ",ifelse(input$china_prints_mid=="","NULL",as.numeric(input$china_prints_mid)),
                      ",CHINA_PRINTS_HIGH= ",ifelse(input$china_prints_high=="","NULL",as.numeric(input$china_prints_high)),
                      ",CHINA_AD_PUB_LOW= ",ifelse(input$china_adpub_low=="","NULL",as.numeric(input$china_adpub_low)),
                      ",CHINA_AD_PUB_BASE= ",ifelse(input$china_adpub_mid=="","NULL",as.numeric(input$china_adpub_mid)),
                      ",CHINA_AD_PUB_HIGH= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_adpub_high)),
                      
                      ",NOTES= $$",input$fcst_notes,
                      "$$,SUBMITTED_TO_FIA= '",input$scenario_submitted,
                      "',SCEN_DATE_SUBMITTED= ", ifelse(length(input$scenario_dateSubmitted)==0,"NULL",paste0("'",input$scenario_dateSubmitted,"'")),
                      ",SCEN_LAST_MODIFIED= '",Sys.time(),
                      "',SCEN_MODIFIED_BY = '",session$user,
                      "'WHERE SCEN_NAME = $$",rv_currentScenario$name,
                      "$$ AND PROJ_TITLE = $$",rv_projectscenarios$project_title,"$$;"))
    
    # rv_projectList$projs <- readRDS("data/000_projects.RDS")
    rv_projectList$projs <- rv_projectList$projs %>% mutate(`Last Scenario` = if_else(Title == rv_projectscenarios$project_title, Sys.time(), `Last Scenario`))
    # saveRDS(rv_projectList$projs, "data/000_projects.RDS")
    
    #all_projects$projs <- readRDS("data/000_projects.RDS")
    all_projects$projs <- all_projects$projs %>% mutate(`Last Scenario` = if_else(Title == rv_projectscenarios$project_title, Sys.time(), `Last Scenario`))
    #saveRDS(all_projects$projs,"data/000_projects.RDS")
    
    print("Update query")
    #print(paste0('UPDATE "THEATRICAL_LIGHTHOUSE"."ODS"."APP_SCENARIO" ',"SET FCST_LOW= ",ifelse(input$fcst_low=="","NULL",as.numeric(input$fcst_low)),  # T20 Soumen 23.07.24
    print(paste0('UPDATE ',database,'."ODS"."APP_SCENARIO" ',"SET FCST_LOW= ",ifelse(input$fcst_low=="","NULL",as.numeric(input$fcst_low)),
                 ",FCST_BASE= ",ifelse(input$fcst_mid=="","NULL",as.numeric(input$fcst_mid)),
                 ",FCST_HIGH= ",ifelse(input$fcst_high=="","NULL",as.numeric(input$fcst_high)),
                 ",FR_LOW= ",ifelse(input$fr_low=="","NULL",as.numeric(input$fr_low)),
                 ",FR_BASE= ",ifelse(input$fr_mid=="","NULL",as.numeric(input$fr_mid)),
                 ",FR_HIGH= ",ifelse(input$fr_high=="","NULL",as.numeric(input$fr_high)),
                 ",FCST_LOW_INTL= ",ifelse(input$int_fcst_low=="","NULL",as.numeric(input$int_fcst_low)),
                 ",FCST_BASE_INTL= ",ifelse(input$int_fcst_mid=="","NULL",as.numeric(input$int_fcst_mid)),
                 ",FCST_HIGH_INTL= ",ifelse(input$int_fcst_high=="","NULL",as.numeric(input$int_fcst_high)),
                 ",AD_PUB_LOW= ",ifelse(input$adpub_low=="","NULL",as.numeric(input$adpub_low)),
                 ",AD_PUB_BASE= ",ifelse(input$adpub_mid=="","NULL",as.numeric(input$adpub_mid)),
                 ",AD_PUB_HIGH= ",ifelse(input$adpub_high=="","NULL",as.numeric(input$adpub_high)),
                 ",PRINTS_LOW= ",ifelse(input$prints_low=="","NULL",as.numeric(input$prints_low)),
                 ",PRINTS_BASE= ",ifelse(input$prints_mid=="","NULL",as.numeric(input$prints_mid)),
                 ",PRINTS_HIGH= ",ifelse(input$prints_high=="","NULL",as.numeric(input$prints_high)),
                 ",INT_AD_PUB_LOW= ",ifelse(input$int_adpub_low=="","NULL",as.numeric(input$int_adpub_low)),
                 ",INT_AD_PUB_BASE= ",ifelse(input$int_adpub_mid=="","NULL",as.numeric(input$int_adpub_mid)),
                 ",INT_AD_PUB_HIGH= ",ifelse(input$int_adpub_high=="","NULL",as.numeric(input$int_adpub_high)),
                 ",INT_PRINTS_LOW= ",ifelse(input$int_prints_low=="","NULL",as.numeric(input$int_prints_low)),
                 ",INT_PRINTS_BASE= ",ifelse(input$int_prints_mid=="","NULL",as.numeric(input$int_prints_mid)),
                 ",INT_PRINTS_HIGH= ",ifelse(input$int_prints_high=="","NULL",as.numeric(input$int_prints_high)),
                 ",INT_FR_LOW= ",ifelse(input$int_fr_low=="","NULL",as.numeric(input$int_fr_low)),
                 ",INT_FR_BASE= ",ifelse(input$int_fr_mid=="","NULL",as.numeric(input$int_fr_mid)),
                 ",INT_FR_HIGH= ",ifelse(input$int_fr_high=="","NULL",as.numeric(input$int_fr_high)),
                 ",IMAX_LOW= ",ifelse(input$imax_low=="","NULL",as.numeric(input$imax_low)),
                 ",IMAX_BASE= ",ifelse(input$imax_mid=="","NULL",as.numeric(input$imax_mid)),
                 ",IMAX_HIGH= ",ifelse(input$imax_high=="","NULL",as.numeric(input$imax_high)),
                 ",THREED_LOW= ",ifelse(input$threed_low=="","NULL",as.numeric(input$threed_low)),
                 ",THREED_BASE= ",ifelse(input$threed_mid=="","NULL",as.numeric(input$threed_mid)),
                 ",THREED_HIGH= ",ifelse(input$threed_high=="","NULL",as.numeric(input$threed_high)),
                 ",FCST_LOW_CHINA= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_fcst_low)),
                 ",FCST_BASE_CHINA= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_fcst_mid)),
                 ",FCST_HIGH_CHINA= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_fcst_high)),
                 ",CHINA_FR_LOW= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_fr_low)),
                 ",CHINA_FR_BASE= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_fr_mid)),
                 ",CHINA_FR_HIGH= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_fr_high)),
                 ",CHINA_PRINTS_LOW= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_prints_low)),
                 ",CHINA_PRINTS_BASE= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_prints_mid)),
                 ",CHINA_PRINTS_HIGH= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_prints_high)),
                 ",CHINA_AD_PUB_LOW= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_adpub_low)),
                 ",CHINA_AD_PUB_BASE= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_adpub_mid)),
                 ",CHINA_AD_PUB_HIGH= ",ifelse(input$imax_low=="","NULL",as.numeric(input$china_adpub_high)),
                 
                 
                 ",NOTES= $$",input$fcst_notes,
                 "$$,SUBMITTED_TO_FIA= '",input$scenario_submitted,
                 "',SCEN_DATE_SUBMITTED= ", ifelse(length(input$scenario_dateSubmitted)==0,"NULL",paste0("'",input$scenario_dateSubmitted,"'")),
                 ",SCEN_LAST_MODIFIED= '",Sys.time(),
                 "',SCEN_MODIFIED_BY = '",session$user,
                 "'WHERE SCEN_NAME = $$",rv_currentScenario$name,
                 "$$ AND PROJ_TITLE = $$",rv_projectscenarios$project_title,"$$;"))
    sendSweetAlert(session, title = "Successfully Saved Forecasts", type = "success")
    
  })
  
  
  # #Network Visual:
  observe({
    if(length(input$scenario_selection_rows_selected)>0 & length(input$project_selection_rows_selected)>0){
      ttl <- rv_projectList$projs %>% slice(input$project_selection_rows_selected) %>% select(Title) %>% pull()
      dta1 <- rv_scenarios$scens %>%
        filter(Title == ttl) %>%
        slice(input$scenario_selection_rows_selected)
      mcs <- dta1 %>% select(`Selected Comps Mcodes`) %>% pull() %>% str_split(.,", ") %>% unlist
      dta2 <- mms %>% filter(Mcode %in% mcs)
      dta3 <- data.frame(Title = character(), mcode = character(), other=character())
      
      #test code akshay
      
      
      #test code akshay 
      
      #selection = "Cast (Major)"
      #selection = "Director"
      
      
      
      selection = input$network_var
      print(selection)
      
      #new chnage to avoid is.na(other) crash :Akshay  
      if(nrow(dta2)>0)
      {
        
        if(selection == "Keyword"){
          dta1$Keyword <- paste(dta1$`Keyword 1`,dta1$`Keyword 2`,dta1$`Keyword 3`,sep = ", ")
          dta2$Keyword <- paste(dta2$`Keyword 1`,dta2$`Keyword 2`,dta2$`Keyword 3`,sep = ", ")
        }
        if(selection == "Genre"){
          dta1$Genre <- paste(dta1$`Primary Genre`,dta1$`Secondary Genre`,dta1$`Other Genre`,sep = ", ")
          dta2$Genre <- paste(dta2$`Primary Genre`,dta2$`Secondary Genre`,dta2$`Other Genre`,sep = ", ")
        }
        
        multi_fields= c("Cast (Major)","Director","Executive Producer","Genre","Keyword")
        for(i in 1:nrow(dta2)){
          title = dta2$Title[i]
          mcode = dta2$Mcode[i]
          if(selection %in% multi_fields){
            other = dta2 %>% slice(i) %>% select(selection) %>% pull() %>% str_split(.,", ") %>% unlist()
            
          }else{
            other = dta2 %>% slice(i) %>% select(selection) %>% pull()
          }
          
          
          #change made to avoid crash due to coalesce function for films having no cast : Akshay Mahale
          print("other")
          print(other)
          #if(length(title)==0|length(mcode)==0|(length(other)==0 | is.na(other))){ Commented 30.10.23 --Soumen T15
          if(length(title)==0|length(mcode)==0|(length(other[1])==0 | is.na(other[1]))){  #Added 30.10.23 --Soumen T15
            
          }else{
            tmp = CJ(title, mcode, other) %>% rename(Title = title, mcode=mcode, other = other) %>% as.data.frame()
            dta3 <- bind_rows(dta3,tmp)
          }
        }
        
        print("other again")
        print(other)
        #change made to avoid crash due to coalesce function for films having no cast : Akshay Mahale
        #if(length(title)==0|length(mcode)==0|(length(other)==0 | is.na(other))){       Commented 30.10.23 --Soumen T15
        if(length(title)==0|length(mcode)==0|(length(other[1])==0 | is.na(other[1]))){  #Added 30.10.23 --Soumen T15
          
        }else{
          unq_other <- dta3 %>% select(other) %>% unique() %>% mutate(id = 1:n())
          dta3 <- left_join(dta3,unq_other,by="other") %>% filter(other != "Jr.")
          if(selection %in% multi_fields){
            scen_selection <- dta1 %>% select(selection) %>% pull() %>% str_split(.,", ") %>% unlist()
          }else{
            scen_selection <- dta1 %>% select(selection) %>% pull
          }
          
          
          dta4 <- CJ(paste0("SCENARIO: ",dta1$Name), "scenario", scen_selection) %>%
            rename(Title = V1, mcode=V2, other = scen_selection)  %>% as.data.frame() %>%
            mutate(id2 = 1:n() + max(dta3$id)) %>% 
            left_join(dta3 %>% select(other,id),by="other") %>% 
            mutate(id = coalesce(id,id2))
          
          
          sizes=c(rep(1,length(unique(dta3$Title))),2)#,rep(.1,length(unique(dta3$other))+length(unique(dta4$other))))
          cols1 =c(rep("#f2d252",length(unique(dta3$Title))),"#228B22")#,rep("#D44500",length(unique(dta3$other))+length(unique(dta4$other))))
          
          
          dta3 <- bind_rows(dta3,dta4)
          print(dta3)
          
          lab <- c(unique(dta3$Title), unique(dta3$other))
          id <- c(unique(dta3$mcode),unique(dta3$id))
          
          sizes = c(sizes, rep(.1, (length(lab)-length(sizes))))
          cols1 = c(cols1, rep("#D44500", (length(lab)-length(cols1))))
          
          nodes <- data.frame(id= id, label = lab)
          edges <- data.frame(from = c(dta3$mcode), to = c(dta3$id), font.size=30)
          
          print(nodes)
          print(edges)
          print(sizes)
          print(cols1)
          
          output$plt_network <- renderVisNetwork({
            # minimal example
            #nodes <- data.frame(id = 1:5, label=1:5)
            #edges <- data.frame(from = c(1,1,2,4), to = c(1,2,3,5), title=1:4, value=2)
            nodes <- data.frame(id= id, label = lab, value=sizes,color=cols1)
            # edges <- data.frame(from = c(dta2$cast_id), to = c(dta2$mcode))
            edges <- data.frame(from = c(dta3$id), to = c(dta3$mcode))
            
            visNetwork(nodes, edges) %>%
              visEdges(smooth = FALSE, shadow = TRUE, arrows = "to",
                       color = "grey") %>%
              visPhysics(enabled = TRUE,
                         solver = "forceAtlas2Based",
                         forceAtlas2Based = list(gravitationalConstant = -200, springLength = 200),
                         stabilization = FALSE) %>%
              visLegend(width = 0.1, position = "right", main = "Node Type")
          })
        }
        
      }
      
      
    }
    
  })
  
  
  #Help documentation
  output$downloadHelp <- downloadHandler(
    filename = "WB Greenlight App User Guide.pdf",
    content= function(file){
      file.copy("www/Greenlight_UserGuide.pdf",file)
    }
  )
  
  observeEvent(ignoreInit=TRUE,input$appWalkthrough,{
    # readCallback <- function(funname = c("switchTabs")) {
    #   funname <- match.arg(funname)
    #   
    #   switch(funname,
    #          switchTabs = I("rintrojs.callback.switchTabs(targetElement)"))
    #   
    # }
    
    steps <- reactive(data.frame(element = c(#NA,
      "#project_selection",
      "#btn_editProject",
      "#btn_newProject",
      "#report_at_a_glance",
      "#scenario_selection"#,
      #"#box_builder"
    ),
    intro = c(#"Welcome to the interactive app walkthrough.",
      "This table represents all existing projects. Click on any row of this table to reveal all of its scenarios below. Try clicking a row now!",
      "If you need to change any metadata for the selected project, you can do so by clicking this button.",
      "If you don't see your project in the table below, click here to add a new project.",
      'Clicking here will download a truncated report of only the films flagged for the "At-a-Glance" report.',
      "After selecting your project, a list of all corresponding scenarios will appear in this table. Try selecting a scenario by clicking a row in this table now!"#,
      #'Now, you can edit more information about a scenario by clicking the "Add More Details" button.'
    )
    )
    )
    
    introjs(session,options = list(steps=steps()))#,events = list(onbeforechange = readCallback("switchTabs")))
  })
  
  
  #Optimise dashboard:Akshay 
  observeEvent(ignoreInit = TRUE,input$btn_compFilter,{
    if(rv_updateMMSFilters$i==1){
      # showModal(modalDialog("Doing a function", footer=NULL))
      print("Update MMS Filters")
      #Filters:
      #future(updateSelectizeInput(session,"filter_cast",choices = mms_actors,server = F, selected=""))
      future(updateSelectizeInput(session,"filter_cast",choices = as.character(mms_actors$.),server = TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000)))#Soumen added 05.05.23
      updateSelectizeInput(session,"filter_genre",choices=mms_genres,server=TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000))
      #future(updateSelectizeInput(session,"filter_director",choices=mms_directors,server=F, selected=""))
      future(updateSelectizeInput(session,"filter_director",choices= as.character(mms_directors$.),server=TRUE, selected="",options = list(virtualScroll=TRUE,maxOptions = 1000)))#Soumen added 04.05.23
      #updateSelectizeInput(session,"filter_voice",choices = mms_voices,server = F, selected="")
      updateSelectizeInput(session,"filter_voice",choices = as.character(mms_voices$.),server = TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000))#Soumen added 05.05.23
      #updateSelectizeInput(session,"filter_distributor",choices = unique(mms$Distributor),server = F, selected="")
      updateSelectizeInput(session,"filter_distributor",choices = unique(mms$Distributor),server = TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000))#Soumen added 05.05.23s
      #future(updateSelectizeInput(session,"filter_execprod",choices = mms_execprods,server = F, selected=""))#Soumen Commented 08.05.23
      future(updateSelectizeInput(session,"filter_execprod",choices = as.character(mms_execprods$.),server = TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000)))#Soumen added 05.05.23))
      updateSelectizeInput(session,"filter_keyword",choices = mms_keywords,server = F, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000))
      # updateSelectizeInput(session,"filter_editor",choices = mms_editors,server = F, selected="")#Soumen Commented 08.05.23
      # updateSelectizeInput(session,"filter_writer",choices = mms_writers,server = F, selected="")Soumen Commented 08.05.23
      updateSelectizeInput(session,"filter_editor",choices = as.character(mms_editors$.),server = TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000))#Soumen added 08.05.23
      updateSelectizeInput(session,"filter_writer",choices = as.character(mms_writers$.),server = TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000))#Soumen added 08.05.23
      updateSelectizeInput(session,"filter_studio",choices = unique(mms$Studio),server = TRUE, selected="",options = list(virtualScroll=TRUE, maxOptions = 1000))
      
      rv_updateMMSFilters$i<-2
      print("End Update MMS Filters")
      # removeModal()
      
    }
    
  })
  
  
  #Optimise dashboard:Akshay
  # observeEvent(ignoreInit = TRUE,input$btn_ScenarioDetails,{
  #   if(rv_updateScenarioFilters$i==1){
  #     # showModal(modalDialog("Doing a function", footer=NULL))
  #     print("Update Scenario Filters")
  #     print(mms_directors)
  #     #Filters:
  #     updateSelectizeInput(session,"scenario_director",choices = mms_directors,server = F)
  #     updateSelectizeInput(session,"scenario_cast",choices = mms_actors,server = F)
  #     updateSelectizeInput(session,"scenario_voice",choices = mms_voices,server = F)
  #     updateSelectizeInput(session,"scenario_producer",choices = mms_producers,server = F)
  #     updateSelectizeInput(session,"scenario_execproducer",choices = mms_execprods,server = F)
  #     updateSelectizeInput(session,"scenario_genre1",choices = mms_genres,server = F,selected="")
  #     updateSelectizeInput(session,"scenario_genre2",choices = mms_genres,server = F,selected="")
  #     updateSelectizeInput(session,"scenario_genre3",choices = mms_genres,server = F,selected="")
  #     updateSelectizeInput(session,"scenario_keyword1",choices = mms_keywords,server = F,selected = "")
  #     updateSelectizeInput(session,"scenario_keyword2",choices = mms_keywords,server = F,selected = "")
  #     updateSelectizeInput(session,"scenario_keyword3",choices = mms_keywords,server = F,selected = "")
  #     updateSelectizeInput(session,"scenario_studio",choices=unique(mms$Studio),server=F)
  #     updateSelectizeInput(session,"scenario_editor",choices=mms_editors,server=F)
  #     updateSelectizeInput(session,"scenario_writer",choices=mms_writers,server = F)
  #     
  #     
  #     rv_updateScenarioFilters$i<-2
  #     print("End Update Scenario Filters")
  #     # removeModal()
  #     
  #   }
  #   
  #})
  
  
  
  
  
  
  
  
  observe({
    if(rv_updateSelects$i == 1){
      print("SELECTIZE UPDATES:")
      #scenario selections:
      # updateSelectizeInput(session,"scenario_director",choices = mms_directors,server = F)
      # updateSelectizeInput(session,"scenario_cast",choices = mms_actors,server = F)
      # updateSelectizeInput(session,"scenario_voice",choices = mms_voices,server = F)
      # updateSelectizeInput(session,"scenario_producer",choices = mms_producers,server = F)
      # updateSelectizeInput(session,"scenario_execproducer",choices = mms_execprods,server = F)
      # updateSelectizeInput(session,"scenario_genre1",choices = mms_genres,server = F,selected="")
      # updateSelectizeInput(session,"scenario_genre2",choices = mms_genres,server = F,selected="")
      # updateSelectizeInput(session,"scenario_genre3",choices = mms_genres,server = F,selected="")
      # updateSelectizeInput(session,"scenario_keyword1",choices = mms_keywords,server = F,selected = "")
      # updateSelectizeInput(session,"scenario_keyword2",choices = mms_keywords,server = F,selected = "")
      # updateSelectizeInput(session,"scenario_keyword3",choices = mms_keywords,server = F,selected = "")
      # updateSelectizeInput(session,"scenario_studio",choices=unique(mms$Studio),server=F)
      # updateSelectizeInput(session,"scenario_editor",choices=mms_editors,server=F)
      # updateSelectizeInput(session,"scenario_writer",choices=mms_writers,server = F)
      # 
      
      # #Filters:
      # future(updateSelectizeInput(session,"filter_cast",choices = mms_actors,server = F, selected=""))
      # updateSelectizeInput(session,"filter_genre",choices=mms_genres,server=F, selected="")
      # future(updateSelectizeInput(session,"filter_director",choices=mms_directors,server=F, selected=""))
      # updateSelectizeInput(session,"filter_voice",choices = mms_voices,server = F, selected="")
      # updateSelectizeInput(session,"filter_distributor",choices = unique(mms$Distributor),server = F, selected="")
      # future(updateSelectizeInput(session,"filter_execprod",choices = mms_execprods,server = F, selected=""))
      # updateSelectizeInput(session,"filter_keyword",choices = mms_keywords,server = F, selected="")
      # updateSelectizeInput(session,"filter_editor",choices = mms_editors,server = F, selected="")
      # updateSelectizeInput(session,"filter_writer",choices = mms_writers,server = F, selected="")
      # updateSelectizeInput(session,"filter_studio",choices = unique(mms$Studio),server = F, selected="")
      # 
      #rv_updateSelects$i <- 2
      print("SELECTIZE UPDATE COMPELTE")
    }
  })
  
  scenarioDetailsFunc<-function(scne_title,proj_title)
  {
    #scenDetail<- dbGetQuery(my_db,paste0("SELECT * FROM  THEATRICAL_LIGHTHOUSE.ODS.APP_SCENARIO WHERE SCEN_NAME=$$",scne_title,"$$ AND PROJ_TITLE=$$",proj_title,"$$;"))  # T20 Soumen 23.07.24
    scenDetail<- dbGetQuery(my_db,paste0("SELECT * FROM  ",database,".ODS.APP_SCENARIO WHERE SCEN_NAME=$$",scne_title,"$$ AND PROJ_TITLE=$$",proj_title,"$$;"))
    rv_SelectedDetails$Scen<-scenDetail
  }
  
  projectDetailsFunc<-function(proj_title)
  {
    #projDetail<- dbGetQuery(my_db,paste0("SELECT * FROM  THEATRICAL_LIGHTHOUSE.ODS.APP_PROJECT_LIST WHERE  PROJ_TITLE=$$",proj_title,"$$;"))     # T20 Soumen 23.07.24
    projDetail<- dbGetQuery(my_db,paste0("SELECT * FROM  ",database,".ODS.APP_PROJECT_LIST WHERE  PROJ_TITLE=$$",proj_title,"$$;"))   
    rv_SelectedDetails$Proj<-projDetail
  }
  
  #Update Project list with scenario names
  updateProjectScenarios<-function()
  {
    rv_projectscenarios$scenario_names <- rv_scenarios$scens %>%
      filter(Title == rv_projectscenarios$project_title) %>%
      select(Name) %>%
      pull()
  }
  
  #CLOSE THE CONNECTION WHEN APP CLOSES:
  session$onSessionEnded(function() {
    DBI::dbDisconnect(my_db)
  })
  
  checkSpChar<-function(text){
    if(!is.null(text))
    {
      if(text!="")
      {
        print("in ifff")
        x<-tail(strsplit(text,"")[[1]],1)
        if(x %in% c("$")){
          return (TRUE)
        }
        else{
          print("in else")
          return (FALSE)
        }
      }
      else{
        return (FALSE)
      }
    }
    else
    {
      print("in else 2")
      return (FALSE)
    }
  }
  
  
  checkSpChar2<-function(text){
    if(!is.null(text))
    {
      if(text!="")
      {
        print("in ifff")
        x<-text
        if(any(str_detect(x,c("\\$","\\'")))){
          return (TRUE)
        }
        else{
          print("in else")
          return (FALSE)
        }
      }
      else{
        return (FALSE)
      }
    }
    else
    {
      print("in else 2")
      return (FALSE)
    }
  }
  
  
  
})
