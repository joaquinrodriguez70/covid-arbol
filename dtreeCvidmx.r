  
  #https://duckduckgo.com/?q=decision+tree+in+r&iax=videos&ia=videos&iai=https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DJFJIQ0_2ijg
  #https://www.gormanalysis.com/blog/decision-trees-in-r-using-rpart/
  #https://www.dummies.com/programming/decision-trees-r/
  #Printing dtm shows the values 
  #Each row corresponds to a node on the tree. 
  #The first entry in the row is the node number followed by a right parenthesis. 
  #The second is the variable and the value that make up the split. 
  #The third is the number of classified cases at that node. 
  #The fourth, loss, is the number of misclassified cases at the node. Misclassified? Compared to what? Compared to the next entry,
  #yval, which is the tree’s best guess of the species at that node. 
  #The final entry is a parenthesized set of proportions that correspond to the proportion of each species at the node.
  
  remove(list = ls())
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  options(repos = r)
  

  
  library(devtools)
  library(rpart)
  library(rpart.plot)
  library(rattle)
  library(RColorBrewer)
  library(dplyr)
  
  

switch(Sys.info()[['sysname']],
Windows= {			install.packages("rpart.plot")
					install.packages("rattle")
					mydir <-  'C:/Users/joaqu/Documents/Mios2020/13-dev2020/covid19/covid-arbol'
					},
Linux  = {
					mydir <-  '/home/joaquin/Documents/covid19/covid-arbol'
					},
Darwin = {			mydir <-  '/Users/joaquin/Documents/Mios2020/covid/covid-arbol'
					})
					
  
  strUrl <-  'http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip'
  strFilename <- 'datos_abiertos_covid19.zip'
  strConfirmedResult  = 1

  notDead = '9999-99-99'
  vecListaEstados <- c(1:33)
  vecnombreEstados <-c ("MEXICO","AGUASCALIENTES","BAJA_CALIFORNIA","BAJA_CALIFORNIA_SUR","CAMPECHE",	"COAHUILA",	"COLIMA","CHIAPAS",	"CHIHUAHUA","CDMX",	"DURANGO","GUANAJUATO",	"GUERRERO",	"HIDALGO","JALISCO","ESTADO_DE_MEXICO","MICHOACAN","MORELOS","NAYARIT","NUEVO_LEON","OAXACA","PUEBLA","QUERETARO","QUINTANA_ROO","SAN_LUIS_POTOSI","SINALOA","SONORA","TABASCO","TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS")
  
##Funcion for creating tree
  genera_arbol <- function(i,type) { 
    
    #EMPEZAMOS EN 2 LOS ESTADOS, 1 ES EL PAIS, POR LO QUE NO HAY ENTIDAD
    if ( i > 1) {
      dfrDatabasePar <- dfrDatabase[dfrDatabase$RESULTADO_LAB ==1 & dfrDatabase$ENTIDAD_RES==i ,]
    } else
    {
      dfrDatabasePar <- dfrDatabase[dfrDatabase$RESULTADO_LAB ==1   ,]
    }
    samplesize <- round(dim (dfrDatabasePar)[1])
    s <-sample(samplesize,round(samplesize*.8))
    
    dfrCases<-dfrDatabasePar[,c("SEXO","DIABHIPERTENSION","AMBULATORIO","INTUBADO","NEUMONIA","EDAD","DIABETES",
                                "HIPERTENSION","EPOC","ASMA","INMUNO_SUPR","TOTRA_COM","CARDIOVASCULAR","OBESIDAD",
                                "RENAL_CRONICA","TABAQUISMO","RESULTADO_LAB","FECHA_DEF","RANGO","SOBREVIVE")]
    dfrTrain <- dfrCases [s,] 
    dfrTest <- dfrCases [-s,] 
    
    #ESTADOS QUE FALLAN
    
    #LOS CASOS CON LO NO CONOCIADO ES NEUMONIA E INTUBADO
    # dtm <-rpart(SOBREVIVE~ NEUMONIA+INTUBADO+RANGO+AMBULATORIO , dfrTrain, method = "class")
    # LO CONOCIDO ES EDAD Y SEXO
    print(vecnombreEstados[[i]]) 
    if (type == TRUE) {
      dtm <-rpart(SOBREVIVE ~ EDAD+NEUMONIA+INTUBADO+AMBULATORIO +DIABETES+HIPERTENSION , dfrTrain, method = "class")
    }
    else {
      dtm <-rpart(SOBREVIVE ~ EDAD+NEUMONIA+INTUBADO+AMBULATORIO +DIABETES+HIPERTENSION , head(dfrTrain,1000), method = "class")
    }
    png(paste(vecnombreEstados[[i]], "-ArbolClasificacion",".png", sep=""), width = 1024, height = 768)
    fancyRpartPlot(dtm, caption = paste("SOBREVIVE", vecnombreEstados[[i]] ), type=4)
    dev.off()
    print(dtm)
    }
    
  
  #######################################
  # Start here
  #######################################
  
  #download and load into dataframe
  setwd(mydir)
  if (TRUE) {
  	download.file(strUrl, strFilename )
  	unzipfile <- unzip (strFilename, list = TRUE)
  	unzip (strFilename, unzipfile$Name)
  	dfrDatabase <- read.csv ( file=unzipfile$Name)
  } else {
  	dfrDatabase <- read.csv ("/Users/joaquin/Documents/Mios2020/covid/covid-arbol/210123COVID19MEXICO.csv")
  }

  dfrDatabase[,"SOBREVIVE"] <- ifelse( dfrDatabase$FECHA_DEF == notDead ,"VIVO","MUERTO")
  dfrDatabase[,"SEXO"] <- as.factor(ifelse( dfrDatabase$SEXO == 1 ,"MUJER","HOMBRE"))
  dfrDatabase[,"AMBULATORIO"] <- ifelse( dfrDatabase$TIPO_PACIENTE == 1 ,TRUE,FALSE)
  dfrDatabase[,"INTUBADO"] <- ifelse( dfrDatabase$INTUBADO == 1 ,TRUE,FALSE)
  dfrDatabase[,"NEUMONIA"] <- ifelse( dfrDatabase$NEUMONIA == 1 ,TRUE,FALSE)
  dfrDatabase[,"DIABETES"] <- ifelse( dfrDatabase$DIABETES == 1 ,TRUE,FALSE)
  dfrDatabase[,"HIPERTENSION"] <- ifelse( dfrDatabase$HIPERTENSION == 1 ,TRUE,FALSE)
  dfrDatabase[,"DIABHIPERTENSION"] <- ifelse( dfrDatabase$HIPERTENSION == 1 &  dfrDatabase$DIABETES == 1 &  dfrDatabase$OBESIDAD == 1 ,TRUE,FALSE)
  dfrDatabase[,"EPOC"] <- ifelse( dfrDatabase$EPOC == 1 ,TRUE,FALSE)
  dfrDatabase[,"ASMA" ] <- ifelse( dfrDatabase$ASMA  == 1 ,TRUE,FALSE)
  dfrDatabase[,"INMUNO_SUPR" ] <- ifelse( dfrDatabase$INMUSUPR  == 1 ,TRUE,FALSE)
  dfrDatabase[,"TOTRA_COM"] <- ifelse( dfrDatabase$OTRA_COM == 1 ,TRUE,FALSE)
  dfrDatabase[,"CARDIOVASCULAR"] <- ifelse( dfrDatabase$CARDIOVASCULAR == 1 ,TRUE,FALSE)
  dfrDatabase[,"OBESIDAD"] <- ifelse( dfrDatabase$OBESIDAD == 1 ,TRUE,FALSE)
  dfrDatabase[,"RENAL_CRONICA"] <- ifelse( dfrDatabase$RENAL_CRONICA == 1 ,TRUE,FALSE)
  dfrDatabase[,"TABAQUISMO"] <- ifelse( dfrDatabase$TABAQUISMO == 1 ,TRUE,FALSE)
  
  dfrDatabase[,"RANGO"] <- paste( (round(dfrDatabase$EDAD / 20 ) -1)  *20 ,"a",round(dfrDatabase$EDAD / 20 )   *20)

  setwd(paste(mydir,"/img",sep=""))
  for (i in 1:33) {
    #ESTADOS QUE FALLAN
    if (i!=28 | i!=31) {
      genera_arbol (i,TRUE)
    } else {
      genera_arbol (i,FALSE)
    }
  }