#################################################
# FUNCIÓN PARA DESCARGAR DATOS DE LA WEB DEL INE
################################################


download_data <- function(base_sitio_ine) {

  # Seleccionar la ruta de cada base de datos
  if (base_sitio_ine == "epf") {
    file <- "https://www.ine.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/viii-epf---(junio-2016---julio-2017)/base-personas-viii-epf-(formato-csv).csv?sfvrsn=8cdf62d7_2&download=true"
    datos <- read_delim(file, delim = ';', col_types = cols(.default = "c"))
    datos <- datos[datos$JHOGAR == 1,] %>% 
      mutate_at(vars(FE, starts_with("GASTO"), starts_with("ING")), ~stringr::str_replace(. , ",", ".")) %>% 
      mutate_at(vars(FE, starts_with("GASTO"), starts_with("ING")), as.numeric)
    
  } 
  
  # else if (base_sitio_ine == "ene") {
  #   file <- "https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/formato-csv/ene-2020-10-son.csv?sfvrsn=35fc8a67_4&download=true"
  #   datos <-  read_delim(file, delim = ';')
  #   
  # } 
  else if (base_sitio_ine == "enusc") {
    file <- "https://www.ine.cl/docs/default-source/seguridad-ciudadana/bbdd/2019/base-de-datos---xvi-enusc-2019-(csv).csv?sfvrsn=d3465758_2&download=true"
    datos <- read_delim(file, delim = ';')
    names(datos) <- tolower(names(datos))
    datos <- datos[datos$kish == 1,]
    
  } else if (base_sitio_ine == "esi") {
    file <- "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_4&download=true"
    datos <-  read_delim(file, delim = ';')
    
  } 
  
  # else if (base_sitio_ine == "enut") {
  #   file <- "https://www.ine.cl/docs/default-source/uso-del-tiempo-tiempo-libre/bbdd/documentos/base_datos_enut_csv.zip?sfvrsn=b399edf0_5"
  #   temp <- tempfile()
  #   download.file(file, temp)
  #   unzip(temp)
  #   datos <-  read_delim("BASE_USUARIO ENUT 2015.csv", delim = ';')
  # }
  
  names(datos) <- tolower(names(datos))
  
  return(datos)  
} 

