library(tidyverse)
library(rio)
library(naniar)

# importamos datos

app_alumno <- import(
  "https://gitlab.com/AxEeduc/datos/-/raw/master/csv_files/app_alumno.csv?inline=false"
)

# codificamos NA en variables


app_alumno %>%
  transmute_if(is.character, as.factor) %>%
  summary()

NA_converter <- function(x) {
  data <- ifelse(x %in% c("", "nc"), NA, x)
  return(data)
}

NA_converter_2 <- function(x) {
  if(is.numeric(x)){
   ifelse(x < 0, NA, x)
  }else{
    x
  }
  
  
}





app_alumno <- map_df(app_alumno, NA_converter)
app_alumno <- map_df(app_alumno, NA_converter_2)



glimpse(app_alumno)

gg_miss_upset(app_alumno,nsets = n_var_miss(app_alumno))


#armamos lista de DF a joinear
model_data <- list()

model_data$app_alumno <- app_alumno %>% 
  select(!contains("ponderador_"))



# Agregamos el df de las escuelas
df_escuelas <- import("https://gitlab.com/AxEeduc/datos/-/raw/master/csv_files/app_escuela.csv?inline=false")


df_alumno<- merge(app_alumno,df_escuelas,by.x="escuela_id",by.y="id")


# transformamos a booleanas las variables
df_alumno$tecnica<- as.logical(ifelse(df_alumno$tecnica=="t",1,0))
df_alumno$tiene_cable<- as.logical(ifelse(df_alumno$tiene_cable=="si",1,0))
df_alumno$tiene_notebook<- as.logical(ifelse(df_alumno$tiene_notebook=="si",1,0))
df_alumno$tiene_pc<- as.logical(ifelse(df_alumno$tiene_pc=="si",1,0))
df_alumno$tiene_tablet<- as.logical(ifelse(df_alumno$tiene_tablet=="si",1,0))
df_alumno$tiene_celular<- as.logical(ifelse(df_alumno$tiene_celular=="si",1,0))
df_alumno$tiene_consola<- as.logical(ifelse(df_alumno$tiene_consola=="si",1,0))
df_alumno$tiene_smarttv<- as.logical(ifelse(df_alumno$tiene_smarttv=="si",1,0))
df_alumno$tiene_smartphone<- as.logical(ifelse(df_alumno$tiene_smartphone=="si",1,0))
df_alumno$tiene_internet_alumno<- as.logical(ifelse(df_alumno$tiene_internet.x=="si",1,0))
df_alumno$tiene_internet_escuela<- as.logical(ifelse(df_alumno$tiene_internet.y=="t",1,0))
