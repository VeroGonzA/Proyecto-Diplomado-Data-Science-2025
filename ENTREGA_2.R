library(readr)
library(tidyverse)
library(dplyr)
bbdd<-read_delim("data/base-de-datos-ele7 (1).csv", 
                 delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                     grouping_mark = ".", encoding = "latin1"), 
                 na = "empty", trim_ws = TRUE)

####DEPURACIÓN I: SELECCIONO Y CAMBIO NOMBRE DE LAS VARIABLES ESCOGIDAS
bbdd<-bbdd %>% 
  rename("emp_nacional"=A057,
         "emp_extranjera"=A058,
         "emp_estatal"=A059,
         "año"=A068,
         "part_grupo_si"=A069,
         "part_grupo_no"=A070,
         "soc_familiar_si"=A0711,
         "gg_sexo_h"=A074,
         "gg_sexo_m"=A075,
         "gg_edad"=A076,
         "n_socios_h"=A078,
         "n_socio_m"=A080,
         "sin_socio_h"=A077,
         "sin_socio_m"=A079,
         "total_socio"=A081,
         "si_directorio"=A082,
         "no_directorio"=A0821,
         "n_directorio_h"=A084,
         "n_directorio_m"=A086,
         "sin_directorio_h"=A083,
         "sin_directorio_m"=A085,
         "total_directorio"=A087,
         "contrata_directa"=I005,
         "contrata_relacionada"=I006,
         "contrata_tercero"=I007,
         "directivo_h"=I143,
         "profesional_h"=I144,
         "tecnico_h"=I145,
         "admin_h"=I146,
         "venta_h"=I147,
         "agro_h"=I148,
         "op_h"=I149,
         "noesp_h"=I150,
         "total_ocupado_h"=I151,
         "directivo_m"=I152,
         "profesional_m"=I153,
         "tecnico_m"=I154,
         "admin_m"=I155,
         "venta_m"=I156,
         "agro_m"=I157,
         "op_m"=I158,
         "noesp_m"=I159,
         "total_ocupado_m"=I160,
         "rem_directivo_h"=I161,
         "rem_profesional_h"=I162,
         "rem_tecnico_h"=I163,
         "rem_admin_h"=I164,
         "rem_venta_h"=I165,
         "rem_agro_h"=I166,
         "rem_op_h"=I167,
         "rem_noesp_h"=I168,
         "rem_total_h"=I169,
         "rem_directivo_m"=I170,
         "rem_profesional_m"=I171,
         "rem_tecnico_m"=I172,
         "rem_admin_m"=I173,
         "rem_venta_m"=I174,
         "rem_agro_m"=I175,
         "rem_op_m"=I176,
         "rem_noesp_m"=I177,
         "rem_total_m"=I178,
         #ver si incluimos lo no imponible
         "si_contratacion"=I050,
         "no_contratacion"=I051,
         "nuevo_directivo_h"=I188,
         "nuevo_profesional_h"=I189,
         "nuevo_tecnico_h"=I190,
         "nuevo_admin_h"=I191,
         "nuevo_venta_h"=I192,
         "nuevo_agro_h"=I193,
         "nuevo_op_h"=I194,
         "nuevo_noesp_h"=I195,
         "nuevo_total_h"=I196,
         "nuevo_directivo_m"=I197,
         "nuevo_profesional_m"=I198,
         "nuevo_tecnico_m"=I199,
         "nuevo_admin_m"=I200,
         "nuevo_venta_m"=I201,
         "nuevo_agro_m"=I202,
         "nuevo_op_m"=I203,
         "nuevo_noesp_m"=I204,
         "nuevo_total_m"=I205,
         "si_fin_contrato"=I073,
         "no_fin_contrato"=I074,
         "fin_directivo_h"=I206,
         "fin_profesional_h"=I207,
         "fin_tecnico_h"=I208,
         "fin_admin_h"=I209,
         "fin_venta_h"=I210,
         "fin_agro_h"=I211,
         "fin_op_h"=I212,
         "fin_noesp_h"=I213,
         "fin_total_h"=I214,
         "fin_directivo_m"=I215,
         "fin_profesional_m"=I216,
         "fin_tecnico_m"=I217,
         "fin_admin_m"=I218,
         "fin_venta_m"=I219,
         "fin_agro_m"=I220,
         "fin_op_m"=I221,
         "fin_noesp_m"=I222,
         "fin_total_m"=I223,
         "jc_directivo_h"=I266,
         "jc_profesional_h"=I267,
         "jc_tecnico_h"=I268,
         "jc_admin_h"=I269,
         "jc_venta_h"=I270,
         "jc_agro_h"=I271,
         "jc_op_h"=I272,
         "jc_noesp_h"=I273,
         "jc_directivo_m"=I274,
         "jc_profesional_m"=I275,
         "jc_tecnico_m"=I276,
         "jc_admin_m"=I277,
         "jc_venta_m"=I278,
         "jc_agro_m"=I279,
         "jc_op_m"=I280,
         "jc_noesp_m"=I281,
         "jp+30_directivo_h"=I282,
         "jp+30_profesional_h"=I283,
         "jp+30_tecnico_h"=I284,
         "jp+30_admin_h"=I285,
         "jp+30_venta_h"=I286,
         "jp+30_agro_h"=I287,
         "jp+30_op_h"=I288,
         "jp+30_noesp_h"=I289,
         "jp+30_directivo_m"=I290,
         "jp+30_profesional_m"=I291,
         "jp+30_tecnico_m"=I292,
         "jp+30_admin_m"=I293,
         "jp+30_venta_m"=I294,
         "jp+30_agro_m"=I295,
         "jp+30_op_m"=I296,
         "jp+30_noesp_m"=I297,
         "jp-30_directivo_h"=I298,
         "jp-30_profesional_h"=I299,
         "jp-30_tecnico_h"=I300,
         "jp-30_admin_h"=I301,
         "jp-30_venta_h"=I302,
         "jp-30_agro_h"=I303,
         "jp-30_op_h"=I304,
         "jp-30_noesp_h"=I305,
         "jp-30_directivo_m"=I306,
         "jp-30_profesional_m"=I307,
         "jp-30_tecnico_m"=I308,
         "jp-30_admin_m"=I309,
         "jp-30_venta_m"=I310,
         "jp-30_agro_m"=I311,
         "jp-30_op_m"=I312,
         "jp-30_noesp_m"=I313,
         "n_sindicatos"=I120,
         "sindicato_h"=I121,
         "sindicato_m"=I122,
         "n_negociador"=I123,
         "negociador_m"=I124,
         "negociador_h"=I125,
         "huelga_2020"=I109,
         "huelga_2021"=I110,
         "huelga_2022"=I126,
         "suministro"=I111,
         "subcontrato"=I112,
         "pacto_jornada"=I113,
         "teletrabajo"=I114,
         "hcompensada"=I115,
         "polivalencia"=I116,
         "sueldo_variable"=I117,
         "otra_flex"=I118,
         "sin_flex"=I119,
         "comp_1"=D062,
         "comp_2"=D063,
         "comp_3"=D064,
         "comp_4"=D065,
         "comp_5"=D066,
         "factor_demanda"=H093,
         "factor_disponibilidad"=H083,
         "factor_competencia"=H084,
         "factor_tributo"=H094,
         "factor_regulacion"=H085,
         "factor_infraestructura"=H086,
         "factor_finaciamiento"=H087,
         "factor_seguridad"=H088,
         "factor_control"=H089,
         "factor_adecuado"=H090,
         "factor_desleal"=H091,
         "factor_corrupcion"=H092,
         "as_gremio"=D082,
         "as_barrio"=D083,
         "as_proyectos"=D084,
         "as_gobregion"=D085,
         "as_corfo"=D086,
         "as_otro"=D087,
         "as_no"=D088,
         "as_nosabe"=D089)
str(bbdd)

##tamaño, ciiu, tipo de propiedad, año, parte de un grupo, sexo gg,
#edad gg, numero socios x sexo, 
#directo, contratacion 
##dotacion hm, nuevos hm, motivo no contrata,
#contratos terminados hm,
#jornada hm, sindicatos, afiliados hm,
#practicas sindicales,
#competidores, volumen ventas,
#otras asociacoines, facotres afectan


variables<-list(c("ROL_FICTICIO", "TAMANO", "CIIU_FINAL",
                  "emp_nacional" ,
                  "emp_extranjera",
                  "emp_estatal",
                  "año",
                  "part_grupo_si",
                  "part_grupo_no",
                  "soc_familiar_si",
                  "gg_sexo_h",
                  "gg_sexo_m",
                  "gg_edad",
                  "n_socios_h",
                  "n_socio_m",
                  "sin_socio_h",
                  "sin_socio_m",
                  "total_socio",
                  "si_directorio",
                  "no_directorio",
                  "n_directorio_h",
                  "n_directorio_m",
                  "sin_directorio_h",
                  "sin_directorio_m",
                  "total_directorio",
                  "contrata_directa",
                  "contrata_relacionada",
                  "contrata_tercero",
                  "directivo_h",
                  "profesional_h",
                  "tecnico_h",
                  "admin_h",
                  "venta_h",
                  "agro_h",
                  "op_h",
                  "noesp_h",
                  "total_ocupado_h",
                  "directivo_m",
                  "profesional_m",
                  "tecnico_m",
                  "admin_m",
                  "venta_m",
                  "agro_m",
                  "op_m",
                  "noesp_m",
                  "total_ocupado_m",
                  "rem_directivo_h",
                  "rem_profesional_h",
                  "rem_tecnico_h",
                  "rem_admin_h",
                  "rem_venta_h",
                  "rem_agro_h",
                  "rem_op_h",
                  "rem_noesp_h",
                  "rem_total_h",
                  "rem_directivo_m",
                  "rem_profesional_m",
                  "rem_tecnico_m",
                  "rem_admin_m",
                  "rem_venta_m",
                  "rem_agro_m",
                  "rem_op_m",
                  "rem_noesp_m",
                  "rem_total_m",
                  #ver si incluimo
                  "si_contratacion",
                  "no_contratacion",
                  "nuevo_directivo_h",
                  "nuevo_profesional_h",
                  "nuevo_tecnico_h",
                  "nuevo_admin_h",
                  "nuevo_venta_h",
                  "nuevo_agro_h",
                  "nuevo_op_h",
                  "nuevo_noesp_h",
                  "nuevo_total_h",
                  "nuevo_directivo_m",
                  "nuevo_profesional_m",
                  "nuevo_tecnico_m",
                  "nuevo_admin_m",
                  "nuevo_venta_m",
                  "nuevo_agro_m",
                  "nuevo_op_m",
                  "nuevo_noesp_m",
                  "nuevo_total_m",
                  "si_fin_contrato",
                  "no_fin_contrato",
                  "fin_directivo_h",
                  "fin_profesional_h",
                  "fin_tecnico_h",
                  "fin_admin_h",
                  "fin_venta_h",
                  "fin_agro_h",
                  "fin_op_h",
                  "fin_noesp_h",
                  "fin_total_h",
                  "fin_directivo_m",
                  "fin_profesional_m",
                  "fin_tecnico_m",
                  "fin_admin_m",
                  "fin_venta_m",
                  "fin_agro_m",
                  "fin_op_m",
                  "fin_noesp_m",
                  "fin_total_m",
                  "jc_directivo_h",
                  "jc_profesional_h",
                  "jc_tecnico_h",
                  "jc_admin_h",
                  "jc_venta_h",
                  "jc_agro_h",
                  "jc_op_h",
                  "jc_noesp_h",
                  "jc_directivo_m",
                  "jc_profesional_m",
                  "jc_tecnico_m",
                  "jc_admin_m",
                  "jc_venta_m",
                  "jc_agro_m",
                  "jc_op_m",
                  "jc_noesp_m",
                  "jp+30_directivo_h",
                  "jp+30_profesional_h",
                  "jp+30_tecnico_h",
                  "jp+30_admin_h",
                  "jp+30_venta_h",
                  "jp+30_agro_h",
                  "jp+30_op_h",
                  "jp+30_noesp_h",
                  "jp+30_directivo_m",
                  "jp+30_profesional_m",
                  "jp+30_tecnico_m",
                  "jp+30_admin_m",
                  "jp+30_venta_m",
                  "jp+30_agro_m",
                  "jp+30_op_m",
                  "jp+30_noesp_m",
                  "jp-30_directivo_h",
                  "jp-30_profesional_h",
                  "jp-30_tecnico_h",
                  "jp-30_admin_h",
                  "jp-30_venta_h",
                  "jp-30_agro_h",
                  "jp-30_op_h",
                  "jp-30_noesp_h",
                  "jp-30_directivo_m",
                  "jp-30_profesional_m",
                  "jp-30_tecnico_m",
                  "jp-30_admin_m",
                  "jp-30_venta_m",
                  "jp-30_agro_m",
                  "jp-30_op_m",
                  "jp-30_noesp_m",
                  "n_sindicatos",
                  "sindicato_h",
                  "sindicato_m",
                  "n_negociador",
                  "negociador_m",
                  "negociador_h",
                  "huelga_2020",
                  "huelga_2021",
                  "huelga_2022",
                  "suministro",
                  "subcontrato",
                  "pacto_jornada",
                  "teletrabajo",
                  "hcompensada",
                  "polivalencia",
                  "sueldo_variable",
                  "otra_flex",
                  "sin_flex",
                  "comp_1",
                  "comp_2",
                  "comp_3",
                  "comp_4",
                  "comp_5",
                  "factor_demanda",
                  "factor_disponibilidad",
                  "factor_competencia",
                  "factor_tributo",
                  "factor_regulacion",
                  "factor_infraestructura",
                  "factor_finaciamiento",
                  "factor_seguridad",
                  "factor_control",
                  "factor_adecuado",
                  "factor_desleal",
                  "factor_corrupcion",
                  "as_gremio",
                  "as_barrio",
                  "as_proyectos",
                  "as_gobregion",
                  "as_corfo",
                  "as_otro",
                  "as_no",
                  "as_nosabe", "FE_TRANSVERSAL" ))

datos_proyecto <- bbdd %>% 
  select(all_of(unlist(variables))) |> 
  mutate(emp_nacional=as.numeric(emp_nacional))


###llamo a la funcion para hacer la tabla
source("ftabla_resumen.R")


##VAriables que estamos usando (actualizar)

vars<-list(c("CIIU_FINAL", "TAMANO","gg_edad", "gg_sexo_h", "gg_sexo_m", "part_grupo_no", "part_grupo_si",
             "no_directorio", "si_directorio", "no_contratacion", "si_contratacion",
             "no_fin_contrato", "si_fin_contrato", "año",
             "comp_1", "comp_2", "comp_3", "comp_4", "comp_5",
             "emp_nacional", "emp_extranjera", "emp_estatal", "total_ocupado_m",
             "total_ocupado_h", "n_directorio_h",
             "n_directorio_m",
             "sin_directorio_h",
             "sin_directorio_m"))

diccionario <- tibble::tibble(
  variable = c("CIIU_FINAL", "TAMANO","gg_edad", "gg_sexo_h", "gg_sexo_m", "part_grupo_no", "part_grupo_si",
               "no_directorio", "si_directorio", "no_contratacion", "si_contratacion",
               "no_fin_contrato", "si_fin_contrato", "año",
               "comp_1", "comp_2", "comp_3", "comp_4", "comp_5",
               "emp_nacional", "emp_extranjera", "emp_estatal","total_ocupado_m",
               "total_ocupado_h","n_directorio_h",
               "n_directorio_m",
               "sin_directorio_h",
               "sin_directorio_m"),
  Nombre = c(
    "Clasificador Internacional Industrial Uniforme (CIIU)",
    "Tamaño de la empresa",
    "Edad gerente general",
    "Sexo Gerente General = Hombre",
    "Sexo Gerente General = Mujer",
    "Empresa no es parte de un grupo",
    "Empresa si es parte de un grupo",
    "No tiene directorio",
    "Si tiene directorio",
    "No tuvo nuevas contrataciones",
    "Si tuvo nuevas contrataciones",
    "No finalizó contrataciones",
    "Si finalizó contrataciones",
    "Año de fundación",
    'Cantidad de competencia = "1 Baja"',
    'Cantidad de competencia = "2"',
    'Cantidad de competencia = "3"',
    'Cantidad de competencia = "4"',
    'Cantidad de competencia = "5 Elevada"',
    "Porcentaje de propiedad nacional",
    "Porcentaje de propiedad extranjera",
    "Porcentaje de propiedad estatal",
    "Total de mujeres ocupadas en la empresa",
    "Total de hombres ocupados en la empresa", 
    "Número de directores",
    "Numero de directoras",
    "Empresa sin directores",
    "Empresa sin directoras"
  )
)



na_summary_todas <- get_na_table(datos_proyecto, variables)
na_summary_actual <- get_na_table(datos_proyecto, vars)
print(na_summary_actual)

names(na_summary_actual)=c("Variable", "Valores Perdidos (NA)", "Valor Mínimo", "Valor Máximo", "Media", "Mediana", "Moda", "N","%NA", "N Válido" )
na_summary_actual=na_summary_actual |> 
  full_join(diccionario, by=c("Variable"="variable")) |> 
  select("Nombre", "N", "N Válido", "Valores Perdidos (NA)", "%NA", "Valor Mínimo", "Valor Máximo", "Media", "Mediana", "Moda") 




print(na_summary_actual)



###las variables que son multi las voy a tratar de juntar



###validar datos que deberia sumar 100

##capital de la empresa
#datos_proyecto<-datos_proyecto |> 
#  mutate(total_capital = emp_nacional + emp_extranjera+emp_estatal,
#         total_participacion=part_grupo_si+part_grupo_no,
#         total_sexogg= gg_sexo_h+gg_sexo_m,
#         total_socios=n_socios_h+n_socio_m-total_socio,
#         total_dir=n_directorio_h+n_directorio_m-total_directorio)
#summary(datos_proyecto$total_capital)##comprobar que sumen 100
#summary(datos_proyecto$total_participacion)##comprobar que no existan valores diferentes a 1
#summary(datos_proyecto$total_sexogg)##comprobar que no existan valores difernetes a 1
#summary(datos_proyecto$total_socios)##no deberia haber valores distinto a 0
#summary(datos_proyecto$total_dir)
#
#ftable(datos_proyecto$n_socios_h,datos_proyecto$sin_socio_h)#aca los 1 no debería tener valor de socio
#ftable(datos_proyecto$n_socio_m,datos_proyecto$sin_socio_m)#aca los 1 no debería tener valor de socio
#ftable(datos_proyecto$n_directorio_h,datos_proyecto$sin_directorio_h)#aca los 1 no debería tener valor de socio
#ftable(datos_proyecto$n_directorio_m,datos_proyecto$sin_directorio_m)#aca los 1 no debería tener valor de socio
#
#
#
#
#participacion_grupo_total <- sum(datos_proyecto$part_grupo_no, na.rm = TRUE) + sum(datos_proyecto$part_grupo_si, na.rm = TRUE)
#total_sexogg<-sum(datos_proyecto$gg_sexo_h, na.rm = TRUE) + sum(datos_proyecto$gg_sexo_m, na.rm = TRUE)
#total_directorio<-sum(datos_proyecto$si_directorio, na.rm = T)+ sum(datos_proyecto$no_directorio, na.rm=T)
#participacion_grupo_total ##tiene que ser igual o menos que el N de la base 6592
#total_sexogg
#total_directorio
#

####CATEGORIZACION VARIABLES####
##### 1. TAMAÑO#####
datos_proyecto <- datos_proyecto %>%
  mutate(TAMANO = case_when(
    TAMANO == 1 ~ "Grande",
    TAMANO == 2 ~ "Mediana",
    TAMANO == 3 ~ "Pequeña 2",
    TAMANO == 4 ~ "Pequeña 1",
    TAMANO == 5 ~ "Micro",
    TRUE ~ as.character(TAMANO)
  ))

table(datos_proyecto$TAMANO)

#####2. SEXO G.GENERAL#####

datos_proyecto <- datos_proyecto %>%
  mutate(
    # Detectar inconsistencias/ NA
    inconsistencia = case_when(
      gg_sexo_h == 1 & gg_sexo_m == 1 ~ TRUE,
      gg_sexo_h == 0 & gg_sexo_m == 0 ~ TRUE,
      is.na(gg_sexo_h) | is.na(gg_sexo_m) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Crear la variable unificada
    gg_sexo = case_when(
      gg_sexo_h == 1 & gg_sexo_m == 0 ~ "Hombre",
      gg_sexo_h == 0 & gg_sexo_m == 1 ~ "Mujer",
      TRUE ~ NA_character_  # Para casos inconsistentes o NA
    )
  )
table(datos_proyecto$gg_sexo)

#####3. PARTE DE UN GRUPO#####
datos_proyecto <- datos_proyecto %>%
  mutate(
    # Detectar inconsistencias/ NA
    inconsistencia = case_when(
      part_grupo_no == 1 & part_grupo_si == 1 ~ TRUE,
      part_grupo_no == 0 & part_grupo_si == 0 ~ TRUE,
      is.na(part_grupo_no) | is.na(part_grupo_no) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Crear la variable unificada
    part_grupo = case_when(
      part_grupo_no == 1 & part_grupo_si == 0 ~ "No",
      part_grupo_no == 0 & part_grupo_si == 1 ~ "Si",
      TRUE ~ NA_character_  # Para casos inconsistentes o NA
    )
  )
table(datos_proyecto$part_grupo)

#####4. DIRECTORIO#####
datos_proyecto <- datos_proyecto %>%
  mutate(
    # Detectar inconsistencias/ NA
    inconsistencia = case_when(
      no_directorio == 1 & si_directorio == 1 ~ TRUE,
      no_directorio == 0 & si_directorio == 0 ~ TRUE,
      is.na(no_directorio) | is.na(si_directorio) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Crear la variable unificada
    directorio = case_when(
     no_directorio == 1 & si_directorio == 0 ~ "No",
      no_directorio == 0 & si_directorio == 1 ~ "Si",
      TRUE ~ NA_character_  # Para casos inconsistentes o NA
    )
  )
table(datos_proyecto$directorio)

#####5. CONTRATACION#####
datos_proyecto <- datos_proyecto %>%
  mutate(
    # Detectar inconsistencias/ NA
    inconsistencia = case_when(
      no_contratacion == 1 & si_contratacion == 1 ~ TRUE,
      no_contratacion == 0 & si_contratacion == 0 ~ TRUE,
      is.na(no_contratacion) | is.na(si_contratacion) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Crear la variable unificada
    contratacion = case_when(
      no_contratacion == 1 & si_contratacion == 0 ~ "No",
      no_contratacion == 0 & si_contratacion == 1 ~ "Si",
      TRUE ~ NA_character_  # Para casos inconsistentes o NA
    )
  )
table(datos_proyecto$contratacion)

#####6. FIN DE CONTRATO#####
datos_proyecto <- datos_proyecto %>%
  mutate(
    # Detectar inconsistencias/ NA
    inconsistencia = case_when(
      no_fin_contrato == 1 & si_fin_contrato == 1 ~ TRUE,
      no_fin_contrato == 0 & si_fin_contrato == 0 ~ TRUE,
      is.na(no_fin_contrato) | is.na(si_fin_contrato) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Crear la variable unificada
    fin_contrato = case_when(
      no_fin_contrato == 1 & si_fin_contrato == 0 ~ "No",
      no_fin_contrato == 0 & si_fin_contrato == 1 ~ "Si",
      TRUE ~ NA_character_  # Para casos inconsistentes o NA
    )
  )
table(datos_proyecto$fin_contrato)

#####7. ANTIGUEDAD####
datos_proyecto <- datos_proyecto %>%
  mutate(ANTIGUEDAD = abs(2022 - año))

#####8. NIVEL DE COMPETENCIA####
datos_proyecto <- datos_proyecto %>%
  mutate(
    nivel_competencia = case_when(
      comp_1 == 1 ~ "1",
      comp_2 == 1 ~ "2",
      comp_3 == 1 ~ "3",
      comp_4 == 1 ~ "4",
      comp_5 == 1 ~ "5",
      TRUE ~ NA_character_  # Para casos donde ninguna tiene 1
    )
  )

datos_proyecto <- datos_proyecto %>%
  mutate(
    suma_comp = comp_1 + comp_2 + comp_3 + comp_4 + comp_5,
    nivel_competencia = case_when(
      suma_comp == 1 & comp_1 == 1 ~ "1",
      suma_comp == 1 & comp_2 == 1 ~ "2",
      suma_comp == 1 & comp_3 == 1 ~ "3",
      suma_comp == 1 & comp_4 == 1 ~ "4",
      suma_comp == 1 & comp_5 == 1 ~ "5",
      suma_comp > 1 ~ "Error: múltiples competencias",
      TRUE ~ NA_character_
    )
  )
table(datos_proyecto$nivel_competencia)

#####9. PREDOMINIO TIPO DE EMPRESA####
datos_proyecto <- datos_proyecto %>%
  rowwise() %>%
  mutate(
    max_val = max(c(emp_nacional, emp_extranjera, emp_estatal), na.rm = TRUE),
    emp_tipo = case_when(
      sum(c(emp_nacional, emp_extranjera, emp_estatal) == max_val, na.rm = TRUE) > 1 ~ "empate",
      emp_nacional == max_val ~ "nacional",
      emp_extranjera == max_val ~ "extranjera",
      emp_estatal == max_val ~ "estatal",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

table(datos_proyecto$emp_tipo)


####10. NÚMERO DE DIRECTORES SEGÚN SEXO

datos_proyecto<-datos_proyecto |> 
  mutate(
    pct_directoras=n_directorio_m/(n_directorio_h+n_directorio_m)
  )

table(datos_proyecto$pct_directoras)

##se hace la tabla resumen pero de estas nuevas variables

####nuevas variables

nuevas_vars<-list(c("TAMANO",
                    "gg_sexo",
                    "part_grupo",
                    "directorio",
                    "contratacion",
                    "fin_contrato",
                    "ANTIGUEDAD",
                    "nivel_competencia",
                    "emp_tipo",
                    "pct_directoras"))
 diccionario<- tibble::tibble(
     variable = c("TAMANO",
                  "gg_sexo",
                  "part_grupo",
                  "directorio",
                  "contratacion",
                  "fin_contrato",
                  "ANTIGUEDAD",
                  "nivel_competencia",
                  "emp_tipo",
                  "pct_directoras"),
     Nombre = c("Tamaño de la empresa", "Sexo Gerente/a General",
                "Participación en grupo de empresas", "Directorio", 
                "Nuevas Contrataciones en el período", "Finalización de contrataciones en el período",
                "Antiguedad de la empresa (en años)", "Evaluación del nivel de competencia",
                "Tipo de empresa", "Porcentaje de Directoras"))
     
   
 
 
nuevas_na_summary<-get_na_table(datos_proyecto, nuevas_vars)

print(nuevas_na_summary)

names(nuevas_na_summary)=c("Variable", "Valores Perdidos (NA)", "Valor Mínimo", "Valor Máximo", "Media", "Mediana", "Moda", "N","%NA", "N Válido" )
nuevas_na_summary=nuevas_na_summary |> 
  full_join(diccionario, by=c("Variable"="variable")) |> 
  select("Nombre", "N", "N Válido", "Valores Perdidos (NA)", "%NA", "Valor Mínimo", "Valor Máximo", "Media", "Mediana", "Moda") 

print(nuevas_na_summary)





#install.packages("writexl")
library(writexl)
write_xlsx(
  list(
    "Resumen_actual" = na_summary_actual,
    "Resumen_nuevas" = nuevas_na_summary
  ),
  path = "Resumen_vars.xlsx"
)




#### ESTADISTICOS DESCRIPTIVOS ####
##### 1. TIPO DE EMPRESAS NAC, EXT o EST ####

datos_proyecto %>%
  count(emp_tipo) %>%
  mutate(
    porcentaje = round((n / sum(n)) * 100, 1)
  ) %>%
  rename(Categoria = emp_tipo, Frecuencia = n)

##### 2. PROMEDIO ANTIGUEDAD #####
mean(datos_proyecto$ANTIGUEDAD)

##### 3. ACTIVIDAD ECONOMICA x TAMAÑO ####
actividad_tamaño <- addmargins(table(datos_proyecto$CIIU_FINAL, 
                                     datos_proyecto$TAMANO))
print(actividad_tamaño)

#Identificar que es cada código
bbdd %>%
  group_by(CIIU_FINAL) %>%
  summarise(glosa_unica = paste(unique(GLOSA_CIIU), collapse = ", "))

##### 4. DISTRIBUCIÓN GENERO X CIIU ####

##### X. DISTRIBUCION HOMBRE VS MUJERES ####
#Dotación de mujeres reportadas
sum(datos_proyecto$total_ocupado_m, na.rm = T)
#% Mujeres > 3424827/9395874*100 > 36.5
#Dotacion de hombres reportadas
sum(datos_proyecto$total_ocupado_h, na.rm = T)
#% Hombres > 5971047/9395874*100 > 63.5

##### X. DISTRIBUCION SEXO X CIIU #####
library(ggplot2)

#1: Agrupar y resumir los datos
tabla_cruce <- datos_proyecto %>%
  group_by(CIIU_FINAL) %>%
  summarise(
    total_ocupado_m = sum(total_ocupado_m, na.rm = TRUE),
    total_ocupado_h = sum(total_ocupado_h, na.rm = TRUE)
  ) %>%
  ungroup()

#2: Transformar a formato largo
tabla_larga <- tabla_cruce %>%
  pivot_longer(cols = c(total_ocupado_m, total_ocupado_h),
               names_to = "sexo",
               values_to = "total") %>%
  group_by(CIIU_FINAL) %>%
  mutate(porcentaje = round(total / sum(total) * 100, 1)) %>%
  ungroup()

#3: Etiquetas más amigables
tabla_larga$sexo <- recode(tabla_larga$sexo,
                           "total_ocupado_m" = "Mujeres",
                           "total_ocupado_h" = "Hombres")

#4: Crear el gráfico
g_gg_ciiu<-ggplot(tabla_larga, aes(x = CIIU_FINAL, y = porcentaje, fill = sexo)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = c("Mujeres" = "#996CA9", "Hombres" = "#56B4E9")) +  # Colores personalizados
  labs(
    title = "Distribución porcentual de mujeres y hombres por Actividad Económica",
    x = "Actividad Económica",
    y = "Porcentaje",
    fill = "Sexo"
  ) +
  theme_minimal()

ggsave("grafico_1.png", g_gg_ciiu, width = 8, height = 6, dpi = 300)
##### X. DISTRIBUCION EDAD GG X GENERO #####
boxplot_gg_edad <- ggplot(subset(datos_proyecto, !is.na(gg_sexo)), aes(x = gg_sexo, y = gg_edad, fill = gg_sexo)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.6) +
  scale_fill_manual(values = c("#56B4E9", "#996CA9")) +
  labs(
    title = "Comparación de edad según sexo del Gerente General",
    x = NULL,
    y = "Edad",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

boxplot_gg_edad

ggsave("grafico_2.png", boxplot_gg_edad, width = 8, height = 6, dpi = 300)



