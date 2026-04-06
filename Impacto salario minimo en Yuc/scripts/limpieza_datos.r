# --- 1. Cargar Librerías ---
library(data.table)
library(tidyverse)

direcion_archivo_socio_dem_24 <- "Broca/Programacion/Portafolio/Portafolio/Impacto salario minimo en Yuc/datos/2024/ENOE_SDEMT424.csv"
direcion_archivo_coestionario_24 <- "Broca/Programacion/Portafolio/Portafolio/Impacto salario minimo en Yuc/datos/2024/ENOE_COE1T424.csv"

datos_socio_dem_24 <- fread(direcion_archivo_socio_dem_24)
datos_coestioanrio_24 <- fread(direcion_archivo_coestionario_24)

names(datos_socio_dem_24) <- tolower(names(datos_socio_dem_24))
names(datos_coestioanrio_24)  <- tolower(names(datos_coestioanrio_24))


ent_yuc <- datos_socio_dem_24$ent == 31
edad_laboral <- datos_socio_dem_24$eda >= 18 & datos_socio_dem_24$eda <= 65
yuc_socio_dem <- datos_socio_dem_24 %>% filter(ent_yuc,edad_laboral)

llaves <- c("r_def", "cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "n_ren")

db_yuc_24 <- inner_join(datos_socio_dem_24, datos_coestioanrio_24, 
                        by = llaves, 
                        relationship = "many-to-many")

db_yuc_24 <- db_yuc_24 %>% distinct()

columnas_p <- names(db_yuc_24)[grep("^p", names(db_yuc_24))]
print(columnas_p)

prestaciones_asalariado <- db_yuc_24$pre_asa
prestaciones_asalariado <- as.numeric(as.character(prestaciones_asalariado))



clave_informalidad <-- 0
clave_formalidad <-- 1

datos_finales <- db_yuc_24 %>%
  mutate(informal = ifelse(pre_asa == clave_informalidad, clave_formalidad, 0))

datos_informalidad <- datos_finales$informal
tasa_informalidad_2024 <- mean(datos_informalidad, na.rm = TRUE) * 100

cat("========================================\n")
cat("RESULTADO FINAL YUCATÁN 4T-2024\n")
cat("Tasa de Informalidad:", round(tasa_2024, 2), "%\n")
cat("========================================\n")
