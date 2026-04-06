library(data.table)
library(dplyr)

direcion_archivo_socio_dem_18 <- "../datos/2018/SDEMT418.csv"
direcion_archivo_coestionario_18 <- "../datos/2018/COE1T418.csv"


datos_socio_dem_18 <- fread(direcion_archivo_socio_dem_18)
datos_coestioanrio_18 <- fread(direcion_archivo_coestionario_18)

# 1. Definir llaves según listas
llaves_18 <- c("r_def", "cd_a", "ent", "con", "upm", "n_pro_viv", "v_sel", "n_hog", "h_mud", "n_ren", "per")

# 2. Filtro Yucatán y Edad en el Socio
yuc_socio_18 <- datos_socio_dem_18 %>%
  filter(ent == 31, 
         eda >= 18 & eda <= 65, 
         clase1 == 1) %>%
  distinct(across(all_of(llaves_18)), .keep_all = TRUE)

# 3. Limpieza del Cuestionario
coet_18_limpio <- datos_coestioanrio_18 %>%
  distinct(across(all_of(llaves_18)), .keep_all = TRUE)

# 4. Merge (Unión)
db_yuc_18 <- inner_join(yuc_socio_18, 
                        coet_18_limpio, 
                        by = llaves_18)

# 5. Preparación de Variables
db_yuc_18 <- db_yuc_18 %>%
  mutate(
    # Definimos informalidad (2 = Sin seguridad social)
    informal = ifelse(seg_soc == 2, 1, 0),
    # Ingreso y educación
    log_ingreso = log(ingocup),
    niv_ins = as.factor(niv_ins),
    edad = eda.x,
    fac = fac.x
  ) %>%
  # Filtramos ingresos válidos
  filter(!is.na(informal), !is.na(ingocup), ingocup > 0)

# 6. Tasa de Informalidad Ponderada 2018
tasa_18 <- sum(db_yuc_18$informal * db_yuc_18$fac) / sum(db_yuc_18$fac) * 100

cat("========================================\n")
cat("Tasa de Informalidad Yucatán 4T-2018:", round(tasa_18, 2), "%\n")
cat("========================================\n")

# 7. Modelo Logit con Pesos Normalizados
db_yuc_18 <- db_yuc_18 %>%
  mutate(fac_norm = fac / mean(fac))

modelo_2018 <- glm(informal ~ edad + niv_ins + log_ingreso,
                         data = db_yuc_18,
                         family = binomial(link = "logit"),
                         weights = fac_norm)

summary(modelo_2018)
