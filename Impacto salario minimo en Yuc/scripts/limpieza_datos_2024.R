library(data.table)
library(dplyr)


direcion_archivo_socio_dem_24 <- "../datos/2024/ENOE_SDEMT424.csv"
direcion_archivo_coestionario_24 <- "../datos/2024/ENOE_COE1T424.csv"


datos_socio_dem_24 <- fread(direcion_archivo_socio_dem_24)
datos_coestioanrio_24 <- fread(direcion_archivo_coestionario_24)



yuc_socio_dem <- datos_socio_dem_24 %>%
  filter(ent == 31,
         eda >= 18 & eda <= 65,
         clase1 == 1)

llaves <- c("r_def", "cd_a", "ent", "con", "upm", "n_pro_viv", "v_sel", "n_hog", "h_mud", "n_ren", "per")

# 4. Eliminar duplicados antes del join
yuc_socio_dem <- yuc_socio_dem %>%
  distinct(across(all_of(llaves)), .keep_all = TRUE)

datos_coestioanrio_24 <- datos_coestioanrio_24 %>%
  distinct(across(all_of(llaves)), .keep_all = TRUE)

# 5. Merge limpio
db_yuc_24 <- inner_join(yuc_socio_dem, 
                        datos_coestioanrio_24, 
                        by = llaves)

# 6. Variable de informalidad (correcta)
db_yuc_24 <- db_yuc_24 %>%
  mutate(informal = case_when(
    seg_soc == 2 ~ 1,
    seg_soc == 1 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(informal))

# 7. Factor de expansión correcto
db_yuc_24 <- db_yuc_24 %>%
  rename(fac = fac_tri.x)

# 8. Cálculo de tasa ponderada
tasa_informalidad_2024 <- 
  sum(db_yuc_24$informal * db_yuc_24$fac, na.rm = TRUE) /
  sum(db_yuc_24$fac, na.rm = TRUE) * 100

# 9. Resultado
cat("========================================\n")
cat("Tasa de Informalidad Yucatán 4T-2024:\n")
cat(round(tasa_informalidad_2024, 2), "%\n")
cat("========================================\n")



db_yuc_24 <- db_yuc_24 %>%
  filter(!is.na(ingocup), ingocup > 0)

db_yuc_24 <- db_yuc_24 %>%
  mutate(log_ingreso = log(ingocup))

db_yuc_24 <- db_yuc_24 %>%
  mutate(niv_ins = as.factor(niv_ins))

db_yuc_24 <- db_yuc_24 %>%
  mutate(edad = eda.x)



db_yuc_24 <- db_yuc_24 %>%
  mutate(fac_norm = fac / mean(fac, na.rm = TRUE))


modelo_2024 <- glm(informal ~ edad + niv_ins + log_ingreso,
                         data = db_yuc_24,
                         family = binomial(link = "logit"),
                         weights = fac_norm)

summary(modelo_2024)