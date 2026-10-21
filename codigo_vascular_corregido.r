# ==============================================================================
# PATOLOGIA VASCULAR CEREBRAL NEUROQUIRURGICA / ENDOVASCULAR
# EGRESOS HOSPITALARIOS DEIS-MINSAL, CHILE 2015-2024
# Tasas de egreso (cruda y estandarizada), letalidad intrahospitalaria,
# tendencia y modelo de mortalidad hospitalaria.
#
# Cohorte (DIAG1): hemorragia subaracnoidea (I60), hemorragia intracerebral (I61),
# otras hemorragias intracraneales no traumaticas (I62), aneurisma cerebral sin
# ruptura (I67.1), diseccion arterial cerebral / moyamoya (I67.0, I67.5) y
# malformaciones vasculares cerebrales (Q28.0-Q28.3).
# EXCLUIDOS: I63 (infarto cerebral), I64, I66, G45 (AIT), I67.2-I67.9, I69 y S06.
# ==============================================================================

# 0. PAQUETES -------------------------------------------------------------------
paquetes <- c("tidyverse", "data.table", "gtsummary", "flextable", "broom")
nuevos <- paquetes[!(paquetes %in% rownames(installed.packages()))]
if (length(nuevos)) install.packages(nuevos, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))
dir.create("resultados_vascular", showWarnings = FALSE)

# 1. PARAMETROS -----------------------------------------------------------------
CARPETA_DEIS      <- "datos_deis"          # un CSV por ano (EGR_DATOS_ABIERTO_AAAA.csv)
ARCHIVO_POBLACION <- "poblacion_ine.csv"   # columnas: ano, sexo, grupo, poblacion
ANO_MIN <- 2015L
ANO_MAX <- 2024L
REFRESCAR_CACHE <- TRUE                    # TRUE si cambia el periodo o los archivos; luego puede dejarlo en FALSE

GRUPOS <- c("0-9", "10-19", "20-29", "30-39", "40-49",
            "50-59", "60-69", "70-79", "80+")

# I65.2 (estenosis carotidea) es de resolucion quirurgica/endovascular, pero los
# egresos incluyen mucho manejo medico y de cirugia vascular. Por defecto se excluye.
INCLUIR_ESTENOSIS_CAROTIDEA <- FALSE

niveles_pat <- c(
  "Hemorragia subaracnoidea (I60)",
  "Hemorragia intracerebral (I61)",
  "Otras hemorragias intracraneales no traumaticas (I62)",
  "Aneurisma cerebral sin ruptura (I67.1)",
  "Diseccion arterial / moyamoya (I67.0, I67.5)",
  "Malformaciones vasculares cerebrales (Q28.0-Q28.3)",
  if (INCLUIR_ESTENOSIS_CAROTIDEA) "Estenosis carotidea (I65.2)"
)

clasificar_patologia <- function(dx) {
  dx3 <- substr(dx, 1, 3)
  dx4 <- substr(dx, 1, 4)
  fcase(
    dx3 == "I60", niveles_pat[1],
    dx3 == "I61", niveles_pat[2],
    dx3 == "I62", niveles_pat[3],
    dx4 == "I671", niveles_pat[4],
    dx4 %in% c("I670", "I675"), niveles_pat[5],
    dx4 %in% c("Q280", "Q281", "Q282", "Q283"), niveles_pat[6],
    INCLUIR_ESTENOSIS_CAROTIDEA & dx4 == "I652", "Estenosis carotidea (I65.2)",
    default = NA_character_)
}

# Codigos inespecificos (se usan en el analisis de sensibilidad)
CODIGOS_INESPECIFICOS <- c("I609", "I619", "I629")

# 2. LECTURA --------------------------------------------------------------------
# Los CSV del DEIS usan ';' como separador. Los acentos vienen danados en origen
# (aparece "m?s"), por eso NO se usan textos con tildes para clasificar:
# la edad se obtiene de los digitos de GRUPO_EDAD.

norm_nombre <- function(x) toupper(gsub("[^A-Za-z0-9_]", "", x))

leer_deis <- function(ruta) {
  nm_orig <- names(fread(ruta, sep = ";", nrows = 0))
  nm      <- norm_nombre(nm_orig)
  pick <- function(p) { i <- grep(p, nm)[1]; if (is.na(i)) NA_character_ else nm_orig[i] }

  map <- c(
    PERT  = pick("^PERTENENCIA"),
    SEXO  = pick("^SEXO"),
    EDAD  = pick("^GRUPO_EDAD|^EDAD"),
    PREV  = pick("^GLOSA_PREVISION|^PREVISION"),
    ANO   = pick("^ANO_EGRESO|^ANIO_EGRESO"),
    DIAG1 = pick("^DIAG1$"),
    DIAG2 = pick("^DIAG2$"),
    DIAS  = pick("^DIAS_ESTAD"),
    COND  = pick("^CONDICION_EGRESO|^COND_EGRESO"),
    ERR   = pick("^ERROR$")
  )
  oblig <- c("ANO", "DIAG1", "DIAS", "COND")
  if (anyNA(map[oblig]))
    stop("Faltan columnas obligatorias en ", basename(ruta), ": ",
         paste(oblig[is.na(map[oblig])], collapse = ", "))

  usar <- map[!is.na(map)]
  df <- fread(ruta, sep = ";", select = unname(usar), colClasses = "character",
              encoding = "Latin-1", showProgress = FALSE)
  setnames(df, unname(usar), names(usar))
  for (v in setdiff(names(map), names(usar))) df[, (v) := NA_character_]

  df[, DIAG1 := substr(toupper(gsub("[. ]", "", DIAG1)), 1, 4)]
  df <- df[!is.na(clasificar_patologia(DIAG1))]

  ano_archivo <- suppressWarnings(
    as.integer(regmatches(basename(ruta), regexpr("(19|20)[0-9]{2}", basename(ruta)))))
  if (length(ano_archivo) == 0) ano_archivo <- NA_integer_
  df[, ANO_ARCHIVO := ano_archivo]
  setnames(df, "ANO", "ANO_RAW")
  df[, ARCHIVO := basename(ruta)]
  message(basename(ruta), ": ", nrow(df), " egresos de patologia vascular")
  df
}

archivo_cache <- file.path("resultados_vascular", "cohorte_vascular_cruda.rds")

if (file.exists(archivo_cache) && !REFRESCAR_CACHE) {
  cohorte <- readRDS(archivo_cache)
} else {
  archivos <- list.files(CARPETA_DEIS, pattern = "\\.csv$", full.names = TRUE)
  # Solo archivos cuyo ano (en el nombre) cae en el periodo de estudio
  ano_nombre <- suppressWarnings(as.integer(sub(".*((19|20)[0-9]{2}).*", "\\1", basename(archivos))))
  archivos <- archivos[!is.na(ano_nombre) & ano_nombre >= ANO_MIN & ano_nombre <= ANO_MAX]
  if (!length(archivos)) stop("No hay .csv de ", ANO_MIN, "-", ANO_MAX, " en '", CARPETA_DEIS, "/'")
  message("Archivos a leer: ", paste(basename(archivos), collapse = ", "))
  cohorte <- rbindlist(lapply(archivos, function(f) { r <- leer_deis(f); gc(); r }),
                       fill = TRUE)
  saveRDS(cohorte, archivo_cache)
}

# 3. RECODIFICACION -------------------------------------------------------------
# Registros con ANO = "*": el DEIS los trae sin sexo, edad, prevision, etc.
# Se les asigna el ano del archivo, pero se marcan y se excluyen del analisis
# principal (no se pueden asignar a un denominador por sexo/edad).
cohorte[, SIN_DEMO := !grepl("^[0-9]{4}$", ANO_RAW)]
cohorte[, ANO := fifelse(SIN_DEMO, ANO_ARCHIVO, as.integer(ANO_RAW))]
cohorte <- cohorte[ANO >= ANO_MIN & ANO <= ANO_MAX]

# Edad: limite inferior del grupo. Los archivos de 2012 y 2021 traen grupos de
# lactantes ("28 DIAS A 2 MES", "7 A 27 DIAS", ...) que NO deben leerse como 28 o 7 anos.
ed <- toupper(trimws(cohorte$EDAD))
cohorte[, EDAD_INF := fifelse(grepl("^MENOR|DIAS|MES", ed), 0L,
                              suppressWarnings(as.integer(sub("^([0-9]+).*", "\\1", ed))))]
cohorte[, GRUPO_ETARIO := GRUPOS[pmin(EDAD_INF %/% 10L, 8L) + 1L]]

niveles_tramo <- c("0-19", "20-39", "40-59", "60-79", "80 o mas")
cohorte[, TRAMO_EDAD := factor(
  fcase(EDAD_INF < 20, "0-19", EDAD_INF < 40, "20-39", EDAD_INF < 60, "40-59",
        EDAD_INF < 80, "60-79", EDAD_INF >= 80, "80 o mas"),
  levels = niveles_tramo)]

s <- toupper(trimws(cohorte$SEXO))
cohorte[, SEXO_GLOSA := fcase(s %chin% c("HOMBRE", "1", "H", "MASCULINO"), "Hombre",
                              s %chin% c("MUJER", "2", "M", "FEMENINO"), "Mujer",
                              default = NA_character_)]

# "No Pertenecientes al SNSS" debe evaluarse ANTES que "Pertenecientes"
p <- toupper(trimws(cohorte$PERT))
cohorte[, SECTOR := fcase(grepl("^NO PERTENECIENTE", p) | p == "2", "No SNSS (privado y otros)",
                          grepl("^PERTENECIENTE", p) | p == "1",    "SNSS (publico)",
                          default = NA_character_)]

pv <- toupper(trimws(cohorte$PREV))
cohorte[, PREVISION_GRP := fcase(
  pv %chin% c("FONASA", "1"), "FONASA",
  pv %chin% c("ISAPRE", "2"), "ISAPRE",
  pv %chin% c("CAPREDENA", "DIPRECA", "3", "4"), "FF.AA. y de Orden",
  !is.na(pv) & pv != "*", "Otra / ninguna / desconocida",
  default = NA_character_)]

# En estas patologias DIAG2 no es causa externa: no se clasifica mecanismo.

cohorte[, DIAS_ESTAD := as.numeric(gsub(",", ".", DIAS))]
cohorte[, FALLECIDO := fcase(toupper(trimws(COND)) %chin% c("2", "FALLECIDO", "FALLECIDA"), 1L,
                             toupper(trimws(COND)) %chin% c("1", "VIVO", "VIVA"), 0L,
                             default = NA_integer_)]
cohorte[, GRUPO_PAT := factor(clasificar_patologia(DIAG1), levels = niveles_pat[!is.na(niveles_pat)])]

# 4. DIAGRAMA DE FLUJO ----------------------------------------------------------
n_total <- nrow(cohorte)
c1 <- cohorte[is.na(ERR) | ERR != "1"]
c2 <- c1[SIN_DEMO == FALSE]
c3 <- c2[!is.na(FALLECIDO) & !is.na(DIAS_ESTAD)]
analisis <- c3[!is.na(SEXO_GLOSA) & !is.na(GRUPO_ETARIO)]

flujo <- data.table(
  Paso = c("Egresos con patologia vascular neuroquirurgica como diagnostico principal (periodo)",
           "Excluidos: registro con ERROR = 1",
           "Excluidos: sin datos demograficos (ANO = '*')",
           "Excluidos: sin condicion de egreso o dias de estada",
           "Excluidos: sexo indeterminado o sin edad",
           "Egresos analizados"),
  N = c(n_total, n_total - nrow(c1), nrow(c1) - nrow(c2), nrow(c2) - nrow(c3),
        nrow(c3) - nrow(analisis), nrow(analisis)))
print(flujo)
fwrite(flujo, "resultados_vascular/flujo_cohorte.csv")
fwrite(cohorte[, .(Sin_demografia = sum(SIN_DEMO), Total = .N), by = ANO][order(ANO)],
       "resultados_vascular/sin_demografia_por_ano.csv")

# 5. TABLA 1 (Word) -------------------------------------------------------------
analisis[, EGRESO := factor(FALLECIDO, levels = c(0, 1), labels = c("Alta vivo", "Fallecido"))]

t1 <- analisis %>%
  as_tibble() %>%
  select(SEXO_GLOSA, TRAMO_EDAD, PREVISION_GRP, SECTOR, GRUPO_PAT, DIAS_ESTAD, EGRESO) %>%
  tbl_summary(
    by = EGRESO,
    label = list(SEXO_GLOSA ~ "Sexo", TRAMO_EDAD ~ "Grupo de edad (anos)",
                 PREVISION_GRP ~ "Prevision", SECTOR ~ "Prestador",
                 GRUPO_PAT ~ "Grupo de patologia (CIE-10)",
                 DIAS_ESTAD ~ "Dias de estada"),
    type = DIAS_ESTAD ~ "continuous",
    statistic = list(all_continuous() ~ "{median} ({p25} - {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany") %>%
  add_overall() %>%
  add_p(test = list(all_categorical() ~ "chisq.test", all_continuous() ~ "wilcox.test")) %>%
  modify_header(label = "**Variable**") %>%
  bold_labels()

as_flex_table(t1) %>% flextable::save_as_docx(path = "resultados_vascular/Tabla1_caracteristicas.docx")

# 6. LETALIDAD INTRAHOSPITALARIA ------------------------------------------------
wilson <- function(x, n, z = 1.96) {
  p <- x / n; d <- 1 + z^2 / n
  ce <- (p + z^2 / (2 * n)) / d
  h  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  list(lo = ce - h, hi = ce + h)
}
letal_por <- function(dt, var) {
  r <- dt[, .(N = .N, MUERTES = sum(FALLECIDO)), by = var]
  w <- wilson(r$MUERTES, r$N)
  r[, `:=`(LETALIDAD = MUERTES / N * 100, LI = w$lo * 100, LS = w$hi * 100)]
  setorderv(r, var)[]
}
let_ano    <- letal_por(analisis, "ANO")
let_sexo   <- letal_por(analisis, "SEXO_GLOSA")
let_edad   <- letal_por(analisis, "GRUPO_ETARIO")
let_pat    <- letal_por(analisis, "GRUPO_PAT")
let_dx     <- letal_por(analisis, "DIAG1")
let_sector <- letal_por(analisis, "SECTOR")
fwrite(let_ano, "resultados_vascular/letalidad_por_ano.csv")
fwrite(let_edad, "resultados_vascular/letalidad_por_edad.csv")
fwrite(let_pat, "resultados_vascular/letalidad_por_grupo_patologia.csv")
fwrite(let_dx, "resultados_vascular/letalidad_por_codigo.csv")

# Estancia: distribucion muy asimetrica -> mediana (RIC) ademas del promedio
estancia <- analisis[, .(Promedio = mean(DIAS_ESTAD), Mediana = median(DIAS_ESTAD),
                         P25 = quantile(DIAS_ESTAD, .25), P75 = quantile(DIAS_ESTAD, .75)),
                     by = ANO][order(ANO)]
fwrite(estancia, "resultados_vascular/estancia_por_ano.csv")

# Modelo logistico: factores asociados a muerte hospitalaria
m_dat <- copy(analisis)
m_dat[, TRAMO_EDAD := relevel(TRAMO_EDAD, ref = "20-39")]
m_dat[, GRUPO_PAT := relevel(GRUPO_PAT, ref = "Hemorragia intracerebral (I61)")]
m_dat[, SECTOR := relevel(factor(SECTOR), ref = "SNSS (publico)")]
m_dat[, ANO_C := ANO - ANO_MIN]
mod_let <- glm(FALLECIDO ~ SEXO_GLOSA + TRAMO_EDAD + GRUPO_PAT +
                 SECTOR + ANO_C, family = binomial, data = m_dat)
or_tab <- tidy(mod_let, exponentiate = TRUE, conf.int = TRUE)
fwrite(or_tab, "resultados_vascular/OR_letalidad.csv")
print(or_tab)

# 7. TASAS DE EGRESO (requieren poblacion INE) ---------------------------------
# Archivo: ano;sexo;grupo;poblacion  (sexo = Hombre/Mujer; grupo = GRUPOS)
# Use UNA sola serie de proyecciones INE para todo el periodo (evita quiebres).
if (!file.exists(ARCHIVO_POBLACION)) {
  plantilla <- CJ(ano = ANO_MIN:ANO_MAX, sexo = c("Hombre", "Mujer"), grupo = GRUPOS)
  plantilla[, poblacion := NA_real_]
  fwrite(plantilla, "poblacion_ine_PLANTILLA.csv", sep = ";")
  stop("Falta '", ARCHIVO_POBLACION, "'. Complete 'poblacion_ine_PLANTILLA.csv' ",
       "con proyecciones INE (sumando grupos quinquenales a decenales; 80+ = 80 y mas), ",
       "guardela como '", ARCHIVO_POBLACION, "' y vuelva a ejecutar.")
}
pob <- fread(ARCHIVO_POBLACION)
setnames(pob, tolower(names(pob)))
stopifnot(all(c("ano", "sexo", "grupo", "poblacion") %in% names(pob)),
          !anyNA(pob$poblacion))

casos <- analisis[, .(N = .N, MUERTES = sum(FALLECIDO)),
                  by = .(ANO, SEXO_GLOSA, GRUPO_ETARIO)]
base <- CJ(ANO = ANO_MIN:ANO_MAX, SEXO_GLOSA = c("Hombre", "Mujer"), GRUPO_ETARIO = GRUPOS)
base <- merge(base, casos, by = c("ANO", "SEXO_GLOSA", "GRUPO_ETARIO"), all.x = TRUE)
base[is.na(N), `:=`(N = 0L, MUERTES = 0L)]
base <- merge(base, pob[, .(ANO = ano, SEXO_GLOSA = sexo, GRUPO_ETARIO = grupo, POB = poblacion)],
              by = c("ANO", "SEXO_GLOSA", "GRUPO_ETARIO"), all.x = TRUE)
stopifnot(!anyNA(base$POB))

# 7.1 Tasas crudas (IC95% Poisson exacto)
ic_pois <- function(x) list(lo = ifelse(x == 0, 0, qchisq(0.025, 2 * x) / 2),
                            hi = qchisq(0.975, 2 * (x + 1)) / 2)
tasa_cruda <- function(dt, por) {
  r  <- dt[, .(N = sum(N), MUERTES = sum(MUERTES), POB = sum(POB)), by = por]
  ci <- ic_pois(r$N)
  r[, `:=`(TASA = N / POB * 1e5, LI = ci$lo / POB * 1e5, LS = ci$hi / POB * 1e5,
           TASA_MORT_HOSP = MUERTES / POB * 1e5)]
  setorderv(r, por)[]
}
t_ano      <- tasa_cruda(base, "ANO")
t_ano_sexo <- tasa_cruda(base, c("ANO", "SEXO_GLOSA"))
t_edad     <- tasa_cruda(base, "GRUPO_ETARIO")   # promedio anual del periodo
t_edad[, GRUPO_ETARIO := factor(GRUPO_ETARIO, levels = GRUPOS)]
t_edad <- t_edad[order(GRUPO_ETARIO)]
t_sexo     <- tasa_cruda(base, "SEXO_GLOSA")

# 7.2 Tasas estandarizadas por edad (metodo directo, estandar OMS 2000-2025
#     agrupado en decenas; verifique los pesos con la fuente OMS antes de publicar)
est <- data.table(GRUPO_ETARIO = GRUPOS,
                  W = c(17.55, 17.07, 16.15, 14.76, 12.63, 9.92, 6.68, 3.73, 1.545))
est[, W := W / sum(W)]
asr_fun <- function(dt, por) {
  x <- dt[, .(N = sum(N), POB = sum(POB)), by = c(por, "GRUPO_ETARIO")]
  x <- merge(x, est, by = "GRUPO_ETARIO")
  r <- x[, .(TASA_EST = sum(W * N / POB) * 1e5,
             EE = sqrt(sum(W^2 * N / POB^2)) * 1e5), by = por]
  r[, `:=`(LI_EST = TASA_EST - 1.96 * EE, LS_EST = TASA_EST + 1.96 * EE)]
  setorderv(r, por)[]
}
asr_ano      <- asr_fun(base, "ANO")
asr_ano_sexo <- asr_fun(base, c("ANO", "SEXO_GLOSA"))

# 7.3 Tendencia: cambio porcentual anual (modelo quasi-Poisson con offset)
apc <- function(m, term) {
  b <- coef(m)[term]; ci <- confint.default(m)[term, ]
  data.table(APC_pct = (exp(b) - 1) * 100, LI = (exp(ci[1]) - 1) * 100,
             LS = (exp(ci[2]) - 1) * 100, p = summary(m)$coefficients[term, 4])
}
ano_anual <- base[, .(N = sum(N), POB = sum(POB)), by = ANO]
m_cruda <- glm(N ~ I(ANO - ANO_MIN) + offset(log(POB)), family = quasipoisson, data = ano_anual)
m_aj    <- glm(N ~ I(ANO - ANO_MIN) + SEXO_GLOSA + GRUPO_ETARIO + offset(log(POB)),
               family = quasipoisson, data = base)
# Sensibilidad: sin 2020-2021 (efecto COVID-19 sobre la actividad hospitalaria)
m_sens  <- glm(N ~ I(ANO - ANO_MIN) + SEXO_GLOSA + GRUPO_ETARIO + offset(log(POB)),
               family = quasipoisson, data = base[!ANO %in% 2020:2021])
tendencia <- rbindlist(list(
  cbind(Modelo = "Cruda", apc(m_cruda, "I(ANO - ANO_MIN)")),
  cbind(Modelo = "Ajustada por sexo y edad", apc(m_aj, "I(ANO - ANO_MIN)")),
  cbind(Modelo = "Ajustada, excluyendo 2020-2021", apc(m_sens, "I(ANO - ANO_MIN)"))))
print(tendencia)
fwrite(tendencia, "resultados_vascular/tendencia_APC.csv")

# 7.4 Tabla 2 (por ano) y exportaciones
tab2 <- Reduce(function(a, b) merge(a, b, by = "ANO"),
               list(t_ano[, .(ANO, Egresos = N, Fallecidos = MUERTES, Poblacion = POB,
                              Tasa_cruda = TASA, LI, LS, Tasa_mort_hosp = TASA_MORT_HOSP)],
                    asr_ano[, .(ANO, Tasa_estandarizada = TASA_EST, LI_EST, LS_EST)],
                    let_ano[, .(ANO, Letalidad_pct = LETALIDAD, LI_let = LI, LS_let = LS)]))
fwrite(tab2, "resultados_vascular/Tabla2_tasas_por_ano.csv")
fwrite(t_ano_sexo, "resultados_vascular/tasas_ano_sexo.csv")
fwrite(t_edad, "resultados_vascular/tasas_por_edad.csv")
fwrite(asr_ano_sexo, "resultados_vascular/tasas_estandarizadas_ano_sexo.csv")
flextable(tab2 %>% mutate(across(where(is.numeric), ~ round(.x, 2)))) %>%
  flextable::save_as_docx(path = "resultados_vascular/Tabla2_tasas_por_ano.docx")

# 7.5 Calidad de codificacion y sensibilidad -----------------------------------
# La proporcion de codigos inespecificos (I60.9, I61.9, I62.9) y de topografias
# especificas cambia entre anos: parte de la tendencia por subgrupo puede ser
# practica de codificacion y no epidemiologia.
cod_ano <- analisis[, .(Egresos = .N,
                        pct_inespecificos = mean(DIAG1 %chin% CODIGOS_INESPECIFICOS) * 100,
                        pct_hemorragicas  = mean(substr(DIAG1, 1, 3) %chin% c("I60", "I61", "I62")) * 100,
                        pct_malformaciones = mean(substr(DIAG1, 1, 1) == "Q") * 100,
                        pct_menores_20 = mean(EDAD_INF < 20, na.rm = TRUE) * 100),
                    by = ANO][order(ANO)]
fwrite(cod_ano, "resultados_vascular/calidad_codificacion_por_ano.csv")
print(cod_ano)

pob_ano <- pob[, .(POB = sum(poblacion)), by = .(ANO = ano)]

# Tasa excluyendo codigos inespecificos
sens_inesp <- merge(analisis[!DIAG1 %chin% CODIGOS_INESPECIFICOS, .(N_sin_inespecificos = .N), by = ANO],
                    pob_ano, by = "ANO")
sens_inesp[, TASA_sin_inespecificos := N_sin_inespecificos / POB * 1e5]
fwrite(sens_inesp, "resultados_vascular/sensibilidad_sin_inespecificos.csv")
print(sens_inesp)

# 7.6 Tasas y letalidad por grupo de patologia y ano (poblacion total)
grp_ano <- analisis[, .(N = .N, MUERTES = sum(FALLECIDO)), by = .(ANO, GRUPO_PAT)]
grp_ano <- merge(CJ(ANO = ANO_MIN:ANO_MAX, GRUPO_PAT = factor(niveles_pat[!is.na(niveles_pat)],
                                                                levels = niveles_pat[!is.na(niveles_pat)])),
                 grp_ano, by = c("ANO", "GRUPO_PAT"), all.x = TRUE)
grp_ano[is.na(N), `:=`(N = 0L, MUERTES = 0L)]
grp_ano <- merge(grp_ano, pob_ano, by = "ANO")
ci_g <- ic_pois(grp_ano$N)
grp_ano[, `:=`(TASA = N / POB * 1e5, LI = ci_g$lo / POB * 1e5, LS = ci_g$hi / POB * 1e5,
               LETALIDAD_pct = fifelse(N > 0, MUERTES / N * 100, NA_real_))]
fwrite(grp_ano, "resultados_vascular/tasas_por_grupo_patologia_ano.csv")

# 7.7 Tendencia por grupo de patologia (APC, quasi-Poisson, ajustada por sexo y edad)
tend_grupo <- rbindlist(lapply(levels(analisis$GRUPO_PAT), function(g) {
  cs <- analisis[GRUPO_PAT == g, .(N = .N), by = .(ANO, SEXO_GLOSA, GRUPO_ETARIO)]
  bb <- merge(base[, .(ANO, SEXO_GLOSA, GRUPO_ETARIO, POB)], cs,
              by = c("ANO", "SEXO_GLOSA", "GRUPO_ETARIO"), all.x = TRUE)
  bb[is.na(N), N := 0L]
  m <- glm(N ~ I(ANO - ANO_MIN) + SEXO_GLOSA + GRUPO_ETARIO + offset(log(POB)),
           family = quasipoisson, data = bb)
  cbind(Grupo = g, Egresos = sum(bb$N), apc(m, "I(ANO - ANO_MIN)"))
}))
fwrite(tend_grupo, "resultados_vascular/tendencia_APC_por_grupo.csv")
print(tend_grupo)

# 8. FIGURAS --------------------------------------------------------------------
tema <- theme_minimal(base_size = 12)

g1 <- merge(t_ano_sexo, asr_ano_sexo, by = c("ANO", "SEXO_GLOSA")) %>%
  ggplot(aes(ANO, TASA_EST, colour = SEXO_GLOSA)) +
  geom_ribbon(aes(ymin = LI_EST, ymax = LS_EST, fill = SEXO_GLOSA), alpha = .15, colour = NA) +
  geom_line(linewidth = 1) + geom_point() +
  scale_x_continuous(breaks = ANO_MIN:ANO_MAX) +
  labs(x = "Ano", y = "Tasa estandarizada por 100.000 hab.", colour = NULL, fill = NULL) +
  tema + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("resultados_vascular/Fig1_tasa_estandarizada_sexo.png", g1, width = 8, height = 5, dpi = 300)

g2 <- ggplot(t_edad, aes(GRUPO_ETARIO, TASA)) +
  geom_col(fill = "#2c6fbb") +
  geom_errorbar(aes(ymin = LI, ymax = LS), width = .25) +
  labs(x = "Grupo de edad (anos)", y = "Tasa promedio anual por 100.000 hab.") + tema
ggsave("resultados_vascular/Fig2_tasa_por_edad.png", g2, width = 8, height = 5, dpi = 300)

g3 <- ggplot(let_ano, aes(ANO, LETALIDAD)) +
  geom_ribbon(aes(ymin = LI, ymax = LS), alpha = .15) + geom_line(linewidth = 1) + geom_point() +
  scale_x_continuous(breaks = ANO_MIN:ANO_MAX) +
  labs(x = "Ano", y = "Letalidad intrahospitalaria (%)") + tema +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("resultados_vascular/Fig3_letalidad_por_ano.png", g3, width = 8, height = 5, dpi = 300)

g4 <- ggplot(grp_ano, aes(ANO, TASA, colour = GRUPO_PAT)) +
  geom_line(linewidth = 1) + geom_point() +
  scale_x_continuous(breaks = ANO_MIN:ANO_MAX) +
  labs(x = "Ano", y = "Tasa cruda por 100.000 hab.", colour = NULL) +
  tema + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 2))
ggsave("resultados_vascular/Fig4_tasa_por_grupo_patologia.png", g4, width = 9, height = 6, dpi = 300)

# 99. VERSIONES (reproducibilidad) -----------------------------------------------
# Guarda la sesion completa y una tabla en Markdown lista para pegar en el README.
writeLines(capture.output(sessionInfo()), "resultados_vascular/session_info.txt")
pk <- c("tidyverse", "data.table", "gtsummary", "flextable", "broom")
vers <- data.frame(
  Componente = c("R", pk),
  Version    = c(paste(R.version$major, R.version$minor, sep = "."),
                 sapply(pk, function(x) as.character(packageVersion(x)))))
writeLines(c("| Componente | Version |", "|---|---|",
             paste0("| ", vers$Componente, " | ", vers$Version, " |")),
           "resultados_vascular/versiones_para_README.md")

message("Listo. Resultados en la carpeta 'resultados_vascular/'.")