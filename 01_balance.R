#=============================================================================#
#   Titutlo:     Balance de la aleatorizacion de la estrategia de permanencia
#   Contenido:   Este archivo toma los datos producidos por la aleatorizacion 
#                en el archivo 00_randomization, y hace un analisis descriptivo
#                para revisar el balance entre el grupo control y tratamiento
#=============================================================================#

#install.packages("tableone")
#install.packages("fastDummies")
library(tableone)
library(fastDummies)

rm(list = ls())
setwd("C:/Users/sergi/Dropbox (Personal)/Phding/Research papers/Dropouts")
data <- read.csv("randomized_data_SED.csv")

data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Fixing some variables
data <- data %>%
  mutate(
    estrato_grouped = case_when(
      ESTRATO %in% c(0, 1, 2, 3) ~ as.character(ESTRATO),   # Keep 0-3 as is
      ESTRATO %in% c(4, 5, 6)    ~ "4 to 6",                   # Group 4,5,6 together
      ESTRATO == 9              ~ "NA",           # Treat 9 as missing
      TRUE                      ~ "NA"            # Catch any unexpected values
    ),
    
    trabaja_dummy = if_else(TRABAJA == 1, 1, 0),
    female = if_else(GENERO == "F", 1, 0),
    has_disability = if_else(TIPO_DISCAPACIDAD %in% 3:18, 1, 0),
    urban = if_else(ZONA.SDP == "URBANA", 1, 0),
    rural = if_else(ZONA.SDP == "RURAL", 1, 0),
    foreign = if_else(NOMBRE_PAIS != "COLOMBIA", 1, 0),
    academic_support = if_else(DESCRIP_APOYO_ACADEMICO != "NO APLICA", 1, 0),
    jornada_unica   = if_else(DESCRIP_JORNADA == "ÃsNICA", 1, 0),
    jornada_manana  = if_else(DESCRIP_JORNADA == "MAÃ'ANA", 1, 0),
    jornada_tarde   = if_else(DESCRIP_JORNADA == "TARDE", 1, 0),
    
    repeat_1 = case_when(
      VECES_REPETIDO_ANO == 1 ~ 1,
      !is.na(VECES_REPETIDO_ANO) ~ 0,
      TRUE ~ NA_real_
    ),
    
    repeat_2plus = case_when(
      VECES_REPETIDO_ANO >= 2 ~ 1,
      !is.na(VECES_REPETIDO_ANO) ~ 0,
      TRUE ~ NA_real_
    ),
    
    repeat_na = case_when(
      is.na(VECES_REPETIDO_ANO) ~ 1,
      TRUE ~ 0
    ),
    
    sisben_group = case_when(
      grepl("^A", SISBEN_IV) ~ "A",
      grepl("^B", SISBEN_IV) ~ "B",
      grepl("^C", SISBEN_IV) ~ "C",
      grepl("^D", SISBEN_IV) ~ "D",
      SISBEN_IV == "NO APLICA" | is.na(SISBEN_IV) ~ "NA",
      TRUE ~ "NA"  # Catch-all for unexpected values
    )
    
  )

# Convert to dummy variables and bind them to the original data
data <- data %>%
  mutate(
    estrato_grouped = ifelse(is.na(estrato_grouped), " NA", estrato_grouped),
    sisben_group    = ifelse(is.na(sisben_group), " NA", sisben_group),
    GRADO           = ifelse(is.na(GRADO), " NA", as.character(GRADO))
  ) %>%
  fastDummies::dummy_cols(
    select_columns = c("estrato_grouped", "sisben_group", "GRADO"),
    remove_selected_columns = FALSE,
    remove_first_dummy = FALSE  # You may want TRUE if you want to omit reference category
  )


  #===========================================================================#
  #                             Balance tables                                #  
  #===========================================================================#


library(dplyr)
library(broom)
library(purrr)
library(stringr)
library(tibble)

# Demographic and academic variables (no change)
vars_demographics <- c(
  "female", "EDAD", "ETNIA", "has_disability", "POB_VICT_CONF_RUV", "trabaja_dummy",
  "foreign", "rural", "academic_support", "repeat_1", "repeat_2plus", "repeat_na"
)

vars_academic <- c("jornada_unica", "jornada_manana", "jornada_tarde", "SABER11")

# Keep only dummy variables that do NOT correspond to __MISSING__
vars_ses <- grep("^(estrato_grouped|sisben_group)_(?!__MISSING__)", names(data), value = TRUE, perl = TRUE)
vars_grade <- grep("^GRADO_(?!__MISSING__)", names(data), value = TRUE, perl = TRUE)

# Manually defined labels
var_labels <- c(
  # Table 1: Demográficos
  female = "Mujer",
  EDAD = "Edad",
  ETNIA = "Pertenencia étnica",
  has_disability = "Tiene discapacidad",
  POB_VICT_CONF_RUV = "Víctima del conflicto armado",
  trabaja_dummy = "Actualmente trabaja",
  foreign = "Estudiante extranjero(a)",
  rural = "Zona rural",
  academic_support = "Recibe apoyo académico",
  repeat_1 = "Repitió un año escolar",
  repeat_2plus = "Repitió dos años o más",
  repeat_na = "Repetición: dato no disponible",
  
  # Table 3: Información académica previa
  jornada_unica = "Jornada única",
  jornada_manana = "Jornada mañana",
  jornada_tarde = "Jornada tarde",
  SABER11 = "Puntaje SABER 11"
)

# Estrato
estrato_labels <- setNames(
  paste0("Estrato ", gsub("estrato_grouped_", "", grep("^estrato_grouped_", names(data), value = TRUE))),
  grep("^estrato_grouped_", names(data), value = TRUE)
)

# SISBEN
sisben_labels <- setNames(
  paste0("SISBEN grupo ", gsub("sisben_group_", "", grep("^sisben_group_", names(data), value = TRUE))),
  grep("^sisben_group_", names(data), value = TRUE)
)

# Grado
grado_labels <- setNames(
  paste0("Grado ", gsub("GRADO_", "", grep("^GRADO_", names(data), value = TRUE))),
  grep("^GRADO_", names(data), value = TRUE)
)

var_labels <- c(
  var_labels,
  estrato_labels,
  sisben_labels,
  grado_labels
)

# ============================================================================#
#  Code for balance tests                                                     #  
# ============================================================================#

balance_ttest <- function(varname, data) {
  if (!varname %in% names(data)) return(NULL)
  
  var <- data[[varname]]
  treat <- data$treatment
  if (all(is.na(var))) return(NULL)
  
  # Categorical variables expanded into dummies
  if (is.character(var) || is.factor(var) || varname %in% c("estrato_grouped", "sisben_group", "GRADO")) {
    dummies <- model.matrix(~ var + 0, data = data)
    colnames(dummies) <- str_replace_all(colnames(dummies), "var", varname)
    df <- cbind(dummies, treatment = treat)
  } else {
    df <- data.frame(temp_var = var, treatment = treat)
    names(df)[1] <- varname
  }
  
  # Run t-test and compute means + SEs
  result <- map_dfr(setdiff(names(df), "treatment"), function(v) {
    test <- tryCatch(t.test(df[[v]] ~ df$treatment), error = function(e) return(NULL))
    if (is.null(test)) return(NULL)
    
    stats <- df %>%
      group_by(treatment) %>%
      summarise(
        mean = mean(.data[[v]], na.rm = TRUE),
        se = sd(.data[[v]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[v]]))),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = treatment,
        values_from = c(mean, se),
        names_sep = "_"
      )
    
    tibble(
      Variable = v,
      Mean_Treatment = stats$mean_Treatment,
      SE_Treatment   = stats$se_Treatment,
      Mean_Control   = stats$mean_Control,
      SE_Control     = stats$se_Control,
      Difference     = stats$mean_Treatment - stats$mean_Control,
      SE_Diff        = test$stderr,
      p_value        = test$p.value
    )
  })
  
  return(result)
}

generate_balance_table <- function(var_list, data, var_labels, table_caption = NULL) {
  results <- map_dfr(var_list, ~ balance_ttest(.x, data))
  if (nrow(results) == 0) return(NULL)
  
  results_labeled <- results %>%
    mutate(
      Label = ifelse(Variable %in% names(var_labels),
                     var_labels[Variable],
                     Variable)
    ) %>%
    mutate(
      row_main = sprintf("%s & %.3f & %.3f & %.3f & %.3f \\\\", 
                         Label, Mean_Treatment, Mean_Control, Difference, p_value),
      row_se   = sprintf(" & {\\footnotesize (%.3f)} & {\\footnotesize (%.3f)} & {\\footnotesize (%.3f)} & \\\\", 
                         SE_Treatment, SE_Control, SE_Diff)
    )
  
  # Calculate N: number of non-missing observations in treatment variable
  N <- sum(!is.na(data$treatment))
  
  # Add N row at the bottom
  n_row <- sprintf("N & %d &  &  &  \\\\", N)
  
  latex_code <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    if (!is.null(table_caption)) paste0("\\caption{", table_caption, "}"),
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Variable & Media Tratamiento & Media Control & Diferencia & p-valor \\\\",
    "\\midrule",
    unlist(purrr::map2(results_labeled$row_main, results_labeled$row_se, ~ c(.x, .y))),
    "\ &  &  &  & \\\\",
    n_row,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  
  return(paste(latex_code, collapse = "\n"))
}

table1 <- generate_balance_table(vars_demographics, data, var_labels, "Características Demográficas")
table2 <- generate_balance_table(vars_ses, data, var_labels, "Nivel Socioeconómico")
table3 <- generate_balance_table(vars_academic, data, var_labels, "Información Académica Previa")
table4 <- generate_balance_table(vars_grade, data, var_labels, "Grado Académico")

writeLines(table1, "balance_table1_demographics.tex")
writeLines(table2, "balance_table2_ses.tex")
writeLines(table3, "balance_table3_academic.tex")
writeLines(table4, "balance_table4_grade.tex")  


# Loop over each decile from 1 to 10
for (d in 1:10) {
  # Subset data for decile d
  data_d <- data %>% filter(IRD_acp_decile == d)
  
  # Generate balance tables
  table1 <- generate_balance_table(vars_demographics, data_d, var_labels, 
                                   paste("Características Demográficas - Decil", d))
  table2 <- generate_balance_table(vars_ses, data_d, var_labels, 
                                   paste("Nivel Socioeconómico - Decil", d))
  table3 <- generate_balance_table(vars_academic, data_d, var_labels, 
                                   paste("Información Académica Previa - Decil", d))
  table4 <- generate_balance_table(vars_grade, data_d, var_labels, 
                                   paste("Grado Académico - Decil", d))
  
  # Save each table as a separate .tex file
  writeLines(table1, paste0("balance_table1_demographics_decile", d, ".tex"))
  writeLines(table2, paste0("balance_table2_ses_decile", d, ".tex"))
  writeLines(table3, paste0("balance_table3_academic_decile", d, ".tex"))
  writeLines(table4, paste0("balance_table4_grade_decile", d, ".tex"))
}




