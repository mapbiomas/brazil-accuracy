
library(RSQLite)
library(DBI)
library(MachineShop)
library(resample)
library(gbm)
library(forecast)
library(caret)
library(data.table)
library(magrittr)
library(tidyverse)
library(scales)
library(ggplot2)
library(dplyr)
library(lattice)
library(survey)
library(srvyr)
library(future)
library(future.apply)
library(openxlsx)
library(writexl)
library(readxl)

#indicar pasta de trabalho
setwd("/collection_110")

# Read points_strata file
points_strata <- read.csv("/collection_110/points_strata.csv", header = TRUE, sep = ',', encoding = 'UTF-8')

# strata population
strata_pop <- read.csv("/collection_110/strata.csv", header = TRUE, sep = ',', encoding = 'UTF-8')



#Criando os dicionários IGNORED_CLASSES e ALL_CLASSES e sub-dicionários de nível.

IGNORED_CLASSES<-c(0,5,23,25,27,29,30,31,32,75,84,91)



ALL_CLASSES_FILE <- "/collection_110/ALL_CLASSES.xlsx"
ALL_CLASSES_df <- read.xlsx(ALL_CLASSES_FILE, sheet = 1)

# Converter para lista de listas indexada por ID
ALL_CLASSES <- lapply(1:nrow(ALL_CLASSES_df), function(i) {
  as.list(ALL_CLASSES_df[i, ])
})
names(ALL_CLASSES) <- as.character(ALL_CLASSES_df$ID)
# Function config_class


# Define functions

# Function get_classes

get_classes <- function(df, level = NULL) {
  
  class_values <- list()
  class_names <- list()
  
  clas_classes <- unique(df$classification)
  ref_classes <- unique(df$reference)
  
  acc_classes <- intersect(clas_classes,ref_classes)
  
  val_remap <- list()
  
  for (value_aux in names(ALL_CLASSES)) {
    if (!(value_aux %in% IGNORED_CLASSES) && (value_aux %in% acc_classes)) {
      val_key <- paste0(level, "_val")
      new_val <- ALL_CLASSES[[value_aux]][[val_key]]
      class_name <- ALL_CLASSES[[value_aux]][[level]]
      
      val_remap[[as.character(value_aux)]] <- new_val
      class_values[[as.character(new_val)]] <- TRUE
      class_names[[class_name]] <- TRUE
    }
  }
  
  df <- df %>% filter(classification %in% names(val_remap))
  df <- df %>% filter(reference %in% names(val_remap))
  
  df$classification <- unlist(lapply(df$classification, function(x) val_remap[[as.character(x)]]))
  df$reference <- unlist(lapply(df$reference, function(x) val_remap[[as.character(x)]]))
  
  class_values <- names(unlist(class_values))
  class_names <- names(unlist(class_names))
  
  return(list(df, class_values, class_names))
}

########################################################################



config_class <- function(df) {
  
  # geral (BRASIL)
  df <- df %>% mutate(classification = case_when(classification %in% c(20, 39, 40, 41, 62) ~ 19,TRUE ~ classification)) #lavouras temporarias
  df <- df %>% mutate(classification = case_when(classification %in% c(46, 47, 48, 35) ~ 36,TRUE ~ classification)) #lavouras perenes
  df <- df %>% mutate(classification = case_when(classification %in% 49 ~ 3,TRUE ~ classification)) #Restinga arbórea
  df <- df %>% mutate(classification = case_when(classification %in% 50 ~ 12,TRUE ~ classification))#restinga herbácea
  df <- df %>% mutate(reference = case_when(reference %in% 50 ~ 12,TRUE ~ reference))#restinga herbácea
  df <- df %>% mutate(classification = case_when(classification %in% 6 ~ 3,TRUE ~ classification))#Floresta alagável
  df <- df %>% mutate(reference = case_when(reference %in% 6 ~ 3,TRUE ~ reference))#Floresta alagável
  df <- df %>% mutate(reference = ifelse(classification == 21 & reference %in% c(15,19,20,36), 21, reference)) #mosaico de usos
  
  # Pampa
  df <- df %>%
    mutate(reference = ifelse(BioNB == 'Pampa' & reference == 15, 21, reference),
           reference = ifelse(BioNB == 'Pampa' & classification == 25 & reference == 23, 25, reference),
           reference = ifelse(BioNB == 'Pampa' & classification == 23 & reference == 25, 23, reference),
           reference = ifelse(BioNB == 'Pampa' & reference == 4, 12, reference))
  
  # Mata Atlântica
  df <- df %>%
     mutate(reference = ifelse(BioNB == 'Mata Atlântica' & classification == 12 & reference == 15, 12, reference))  
  
  # Pantanal
  df <- df %>%
    mutate(reference = ifelse(BioNB == 'Pantanal' & classification == 11 & reference == 12, 11, reference),
           reference = ifelse(BioNB == 'Pantanal' & classification == 11 & reference == 33, 11, reference),
           reference = ifelse(BioNB == 'Pantanal' & classification == 12 & reference == 33, 12, reference),
           reference = ifelse(BioNB == 'Pantanal' & classification == 12 & reference == 11, 12, reference),
           classification = ifelse(BioNB == 'Pantanal' & classification == 33 & reference == 11, 11, classification),
           classification = ifelse(BioNB == 'Pantanal' & classification == 33 & reference == 12, 12, classification)
           ,reference = ifelse(BioNB == 'Pantanal' & classification == 7 & reference %in% c(11,4,33), 7, reference)) 
  
  # Amazônia
  df <- df %>%
    mutate(reference = ifelse(BioNB == 'Amazônia' & classification == 6 & reference == 3, 3, reference),
           reference = ifelse(BioNB == 'Amazônia' & classification == 3 & reference == 6, 3, reference))
  
  # Cerrado
  df <- df %>%
    mutate(reference = ifelse(BioNB == 'Cerrado' & classification == 25 & reference == 23, 25, reference),
           reference = ifelse(BioNB == 'Cerrado' & classification == 23 & reference == 25, 23, reference),
           reference = ifelse(BioNB == 'Cerrado' & classification == 50 & reference == 12, 12, reference))
  
  # Caatinga
  df <- df %>%
    mutate(#reference = ifelse(BioNB == 'Caatinga' & reference == 9,  18, reference),
      reference = ifelse(BioNB == 'Caatinga' & reference == 11, 12, reference),
      reference = ifelse(BioNB == 'Caatinga' & classification == 13 & reference %in% c(12,4), 13, reference)
      ,reference = ifelse(BioNB == 'Caatinga' & classification == 77 & reference %in% c(12,4), 77, reference)) #mosaico herbaceo rupestre))
  
  return(df)
}


config_class_nova <- function(df) {
  
  
  # 1. Carregar Regras Gerais 📚
  # (Substitua 'caminho/para/o/arquivo/' pelo caminho real do seu arquivo)
  regras_gerais <- read_excel("tab_config_classes.xlsx")
  
  # Mapeamento para 'classification'
  mapa_class <- regras_gerais %>%
    filter(target_field == 'classification') %>%
    select(original_value, new_value)
  
  # Mapeamento para 'reference'
  mapa_ref <- regras_gerais %>%
    filter(target_field == 'reference') %>%
    select(original_value, new_value)
  
  # Aplica o mapeamento para 'classification'
  df <- df %>%
    mutate(
      classification = coalesce(
        mapa_class$new_value[match(df$classification, mapa_class$original_value)],
        classification
      )
    )
  
  # Aplica o mapeamento para 'reference'
  df <- df %>%
    mutate(
      reference = coalesce(
        mapa_ref$new_value[match(df$reference, mapa_ref$original_value)],
        reference
      )
    )
  
  ------------------------------------------------------------------
    # APLICAÇÃO DAS REGRAS ESPECÍFICAS POR BIOMA (MAIS COMPLEXAS)
    ------------------------------------------------------------------
    
    # Para as regras complexas (que envolvem BioNB, classification E reference),
    # é melhor usar uma tabela de mapeamento que inclua as 4 colunas.
    # Exemplo: BioNB, classification_original, reference_original, reference_nova
    
    # BioNB, classification_match, reference_original, reference_nova
    
    
    # 2. Carregar Regras de Bioma 🗺
    regras_bioma <- read_excel("tab_config_classes.xlsx")
  
  # APLICAÇÃO USANDO left_join (mais limpo para múltiplas condições)
  
  # Etapa 1: Juntar as regras de bioma com o dataframe principal
  df_temp <- df %>%
    left_join(
      regras_bioma,
      by = c("BioNB" = "BioNB", # Colunas de junção
             "classification" = "classification_match",
             "reference" = "reference_original")
    )
  
  # Etapa 2: Usar o valor mapeado se ele existir (ou seja, se a junção deu match)
  df <- df_temp %>%
    mutate(
      reference = coalesce(reference_nova, reference), # Se reference_nova não for NA, usa ele
      classification = coalesce(classification_nova, classification) # Se classification_nova não for NA, usa ele
    ) %>%
    select(-reference_nova, -classification_nova) # Remove as colunas temporárias
  
  return(df)
}




# Função para filtrar por bioma específico
filter_by_biome <- function(df, biome_name) {
  df_filtered <- df %>% filter(BioNB == biome_name)
  return(df_filtered)
}

# # Função para calcular matriz de confusão personalizada com pesos e contar amostras
# custom_confusion <- function(predicted, actual, weights) {
#   actual <- factor(actual)
#   predicted <- factor(predicted)
#   
#   levels <- union(levels(actual), levels(predicted))
#   actual <- factor(actual, levels = levels)
#   predicted <- factor(predicted, levels = levels)
#   
#   conf_matrix <- matrix(0, nrow = length(levels), ncol = length(levels), dimnames = list(Predicted = levels, Actual = levels))
#   
#   
#   #contador ineficiente
#   sample_count_matrix <- matrix(0, nrow = length(levels), ncol = length(levels), dimnames = list(Predicted = levels, Actual = levels))
#   
#   for (i in 1:length(actual)) {
#     conf_matrix[predicted[i], actual[i]] <- conf_matrix[predicted[i], actual[i]] + weights[i]
#     sample_count_matrix[predicted[i], actual[i]] <- sample_count_matrix[predicted[i], actual[i]] + 1
#   }
#   
#   conf_matrix_df <- as.data.frame(as.table(conf_matrix))
#   colnames(conf_matrix_df) <- c("Predicted", "Actual", "Value")
#   
#   sample_count_df <- as.data.frame(as.table(sample_count_matrix))
#   colnames(sample_count_df) <- c("Predicted", "Actual", "SampleCount")
#   
#   result_df <- merge(conf_matrix_df, sample_count_df, by = c("Predicted", "Actual"))
#   
#   return(result_df)
# }


# Initialize an empty data frame to store all data
final_df <- data.frame()

biomes <- c('Amazônia', 'Mata Atlântica', 'Pantanal', 'Cerrado', 'Caatinga', 'Pampa')
levels <- c('l1','l2','l3')

# sequência de anos
#anos <- 1985:2024
anos <- 1985:2024
#'c8','c9','c10',,'c110-4-spt_1'
colecoes = 'c11'


##########################################################################################################################

# ==============================================================================
# ETAPA 2: Processamento por Coleção (Subpastas ACC_<colecao>) e Nível
# ==============================================================================

# Diretório base onde estão as pastas das coleções
base_dir <- "C:/Users/Windows 11/Documents/Acuracias_colecoes/85k_v3"

# Loop principal sobre cada coleção listada no vetor `colecoes`
for (col in colecoes) {
  
  cat("\n==========================================")
  cat("\nProcessando Coleção:", col)
  cat("\n==========================================\n")
  
  # Monta o caminho completo da subpasta da coleção (ex: "C:/Users/.../Acuracias/ACC_c8")
  pasta_colecao <- file.path(base_dir, paste0("ACC_", col))
  
  # Monta os caminhos dos arquivos para os anos definidos (ex: "ACC_c8/acc_mapbiomas_2020.csv")
  arquivos <- file.path(pasta_colecao, paste0("acc_mapbiomas_", anos, ".csv"))
  
  # Mantém somente os arquivos que realmente existem no disco
  arquivos <- arquivos[file.exists(arquivos)]
  
  if (length(arquivos) == 0) {
    warning(paste("Nenhum arquivo encontrado na pasta:", pasta_colecao))
    next # Pula para a próxima coleção
  }
  
  # 1. Ler e empilhar os arquivos da coleção atual
  df_col <- map_dfr(
    arquivos,
    ~ read_delim(
      .x,
      delim = ",",                           
      locale = locale(decimal_mark = "."),   
      show_col_types = FALSE
    ) |>
      mutate(
        year = as.integer(str_extract(basename(.x), "\\d{4}"))
      )
  )
  
  # 2. Merge com as tabelas de suporte
  df_col <- merge(df_col, points_strata, by = "TARGETID")
  df_col <- merge(df_col, strata_pop, by = "strata_id")
  
  # 3. Converter códigos numéricos de Bioma para os nomes oficiais
  mod_BioNB <- function(df) {
    df$BioNB[df$BioNB == 1] <- 'Amazônia'
    df$BioNB[df$BioNB == 4] <- 'Mata Atlântica'
    df$BioNB[df$BioNB == 6] <- 'Pantanal'
    df$BioNB[df$BioNB == 3] <- 'Cerrado'
    df$BioNB[df$BioNB == 2] <- 'Caatinga'
    df$BioNB[df$BioNB == 5] <- 'Pampa'
    
    return(df)
  }
  
  df_col <- mod_BioNB(df_col)
  
  # 4. Filtrar amostras conforme a regra de NA
  df_col <- df_col %>% filter(is.na(AMOSTRAS) == TRUE)
  
  # ----------------------------------------------------------------------------
  # Loop interno por Nível de Legenda (l1, l2, l3)
  # ----------------------------------------------------------------------------
  for (level in levels) {
    
    cat("  -> Processando Nível:", level, "para a coleção:", col, "\n")
    
    # Aplica as regras de harmonização de classe
    dados <- config_class(df_col)
    
    # Remapeia e filtra classes por nível
    get_classes_result <- get_classes(dados, level = level)
    dados          <- get_classes_result[[1]]
    valores_classe <- get_classes_result[[2]]
    nomes_classe   <- get_classes_result[[3]]
    
    # Ajusta fatores e variáveis de design amostral
    levels_class_union <- union(unique(dados$classification),
                                unique(dados$reference))
    
    dados$classification <- factor(dados$classification, levels = levels_class_union)
    dados$reference      <- factor(dados$reference, levels = levels_class_union)
    dados$strata_aux     <- factor(paste0(dados$BioNB, ".", dados$DECLIVIDAD))
    dados$strata_id      <- factor(dados$strata_id)
    dados$CARTA_2        <- factor(dados$CARTA_2)
    dados$WEIGHT_VOT     <- 1 / dados$PESO_VOT
    
    # Seleção final de colunas necessárias
    dados <- dados %>%
      select(TARGETID, year, BioNB, strata_id, classification, reference,
             WEIGHT_VOT, pop)
    
    dados$year      <- factor(dados$year)
    dados$BioNB     <- factor(dados$BioNB)
    dados$TARGETID  <- factor(paste0(dados$TARGETID, "_", dados$year))
    dados$strata_id <- factor(paste0(dados$strata_id, "_", dados$year))
    
    # 5. Salvar o arquivo gerado com a coleção no nome (ex: dados_c8_l1.csv)
    nome_saida <- paste0("dados_", col, "_", level, ".csv")
    write.csv2(dados, nome_saida)
    
    cat("     Arquivo gerado:", nome_saida, "\n")
  }
}

#########################################################################
# cálculos de acurácia (por Coleção e Nível)
#########################################################################

# Pacotes (ajuste conforme seu ambiente)
library(readr)
library(dplyr)
library(tibble)
library(purrr)
library(survey)
library(future.apply)
library(openxlsx)
library(writexl)

# limpar memória
gc()

# Cria uma lista vazia para armazenar acurácias por níveis
lista_mapbiomas_acc_levels <- list()

# tabela de calibração (se for usar o calibrate depois)
calibrate_strata_pop_geral <- data.frame(
  strata_id = paste0(rep(strata_pop$strata_id, length(1985:2024)), "_",
                     rep(1985:2024, rep(length(strata_pop$strata_id), length(1985:2024)))),
  pop = rep(strata_pop$pop, length(1985:2024))
)

# ==============================================================================
# LOOP PRINCIPAL POR COLEÇÃO
# ==============================================================================
for (col in colecoes) {
  
  cat("\n==========================================")
  cat("\nCalculando Acurácias para a Coleção:", col)
  cat("\n==========================================\n")
  
  for (level in levels) {
    
    # Nome do arquivo CSV gerado na Etapa 2
    file_input <- paste0("dados_", col, "_", level, ".csv")
    
    # Verifica se o arquivo existe antes de tentar carregar
    if (!file.exists(file_input)) {
      warning(paste("Arquivo não encontrado:", file_input, "- Pulando..."))
      next
    }
    
    cat("  -> Processando Nível:", level, "da Coleção:", col, "\n")
    
    # leitura dos dados
    dados <- read_delim(
      file    = file_input,
      delim   = ";",
      escape_double = FALSE,
      locale = locale(decimal_mark = ","),
      trim_ws = TRUE,
      show_col_types = FALSE
    )
    
    anos   <- sort(unique(dados$year))
    biomas <- sort(unique(dados$BioNB))
    
    # ---------- Helpers ----------
    make_cbind_formula <- function(terms_chr) {
      as.formula(paste0("~cbind(", paste(terms_chr, collapse = ", "), ")"))
    }
    truncate01 <- function(x) pmin(pmax(x, 0), 1)
    
    # ---------- Funções com 'n' ----------
    
    # Matriz de confusão em proporções (todas as classes) + n
    compute_confusion_props_from_design <- function(des) {
      if (is.null(des) || nrow(des$variables) == 0L) {
        return(tibble(
          class = character(),
          reference = character(),
          p = numeric(), SE_p = numeric(), CI_p_L = numeric(), CI_p_U = numeric(),
          n = integer()
        ))
      }
      
      # n total de observações usadas nessa estimativa (após subset no design)
      n_total <- nrow(des$variables)
      
      classes_here <- sort(unique(c(des$variables$classification, des$variables$reference)))
      
      res <- lapply(classes_here, function(i) {
        lapply(classes_here, function(j) {
          m <- try(svymean(~ I(classification == i & reference == j),
                           design = des, na.rm = TRUE), silent = TRUE)
          if (inherits(m, "try-error")) {
            mu <- NA_real_; se <- NA_real_; cil <- NA_real_; ciu <- NA_real_
          } else {
            nm_true <- grep("TRUE$", names(coef(m)), value = TRUE)
            if (length(nm_true) == 1L) {
              mu <- as.numeric(coef(m)[nm_true])
              se <- as.numeric(SE(m)[nm_true])
              ci <- try(confint(m, level = 0.95), silent = TRUE)
              if (inherits(ci, "try-error")) {
                cil <- NA_real_; ciu <- NA_real_
              } else {
                idx <- which(rownames(ci) == nm_true)
                if (length(idx) == 1L) {
                  cil <- ci[idx, 1]; ciu <- ci[idx, 2]
                } else {
                  cil <- NA_real_; ciu <- NA_real_
                }
              }
            } else {
              mu <- NA_real_; se <- NA_real_; cil <- NA_real_; ciu <- NA_real_
            }
          }
          
          tibble(
            class = i,
            reference = j,
            p = truncate01(mu),
            SE_p = se,
            CI_p_L = truncate01(cil),
            CI_p_U = truncate01(ciu),
            n = n_total
          )
        }) |> bind_rows()
      }) |> bind_rows()
      
      res
    }
    
    # Acurácia global (OA) + n (robusto para nome do coeficiente)
    compute_global_acc_from_design <- function(des) {
      if (is.null(des) || nrow(des$variables) == 0L) {
        return(list(mu = NA_real_, se = NA_real_, ci_l = NA_real_, ci_u = NA_real_, n = 0L))
      }
      
      n_total <- nrow(des$variables)
      
      m <- svymean(~ I(classification == reference), design = des, na.rm = TRUE)
      
      nm <- names(coef(m))
      idx_true  <- grep("^I\\(classification == reference\\).*TRUE$", nm)
      idx_plain <- which(nm == "I(classification == reference)")
      idx_all   <- grep("^I\\(classification == reference\\)", nm)
      
      if (length(idx_true) == 1) {
        idx <- idx_true
      } else if (length(idx_plain) == 1) {
        idx <- idx_plain
      } else if (length(idx_all) == 2) {
        idx <- idx_all[grepl("TRUE$", nm[idx_all])]
      } else {
        return(list(mu = NA_real_, se = NA_real_, ci_l = NA_real_, ci_u = NA_real_, n = n_total))
      }
      
      mu <- as.numeric(coef(m)[idx])
      se <- as.numeric(SE(m)[idx])
      ci <- try(confint(m, level = 0.95), silent = TRUE)
      if (inherits(ci, "try-error")) {
        ci_l <- NA_real_; ci_u <- NA_real_
      } else {
        ci_l <- ci[idx, 1]; ci_u <- ci[idx, 2]
      }
      
      list(mu = truncate01(mu), se = se, ci_l = truncate01(ci_l), ci_u = truncate01(ci_u), n = n_total)
    }
    
    # UA/PA/CE/OE por classe + n
    compute_class_metrics_from_design <- function(des) {
      if (is.null(des) || nrow(des$variables) == 0L) {
        return(tibble(
          class_aux   = character(),
          UA      = numeric(), SE_UA = numeric(), CI_UA_L = numeric(), CI_UA_U = numeric(),
          PA      = numeric(), SE_PA = numeric(), CI_PA_L = numeric(), CI_PA_U = numeric(),
          EC      = numeric(), SE_EC = numeric(), CI_EC_L = numeric(), CI_EC_U = numeric(),
          EO      = numeric(), SE_EO = numeric(), CI_EO_L = numeric(), CI_EO_U = numeric(),
          n       = integer()
        ))
      }
      
      n_total <- nrow(des$variables)
      classes_here <- sort(unique(c(des$variables$classification, des$variables$reference)))
      
      res <- lapply(classes_here, function(k) {
        # UA_k = P(ref=k | class=k)
        ra_u <- try(svyratio(
          ~ I(classification == k & reference == k),
          ~ I(classification == k),
          design = des
        ), silent = TRUE)
        
        if (inherits(ra_u, "try-error")) {
          ua  <- NA_real_; seu <- NA_real_; ciu_l <- NA_real_; ciu_u <- NA_real_
        } else {
          ua  <- as.numeric(ra_u$ratio)
          seu <- as.numeric(SE(ra_u))
          ciu <- try(confint(ra_u, level = 0.95), silent = TRUE)
          if (inherits(ciu, "try-error")) {
            ciu_l <- NA_real_; ciu_u <- NA_real_
          } else {
            ciu_l <- ciu[1, 1]; ciu_u <- ciu[1, 2]
          }
        }
        
        # PA_k = P(class=k | ref=k)
        ra_p <- try(svyratio(
          ~ I(classification == k & reference == k),
          ~ I(reference == k),
          design = des
        ), silent = TRUE)
        
        if (inherits(ra_p, "try-error")) {
          pa  <- NA_real_; sep <- NA_real_; cip_l <- NA_real_; cip_u <- NA_real_
        } else {
          pa  <- as.numeric(ra_p$ratio)
          sep <- as.numeric(SE(ra_p))
          cip <- try(confint(ra_p, level = 0.95), silent = TRUE)
          if (inherits(cip, "try-error")) {
            cip_l <- NA_real_; cip_u <- NA_real_
          } else {
            cip_l <- cip[1, 1]; cip_u <- cip[1, 2]
          }
        }
        
        # Erros (transformações)
        ec   <- if (is.na(ua)) NA_real_ else 1 - ua
        seec <- seu
        ciec_l <- if (is.na(ciu_u)) NA_real_ else 1 - ciu_u
        ciec_u <- if (is.na(ciu_l)) NA_real_ else 1 - ciu_l
        
        eo   <- if (is.na(pa)) NA_real_ else 1 - pa
        seeo <- sep
        cieu_l <- if (is.na(cip_u)) NA_real_ else 1 - cip_u
        cieu_u <- if (is.na(cip_l)) NA_real_ else 1 - cip_l
        
        tibble(
          class_aux   = k,
          UA      = truncate01(ua),
          SE_UA   = seu,
          CI_UA_L = truncate01(ciu_l),
          CI_UA_U = truncate01(ciu_u),
          PA      = truncate01(pa),
          SE_PA   = sep,
          CI_PA_L = truncate01(cip_l),
          CI_PA_U = truncate01(cip_u),
          EC      = truncate01(ec),
          SE_EC   = seec,
          CI_EC_L = truncate01(ciec_l),
          CI_EC_U = truncate01(ciec_u),
          EO      = truncate01(eo),
          SE_EO   = seeo,
          CI_EO_L = truncate01(cieu_l),
          CI_EO_U = truncate01(cieu_u),
          n       = n_total
        )
      })
      
      bind_rows(res)
    }
    
    compute_pairwise_metrics_from_design <- function(des, level = 0.95) {
      if (is.null(des) || is.null(des$variables) || nrow(des$variables) == 0L) {
        return(list(
          pairs  = tibble::tibble(),
          by_map = tibble::tibble(),
          by_ref = tibble::tibble()
        ))
      }
      vars <- des$variables
      classes <- sort(unique(c(vars$classification, vars$reference)))
      n_total <- nrow(vars)
      
      rows <- list()
      for (m in classes) {  # class (mapa)
        n_map_m <- sum(vars$classification == m, na.rm = TRUE)
        T_map_m <- try(svytotal(~ I(classification == m), design = des), silent = TRUE)
        Nhat_map_m <- if (inherits(T_map_m, "try-error")) NA_real_ else as.numeric(coef(T_map_m))
        
        for (k in classes) {  # ref
          n_ref_k <- sum(vars$reference == k, na.rm = TRUE)
          T_ref_k <- try(svytotal(~ I(reference == k), design = des), silent = TRUE)
          Nhat_ref_k <- if (inherits(T_ref_k, "try-error")) NA_real_ else as.numeric(coef(T_ref_k))
          
          n_both_km <- sum(vars$classification == m & vars$reference == k, na.rm = TRUE)
          
          # P(ref = k | class = m)
          ra1 <- try(svyratio(~ I(classification == m & reference == k),
                              ~ I(classification == m), design = des), silent = TRUE)
          if (inherits(ra1, "try-error")) {
            p1 <- NA_real_; se1 <- NA_real_; ci1_l <- NA_real_; ci1_u <- NA_real_
          } else {
            p1 <- as.numeric(ra1$ratio)
            se1 <- as.numeric(SE(ra1))
            ci1 <- try(confint(ra1, level = level), silent = TRUE)
            if (inherits(ci1, "try-error")) { ci1_l <- NA_real_; ci1_u <- NA_real_ } else {
              ci1_l <- ci1[1,1]; ci1_u <- ci1[1,2]
            }
          }
          
          # P(class = m | ref = k)
          ra2 <- try(svyratio(~ I(classification == m & reference == k),
                              ~ I(reference == k), design = des), silent = TRUE)
          if (inherits(ra2, "try-error")) {
            p2 <- NA_real_; se2 <- NA_real_; ci2_l <- NA_real_; ci2_u <- NA_real_
          } else {
            p2 <- as.numeric(ra2$ratio)
            se2 <- as.numeric(SE(ra2))
            ci2 <- try(confint(ra2, level = level), silent = TRUE)
            if (inherits(ci2, "try-error")) { ci2_l <- NA_real_; ci2_u <- NA_real_ } else {
              ci2_l <- ci2[1,1]; ci2_u <- ci2[1,2]
            }
          }
          
          rows[[length(rows) + 1L]] <- tibble::tibble(
            class_map = m,
            class_ref = k,
            P_ref_given_class     = truncate01(p1),
            SE_ref_given_class    = se1,
            CI_ref_given_class_L  = truncate01(ci1_l),
            CI_ref_given_class_U  = truncate01(ci1_u),
            P_class_given_ref     = truncate01(p2),
            SE_class_given_ref    = se2,
            CI_class_given_ref_L  = truncate01(ci2_l),
            CI_class_given_ref_U  = truncate01(ci2_u),
            n_total = n_total,
            n_map_m = n_map_m,
            n_ref_k = n_ref_k,
            n_both_km = n_both_km,
            is_diag = (m == k)
          )
        }
      }
      pairs <- dplyr::bind_rows(rows)
      
      by_map <- pairs %>%
        dplyr::filter(is_diag) %>%
        dplyr::transmute(
          class_map,
          UA = P_ref_given_class,
          SE_UA = SE_ref_given_class,
          CI_UA_L = CI_ref_given_class_L,
          CI_UA_U = CI_ref_given_class_U,
          EC = truncate01(1 - UA),
          SE_EC = SE_UA,
          CI_EC_L = truncate01(1 - CI_UA_U),
          CI_EC_U = truncate01(1 - CI_UA_L),
          n_total
        )
      
      by_ref <- pairs %>%
        dplyr::filter(is_diag) %>%
        dplyr::transmute(
          class_ref,
          PA = P_class_given_ref,
          SE_PA = SE_class_given_ref,
          CI_PA_L = CI_class_given_ref_L,
          CI_PA_U = CI_class_given_ref_U,
          EO = truncate01(1 - PA),
          SE_EO = SE_PA,
          CI_EO_L = truncate01(1 - CI_PA_U),
          CI_EO_U = truncate01(1 - CI_PA_L),
          n_total
        )
      
      list(pairs = pairs, by_map = by_map, by_ref = by_ref)
    }
    
    # Q (quantidade), D (discordância global), A (alocação) + n via delta method
    compute_pontius_from_design <- function(des, conf_level = 0.95) {
      if (is.null(des) || nrow(des$variables) == 0L) {
        return(list(
          Q = list(mu = NA_real_, se = NA_real_, ci_l = NA_real_, ci_u = NA_real_, n = 0L),
          D = list(mu = NA_real_, se = NA_real_, ci_l = NA_real_, ci_u = NA_real_, n = 0L),
          A = list(mu = NA_real_, se = NA_real_, ci_l = NA_real_, ci_u = NA_real_, n = 0L)
        ))
      }
      
      n_total <- nrow(des$variables)
      vars <- des$variables
      
      classes <- sort(unique(c(as.character(vars$classification),
                               as.character(vars$reference))))
      classes <- classes[!is.na(classes) & nzchar(classes)]
      
      frm <- ~ factor(reference, levels = classes) +
        factor(classification, levels = classes) +
        I(classification == reference)
      
      m  <- svymean(frm, design = des, na.rm = TRUE)
      V  <- vcov(m)
      est <- coef(m)
      nm  <- names(est)
      
      idx_ref <- grep("^factor\\(reference", nm)        # p_{+i}
      idx_map <- grep("^factor\\(classification", nm)   # p_{i+}
      
      idx_hit_true  <- grep("^I\\(classification == reference\\).*TRUE$", nm)
      idx_hit_plain <- which(nm == "I(classification == reference)")
      idx_hit_all   <- grep("^I\\(classification == reference\\)", nm)
      
      if (length(idx_hit_true) == 1) {
        idx_hit <- idx_hit_true
      } else if (length(idx_hit_plain) == 1) {
        idx_hit <- idx_hit_plain
      } else if (length(idx_hit_all) == 2) {
        idx_hit <- idx_hit_all[ grepl("TRUE$", nm[idx_hit_all]) ]
      } else {
        stop("Não encontrei o termo de acerto global (I(classification == reference)) em nm.")
      }
      
      p_ref <- as.numeric(est[idx_ref])
      p_map <- as.numeric(est[idx_map])
      theta <- as.numeric(est[idx_hit])  # OA
      
      K <- length(idx_ref)
      stopifnot(length(est) == nrow(V), nrow(V) == ncol(V))
      
      # Q = 0.5 * sum |p_ref - p_map|
      dif <- p_ref - p_map
      sgn <- sign(dif); sgn[is.na(sgn)] <- 0; sgn[abs(dif) < 1e-12] <- 0
      Qhat <- 0.5 * sum(abs(dif))
      
      # D = 1 - OA
      Dhat <- 1 - theta
      
      # Gradientes
      g_Q <- rep(0, length(est))
      g_Q[idx_ref] <-  0.5 * sgn
      g_Q[idx_map] <- -0.5 * sgn
      
      g_D <- rep(0, length(est))
      g_D[idx_hit] <- -1
      
      g_A <- g_D - g_Q
      
      # Delta method
      var_Q <- as.numeric(t(g_Q) %*% V %*% g_Q); se_Q <- sqrt(var_Q)
      var_D <- as.numeric(t(g_D) %*% V %*% g_D); se_D <- sqrt(var_D)
      var_A <- as.numeric(t(g_A) %*% V %*% g_A); se_A <- sqrt(var_A)
      z <- qnorm((1 + conf_level)/2)
      ci <- function(mu, se) c(mu - z*se, mu + z*se)
      
      list(
        Q = {ciQ <- ci(Qhat, se_Q); list(mu = truncate01(Qhat), se = se_Q,
                                         ci_l = truncate01(ciQ[1]), ci_u = truncate01(ciQ[2]), n = n_total)},
        D = {ciD <- ci(Dhat, se_D); list(mu = truncate01(Dhat), se = se_D,
                                         ci_l = truncate01(ciD[1]), ci_u = truncate01(ciD[2]), n = n_total)},
        A = {Ahat <- Dhat - Qhat; ciA <- ci(Ahat, se_A); list(mu = truncate01(Ahat), se = se_A,
                                                              ci_l = truncate01(ciA[1]), ci_u = truncate01(ciA[2]), n = n_total)}
      )
    }
    
    # ---------- Designs ----------
    
    # desenho amostral por ANO (Brasil)
    compute_year_design <- function(yr) {
      dados_year <- droplevels(filter(dados, year == yr))
      if (nrow(dados_year) == 0) return(NULL)
      options(survey.lonely.psu = "adjust")
      des=svydesign(
        ids    = ~ TARGETID,
        strata = ~ strata_id,
        fpc    = ~ pop,
        weight = ~ WEIGHT_VOT,
        data   = dados_year
      )
      postStratify(des, ~ strata_id, calibrate_strata_pop_geral,partial=TRUE)
    }
    
    # desenho amostral geral
    compute_geral_design <- function(dados) {
      if (nrow(dados) == 0) return(NULL)
      options(survey.lonely.psu = "adjust")
      des=svydesign(
        ids    = ~ 1,
        strata = ~ strata_id,
        fpc    = ~ pop,
        weight = ~ WEIGHT_VOT,
        data   = dados,
        check.strata = FALSE
      )
      
      postStratify(des, ~ strata_id, calibrate_strata_pop_geral,partial=TRUE)
    }
    
    # ---------- Cálculos ----------
    
    # Acurácia global e Geral (agrupando todos os anos)
    des_geral <- compute_geral_design(dados)
    rG <- compute_global_acc_from_design(des_geral)
    
    oa_geral <- tibble::tibble(
      year      = "All",
      region    = "Brasil",
      class     = "All",
      reference = "All",
      metric    = "OA",
      estimate  = rG$mu,
      se        = rG$se,
      ci        = rG$ci_l,
      cs        = rG$ci_u,
      n         = rG$n
    )
    
    # Acurácia global por ano (Brasil)
    oa_year_BR <- bind_rows(future_lapply(anos, function(yr) {
      des_year <- compute_year_design(yr)
      r <- compute_global_acc_from_design(des_year)
      tibble(
        year      = as.character(yr),
        region    = "Brasil",
        class     = "All",
        reference = "All",
        metric    = "OA",
        estimate  = r$mu,
        se        = r$se,
        ci        = r$ci_l,
        cs        = r$ci_u,
        n         = r$n
      )
    })) %>% arrange(year, region)
    
    # Acurácia global por ano e bioma
    oa_year_biome <- bind_rows(future_lapply(anos, function(yr) {
      des_year <- compute_year_design(yr)
      if (is.null(des_year)) return(tibble())
      biomas_no_ano <- sort(unique(des_year$variables$BioNB))
      bind_rows(lapply(biomas_no_ano, function(b) {
        des_b <- subset(des_year, BioNB == b)
        r <- compute_global_acc_from_design(des_b)
        tibble(
          year      = as.character(yr),
          region    = b,
          class     = "All",
          reference = "All",
          metric    = "OA",
          estimate  = r$mu,
          se        = r$se,
          ci        = r$ci_l,
          cs        = r$ci_u,
          n         = r$n
        )
      }))
    })) %>% arrange(year, region)
    
    # OA por BIOMA (todos os anos juntos: year = "All")
    oa_all_biome <- bind_rows(lapply(biomas, function(b) {
      dados_b <- dplyr::filter(dados, BioNB == b)
      des_b_all <- compute_geral_design(dados_b)
      r <- compute_global_acc_from_design(des_b_all)
      tibble::tibble(
        year      = "All",
        region    = b,
        class     = "All",
        reference = "All",
        metric    = "OA",
        estimate  = r$mu,
        se        = r$se,
        ci        = r$ci_l,
        cs        = r$ci_u,
        n         = r$n
      )
    })) %>% arrange(region)
    
    # UA/PA/EC/OE/Brasil por ano (por classe)
    acc_class_year_BR <- bind_rows(
      future_lapply(anos, function(yr) {
        des_year <- compute_year_design(yr)
        compute_class_metrics_from_design(des_year) %>%
          mutate(year = as.character(yr), region = "Brasil", .before = 1)
      })
    ) %>% arrange(year, class_aux)
    
    # BRASIL — todos os anos (pares (m,k)) acumulado
    acc_pairs_all_BR <- {
      des_geral <- compute_geral_design(dados)  
      if (is.null(des_geral)) tibble()
      else {
        out_all <- compute_pairwise_metrics_from_design(des_geral)
        out_all$pairs %>% mutate(year = "All", region = "Brasil", .before = 1)
      }
    } %>% arrange(year, region, class_map, class_ref)
    
    acc_pairs_all_BR_std <- bind_rows(
      acc_pairs_all_BR %>% transmute(
        year, region,
        class     = as.character(class_map),
        reference = as.character(class_ref),
        metric    = ifelse(is_diag == TRUE, "UA", "CE"),   # base mapa
        estimate  = P_ref_given_class,
        se        = SE_ref_given_class,
        ci        = CI_ref_given_class_L,
        cs        = CI_ref_given_class_U,
        n         = n_total
      ),
      acc_pairs_all_BR %>% transmute(
        year, region,
        class     = as.character(class_map),
        reference = as.character(class_ref),
        metric    = ifelse(is_diag == TRUE, "PA", "OE"),   # base referência
        estimate  = P_class_given_ref,
        se        = SE_class_given_ref,
        ci        = CI_class_given_ref_L,
        cs        = CI_class_given_ref_U,
        n         = n_total
      )
    ) %>% arrange(year, region, class, reference, metric)
    
    # BRASIL por ano — pares (m,k)
    acc_pairs_year_BR <- bind_rows(
      future_lapply(anos, function(yr) {
        des_year <- compute_year_design(yr)
        out <- compute_pairwise_metrics_from_design(des_year)
        out$pairs %>% mutate(year = as.character(yr), region = "Brasil", .before = 1)
      })
    ) %>% arrange(year, region, class_map, class_ref)
    
    acc_pairs_year_BR_std <- bind_rows(
      acc_pairs_year_BR %>% transmute(
        year, region,
        class     = as.character(class_map),
        reference = as.character(class_ref),
        metric    = ifelse(is_diag==T,"UA","CE"),
        estimate  = P_ref_given_class,
        se        = SE_ref_given_class,
        ci        = CI_ref_given_class_L,
        cs        = CI_ref_given_class_U,
        n         = n_total
      ),
      acc_pairs_year_BR %>% transmute(
        year, region,
        class     = as.character(class_map),
        reference = as.character(class_ref),
        metric    = ifelse(is_diag==T,"PA","OE"),
        estimate  = P_class_given_ref,
        se        = SE_class_given_ref,
        ci        = CI_class_given_ref_L,
        cs        = CI_class_given_ref_U,
        n         = n_total
      )
    ) %>% arrange(year, region, class, reference, metric)
    
    # POR BIOMA — todos os anos (pares (m,k)) acumulado
    acc_pairs_all_biome <- bind_rows(lapply(biomas, function(b) {
      dados_b <- dplyr::filter(dados, BioNB == b)
      des_b_all <- compute_geral_design(dados_b)
      if (is.null(des_b_all)) return(tibble())
      out_b <- compute_pairwise_metrics_from_design(des_b_all)
      out_b$pairs %>% mutate(year = "All", region = b, .before = 1)
    })) %>% arrange(year, region, class_map, class_ref)
    
    acc_pairs_all_biome_std <- bind_rows(
      acc_pairs_all_biome %>% transmute(
        year, region,
        class     = as.character(class_map),
        reference = as.character(class_ref),
        metric    = ifelse(is_diag == TRUE, "UA", "CE"),
        estimate  = P_ref_given_class,
        se        = SE_ref_given_class,
        ci        = CI_ref_given_class_L,
        cs        = CI_ref_given_class_U,
        n         = n_total
      ),
      acc_pairs_all_biome %>% transmute(
        year, region,
        class     = as.character(class_map),
        reference = as.character(class_ref),
        metric    = ifelse(is_diag == TRUE, "PA", "OE"),
        estimate  = P_class_given_ref,
        se        = SE_class_given_ref,
        ci        = CI_class_given_ref_L,
        cs        = CI_class_given_ref_U,
        n         = n_total
      )
    ) %>% arrange(year, region, class, reference, metric)
    
    # POR BIOMA e ano — pares (m,k)
    acc_pairs_year_biome <- bind_rows(
      future_lapply(anos, function(yr) {
        des_year <- compute_year_design(yr)
        if (is.null(des_year)) return(tibble())
        biomas_no_ano <- sort(unique(des_year$variables$BioNB))
        bind_rows(lapply(biomas_no_ano, function(b) {
          des_b <- subset(des_year, BioNB == b)
          out   <- compute_pairwise_metrics_from_design(des_b)
          out$pairs %>% mutate(year = as.character(yr), region = b, .before = 1)
        }))
      })
    ) %>% arrange(year, region, class_map, class_ref)
    
    acc_pairs_year_biome_std <- bind_rows(
      acc_pairs_year_biome %>% transmute(
        year, region,
        class     = as.character(class_map),
        reference = as.character(class_ref),
        metric    = ifelse(is_diag==T,"UA","CE"),
        estimate  = P_ref_given_class,
        se        = SE_ref_given_class,
        ci        = CI_ref_given_class_L,
        cs        = CI_ref_given_class_U,
        n         = n_total
      ),
      acc_pairs_year_biome %>% transmute(
        year, region,
        class     = as.character(class_map),
        reference = as.character(class_ref),
        metric    = ifelse(is_diag==T,"PA","OE"),
        estimate  = P_class_given_ref,
        se        = SE_class_given_ref,
        ci        = CI_class_given_ref_L,
        cs        = CI_class_given_ref_U,
        n         = n_total
      )
    ) %>% arrange(year, region, class, reference, metric)
    
    # Pontius geral (All years)
    rPontG <- compute_pontius_from_design(des_geral)
    pontius_geral_BR_std <- tibble::tibble(
      year      = "All",
      region    = "Brasil",
      class     = "All",
      reference = "All",
      metric    = c("QUANTITY", "ALLOCATION"),
      estimate  = c(rPontG$Q$mu, rPontG$A$mu),
      se        = c(rPontG$Q$se, rPontG$A$se),
      ci        = c(rPontG$Q$ci_l, rPontG$A$ci_l),
      cs        = c(rPontG$Q$ci_u, rPontG$A$ci_u),
      n         = c(rPontG$Q$n,  rPontG$A$n)
    )
    
    # Pontius por ano (Brasil)
    pontius_year_BR_std <- bind_rows(
      future_lapply(anos, function(yr) {
        des_year <- compute_year_design(yr)
        r <- compute_pontius_from_design(des_year)
        tibble::tibble(
          year      = as.character(yr),
          region    = "Brasil",
          class     = "All",
          reference = "All",
          metric    = c("QUANTITY", "ALLOCATION"),
          estimate  = c(r$Q$mu, r$A$mu),
          se        = c(r$Q$se, r$A$se),
          ci        = c(r$Q$ci_l, r$A$ci_l),
          cs        = c(r$Q$ci_u, r$A$ci_u),
          n         = c(r$Q$n,  r$A$n)
        )
      })
    ) %>% arrange(year, region, metric)
    
    # Pontius por BIOMA (todos os anos juntos: year = "All")
    pontius_all_biome_std <- bind_rows(lapply(biomas, function(b) {
      dados_b <- dplyr::filter(dados, BioNB == b)
      des_b_all <- compute_geral_design(dados_b)
      r <- compute_pontius_from_design(des_b_all)
      tibble::tibble(
        year      = "All",
        region    = b,
        class     = "All",
        reference = "All",
        metric    = c("QUANTITY", "ALLOCATION"),
        estimate  = c(r$Q$mu, r$A$mu),
        se        = c(r$Q$se, r$A$se),
        ci        = c(r$Q$ci_l, r$A$ci_l),
        cs        = c(r$Q$ci_u, r$A$ci_u),
        n         = c(r$Q$n,     r$A$n)
      )
    })) %>% arrange(region, metric)
    
    # Pontius por ano e bioma
    pontius_year_biome_std <- bind_rows(
      future_lapply(anos, function(yr) {
        des_year <- compute_year_design(yr)
        if (is.null(des_year)) return(tibble())
        biomas_no_ano <- sort(unique(des_year$variables$BioNB))
        bind_rows(lapply(biomas_no_ano, function(b) {
          des_b <- subset(des_year, BioNB == b)
          r <- compute_pontius_from_design(des_b)
          tibble::tibble(
            year      = as.character(yr),
            region    = b,
            class     = "All",
            reference = "All",
            metric    = c("QUANTITY", "ALLOCATION"),
            estimate  = c(r$Q$mu, r$A$mu),
            se        = c(r$Q$se, r$A$se),
            ci        = c(r$Q$ci_l, r$A$ci_l),
            cs        = c(r$Q$ci_u, r$A$ci_u),
            n         = c(r$Q$n,  r$A$n)
          )
        }))
      })
    ) %>% arrange(year, region, metric)
    
    # Consolidado (tudo junto, exceto a matriz de confusão)
    metrics_all <- bind_rows(
      oa_geral,
      oa_year_BR,
      oa_year_biome,
      oa_all_biome,              
      acc_pairs_all_BR_std,
      acc_pairs_all_biome_std,
      acc_pairs_year_BR_std,
      acc_pairs_year_biome_std,
      pontius_geral_BR_std,
      pontius_year_BR_std,
      pontius_year_biome_std,
      pontius_all_biome_std      
    ) %>%
      arrange(year, region, metric, class, reference)
    
    # Nome do arquivo XLSX dinâmico por coleção e nível (ex: tabela_mapbiomas_metrics_c8_l1.xlsx)
    nome_saida_xlsx <- paste0("tabela_mapbiomas_metrics_", col, "_", level, ".xlsx")
    
    # Exportação
    write_xlsx(metrics_all, nome_saida_xlsx)
    cat("     Métricas salvas em:", nome_saida_xlsx, "\n")
    
  }
}

library(readxl)
library(dplyr)
library(tidyr)

options(scipen = 999)

tabela_mapbiomas_metrics_col11_l3 <- read_excel("tabela_mapbiomas_metrics_c110-4-14-c84-5_l3.xlsx")
tabela_mapbiomas_metrics_col11_l2 <- read_excel("tabela_mapbiomas_metrics_c110-4-14-c84-5_l2.xlsx")
tabela_mapbiomas_metrics_col11_l1 <- read_excel("tabela_mapbiomas_metrics_c110-4-14-c84-5_l1.xlsx")

tabela_mapbiomas_metrics_col11_l1$iniciative=rep("MapBiomas Brazil",
                                                 nrow(tabela_mapbiomas_metrics_col11_l1))	
tabela_mapbiomas_metrics_col11_l2$iniciative=rep("MapBiomas Brazil",
                                                 nrow(tabela_mapbiomas_metrics_col11_l2))	
tabela_mapbiomas_metrics_col11_l3$iniciative=rep("MapBiomas Brazil",
                                                 nrow(tabela_mapbiomas_metrics_col11_l3))	


tabela_mapbiomas_metrics_col11_l1$collection=rep("c110",
                                                 nrow(tabela_mapbiomas_metrics_col11_l1))	
tabela_mapbiomas_metrics_col11_l2$collection=rep("c110",
                                                 nrow(tabela_mapbiomas_metrics_col11_l2))	
tabela_mapbiomas_metrics_col11_l3$collection=rep("c110",
                                                 nrow(tabela_mapbiomas_metrics_col11_l3))	

tabela_mapbiomas_metrics_col11_l1$version=rep("85k_col5_v3",
                                              nrow(tabela_mapbiomas_metrics_col11_l1))	
tabela_mapbiomas_metrics_col11_l2$version=rep("85k_col5_v3",
                                              nrow(tabela_mapbiomas_metrics_col11_l2))	
tabela_mapbiomas_metrics_col11_l3$version=rep("85k_col5_v3",
                                              nrow(tabela_mapbiomas_metrics_col11_l3))	

tabela_mapbiomas_metrics_col11_l1$level=rep("l1",
                                            nrow(tabela_mapbiomas_metrics_col11_l1))	
tabela_mapbiomas_metrics_col11_l2$level=rep("l2",
                                            nrow(tabela_mapbiomas_metrics_col11_l2))	
tabela_mapbiomas_metrics_col11_l3$level=rep("l3",
                                            nrow(tabela_mapbiomas_metrics_col11_l3))	

df=rbind(tabela_mapbiomas_metrics_col11_l1,
         tabela_mapbiomas_metrics_col11_l2,
         tabela_mapbiomas_metrics_col11_l3)

val_cols <- intersect(c("estimate","se","ci","cs"), names(df))
id_cols  <- setdiff(names(df), c("metric", val_cols))

wide_multi <- df |>
  mutate(metric = as.character(metric)) |>
  pivot_wider(
    id_cols     = all_of(id_cols),
    names_from  = metric,
    values_from = all_of(val_cols),
    names_glue  = "{metric}_{.value}" # ex.: OA_estimate, UA_std_error...
    # ,values_fn = list(estimate = first, std_error = first, lower = first, upper = first)
  )


names(wide_multi)=c("year" , 
                    "territory" , 
                    "id_class_map",              
                    "id_class_ref" , 
                    "Samples_Used",                   
                    "iniciative",         
                    "collection" ,
                    "version",
                    "level" ,
                    "Allocation_Tot", 
                    "Comission_Error",         
                    "GlobalAccuracy" ,       
                    "Omission_Error" ,        
                    "Producer_Acc",         
                    "Quantity_Tot",  
                    "User_Acc",
                    "Allocation_Tot_stdErr",       
                    "Comission_Error_stdErr",              
                    "GlobalAccuracy_stdErr",
                    "Omission_Error_stdErr",
                    "Producer_stdErr",              
                    "Quantity_Tot_stdErr",         
                    "User_stdErr",               
                    "Allocation_Tot_ci" ,     
                    "Comission_Error_ci",
                    "GlobalAccuracy_ci",
                    "Omission_Error_ci" ,             
                    "Producer_Acc_ci",
                    "Quantity_Tot_ci",
                    "User_Acc_ci" ,             
                    "Allocation_Tot_cs",
                    "Comission_Error_cs" ,
                    "GlobalAccuracy_cs",              
                    "Omission_Error_cs" ,
                    "Producer_Acc_cs",
                    "Quantity_Tot_cs",        
                    "User_Acc_cs" )             


#write_xlsx(wide_multi, paste0("acc_c100_info", ".xlsx"))

######################################################################################

n <- nrow(wide_multi)

df_final <- data.frame(
  iniciative     = wide_multi$iniciative,
  collection     = wide_multi$collection,
  territory      = wide_multi$territory,
  year           = wide_multi$year,
  version        = wide_multi$version,
  level          = wide_multi$level,
  id_class_ref   = wide_multi$id_class_ref,
  id_class_map   = wide_multi$id_class_map,
  value          = rep(0, n),
  confusion_user = rep(0, n),
  confusion_prod = rep(0, n),
  Samples_Used   = wide_multi$Samples_Used,
  Adj_population    = rep(0, n),
  Adj_population_se = rep(0, n),
  Producer_Acc    = wide_multi$Producer_Acc,
  Producer_stdErr = wide_multi$Producer_stdErr,
  Omission_Error  = wide_multi$Omission_Error,
  Allocation_Tot  = wide_multi$Allocation_Tot,
  Pop_Prop     = rep(0, n),
  Pop_Bias     = rep(0, n),
  Pop_Bias_SE  = rep(0, n),
  User_Acc        = wide_multi$User_Acc,
  User_stdErr     = wide_multi$User_stdErr,
  Comission_Error = wide_multi$Comission_Error,
  Quantity_Tot    = wide_multi$Quantity_Tot,
  GlobalAccuracy  = wide_multi$GlobalAccuracy
)

df_final[is.na(df_final)] <- 0

df_final[] <- lapply(df_final, function(x) {
  if (is.character(x)) {
    x[x == "All"] <- "Total"
  }
  x
})

#write_xlsx(df_final, paste0("acc_c100_info", ".xlsx"))
write.csv(df_final, "acc_c11_85k_col5_v3_info.csv", row.names = FALSE)
