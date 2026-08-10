# Avaliacao de acuracia - Colecao 11

Esta pasta reune os scripts e arquivos auxiliares usados para estimar a acuracia da Colecao 11 do MapBiomas Brasil. O fluxo combina a classificacao do MapBiomas com amostras de referencia no Google Earth Engine (GEE) e calcula, em R, metricas ponderadas para o Brasil e seus biomas.

## Conteudo da pasta

| Arquivo | Descricao |
| --- | --- |
| `1_export_gee_input.js` | Extrai, no GEE, os valores anuais da classificacao nos pontos de referencia e exporta um CSV por ano. |
| `2_accuracy_estimates.R` | Prepara os dados, harmoniza as classes e calcula as metricas de acuracia nos niveis 1, 2 e 3 da legenda. |
| `points_strata.csv` | Relaciona cada amostra (`TARGETID`) ao seu estrato amostral (`strata_id`). |
| `strata.csv` | Informa a populacao (`pop`) de cada estrato amostral. |
| `ALL_CLASSES.xlsx` | Tabela de correspondencia das classes entre os niveis da legenda. |

## Requisitos

- acesso ao [Google Earth Engine Code Editor](https://code.earthengine.google.com/);
- permissao de leitura para os assets indicados em `1_export_gee_input.js`;
- R com os pacotes `RSQLite`, `DBI`, `MachineShop`, `resample`, `gbm`, `forecast`, `caret`, `data.table`, `magrittr`, `tidyverse`, `scales`, `ggplot2`, `dplyr`, `lattice`, `survey`, `srvyr`, `future`, `future.apply`, `openxlsx`, `writexl`, `readxl`, `readr`, `tibble` e `purrr`.

Os pacotes ausentes podem ser instalados no R com:

```r
install.packages(c(
  "RSQLite", "DBI", "MachineShop", "resample", "gbm", "forecast",
  "caret", "data.table", "magrittr", "tidyverse", "scales", "ggplot2",
  "dplyr", "lattice", "survey", "srvyr", "future", "future.apply",
  "openxlsx", "writexl", "readxl", "readr", "tibble", "purrr"
))
```

## Como executar

### 1. Exportar os dados no GEE

1. Abra `1_export_gee_input.js` no GEE Code Editor.
2. Confira os parametros no inicio do script, principalmente:
   - `anos`: anos processados (atualmente, 1985 a 2024);
   - `col_list`: colecoes processadas (atualmente, `c11`);
   - `assetSamples`: asset dos pontos de referencia;
   - `assetMapBiomas`: asset da classificacao, definido pelo dicionario `cols`;
   - `folder`: pasta de destino no Google Drive.
3. Execute o script.
4. Na aba **Tasks**, inicie todas as exportacoes criadas.

O GEE gera um arquivo por ano, com o padrao:

```text
ACC_c11_v5_no_EDGE_github/
  acc_mapbiomas_1985.csv
  acc_mapbiomas_1986.csv
  ...
  acc_mapbiomas_2024.csv
```

Durante a exportacao, o script remove classes de referencia invalidas e pixels de borda, ajusta os pesos amostrais e adiciona aos pontos os valores de classificacao, estado e bioma.

### 2. Configurar o processamento em R

Antes de executar `2_accuracy_estimates.R`, ajuste:

- `setwd()` e os caminhos de `points_strata.csv`, `strata.csv` e `ALL_CLASSES.xlsx`;
- `base_dir`, apontando para o diretorio que contem as subpastas `ACC_<colecao>` com os CSVs exportados;
- `colecoes`, mantendo apenas as colecoes que devem ser processadas;
- `anos` e `levels`, caso o processamento nao contemple todo o periodo ou os tres niveis;
- os nomes dos arquivos lidos na etapa final de consolidacao, para que correspondam a colecao escolhida.

> **Atencao:** os caminhos e a lista `colecoes` presentes no script refletem o ambiente em que a analise foi desenvolvida. Eles nao correspondem diretamente ao nome da pasta de exportacao configurado atualmente no JavaScript e devem ser revisados antes da execucao.

A estrutura esperada em `base_dir` e:

```text
<base_dir>/
  ACC_<colecao>/
    acc_mapbiomas_1985.csv
    ...
    acc_mapbiomas_2024.csv
```

Com os parametros configurados, execute a partir do R ou do terminal:

```r
source("2_accuracy_estimates.R")
```

## Etapas do processamento

O script R:

1. une os CSVs anuais aos estratos e suas populacoes;
2. harmoniza classes gerais e regras especificas por bioma;
3. remapeia a legenda para os niveis `l1`, `l2` e `l3`;
4. cria os objetos de desenho amostral ponderado;
5. calcula as metricas para o Brasil e para cada bioma, por ano e para todo o periodo;
6. consolida os resultados no formato de entrega.

As principais metricas calculadas sao:

- acuracia global (`OA`);
- acuracia do usuario (`UA`) e erro de comissao (`CE`);
- acuracia do produtor (`PA`) e erro de omissao (`OE`);
- desacordo de quantidade (`QUANTITY`);
- desacordo de alocacao (`ALLOCATION`);
- erro-padrao e intervalos de confianca associados.

## Saidas

Os arquivos sao gravados no diretorio de trabalho configurado no R:

| Padrao | Conteudo |
| --- | --- |
| `dados_<colecao>_<nivel>.csv` | Base harmonizada usada nos calculos de cada nivel. |
| `tabela_mapbiomas_metrics_<colecao>_<nivel>.xlsx` | Metricas consolidadas por colecao e nivel. |
| `acc_c11_85k_col5_v3_info.csv` | Tabela final da Colecao 11 no formato de entrega. |

Como o processamento cobre 40 anos, varios recortes territoriais e tres niveis de legenda, o tempo de execucao e o consumo de memoria podem ser altos.
