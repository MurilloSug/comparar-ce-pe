library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(furrr)

# --- seu dicionário ---
setores_dict <- list(
  mel = c(
    "mel",
    "abelha", "abelhas",
    "apicultura",
    "apícola", "apicola",
    "colmeia", "colméia",
    "pólen", "polen",
    "própolis", "propolis",
    "meliponicultura",
    "meliponário", "meliponario"
  ),
  
  queijo = c(
    "queijo",
    "queijaria",
    "coalho",
    "manteiga", "Manteiga",
    "laticínio", "laticinio",
    "leite cru", "Leite cru",
    "leite in natura",
    "maturação", "maturacao"
  ),
  
  macro_setor = c(
    "cadeia produtiva do leite", "cadeias produtivas do leite",
    "cadeia produtiva do mel", "cadeias produtivas do mel",
    "cadeia produtiva animal", "cadeias produtivas animais",
    "cadeia produtiva agropecuária", "cadeia produtiva agropecuaria",
    "agroindústria", "agroindustria",
    "indústria agroalimentar",
    "setor agroalimentar",
    "setor agropecuário", "setor agropecuario",
    "agricultura familiar",
    "agricultura orgânica",
    "extensão rural",
    "assistência técnica rural",
    "agricultura e pecuária",
    "produção animal",
    "escoamento da produção",
    "infraestrutura rural",
    "cooperativas agropecuárias", "cooperativas rurais",
    "inspeção sanitária",
    "vigilância agropecuária",
    "controle sanitário animal"
  )
)

# Adiciona um delimitador de palavras por termo
setores_dict <- lapply(setores_dict, function(termos) {
  unique(paste0("\\b", termos, "\\b"))
})

# --- exemplo de data.frame ---
df <- readRDS("escopo_ce_final.Rds")

df <- df %>% 
  mutate(id = 1:nrow(df))

# --- função para extrair contexto KWIC ---
extrair_contexto <- function(texto, padrao, window = 50) {
  regex_pat <- str_c("(.{0,", window, "})(", padrao, ")(.{0,", window, "})")
  str_match_all(texto, regex(regex_pat, ignore_case = TRUE)) %>%
    map_dfr(~{
      if (nrow(.) == 0) return(NULL)
      tibble(
        antes = .[,2],
        termo = .[,3],
        depois = .[,4]
      )
    })
}

# --- aplica dicionário ---
plan(multisession, workers = parallel::detectCores() - 1)

resultados <- future_map2_dfr(
  names(setores_dict), setores_dict,
  ~{
    padrao <- str_c(.y, collapse = "|")
    df %>%
      mutate(match = map(texto_limpo, ~ extrair_contexto(.x, padrao))) %>%
      unnest(match) %>%
      mutate(setor = .x)
  }
)

saveRDS(resultados, "resultados_filtro3.Rds")

# --- resultado ---
ver <- resultados %>%
  select(id, setor, antes, termo, depois)

which(ver$id == 1)
