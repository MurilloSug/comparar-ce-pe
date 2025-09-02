########## Script para seleção de leis por tema 
library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(purrr)
library(lubridate)

###### Processamento
df <- escopo_pe

#criação de um dicionário
setores_dict = list(
  mel = c(
    "mel", "Mel",
    "abelha", "abelhas", "Abelha", "Abelhas",
    "apicultura", "Apicultura",
    "apícola", "apicola", "Apícola", "Apicola",
    "colmeia", "colméia", "Colmeia", "Colméia",
    "pólen", "polen", "Pólen", "Polen",
    "própolis", "propolis", "Própolis", "Propolis",
    "meliponicultura", "Meliponicultura",
    "meliponário", "meliponario", "Meliponário", "Meliponario"
  ),
  
  queijo = c(
    "queijo", "Queijo",
    "queijaria", "Queijaria",
    "coalho", "Coalho",
    "manteiga", "Manteiga",
    "laticínio", "laticinio", "Laticínio", "Laticinio",
    "leite cru", "Leite cru",
    "leite in natura", "Leite in natura",
    "maturação", "maturacao", "Maturação", "Maturacao"
  ),
  
  macro_setor = c(
    # Cadeias explicitamente compatíveis
    "cadeia produtiva do leite", "cadeias produtivas do leite",
    "cadeia produtiva do mel", "cadeias produtivas do mel",
    "cadeia produtiva animal", "cadeias produtivas animais",
    "cadeia produtiva agropecuária", "cadeia produtiva agropecuaria",
    
    # Termos agroindustriais focados
    "agroindústria", "agroindustria",
    "indústria agroalimentar",
    "setor agroalimentar",
    "setor agropecuário", "setor agropecuario",
    
    # Agricultura com ligação mais específica
    "agricultura familiar",
    "agricultura orgânica",
    
    # Fomento e apoio com foco técnico
    "extensão rural",
    "assistência técnica rural",
    "agricultura e pecuária",
    "produção animal",
    "escoamento da produção",
    "infraestrutura rural",
    "cooperativas agropecuárias", "cooperativas rurais",
    
    # Qualidade, inspeção e regulação
    "inspeção sanitária",
    "vigilância agropecuária",
    "controle sanitário animal"
  )
)



# Adiciona um delimitador de palavras por termo
setores_dict_regex <- lapply(setores_dict, function(termos) {
  unique(paste0("\\b", termos, "\\b"))
})

#função para categorizar setores
detectar_setores <- function(texto, dicionario) {
  setores_presentes <- names(dicionario)[
    map_lgl(dicionario, function(padroes) {
      any(str_detect(texto, regex(paste(padroes, collapse = "|"), ignore_case = TRUE)))
    })
  ]
  if (length(setores_presentes) == 0) {
    return(NA_character_)
  } else {
    return(paste(setores_presentes, collapse = ", "))
  }
}

#categorizando setores e filtrando interesse
escopo <- df %>% 
  mutate(setor = map_chr(texto, detectar_setores, dicionario = setores_dict_regex))

#refinamento da categorização, mantendo uma categoria por lei
escopo <- escopo %>%
  mutate(setor = case_when(
    str_detect(setor, "macro_setor") & (str_detect(setor, "mel") | str_detect(setor, "queijo")) ~ "macro_setor",
    str_detect(setor, "mel") & str_detect(setor, "queijo") ~ "ambos",
    str_detect(setor, "^mel$") ~ "mel",
    str_detect(setor, "^queijo$") ~ "queijo",
    str_detect(setor, "^macro_setor$") ~ "macro_setor",
    TRUE ~ setor  # fallback caso apareça algo não esperado
  ))

##### Padronizando datas
escopo_pe <- escopo %>%
  mutate(
    data = data %>%
      str_to_lower() %>% 
      str_replace_all("(\\bde\\b|'|\\r)", " ") %>% 
      str_squish(),
    data_padronizada = dmy(data, locale = "pt_BR.UTF-8")
  )

escopo <- escopo %>% 
  mutate(tipo = str_to_upper(tipo))

##### Correção das ementas

escopo <- escopo %>%
  mutate(
    texto_limpo = texto %>%
      # Normalizar espaços
      str_replace_all("[\\s\\u00A0]+", " ") %>%
      # Remover trechos de "supportFields" e similares
      str_remove_all("\\[if supportFields\\].*?\\[endif\\]") %>%
      # Remover tags HTML <...>
      str_remove_all("<[^>]+>") %>%
      # Remover atributos mso-... (Office/Word)
      str_remove_all("mso-[a-zA-Z0-9-]+:[^;'\"]+;?") %>%
      # Remover sobras de múltiplos spans/fontes
      str_remove_all("style='[^']*'") %>%
      str_remove_all("font-family:[^;'\"]+;?") %>%
      # Padronizar espaços de novo
      str_replace_all("\\s+", " ") %>%
      str_squish(),
    
    resumo_extraido = case_when(
      str_detect(texto_limpo, "EMENTA:") ~ str_match(texto_limpo, "EMENTA: (.*?)(?=Artigo? ?1|O GOVERNADOR|A GOVERNADORA|O PRESIDENTE|$)")[, 2],
      TRUE ~ str_match(
        texto_limpo,
        "\\d{4}\\.?\\s*(?:\\(.*?\\))*\\s*(.*?)(?=\\s*(?:A GOVERNADORA|O GOVERNADOR|O GOVERNO DO ESTADO|O VICE-GOVERNADOR|O VICE GOVERNADOR|O 1º VICE-PRESIDENTE|O PRESIDENTE DA ASSEMBL[EÉ]IA|O PRESIDENTE DO TRIBUNAL DE JUSTIÇA|DECRETA:|DISPOSIÇÕES PRELIMINARES|CAPÍTULO I|Artigo? ?1|$))"
      )[, 2]
    ),
    
    resumo_final = ifelse(
      is.na(ementa) | ementa == "",
      resumo_extraido,
      ementa
    ),
    
    resumo_final = str_squish(resumo_final)
  ) #%>%
  # select(id, autor, tipo, numero, data_padronizada, resumo_final, revisa, texto, setor) %>% 
  # rename(ementa = resumo_final, data = data_padronizada)

##### Guardando dataframes em .csv
saveRDS(escopo_pe, "escopo_pe.Rds")
