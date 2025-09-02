# Defina quantas normas deseja inserir
n_iter <- nrow(ver)

# Inicializa os vetores
escolhas <- c()
entradas <- c()

for (i in 1:n_iter) {
  cat("\n--- Iteração", i, "---\n")
  cat(paste0("norma (", i, ")\n"))
  
  # Pede escolha 0 ou 1, padrão = 1
  entrada_escolha <- readline(paste(ver$antes[i], "*", ver$termo[i], "*", ver$depois[i], ":"))
  
  # Se vazio, assume 1
  if (entrada_escolha == "") {
    escolha <- 1
  } else {
    escolha <- as.integer(entrada_escolha)
    
    # Garante que só entra se for 0 ou 1
    while (!(escolha %in% c(0,1))) {
      entrada_escolha <- readline("Valor inválido. Digite 0 ou 1 (Enter = 1): ")
      if (entrada_escolha == "") {
        escolha <- 1
      } else {
        escolha <- as.integer(entrada_escolha)
      }
    }
  }
  
  # Armazena a escolha
  escolhas <- c(escolhas, escolha)
  
  # Se for 0, pede string; se for 1, insere NA
  if (escolha == 0) {
    entrada <- readline("############ Digite o termo: ")
    entradas <- c(entradas, entrada_escolha)
  } else {
    entradas <- c(entradas, NA_character_)
  }
}

# Mostra resultados finais
cat("\n--- Resultados ---\n")
print(data.frame(normas = 1:n_iter, escolha = escolhas, entrada = entradas))
