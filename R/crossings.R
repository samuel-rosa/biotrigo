#' Combinações formadas nos cruzamentos
#' 
#' Identificar as combinações formadas durante os cruzamentos de genótipos.
#' 
#' @param base.de.dados Objeto do tipo data.frame com a base de dados dos genótipos.
#' 
#' @param cruzamento Objeto do tipo data.frame especificando os cruzamentos realizados.
#' 
rm(list = ls())
combinacoes <-
  function (base.de.dados, cruzamento) {
    
    # Identificar o nome dos genótipos envolvidos no cruzamento. A mãe deve ficar na primeira coluna.
    gen <- cbind(
      mae = base.de.dados$genotipo_nome[base.de.dados$genotipo_id %in% cruzamento$mae_id],
      pai = base.de.dados$genotipo_nome[base.de.dados$genotipo_id %in% cruzamento$pai_id]
    )
    
    # Procurar por NAs no nome do genótipo e substituir com o cruzamento que o originou
    na_id <- which(is.na(gen), arr.ind = TRUE)
    gen[na_id] <- base.de.dados$cruzamento_fonte[base.de.dados$genotipo_id %in% cruzamento[na_id]]
    
    # Organizar a combinação de genétipos
    separator <- as.character("/")
    full_cross <-
      data.frame(
        mae_id = cruzamento$mae,
        pai_id = cruzamento$pai,
        genotipo_nome = apply(gen, 1, function (x) paste(x, collapse = separator)),
        sementes_numero = cruzamento$sementes_numero
      )
    
    # Resultado
    return (full_cross)
  }

combinacoes(
  base.de.dados = read.csv(
    'data/database.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA"),
  cruzamento = read.csv(
    'data/cruzas.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE))
