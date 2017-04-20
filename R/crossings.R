#' Combinações formadas nos cruzamentos
#' 
#' Identificar as combinações formadas durante os cruzamentos de genótipos.
#' 
#' @param base.de.dados Objeto do tipo data.frame contendo a base de dados dos genótipos.
#' 
#' @param cruzamento Objeto do tipo data.frame especificando os cruzamentos realizados.
#' 
#' @details 
#' \subsection{Base de dados}{
#' A base de dados deve ser constituída 
#' }
#' 
#' \subsection{Cruzamentos}{
#' 
#' }
#' 
#' @return
#' Um objeto do tipo data.frame contendo o número de identificação dos genótipos mãe e pai, o nome do 
#' cruzamento e o número de sementes.
#' 
rm(list = ls())
combinacoes <-
  function (base.de.dados, cruzamento) {
    
    # Identificar o nome dos genótipos envolvidos no cruzamento.
    # Procurar por NAs no nome do genótipo e substituir com o cruzamento que o originou quando necessário.
    idx_mae <- match(cruzamento$mae_id, base.de.dados$genotipo_id)
    idx_pai <- match(cruzamento$pai_id, base.de.dados$genotipo_id)
    gen <- cbind(
      mae = ifelse(
        is.na(base.de.dados$genotipo_nome[idx_mae]), 
        base.de.dados$cruzamento_fonte[idx_mae],
        base.de.dados$genotipo_nome[idx_mae]
      ), 
      pai = ifelse(
        is.na(base.de.dados$genotipo_nome[idx_pai]), 
        base.de.dados$cruzamento_fonte[idx_pai],
        base.de.dados$genotipo_nome[idx_pai]
      )
    )
    
    # Organizar a combinação de genótipos
    separator <- as.character("/?/")
    full_cross <-
      data.frame(
        mae_id = cruzamento$mae,
        pai_id = cruzamento$pai,
        cruzamento_nome = apply(gen, 1, function (x) paste(x, collapse = separator)),
        sementes_numero = cruzamento$sementes_numero,
        stringsAsFactors = FALSE
      )
    
    # Identificar o número do cruzamento (para os casos em que os genótipos mãe e/ou pai não possuem nome).
    # Pressuposto: nenhum genótipo ou cruzamento é identificado usando apenas números.
    # cross_number <- rep(NA, nrow(full_cross))
    cross_names <- lapply(full_cross$cruzamento_nome, stringr::str_split_fixed, pattern = "/", n = Inf)
    cross_names <- lapply(cross_names, function (x) {
      suppressWarnings(as.numeric(x))
    })
    # Cruza >= 4
    idx <- sapply(cross_names, function (x) all(is.na(x)))
    n <- paste("/", sapply(cross_names[!idx], max, na.rm = TRUE) + 1, "/", sep = "")
    full_cross$cruzamento_nome[!idx] <- 
      gsub(pattern = "/?/", n, x = full_cross$cruzamento_nome[!idx], fixed = TRUE)
    # Cruza == 3
    idx2 <- grep(pattern = "//", full_cross$cruzamento_nome[idx])
    n <- paste("/", 3, "/", sep = "")
    full_cross$cruzamento_nome[idx][idx2] <-
      gsub(pattern = "/?/", n, x = full_cross$cruzamento_nome[idx][idx2], fixed = TRUE)
    # Cruza == 2
    idx1 <- gsub("/?/", " ", full_cross$cruzamento_nome[idx][-idx2], fixed = TRUE)
    idx1 <- grep(pattern = "/", idx1)
    full_cross$cruzamento_nome[idx][-idx2][idx1] <- 
      gsub("/?/", "//", full_cross$cruzamento_nome[idx][-idx2][idx1], fixed = TRUE)
    # Cruza == 1
    full_cross$cruzamento_nome[idx][-idx2][-idx1] <-
      gsub("/?/", "/", full_cross$cruzamento_nome[idx][-idx2][-idx1], fixed = TRUE)
    
    # Resultado
    return (full_cross)
  }
#' 
#' @examples
#' 
tmp <- combinacoes(
  base.de.dados = read.csv(
    'data/database.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = ""),
  cruzamento = read.csv(
    'data/cruzas.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE))
tmp
write.csv(tmp, "data/combinations.csv")
