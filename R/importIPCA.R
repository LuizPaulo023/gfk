#' @title Importação e Limpeza do dataset do IPCA
#' @name fun_importCleanIPCA
#'
#' @description Função para importar, limpar e organizar a base de dados do IPCA
#'
#' @param keys São as chaves APIs para puxar os dados da variação mensal do IPCA e do peso dos items que compõe o IPCA direto do SIDRA com \code{Variável}, \code{Mês}, \code{Geral, grupo, subgrupo, item e subitem}, \code{valor}.
#'
#'
#' @author Luiz Paulo
#'
#' @details O arquivo de input deve corresponder as chaves Apis do Sidra-IBGE (package: sidra).
#'
#' \code{Mês} - coluna de datas no formato MM/YYYY;
#' \code{Variável} - coluna corresponde a variação do IPCA e do peso dos items que compõe o IPCA;
#' \code{Geral, grupo, subgrupo, item e subitem} - corresponde ao grupo de produtos do item;
#' \code{valor} - valor corresponde a variação nominal do IPCA e ao peso dos items que compõe o IPCA.
#'
#' @return O retorno é um data.frame com a base de dados do IPCA limpo e organizado;
#' \code{db_ipca} - data.frame com a base de dados limpa e organizado.
#'
#
#'
#' @examples
#' \dontrun{
#' db_ipca = fun_importCleanIPCA()
#' }
#'
#' @export

# function: import and clean database

# Chaves apis para a variação mensal do IPCA e do peso dos items selecionados do IPCA mensal
# Tabelas: 2938, 1419 e 7060

importIPCA <- function(){

  keys = c("/t/2938/n1/all/v/63,66/p/last%2036/c315/7523,7524,7526,7529,7530,7531,7540,7542,7543,7547,7761,7794,107647/d/v63%202,v66%204",
           "/t/1419/n1/all/v/63,66/p/all/c315/7523,7526,7529,7530,7531,7540,7542,7543,7547,7761,7794,12404,12434,107647/d/v63%202,v66%204",
           "/t/7060/n1/all/v/63,66/p/all/c315/7523,7526,7530,7531,7542,7543,7794,12434,47641/d/v63%202,v66%204")

  ipca_raw = data.frame()
  # Puxando a base de dados primário do IPCA
  for (i in keys) {
    data_ipca = sidrar::get_sidra(api = i)
    ipca_raw = ipca_raw %>%
    dplyr::bind_rows(data_ipca)
  }
  # limpeza e organização da base de dados do IPCA

  db_ipca <- ipca_raw %>%
    janitor::clean_names() %>%
    dplyr::select(mes, variavel, geral_grupo_subgrupo_item_e_subitem, valor) %>%
   tidyr::pivot_wider(names_from = variavel,
                values_from = valor) %>%
    dplyr::mutate(
      items = gsub('^[[:digit:]]+', "", geral_grupo_subgrupo_item_e_subitem),
      items = stringr::str_sub(items, start = 2),
      date = zoo::as.yearmon(mes),
      date = zoo::as.Date(date, format = "Y%/%m/%d"),
      product_groups = dplyr::case_when(
        items == "Refrigerador" ~ "Linha Branca",
        items == "Máquina de lavar roupa" ~ "Linha Branca",
        items == "Fogão" ~ "Linha Branca",
        items == "Ar-condicionado" ~ "Linha Branca",
        items == "Forno de micro-ondas" ~ "Linha Branca",
        items == "Televisor" ~ "Linha Marrom",
        items == "Condicionador de ar" ~ "Linha Branca",
        items == "Aparelho de som" ~ "Linha Marrom",
        items == "Aparelho de DVD" ~ "Linha Marrom",
        items == "Antena" ~ "Linha Marrom",
        items == "Microcomputador" ~ "Informática",
        items == "Computador pessoal" ~ "Informática",
        items == "Máquina fotográfica" ~ "Foto",
        items == "Ventilador" ~ "Portáteis",
        items == "Liquidificador" ~ "Portáteis",
        items == "Aparelho telefônico" ~ "Telecom")) %>%
    dplyr::rename("ipca_mensal" = "IPCA - Variação mensal",
           "ipca_peso" = "IPCA - Peso mensal") %>%
    dplyr::select(date,
           product_groups,
           items,
           ipca_mensal,
           ipca_peso)

  # verificação do grupo de produtos

  ifelse(is.na(db_ipca$product_groups),
         print("Algum item não está especificado em grupos de produtos"),
         print("Todos os items estão especificados em grupos de produtos"))


  return(db_ipca) # Retorna um data.frame com os dados do IPCA

}
