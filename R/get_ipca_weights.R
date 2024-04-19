#' @title Cálculo do IPCA ponderado pelos pesos dos items
#' @name get_ipca_weights
#'
#' @description Função para calcular o IPCA ponderado pelos pesos dos items que compõe o índice GFK.
#'
#' @param db_ipca Corresponde a base de dados do IPCA limpa e organizada com \code{date}, \code{product_groups}, \code{items}, \code{ipca_mensal}, \code{ipca_peso}.
#'
#' @author Luiz Paulo
#'
#' @details O arquivo de input deve corresponder a base de dados do IPCA limpa e organizada.
#'
#' @return Retorna um data.frame com o IPCA ponderado e com o índice do IPCA ponderado.
#' \code{ipca_group} - corresponde ao ipca ponderado e ao índice do IPCA.
#
#'
#' @examples
#' \dontrun{
#' ipca_ponderado = get_ipca_weights()
#' }
#'
#' @export

get_ipca_weights <- function(){

  options(scipen = 100)

  # Chamando a função import IPCA
  db_ipca = get_ipca()

  # Calculando o IPCA Ponderado

  ipca_weights <- db_ipca %>%
                  dplyr::group_by(date, product_groups) %>%
                  # Encontrando os pesos
                  dplyr::mutate(peso = ipca_peso/sum(ipca_peso, na.rm = T),
                                # IPCA ponderado
                                ipca_ponderado = ipca_mensal*peso )%>% dplyr::ungroup()

  # Calculando o índice do IPCA

  ipca_group <- ipca_weights %>%
                dplyr::group_by(date, product_groups) %>%
                dplyr::summarise(ipca_ponderado = sum(ipca_ponderado)) %>%
                dplyr::group_by(product_groups) %>%
                dplyr::mutate(ipca_index = ipca_ponderado / 100,
                              ipca_index = 1 + ipca_index,
                              ipca_index = ifelse(date == min(date), 100, ipca_index),
                              ipca_index = cumprod(ipca_index)) %>% dplyr::ungroup()

  # Base - 2016-12 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  base_ipca <- ipca_group %>%
               dplyr::filter(date == "2016-12-01") %>%
               dplyr::rename(base = ipca_index) %>%
               dplyr::select(-date) %>%
               dplyr::full_join(select(ipca_group, product_groups, ipca_index, date),
                                by = "product_groups") %>%
               dplyr::relocate(date, .after = NULL)

  return(base_ipca) # Retorna um data.frame com os dados do IPCA

}




