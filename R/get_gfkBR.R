#' @title Cálculo do índice GFK BR
#' @name get_gfkBR
#'
#' @description Função para calcular o índice GFK BR, isto é, em nível agregado.
#'
#' @author Luiz Paulo Tavares Gonçalves
#'
#' @details O arquivo de input deve corresponder a base de dados do IPCA e do índice GFK limpas e organizadas.
#'
#' @return Retorna um data.frame com o índice GFK BR calculado
#' \code{gfk_br} - corresponde ao índice GFK agregado calculado
#
#'
#' @examples
#' \dontrun{
#' indice_gfk = get_gfkBR()
#' }
#'
#' @export

get_gfkBR = function(){

  db_gfk = run_updatedDB()
  ipca_ponderado = get_ipca_weights()

  # Unindo os data.frames

  gfk_br <- db_gfk[[2]] %>%
            dplyr::full_join(ipca_ponderado,
                             by = c("date", "product_groups")) %>%
            dplyr::group_by(date) %>%
             # Cálculo - valor real
            dplyr::summarise(valor_real = sum(valor*ipca_index/base, na.rm = T)) %>%
            dplyr::ungroup()

  # Base do GFK

  base_gfk = gfk_br %>%
             dplyr::filter(date > "2013-12-01" &
                             date <= "2014-12-01") %>%
             dplyr::summarise(media = mean(valor_real)) %>%
             purrr::pluck()

  # Calculando o índice GFK

  gfk_br = gfk_br %>%
           dplyr::group_by(date) %>%
           dplyr::summarise(gfk_br = (valor_real/base_gfk)*100,
                     gfk_br = as.numeric(gfk_br)) %>%
           dplyr::ungroup()

  return(gfk_br) # Retorna um data.frame com o índice GFK

}



