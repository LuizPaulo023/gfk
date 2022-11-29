#' @title Cálculo do índice GFK BR
#' @name fun_cal_gfk_br
#'
#' @description Função para calcular o índice GFK BR, isto é, em nível agregado.
#'
#' @param ipca_ponderado Corresponde a base de dados do IPCA limpa e organizada com \code{date}, \code{product_groups}, \code{items}, \code{ipca_mensal}, \code{ipca_peso}.
#' @param db_gfk Corresponde aos dados para o cáculo do índice GFK. Primeira aba com \code{date}, \code{uf}, \code{product_groups}, \code{valor}; segunda aba com \code{date}, \code{product_groups}, \code{valor}.
#'
#' @author Luiz Paulo
#'
#' @details O arquivo de input deve corresponder a base de dados do IPCA e do índice GFK limpas e organizadas.
#'
#' \code{Mês} - coluna de datas no formato MM/YYYY;
#' \code{Variável} - coluna corresponde a variação do IPCA e do peso dos items que compõe o IPCA;
#' \code{Geral, grupo, subgrupo, item e subitem} - corresponde ao grupo de produtos do item;
#' \code{valor} - valor corresponde a variação nominal do IPCA e ao peso dos items que compõe o IPCA.
#'
#' \code{date} - coluna de datas no formato YYYY/MM;
#' \code{uf} - coluna das unidades federativas;
#' \code{product_groups} - nome do grupo de produtos classificado;
#' \code{valor} - valor nominal.
#'
#'
#'
#'
#' @return Retorna um data.frame com o índice GFK BR calculado
#' \code{gfk_br} - corresponde ao índice GFK agregado calculado
#
#'
#' @examples
#' \dontrun{
#' indice_gfk = fun_cal_gfk_br()
#' }
#'
#' @export

gfkBR = function(){

  db_gfk = updated()
  ipca_ponderado = ipcaPonderado()

  # Unindo os data.frames

  gfk_br <- db_gfk[[2]] %>%
   dplyr::full_join(ipca_ponderado, by = c("date", "product_groups")) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      valor_real = sum(valor*ipca_index/base, na.rm = T)) %>%
    dplyr::ungroup()

  # Base do GFK

  base_gfk = gfk_br %>%
    dplyr::filter(date > "2013-12-01" & date <= "2014-12-01") %>%
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



