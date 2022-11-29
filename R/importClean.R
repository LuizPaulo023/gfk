#' @title Importação e Limpeza do Banco de dados GFK
#' @name fun_importCleanGFK
#'
#' @description Função para importar, limpar e organizar a base de dados do índice GFK
#'
#' @param DB_GFK.xlsx Um arquivo em formato xlsx, com duas abas, contendo os dados para o cáculo do índice GFK. Primeira aba com \code{date}, \code{uf}, \code{product_groups}, \code{valor}; segunda aba com \code{date}, \code{product_groups}, \code{valor}.
#'
#'
#' @author Luiz Paulo
#'
#' @details O arquivo de input deve corresponder a base de dados que compõe o índice GFK.
#' Cada linha representa um mês e tem informações como:
#'
#' \code{date} - coluna de datas no formato YYYY/MM;
#' \code{uf} - coluna das unidades federativas;
#' \code{product_groups} - nome do grupo de produtos classificado;
#' \code{valor} - valor nominal.
#'
#' @return O retorno é uma lista com o banco de dados limpo e organizado para as unidades federativas e em nível nacional (agregado);
#' \code{db_region} - lista com os dados das unidades federativas;
#' \code{db_br} - lista com os dados agregados em nível nacional.
#
#'
#' @examples
#' \dontrun{
#' db_gfk = fun_importCleanGFK(path_raw = "DB_GFK.xlsx")
#' }
#'
#' @export

# function: import and clean database

importClean <- function(path_raw){

  db <- path_raw %>%
    readxl::excel_sheets() %>%
    magrittr::set_names() %>%
    purrr::map(read_excel, path = path_raw)

  # Juntando a base com a identificação das UFs

  db_region <- base::merge(db[["ID_UF"]], db[["ComposicaoGFK"]], by = "REGION2") %>%
    janitor::clean_names()%>%
    dplyr::group_by(period,
             uf,
             domain_productgroup) %>%
    dplyr::summarise(valor = sum(sales_value_brl)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(period = parse_date_time(period, "my")) %>%
    dplyr::rename(
      region = uf,
      date = period,
      product_groups = domain_productgroup)

  # Agregando em nível nacional

  db_br <- db_region %>%
    dplyr::group_by(date, product_groups) %>%
    dplyr::summarise(valor = sum(valor)) %>%
    dplyr::ungroup()

  return(list(db_region,
              db_br, db)) # Retorna uma lista para UF e agregado nível nacional

}

