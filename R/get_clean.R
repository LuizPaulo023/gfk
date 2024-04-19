#' @title Importação e Limpeza do Banco de dados GFK
#' @name get_clean
#'
#' @description Função para importar, limpar e organizar a base de dados do índice GFK
#'
#' @param DB_GFK.xlsx Um arquivo em formato xlsx, com duas abas, contendo os dados para o cáculo do índice GFK. Primeira aba com \code{date}, \code{uf}, \code{product_groups}, \code{valor}; segunda aba com \code{date}, \code{product_groups}, \code{valor}.
#'
#' @author Luiz Paulo Tavares Gonçalves
#'
#' @details O arquivo de input deve corresponder a base de dados que compõe o índice GFK.
#' Cada linha representa um mês e tem informações como:
#'
#' @examples
#' \dontrun{
#' db_gfk = get_clean(path_raw)
#' }
#'
#' @export

# function: import and clean database \\\\\\\\\\\\\\

get_clean <- function(path_raw){

  db <- path_raw %>%
        readxl::excel_sheets() %>% set_names() %>%
        purrr::map(read_excel, path = path_raw)

  # Juntando a base com a identificação das UFs

  db_region <- base::merge(db[["ID_UF"]],
                           db[["ComposicaoGFK"]], by = "REGION2") %>%
               janitor::clean_names()%>%
               dplyr::group_by(period,
                        uf,
                        domain_productgroup) %>%
               dplyr::summarise(valor = sum(sales_value_brl)) %>%
               dplyr::ungroup() %>%
               dplyr::mutate(period = parse_date_time(period, "my")) %>%
               dplyr::rename(region = uf,
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

