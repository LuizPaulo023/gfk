#' @title Atualização do Banco de dados GFK
#' @name fun_updated()
#'
#' @description Função para atualizar a base de dados do índice GFK
#'
#' @param
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
#' @return O retorno é uma lista com o banco de dados atualizada
#' \code{db_updated_region} - lista com os dados das unidades federativas;
#' \code{db_updated_br} - lista com os dados agregados em nível nacional.
#
#'
#' @examples
#' \dontrun{
#' db_gfk = fun_updated()
#' }
#'
#' @export

# Função de atualização

updated <- function(){
  # Chamando função com o DB GFK
  db_gfk = importClean(path_raw = "DB_GFK.xlsx")
  # Atualizando a base de dados GFK

  updated_data <- base::list.files(pattern = "GFK_HITLISTFULL_",
                             full.names = T) %>%
    purrr::map_dfr(., read_xlsx, col_names = F) %>%
    stats::na.omit() %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::distinct(Period, DomainProductgroup,
                    REGION2, .keep_all = T) %>% as.list()


  # Concatenando, limpando e organizando

  db_updated <- merge(db_gfk[[3]][["ID_UF"]], updated_data, by = "REGION2") %>%
    janitor::clean_names()%>%
    dplyr::mutate(
      period = parse_date_time(period, "my"),
      sales_units = as.numeric(sales_units),
      sales_value_brl = as.numeric(sales_value_brl)) %>%
  dplyr::group_by(period,
             uf,
             domain_productgroup) %>%
  dplyr::summarise(valor = sum(sales_value_brl)) %>%
  dplyr::ungroup() %>%
   dplyr::rename(
      region = uf,
      date = period,
      product_groups = domain_productgroup)  %>%
    dplyr::distinct(date, product_groups,
                    region, .keep_all = T)

  # Base de dados GFK região atualizadas

  db_updated_region <- as.list(db_updated) %>%
   dplyr::bind_rows(db_gfk[[1]]) %>%
    dplyr::distinct(date, product_groups,
                    region, .keep_all = T) %>%
   dplyr::arrange(-dplyr::desc(date))

  # GFK nivel agregado BR

  db_updated_br <- db_updated %>%
   dplyr::group_by(date, product_groups) %>%
    dplyr::summarise(valor = sum(valor)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(db_gfk[[2]]) %>%
    dplyr::distinct(date, product_groups,
                    .keep_all = T) %>%
    dplyr::arrange(-desc(date))


  return(list(db_updated_region,
              db_updated_br))

}




