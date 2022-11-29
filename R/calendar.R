#' @title funcão calendário
#' @name fun_importCalendar
#'
#' @description Função para importar e organizar a base de dados do calendário para o ajuste sazonal
#'
#' @param
#'
#'
#' @author Luiz Paulo
#'
#' @details O arquivo de input deve corresponder a base de dados calendário:
#'
#' \code{date} - coluna de datas no formato YYYY/MM;
#' \code{peso_carnaval} - coluna com o calendário do carnaval;
#' \code{peso_corpus christi} - coluna com o calendário do Corpus Christi.
#'
#' @return O retorno é um objeto ts (time series) com os dados do calendário
#' \code{calendar_ts} - dados, em objeto ts, do calendário.
#
#'
#' @examples
#' \dontrun{
#' data_calendar = fun_importCalendar()
#' }
#'
#' @export

importCalendar <- function(){

  # importando o dataset

    calendar = readxl::read_excel("base_calendar.xlsx") %>%
    janitor::clean_names() %>%
    dplyr::filter(date <= Sys.Date()+(31*12))

  # time series

  calendar_ts = stats::ts(data = calendar[, 2:3],
                   start = c(min(calendar$date) %>% format("%Y") %>% as.numeric(),
                             min(calendar$date) %>% format("%m") %>% as.numeric()),
                   end = c(max(calendar$date) %>% format("%Y") %>% as.numeric(),
                           max(calendar$date) %>% format("%m") %>% as.numeric()),
                   frequency = 12)

  return(calendar_ts)

}


