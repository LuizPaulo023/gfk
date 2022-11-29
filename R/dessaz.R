#' @title Calculando o ajuste sazonal do índice GFK BR & GFK UF
#' @name fun_dessaz_gfk
#'
#' @description Função para calcular o ajuste sazonal do índice GFK UF e em nível Nacional
#'
#' @param indice_gfk_uf corresponde ao índice GFK UF calculado \code{gfk_uf};
#' @param indice_gfk corresponde ao índice GFK em nível nacional \code{gfk_br};
#' @param data_calendar corresponder ao calenário para ajuste sazonal \code{peso_carnval}, \code{peso_corpus_christi}.
#' @author Luiz Paulo
#'
#' @details O arquivo de input deve corresponder ao índice GFK BR-UF e ao calendário.
#'
#' \code{date} - coluna de datas no formato YYYY/MM/DD;
#' \code{gfk_uf} - coluna corresponde ao índice GFK UF calculado;
#' \code{gfk_br} - corresponde ao índice GFK BR calculado;
#' \code{data_calendar} - corresponde ao calendário para o ajuste sazonal.
#'
#'
#'
#'
#'
#'
#' @return Retorna um data.frame com o índice GFK BR-UF com ajuste sazonal.
#' \code{db_gfk_dessaz} - corresponde ao índice GFK BR-UF com ajuste sazonal.
#
#'
#' @examples
#' \dontrun{
#' bd_gfk <- fun_dessaz_gfk(arima.model = "(0 1 1) (0 1 1)")
#' }
#'
#' @export

# Juntando e organizando as bases para o ajuste sazonal

dessaz_gfk <- function(arima.model = ""){

  indice_gfk = gfkBR()
  data_calendar = importCalendar()

  # Organizando os datasets para o ajuste sazonal
  # Passando para time series

  if(!is.null(indice_gfk)){
    indice_gfk = indice_gfk %>%
      stats::na.omit() %>%
      dplyr::filter(gfk_br > 0)
  }

  gfk_ts = stats::ts(indice_gfk[, 2],
              start = c(min(indice_gfk$date) %>% format("%Y") %>% as.numeric(),
                        min(indice_gfk$date) %>% format("%m") %>% as.numeric()),
              end = c(max(indice_gfk$date) %>% format("%Y") %>% as.numeric(),
                      max(indice_gfk$date) %>% format("%m") %>% as.numeric()), frequency = 12)

  # Ajuste Sazonal
  print(gfk_ts)
  print(checkX13()) # confirmando o X-13 ARIMA

  x_arima_br <- seasonal::seas(gfk_ts,
                               arima.model = arima.model,
                               xreg = cbind(data_calendar[,1], data_calendar[,2]),
                               regression.variables = c("td1coef", "Easter[8]"),
                               transform.function = "log",
                               x11.mode = "mult")

  ajuste = final(x_arima_br) %>% # Salvando o ajuste Sazonal
    base::data.frame(indice_gfk) %>%
    dplyr::rename(gfk_dessaz = ".") %>%
    dplyr::relocate(date,gfk_dessaz, gfk_br)

  saveRDS(ajuste, "bd_gfk.rds")

  return(ajuste)

}

