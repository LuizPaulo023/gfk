#' @title Função pipe com todas as funções acopladas para cálculo do índice GFK
#' @name func_get_gfk
#'
#' @description Função encapsula todas as funções utilizadas no índice GFK
#'
#' @param
#'
#'
#' @author Luiz Paulo
#'
#' @details O arquivo de input deve corresponder a todas as funções
#'
#' @examples
#' \dontrun{
#' db_gfk = get_gfk()
#' }
#'
#' @export

get_gfk = function(dir_gfk = as.character()){

  setwd(diretorio_gfk)
  print(getwd())

  #db_gfk = fun_importCleanGFK(path_raw = "DB_GFK.xlsx")
  #db_gfk = fun_updated()
  #db_ipca = fun_importCleanIPCA()
  #ipca_ponderado = fun_cal_IPCA()
  #indice_gfk = fun_cal_gfk_br()
  #indice_gfk_region = fun_cal_gfk_uf(ipca_ponderado, db_gfk)
  #data_calendar = fun_importCalendar()
  bd_dessaz = dessaz_gfk(arima.model = "(0 1 1) (0 1 1)")

  return(db_gfk = bd_dessaz)

}







