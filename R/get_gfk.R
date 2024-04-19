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

  # Definindo Diretório 
  setwd(diretorio_gfk)
  print(getwd())

  bd_dessaz = dessaz_gfk(arima.model = "(0 1 1) (0 1 1)")

  return(db_gfk = bd_dessaz)

}







