#' @title Pipe: Script que chama as funções que compõe o índice GFK
#' @author Luiz Paulo Tavares Gonçalves

rm(list = ls())
# graphics.off()

# Dependências ===========================================================

pacman::p_load(tidyverse,
               lubridate,
               readxl,
               zoo,
               sidrar,
               seasonal,
               stringr,
               xts,
               series.4macro,
               janitor,
               writexl,
               pacman)

# install.packages("devtools")
# devtools::install_github("LuizPaulo023/gfk")
library(gfk4i)

# Congigurações do usuário \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

user = base::getwd() %>%
       stringr::str_extract("^((?:[^/]*/){3})") %>% print()

diretorio_gfk <- paste0(user,
                 "4intelligence/IT Admin - Operacional/trabalho/dados_alternativos/gfk/dados_primarios/")


# Chamando pacote {gfk} \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# função run_update_gfk() atualiza o índice GFK
# Calculando e salvando o índice GFK:
# Destino: \4intelligence\Gfk - Gfk\versao_R\dados_primarios

bd_gfk = gfk4i::run_update_gfk(dir_gfk = diretorio_gfk)

