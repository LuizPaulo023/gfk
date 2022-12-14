% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importIPCA.R
\name{fun_importCleanIPCA}
\alias{fun_importCleanIPCA}
\alias{importIPCA}
\title{Importação e Limpeza do dataset do IPCA}
\usage{
importIPCA()
}
\arguments{
\item{keys}{São as chaves APIs para puxar os dados da variação mensal do IPCA e do peso dos items que compõe o IPCA direto do SIDRA com \code{Variável}, \code{Mês}, \code{Geral, grupo, subgrupo, item e subitem}, \code{valor}.}
}
\value{
O retorno é um data.frame com a base de dados do IPCA limpo e organizado;
\code{db_ipca} - data.frame com a base de dados limpa e organizado.
}
\description{
Função para importar, limpar e organizar a base de dados do IPCA
}
\details{
O arquivo de input deve corresponder as chaves Apis do Sidra-IBGE (package: sidra).

\code{Mês} - coluna de datas no formato MM/YYYY;
\code{Variável} - coluna corresponde a variação do IPCA e do peso dos items que compõe o IPCA;
\code{Geral, grupo, subgrupo, item e subitem} - corresponde ao grupo de produtos do item;
\code{valor} - valor corresponde a variação nominal do IPCA e ao peso dos items que compõe o IPCA.
}
\examples{
\dontrun{
db_ipca = fun_importCleanIPCA()
}

}
\author{
Luiz Paulo
}
