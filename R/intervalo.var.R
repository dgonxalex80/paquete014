#' @title Intervalo de confianza para una varianza
#' @description Función que calcula el intervalo de confianza para una varianza.
#' @param x Vector con variable numérica.
#' @param niv.conf Nivel de confianza, por ejemplo, 95.
#' @return ic.var Intervalo de confianza para una varianza.
#' @export intervalo.var
#' @importFrom(stats qchisq var)
#' @examples intervalo.var(c(3, 4, 3, 2, 3, 4, 5), 0.95)
# '

intervalo.var <- function(x, niv.conf = 0.95) {
  n <- length(x)
  per.chi2 <- qchisq(c(1 - niv.conf/2, niv.conf/2), n - 1)   # percentiles chi-cuadrado para 95% de confianza
  intervalo.var <- (n - 1) * var(x) / per.chi2
  return(intervalo.var)
}


