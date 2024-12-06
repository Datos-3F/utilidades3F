#' validador_paquetes
#'
#' permite validar si los paquetes necesarios en un script se encuentran instalados
#' en caso de no estar instalados, se instalan
#' @param paquetes vector con los paquetes a instalar
#' @examples
#' validador_paquetes(paquetes);
#' @export
validador_paquetes <- function(paquetes) {
  for (i in paquetes) {
    if (!requireNamespace(i, quietly = TRUE)) {
      cat(i, "no está instalado \n")
      install.packages(i)
    } else {
      cat(i, "está instalado \n")
    }
  }
  # borrar el listado de paquetes
  require(paquetes)
  rm(paquetes)
}

