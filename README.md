
<!-- README.md is generated from README.Rmd. Please edit that file -->

# encftr

<!-- badges: start -->

[![R build
status](https://github.com/endomer/encftr/workflows/R-CMD-check/badge.svg)](https://github.com/endomer/encftr/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/encftr)](https://CRAN.R-project.org/package=encftr)
[![Codecov test
coverage](https://codecov.io/gh/endomer/encftr/branch/master/graph/badge.svg)](https://codecov.io/gh/endomer/encftr?branch=master)
<!-- badges: end -->

encftr es una interfaz para trabajar con la base de datos de la Encuesta
Nacional Continua de Fuerza de Trabajo (ENCFT) en R.

## Instalación

encftr aun no está en CRAN.

<!-- You can install the released version of encftr from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("encftr") -->
<!-- ``` -->

Pero puedes intalar la versión de desarrollo desde
[GitHub](https://github.com/) con:

``` r
tryCatch(
  library(remotes),
  error = function(e){
    install.packages('remotes')
  }
)
remotes::install_github("endomer/encftr")
```

## Contribuye

Tienes comentarios o quieres contribuir?

Por favor, revisa las [gias de contribución (en
inglés)](https://endomer.github.io/encftr/CONTRIBUTING.html) antes de
iniciar un issue o pull request.

Por favor, observa que el proyecto encftr está sujeto a un [Código del
contribuyente](https://contributor-covenant.org/es/version/2/0/CODE_OF_CONDUCT.html).
Contribuyendo con el proyecto aceptas las términos y condiciones.
