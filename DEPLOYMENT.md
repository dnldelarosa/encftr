# Preparar una entrega de encftr

El proceso construye y verifica archivos locales. Publicar en un registro de
paquetes o activar GitHub Pages es una acción separada.

1. Instale R >= 4.1, las dependencias de DESCRIPTION y labeler >= 0.11.0.
   La entrega incluye labeler si esa versión aún no está publicada. Instale el
   archivo con `install.packages("labeler_0.11.0.tar.gz", repos=NULL, type="source")`.
2. Desde la carpeta ENDOM, ejecute `Rscript encftr/scripts/check-release.R`.
   Este paso genera el paquete fuente, ejecuta ejemplos, viñetas y pruebas, y
   completa R CMD check. En Windows, cree el binario con `R CMD INSTALL --build`
   sobre el archivo fuente validado, usando una biblioteca de salida separada.
3. Regenere la referencia con `Rscript encftr/scripts/export-rules.R`,
   `python endompy/scripts/compile-survey-rules.py`,
   `python endompy/scripts/compile-iih.py`,
   `python encftr/scripts/author-docs.py` y
   `python endompy/scripts/build-reference.py`.
4. Ejecute `Rscript encftr/scripts/build-docs.R` y después
   `python endompy/scripts/check-sites.py artifacts/encftr-release/sites/r --kind r`.
   El sitio tiene español en la raíz e inglés en `en/`, con selector de idioma,
   búsqueda independiente y páginas equivalentes.
5. Instale el tarball o ZIP en una biblioteca nueva y ejecute el ejemplo sintético.
   Conserve el informe de validación y SHA256 de cada archivo entregado.

Las fuentes canónicas son `R/`, `man/`, `vignettes/` y los recursos de `inst/`.
Las traducciones de pkgdown están en `pkgdown/i18n/en/`; `manifest.json` registra
hashes de ambas ediciones. Un cambio de API exige regenerar la ayuda, revisar la
traducción y repetir las comprobaciones antes de empaquetar.

La entrega pública contiene ejemplos sintéticos y resúmenes agregados de pruebas.
No incluya `data-raw/`, bases de personas/hogares ni artefactos internos de
comparación. Las tablas económicas de referencia y coeficientes del modelo son
recursos del paquete; su actualización requiere una fuente documentada y nuevas
comparaciones. El nombre canónico configurado es
https://endomer-py.github.io/encftr/; revise ese destino antes de activar Pages.

## ICV SIUBEN / encftr0

Ejecute `Rscript encftr/scripts/export-icv-rules.R` antes de construir el wheel de Python. Exporta las reglas cerradas del ICV histórico y el ejemplo sintético compartido. Instale encftr 0.10.0 antes del paquete opcional de compatibilidad encftr0 0.1.0. Consulte la guía de migración del ICV. Defina `ENCFT_RELEASE_DIR` con otra carpeta de artefactos para conservar una entrega anterior.
