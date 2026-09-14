"""Maintain paired source guides and the English API edition from reviewed contracts.

Run after roxygenise. Spanish Rd remains the package's canonical native help.
English signatures, argument names and examples are copied without translation.
"""
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
R = ROOT / "encftr"
P = ROOT / "endompy"
EN = R / "pkgdown/i18n/en"
EN.mkdir(parents=True, exist_ok=True)

GUIDES = {
"encftr": {
"title": ("Empezar con ENCFT", "Getting started with ENCFT"),
"es": """encftr 0.10.0 y endompy 0.3.0 preparan y analizan respuestas de personas de la Encuesta Nacional Continua de Fuerza de Trabajo. Incluyen etiquetas, educación, trabajo, ingresos, indicadores del hogar, pobreza monetaria 2012/2022 e índice de ingresos del hogar (IIH).

Instale primero labeler >= 0.11.0 en R o labelerpy >= 0.2.1 en Python. La entrega contiene los paquetes locales para instalar sin depender de que esas versiones estén publicadas. Los paquetes de R necesitan R >= 4.1; Python admite Python >= 3.9 con pandas >= 1.5. Consulte la guía de despliegue para construir e instalar desde los archivos de entrega.

El ejemplo incluido es completamente sintético: 12 personas, cuatro hogares a través de los períodos y claves ficticias. No contiene microdatos de la encuesta. Las funciones devuelven una tabla con los resultados y mantienen las filas originales, salvo cuando se solicita explícitamente una salida por hogar del IIH.

Calcule los pesos antes de filtrar personas. El divisor anual es el número de trimestres distintos observados dentro de cada año; el semestral se calcula dentro de cada año y semestre. Una muestra de dos trimestres no equivale a observar un año completo. Si el extracto no contiene períodos y usted conoce su cobertura, declare `periods = 4` para el anual o `periods = 2` para el semestral. Sin esa declaración, el cálculo falla.

`TRIMESTRE` acepta AAAAT (por ejemplo 20191), o 1–4 acompañado de `ANO`. Los identificadores y períodos no pueden estar ausentes. Los pesos ausentes se conservan; pesos negativos, infinitos o de tipo lógico se rechazan.

R usa nombres `ftc_*`; Python ofrece los mismos nombres sin prefijo y alias `ftc_*` para migrar scripts. Las operaciones también están disponibles en `EncftDataFrame` y se pueden encadenar. Las convenciones de tipos son nativas: R usa factores y valores NA; Python usa categorías y tipos anulables de pandas.
""",
"en": """encftr 0.10.0 and endompy 0.3.0 prepare and analyze person responses from the Dominican Republic Continuous National Labour Force Survey. They include labeling, education, labour, income, household indicators, monetary poverty 2012/2022 and the household income index (IIH).

Install labeler >= 0.11.0 for R or labelerpy >= 0.2.1 for Python first. The delivery includes local packages so installation does not depend on these versions being published. R requires R >= 4.1; Python supports Python >= 3.9 with pandas >= 1.5. See the deployment guide for building and installing delivery files.

The bundled example is entirely synthetic: 12 people, four households across periods and fictitious identifiers. It contains no survey microdata. Functions return a table with results and preserve original rows, except when household output is explicitly requested for IIH.

Calculate weights before filtering people. The annual divisor is the number of distinct observed quarters within each year; the semester divisor is calculated within each year and semester. A two-quarter extract does not provide full-year coverage. If an extract has no period fields and its coverage is known, declare `periods = 4` for annual weights or `periods = 2` for semester weights. Otherwise the calculation fails.

`TRIMESTRE` accepts YYYYQ (for example 20191), or 1–4 with `ANO`. Identifiers and periods cannot be missing. Missing weights are preserved; negative, infinite or boolean weights are rejected.

R uses `ftc_*` names; Python provides the same names without the prefix and `ftc_*` aliases for script migration. Calculations are also chainable methods of `EncftDataFrame`. Types follow each language: R factors and NA values, and pandas categories and nullable dtypes.
""",
"r": """library(encftr)
x <- jsonlite::fromJSON(system.file("examples", "synthetic-members.json", package = "encftr"))
y <- ftc_factor_expansion_anual(x)
y <- ftc_anos_educacion(y)
stopifnot(nrow(y) == 12, all(y$factor_expansion_anual == 60))
head(y[c("PERIODO", "EDAD", "anos_educacion", "factor_expansion_anual")])""",
"py": """import json
from importlib.resources import files
from endompy import EncftDataFrame
from endompy import encftr as encft
x = EncftDataFrame(json.loads(files("endompy.encftr").joinpath("resources/synthetic-members.json").read_text()))
y = x.factor_expansion_anual().anos_educacion()
assert len(y) == 12 and (y["factor_expansion_anual"] == 60).all()
print(y[["PERIODO", "EDAD", "anos_educacion", "factor_expansion_anual"]].head())"""
},
"diccionario": {
"title": ("Diccionarios y ediciones", "Dictionaries and editions"),
"es": """La edición incluida se llama `baseline-1` y contiene 636 definiciones. Conserva la versión disponible en el proyecto, con integridad verificable, pero no declara fechas históricas de vigencia. Por eso una selección por fecha falla hasta que se registre una edición fechada con respaldo documental. No se interpreta la fecha de creación como fecha de vigencia.

Cada edición es una instantánea completa y se registra con un identificador inmutable. Internamente se guardan definiciones por contenido: si cambia una sola variable entre dos ediciones de 636 variables, las otras 635 reutilizan sus definiciones. La aplicación recibe un diccionario completo, sin tener que resolver parches manualmente. Los nombres y etiquetas de las variables pueden cambiar sin alterar las ediciones ya registradas.

La selección admite una versión exacta o una fecha ISO de aplicabilidad. Los intervalos son inclusivos. Fechas sin cobertura o ambiguas producen error. Los renombramientos deben declararse explícitamente al registrar la revisión; no se infieren por coincidencia de etiquetas. Use `labeler::dict_diff()` en R o el método `diff()` en Python para inspeccionar las diferencias entre ediciones.

El siguiente registro usa fechas ficticias únicamente para explicar el mecanismo. No representa una serie histórica oficial de ENCFT. La conexión pertenece al llamador. Use archivos SQLite separados para sus registros de trabajo y siga la política de transacciones de labeler/labelerpy.

La versión del diccionario describe el cuestionario; `pobreza_monetaria_2012` y `pobreza_monetaria_2022` eligen la metodología del cálculo. Son decisiones independientes. Al guardar un análisis conserve versión del paquete, versión y hash del diccionario, metodología, período de las respuestas y cobertura de las tablas económicas.
""",
"en": """The bundled edition is `baseline-1` and contains 636 definitions. It preserves the dictionary available in the project with verifiable integrity, but asserts no historical applicability dates. Date selection therefore fails until a dated edition supported by documentation is registered. Creation time is not treated as an applicability date.

Each edition is a complete snapshot with an immutable identifier. Definitions are stored by content: if one variable changes between two 636-variable editions, the other 635 reuse their definitions. Applications receive a complete dictionary without manually resolving patches. Names and labels can change without altering previously registered editions.

Selection accepts an exact version or an ISO applicability date. Intervals are inclusive. Uncovered or ambiguous dates raise errors. Renames must be declared explicitly when registering a revision; they are not inferred from matching labels. Use `labeler::dict_diff()` in R or the Python `diff()` method to inspect differences between editions.

The following registry uses fictitious dates only to explain the mechanism. It is not an official historical ENCFT dictionary series. The caller owns the connection. Use separate SQLite files for working registries and follow labeler/labelerpy transaction policy.

Dictionary versions describe questionnaires; `pobreza_monetaria_2012` and `pobreza_monetaria_2022` select calculation methodologies. These are independent choices. Persist package version, dictionary version and hash, methodology, response periods and economic-table coverage with an analysis.
""",
"r": """library(encftr)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
base <- labeler::dict_draft(ftc_dict())
v1 <- ftc_register_dict(con, base, "example-1", valid_from = "2020-01-01", valid_to = "2020-12-31")
draft <- labeler::dict_draft(v1)
draft$SEXO$label <- "Sexo de la persona"
v2 <- ftc_register_dict(con, draft, "example-2", valid_from = "2021-01-01", valid_to = "2021-12-31")
selected <- ftc_dict(at = "2021-04-01", con = con)
stopifnot(labeler::dict_revision(selected)$version == "example-2")
ftc_dict_versions(con)
DBI::dbDisconnect(con)""",
"py": """import sqlite3
from endompy import encftr as encft
con = sqlite3.connect(":memory:")
v1 = encft.register_dict(con, encft.get_dict().draft(), "example-1", valid_from="2020-01-01", valid_to="2020-12-31")
draft = v1.draft()
draft["SEXO"].label = "Sexo de la persona"
v2 = encft.register_dict(con, draft, "example-2", valid_from="2021-01-01", valid_to="2021-12-31")
selected = encft.get_dict(at="2021-04-01", con=con)
assert selected.revision()["version"] == "example-2"
print(encft.dict_versions(con))
con.close()"""
},
"pobreza-monetaria": {
"title": ("Pobreza monetaria: 2012 y 2022", "Monetary poverty: 2012 and 2022"),
"es": """Seleccione la metodología explícitamente. La función histórica `ftc_pobreza_monetaria()` sigue apuntando a 2012 por compatibilidad y emite una advertencia de deprecación. No cambia de metodología según el año de los datos.

| Contrato | Metodología 2012 | Metodología 2022 |
|---|---|---|
| Deflación | Nacional | Cuatro macrorregiones |
| Líneas | Urbana y rural | Ozama, Norte, Sur y Este |
| Denominador per cápita | CANTIDAD_MIEMBROS_HOGAR reportada | Personas observadas en el hogar y período |
| Alimentación escolar | Excluida | Incluida |
| Selección | pobreza_monetaria_2012 | pobreza_monetaria_2022 |

La entrada contiene respuestas de personas, con nombres del cuestionario en mayúsculas y claves `PERIODO` (AAAAMM), `TRIMESTRE`, `VIVIENDA` y `HOGAR`. La metodología 2012 requiere además `ZONA` y `CANTIDAD_MIEMBROS_HOGAR`. Debe aportar todos los miembros del hogar para sumar sus ingresos. La de 2022 necesita una macrorregión identificable mediante `ID_PROVINCIA` o `GRUPO_REGION`.

`prepare_poverty` resuelve únicamente alias conocidos de programas sociales y ayuda anual, y completa los campos opcionales que el código metodológico trata como cero o ausentes. No completa arbitrariamente las otras respuestas obligatorias. La clasificación conserva ausentes cuando faltan líneas, el ingreso calculado no es finito o el denominador 2012 no es positivo. Los componentes omitidos que el código oficial suma con `na.rm=TRUE` contribuyen cero; esto no convierte cualquier respuesta faltante en un error ni sustituye una auditoría de calidad de la encuesta.

Las salidas monetarias son pesos dominicanos mensuales. `ing_total_pobreza` es nominal; `ing_total_pobreza_def` e `ing_pc_pobreza_def` usan la base de precios de la metodología. `linea_pobreza` y `linea_pobreza_extrema` corresponden al mismo criterio de comparación. `pobreza_monetaria` vale 1 (extrema), 2 (no extrema) o 3 (no pobre); `pobre` e `indigente` son lógicos anulables. La igualdad con una línea pertenece a la categoría superior.

`keep=TRUE` conserva componentes intermedios; una lista conserva los nombres solicitados. En R los argumentos son `.keep` y `.reuse`. En las dos funciones metodológicas `reuse` se acepta por compatibilidad y siempre se recalcula desde las respuestas. Repetir el cálculo actualiza las columnas de salida. Los auxiliares históricos `ing_*` conservan su contrato particular de reutilización: son útiles para inspeccionar componentes, pero el flujo oficial completo debe ejecutarse con la función metodológica explícita.

Las líneas empaquetadas de ambas metodologías cubren enero de 2016 a diciembre de 2022; los IPC cubren julio de 2015 a diciembre de 2022. La tabla de tasas llega a diciembre de 2024, pero eso no amplía las líneas de pobreza. Los períodos desde 2023 quedan sin clasificación con esta entrega. Revise la fuente antes de actualizar tasas o líneas; actualizar el diccionario no actualiza estas tablas. Las tasas de remesas se asignan al mes declarado, sin depender del orden de las personas.

La validación de 2019 cubre 83,031 personas y 26,697 hogares. R reproduce ingresos, líneas y clasificación del código oficial 2012; la clasificación 2022 coincide con el archivo de referencia de la metodología. Python reproduce los resultados de R en ambos métodos. Esta evidencia valida esa base y los casos sintéticos de regresión; no certifica períodos para los que no se ejecutó una comparación independiente.
""",
"en": """Select the methodology explicitly. The historical `ftc_pobreza_monetaria()` entry point still selects 2012 for compatibility and emits a deprecation warning. It does not select a methodology from the survey year.

| Contract | 2012 methodology | 2022 methodology |
|---|---|---|
| Deflation | National | Four macroregions |
| Lines | Urban and rural | Ozama, Norte, Sur and Este |
| Per-capita denominator | Reported CANTIDAD_MIEMBROS_HOGAR | Observed people in the household and period |
| School food | Excluded | Included |
| Entry point | pobreza_monetaria_2012 | pobreza_monetaria_2022 |

Input consists of person responses with uppercase questionnaire names and keys `PERIODO` (YYYYMM), `TRIMESTRE`, `VIVIENDA` and `HOGAR`. The 2012 method additionally requires `ZONA` and `CANTIDAD_MIEMBROS_HOGAR`. Supply all household members when aggregating their income. The 2022 method needs an identifiable macroregion through `ID_PROVINCIA` or `GRUPO_REGION`.

`prepare_poverty` resolves only known social-program and annual-aid aliases, and fills optional fields that the methodology code treats as zero or missing. It does not invent other required answers. Classification remains missing when lines are unavailable, calculated income is nonfinite, or the 2012 denominator is not positive. Missing components summed with `na.rm=TRUE` in the official code contribute zero; this does not make every missing answer an error or replace survey quality review.

Monetary outputs are monthly Dominican pesos. `ing_total_pobreza` is nominal; `ing_total_pobreza_def` and `ing_pc_pobreza_def` use the methodology's price basis. `linea_pobreza` and `linea_pobreza_extrema` use the corresponding comparison basis. `pobreza_monetaria` is 1 (extreme), 2 (non-extreme) or 3 (nonpoor); `pobre` and `indigente` are nullable booleans. Equality with a line belongs to the higher category.

`keep=True` retains intermediate components; a list retains requested names. R names these arguments `.keep` and `.reuse`. Both methodology functions accept `reuse` for compatibility and always recompute from responses. Repeating a calculation updates output columns. Historical `ing_*` helpers retain their individual reuse contracts: they are useful for inspecting components, while an official complete workflow should use an explicit methodology function.

Bundled lines for both methodologies cover January 2016 through December 2022; CPI tables cover July 2015 through December 2022. Exchange rates extend through December 2024, which does not extend poverty-line coverage. Periods from 2023 onward remain unclassified in this delivery. Inspect provenance before updating rates or lines; dictionary updates do not update economic tables. Remittance rates follow the reported month and do not depend on person order.

Validation on 2019 covers 83,031 people and 26,697 households. R reproduces income, lines and classification from the official 2012 code; 2022 classification matches the methodology reference file. Python reproduces R outputs for both methods. This evidence validates that dataset and synthetic regression cases; it does not certify periods without an independent comparison.
""",
"r": """library(encftr)
x <- jsonlite::fromJSON(system.file("examples", "synthetic-members.json", package = "encftr"))
p12 <- ftc_pobreza_monetaria_2012(x)
p22 <- ftc_pobreza_monetaria_2022(x)
stopifnot(nrow(p12) == nrow(x), nrow(p22) == nrow(x))
head(p22[c("ing_total_pobreza", "ing_pc_pobreza_def", "linea_pobreza", "pobreza_monetaria")])""",
"py": """import json
import pandas as pd
from importlib.resources import files
from endompy import encftr as encft
x = pd.DataFrame(json.loads(files("endompy.encftr").joinpath("resources/synthetic-members.json").read_text()))
p12 = encft.pobreza_monetaria_2012(x)
p22 = encft.pobreza_monetaria_2022(x)
assert len(p12) == len(x) == len(p22)
print(p22[["ing_total_pobreza", "ing_pc_pobreza_def", "linea_pobreza", "pobreza_monetaria"]].head())"""
},
"indicadores": {
"title": ("Educación, trabajo y hogares", "Education, labour and households"),
"es": """Los indicadores respetan universos explícitos. Alfabetización, matrícula y asistencia se calculan entre edades inclusivas; fuera de ese universo devuelven ausentes. Dentro del universo, las condiciones no reconocidas siguen la codificación del indicador de R (por ejemplo, alfabetización distinta de código 1 produce cero). No interprete ese cero como una imputación documental de la respuesta original.

Los años de educación permiten tres bases: `armonizada_6_6` suma seis años a secundaria; `legacy_8_4` suma ocho; `historica_por_ano` usa ocho antes de `anio_corte` y seis a partir de ese año. La base predeterminada es 6+6 y el corte predeterminado es 2022. La opción histórica requiere `ANO`. Los estudios universitarios parten de 12 y los de posgrado de 16. Una configuración analítica no modifica los códigos del cuestionario.

`summer_fix` incluye como estudiantes a quienes esperan el inicio de clases en junio–agosto según el motivo codificado. Se aplica de forma consistente a matrícula, asistencia y trabajo infantil. Este último tiene universo 5–14 años y cuatro categorías: trabaja y asiste (1), trabaja y no asiste (2), no trabaja y asiste (3), no trabaja y no asiste (4).

La tasa de dependencia expresa dependientes por cada 100 personas de edad activa (15–64 por defecto). Los hogares sin denominador tienen resultado ausente; una edad ausente conserva la indeterminación de la suma. `limit` permite dependencia total, de mayores o de menores. La alfabetización del hogar excluye del denominador a quienes están fuera del universo. El hacinamiento cuenta personas por dormitorio a nivel de vivienda; cero dormitorios produce infinito. La jefatura se agrupa por mes, vivienda y hogar; un hogar sin jefatura queda ausente y más de una jefatura genera error.

Para cortes categóricos use límites numéricos explícitos y etiquetas explícitas al comparar R y Python. Los intervalos son abiertos a la izquierda y cerrados a la derecha. Las funciones de cortes avanzados propias de Dmisc en R no tienen equivalencia general automática con pandas. La conexión configurada de Dmisc y el visor HTML de R son adaptadores del entorno: en Python utilice una conexión propia (`sqlite3`, SQLAlchemy) y `browse_dict()`, que devuelve una tabla para cuadernos o exportación.
""",
"en": """Indicators have explicit populations. Literacy, enrollment and attendance use inclusive age bounds; people outside the population receive missing results. Within it, unmatched conditions follow the R indicator's coding (for example, literacy codes other than 1 produce zero). That zero does not document an imputation of the original answer.

Years of schooling support three bases: `armonizada_6_6` adds six years for secondary schooling; `legacy_8_4` adds eight; `historica_por_ano` uses eight before `anio_corte` and six from that year onward. The default is 6+6 with a default cutoff of 2022. The historical option requires `ANO`. University starts at 12 years and postgraduate schooling at 16. Analytical configuration does not alter questionnaire codes.

`summer_fix` includes students waiting for classes to start in June–August according to the coded reason. It is passed consistently to enrollment, attendance and child labour. Child labour covers ages 5–14 and has four categories: working and attending (1), working and not attending (2), not working and attending (3), neither working nor attending (4).

The dependency ratio is dependents per 100 working-age people (15–64 by default). Households without a denominator have missing results; a missing age preserves uncertainty in the sum. `limit` selects total, older or younger dependency. Household literacy excludes people outside the selected ages from its denominator. Crowding counts people per bedroom at dwelling level; zero bedrooms yields infinity. Headship groups by month, dwelling and household; missing heads yield missing results, and multiple heads raise an error.

Use explicit numeric cut points and labels for R/Python comparisons. Intervals are open on the left and closed on the right. Advanced Dmisc-specific R cut functions have no general automatic pandas equivalent. The configured Dmisc connection and R HTML viewer are environment adapters: Python uses caller-owned connections (`sqlite3`, SQLAlchemy) and `browse_dict()`, which returns a notebook/export table.
""",
"r": """library(encftr)
x <- data.frame(NIVEL_ULTIMO_ANO_APROBADO = c(3,3,5), ULTIMO_ANO_APROBADO = c(4,6,2), ANO = c(2021,2022,2022))
stopifnot(all(ftc_anos_educacion(x)$anos_educacion == c(10,12,14)))
ftc_anos_educacion(x, secundaria_base = "historica_por_ano")""",
"py": """import pandas as pd
from endompy import encftr as encft
x = pd.DataFrame({"NIVEL_ULTIMO_ANO_APROBADO": [3,3,5], "ULTIMO_ANO_APROBADO": [4,6,2], "ANO": [2021,2022,2022]})
assert encft.anos_educacion(x)["anos_educacion"].tolist() == [10,12,14]
print(encft.anos_educacion(x, secundaria_base="historica_por_ano"))"""
},
"iih": {
"title": ("Índice de ingresos del hogar", "Household income index"),
"es": """El IIH estima una clasificación a partir de características de las personas y la vivienda mediante modelos de coeficientes fijos. No es el ingreso observado de la encuesta ni una sustitución de la medición monetaria de pobreza. La edición del diccionario y la metodología de pobreza no seleccionan otro modelo IIH.

`variables_iih()` devuelve las columnas obligatorias, opcionales y de salida. Se necesita `ID_HOGAR` junto a `PERIODO` para construir una clave estable de cálculo. La función acepta una tabla de personas o una lista con `miembros` y, opcionalmente, `vivienda`; también puede suministrar la tabla de vivienda por separado. El texto adicional de pared se completa por período y vivienda cuando falta en personas.

El cálculo normaliza respuestas, construye indicadores de personas, agrega hogares y aplica los modelos oficial y de focalización. La base escolar de secundaria del IIH se detecta por año a partir de los grados observados, conforme a su implementación de R; es un contrato propio del modelo, distinto de la opción configurable de `anos_educacion()`.

Por defecto los resultados se añaden a cada persona. `return_households=TRUE` devuelve una fila por hogar. `include_details=TRUE` añade componentes, predictores y diagnósticos. `IIH` toma 1, 2 o 3 para las categorías del modelo y 9 cuando no puede clasificarse. `hconmissing` identifica las carencias educativas especificadas por el modelo; otros códigos no representados en sus coeficientes también pueden producir una clasificación 9.

`filter_valid_households=TRUE` filtra hogares según `hconmissing == 0`. En la salida por persona se preservan las filas originales y se dejan ausentes las puntuaciones de hogares filtrados. Recalcular actualiza las puntuaciones antiguas. Para analizar por hogar no sume los ingresos estimados repetidos en todas las personas.

Python reproduce las 77 columnas de resultado y diagnóstico de R para los 26,697 hogares de referencia 2019. Es una validación de equivalencia entre implementaciones, no una reestimación ni una certificación externa de los coeficientes. Conserve el modelo y sus tablas al reproducir resultados.
""",
"en": """IIH estimates a classification from person and dwelling characteristics using fixed coefficient models. It is not observed survey income and does not replace monetary poverty measurement. Dictionary editions and poverty methodology do not select another IIH model.

`variables_iih()` returns required, optional and output columns. `ID_HOGAR` and `PERIODO` form a stable calculation key. The function accepts a person table or a mapping/list with `miembros` and optional `vivienda`; a dwelling table can also be supplied separately. Additional wall text is filled by period and dwelling when absent in person records.

The calculation normalizes responses, builds person indicators, aggregates households and applies official and targeting models. IIH detects the secondary-school base within each year using observed grades, following its R implementation; this model-specific contract differs from the configurable `anos_educacion()` option.

Results are attached to people by default. `return_households=True` returns one row per household. `include_details=True` adds components, predictors and diagnostics. `IIH` is 1, 2 or 3 for model categories and 9 when unclassified. `hconmissing` flags the educational deficiencies specified by the model; other codes absent from coefficient maps can also lead to category 9.

`filter_valid_households=True` filters households using `hconmissing == 0`. Person output preserves original rows and leaves scores missing for filtered households. Recalculation updates previous scores. Do not sum estimated household income repeated across people when analyzing households.

Python reproduces all 77 result and diagnostic columns from R for the 26,697 reference households in 2019. This validates implementation equivalence; it is neither model re-estimation nor external coefficient certification. Preserve the model and its tables for reproducibility.
""",
"r": """library(encftr)
x <- jsonlite::fromJSON(system.file("examples", "synthetic-members.json", package = "encftr"))
households <- ftc_iih(x, return_households = TRUE, include_details = TRUE)
stopifnot(nrow(households) == 4)
households[c("PERIODO", "ID_HOGAR", "IIH", "hconmissing")]""",
"py": """import json
import pandas as pd
from importlib.resources import files
from endompy import encftr as encft
x = pd.DataFrame(json.loads(files("endompy.encftr").joinpath("resources/synthetic-members.json").read_text()))
households = encft.iih(x, return_households=True, include_details=True)
assert len(households) == 4
print(households[["PERIODO", "ID_HOGAR", "IIH", "hconmissing"]])"""
}}

GUIDES["icv-siuben"] = {
"title": ("ICV SIUBEN y migración desde encftr0", "ICV SIUBEN and migration from encftr0"),
"es": """Desde encftr 0.10.0, el cálculo del ICV de encftr0 0.0.2.9002 se mantiene en `ftc_icv_siuben()`. endompy 0.3.0 ofrece `icv_siuben()` y métodos equivalentes de `EncftDataFrame`. La implementación conserva coeficientes, límites y precedencia de las recodificaciones históricas. El identificador `encftr0-0.0.2.9002` identifica ese código; no certifica una metodología vigente de SIUBEN ni selecciona ICV4. ICV, IIH y pobreza monetaria son cálculos diferentes.

Entregue una tabla de personas con hogares completos. `variables_icv_siuben()` enumera las 32 columnas obligatorias y las opcionales. Use códigos numéricos enteros o ausentes, antes de convertirlos a etiquetas. `TRIMESTRE` admite AAAAT o 1–4 con `ANO`. Las claves, la ubicación y `PARENTESCO` deben estar completos; cada hogar y período necesita exactamente una jefatura (`PARENTESCO == 1`). La ubicación debe ser consistente dentro del hogar. No se agrupan hogares de años distintos. El hacinamiento conserva su agrupación histórica por vivienda y período, incluyendo todos los hogares de esa vivienda.

La salida conserva las filas, el orden y las columnas originales, y agrega `icv_global` (clases 1–4), `icv_puntaje` e `icv_metodo`. Los 19 componentes se incluyen por defecto; `include_details = FALSE` omite componentes nuevos y actualiza los que ya existían. Recalcular no reutiliza resultados intermedios. No sume puntajes repetidos en las filas de las personas para obtener un total del hogar.

Se conserva el tratamiento histórico de ausentes: catorce componentes ausentes contribuyen cero antes de sumar; provincia, basura o tipo de vivienda pueden dejar sin clasificación casos urbanos o rurales. El componente históricamente llamado menores de cinco conserva `EDAD <= 5`. Si falta la columna opcional de texto de pared, se usa texto vacío. Estas decisiones reproducen el programa de origen y no reinterpretan su metodología.

`ftc_dict_icv_siuben()`/`dict_icv_siuben()` contiene las etiquetas del resultado. Se aplica por separado del cuestionario inmutable `baseline-1`, cuya versión y contenido no cambian. `set_labels_icv_siuben()` conserva códigos; `use_labels_icv_siuben()` convierte la categoría a factor en R o categoría de pandas en Python. `vars` limita las columnas seleccionadas.

Para migrar R, sustituya `encftr0::ftc0_compute_icv_siuben(x)` por `encftr::ftc_icv_siuben(x)`. Los cuatro nombres `ftc0_*` permanecen en encftr con advertencia de deprecación; encftr0 0.1.0 es un paquete de compatibilidad que delega en encftr >= 0.10.0. Instale primero encftr. `ftc0_setLabels()` etiqueta cuestionario e ICV; `ftc0_setLabels_icv_global()` etiqueta solo la clase; `ftc0_useLabels()` convierte las columnas seleccionadas. Ahora se respeta `vars`, corrigiendo el comportamiento anterior. Python también incluye estos cuatro alias para scripts pareados.

La referencia independiente contiene 108 personas sintéticas, 36 hogares, tres zonas geográficas y las cuatro clases. Los 20 resultados históricos se comparan con la ejecución preservada de encftr0. Pruebas adicionales cubren años repetidos, viviendas con varios hogares, ausentes, jefaturas inválidas, recálculo y etiquetas. Esta evidencia verifica equivalencia del software; no valida una población real ni actualiza coeficientes.
""",
"en": """Starting with encftr 0.10.0, the ICV calculation from encftr0 0.0.2.9002 is maintained in `ftc_icv_siuben()`. endompy 0.3.0 provides `icv_siuben()` and equivalent `EncftDataFrame` methods. Coefficients, thresholds and historical recode precedence are preserved. The identifier `encftr0-0.0.2.9002` identifies that code; it neither certifies the currently adopted SIUBEN methodology nor selects ICV4. ICV, IIH and monetary poverty are separate calculations.

Supply a person table with complete households. `variables_icv_siuben()` lists the 32 required columns and optional inputs. Use integer numeric codes or missing values before converting codes to display labels. `TRIMESTRE` accepts YYYYQ, or 1–4 together with `ANO`. Keys, location and `PARENTESCO` must be complete; each household-period requires exactly one head (`PARENTESCO == 1`). Location must be consistent within a household. Households from different years are not combined. Crowding retains its original dwelling-period aggregation across all households in the dwelling.

Output preserves input rows, order and original columns, and adds `icv_global` (classes 1–4), `icv_puntaje` and `icv_metodo`. The 19 components are included by default; `include_details = FALSE` omits new components and refreshes any already present. Recalculation never reuses intermediate results. Do not sum household scores repeated across person rows.

Historical missing-value treatment is preserved: fourteen missing components contribute zero before summation; province, waste disposal or dwelling type can leave urban/rural cases unclassified. The component historically called under-five retains `EDAD <= 5`. An absent optional wall-text column is treated as empty text. These choices reproduce the original program without reinterpreting its methodology.

`ftc_dict_icv_siuben()`/`dict_icv_siuben()` contains result labels. It is applied separately from the immutable `baseline-1` questionnaire, whose edition and content remain unchanged. `set_labels_icv_siuben()` preserves codes; `use_labels_icv_siuben()` converts classification to an R factor or pandas category. `vars` restricts selected columns.

To migrate R, replace `encftr0::ftc0_compute_icv_siuben(x)` with `encftr::ftc_icv_siuben(x)`. All four `ftc0_*` names remain in encftr with a deprecation warning; encftr0 0.1.0 is a compatibility package delegating to encftr >= 0.10.0. Install encftr first. `ftc0_setLabels()` labels questionnaire and ICV; `ftc0_setLabels_icv_global()` labels classification only; `ftc0_useLabels()` converts selected columns. The `vars` selection is now respected, correcting previous behavior. Python includes the same four aliases for paired scripts.

The independent reference contains 108 synthetic people, 36 households, three geographic zones and all four classes. The 20 historical outputs are compared against the preserved encftr0 execution. Additional tests cover repeated years, dwellings with multiple households, missing data, invalid heads, recalculation and labeling. This evidence verifies software equivalence; it neither validates a real population nor updates coefficients.
""",
"r": """library(encftr)
x <- jsonlite::fromJSON(system.file("examples", "synthetic-icv.json", package = "encftr"))
y <- ftc_icv_siuben(x, include_details = FALSE)
stopifnot(nrow(y) == 108, setequal(y$icv_global, 1:4))
labelled <- ftc_use_labels_icv_siuben(y, vars = "icv_global")
head(labelled[c("icv_global", "icv_puntaje", "icv_metodo")])""",
"py": """import json
import pandas as pd
from importlib.resources import files
from endompy import encftr as encft
x = pd.DataFrame(json.loads(files("endompy.encftr").joinpath("resources/synthetic-icv.json").read_text()))
y = encft.icv_siuben(x, include_details=False)
assert len(y) == 108 and set(y.icv_global) == {1, 2, 3, 4}
labelled = encft.use_labels_icv_siuben(y, vars=["icv_global"])
print(labelled[["icv_global", "icv_puntaje", "icv_metodo"]].head())"""
}

SOURCES = """
## Fuentes / Sources

- [Código oficial de pobreza / Official poverty code](https://mepyd.gob.do/vaes/codigos-de-pobreza/).
- [Metodología oficial 2022 / Official 2022 methodology](https://one.gob.do/media/skoevafz/nueva-metodologia-de-medicion-oficial-de-pobreza-monetaria-2023.pdf).
"""

for lang in ("es", "en"):
    rdir = R / "vignettes" if lang == "es" else EN / "vignettes"
    pdir = P / "docs" / lang
    rdir.mkdir(parents=True, exist_ok=True); pdir.mkdir(parents=True, exist_ok=True)
    for name, spec in GUIDES.items():
        title = spec["title"][lang == "en"]
        body = spec[lang]
        sources = SOURCES if name == "pobreza-monetaria" else ""
        rmd = f'''---
title: "{title}"
output: rmarkdown::html_vignette
vignette: >
  %\\VignetteIndexEntry{{{title}}}
  %\\VignetteEngine{{knitr::rmarkdown}}
  %\\VignetteEncoding{{UTF-8}}
---

{body}
```{{r}}
{spec['r']}
```
{sources}
'''
        (rdir / (name + ".Rmd")).write_text(rmd.rstrip() + "\n", encoding="utf-8")
        (pdir / (name + ".md")).write_text(f"# {title}\n\n{body}\n```python\n{spec['py']}\n```\n{sources}", encoding="utf-8")
    welcome = ("Herramientas para analizar la ENCFT con resultados reproducibles.", "Tools for reproducible ENCFT analysis.")[lang == "en"]
    links = "\n".join(f"- [{g['title'][lang == 'en']}]({name}.md)" for name, g in GUIDES.items())
    (pdir / "index.md").write_text(f"# endompy 0.3.0\n\n{welcome}\n\n{links}\n\n- [API](reference/index.md)\n- [{'Despliegue' if lang == 'es' else 'Deployment'}](deployment.md)\n", encoding="utf-8")
    rread = f"# encftr 0.10.0\n\n{welcome}\n\n" + "\n".join(f"- [{g['title'][lang == 'en']}](articles/{name}.html)" for name, g in GUIDES.items())
    (R / "README.md" if lang == "es" else EN / "README.md").write_text(rread + "\n", encoding="utf-8")
    if lang == "es":
        (R / "README.Rmd").write_text('---\noutput: github_document\n---\n\n' + rread + "\n", encoding="utf-8")
    config = f"""url: https://endomer-py.github.io/encftr/{'en/' if lang == 'en' else ''}
lang: {lang}
template:
  bootstrap: 5
  bootswatch: flatly
navbar:
  structure:
    left: [intro, reference, articles, news]
    right: [search, github]
  components:
    intro:
      text: {'Inicio' if lang == 'es' else 'Home'}
      href: index.html
articles:
  - title: {'Guías' if lang == 'es' else 'Guides'}
    navbar: ~
    contents:
""" + "".join(f"      - {name}\n" for name in GUIDES)
    (R / "_pkgdown.yml" if lang == "es" else EN / "_pkgdown.yml").write_text(config, encoding="utf-8")

(P / "README.md").write_text("""# endompy 0.3.0

Python tools for Dominican surveys. The ENCFT module matches the calculation
contracts of encftr 0.10.0: education, labour, income, household indicators,
monetary poverty 2012/2022, IIH, historical ICV SIUBEN and versioned dictionaries.

Requires Python >= 3.9, pandas >= 1.5, numpy >= 1.21 and labelerpy >= 0.2.1.
R is not required at runtime. Install the supplied labelerpy wheel before the
endompy wheel when these versions are not yet available in a package index.

```python
from endompy import EncftDataFrame
x = EncftDataFrame({"FACTOR_EXPANSION": [100, 200]})
result = x.factor_expansion_anual(periods=4)
assert result["factor_expansion_anual"].tolist() == [25, 50]
```

Read the paired [Spanish](docs/es/encftr.md) and [English](docs/en/encftr.md)
guides, [dictionary editions](docs/en/diccionario.md),
[methodology contracts](docs/en/pobreza-monetaria.md) and
[deployment guide](DEPLOYMENT.md).
""", encoding="utf-8")
print(f"Authored {len(GUIDES)} paired R/Python guides.")
