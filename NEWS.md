# encftr 0.10.0

- Integra el ICV SIUBEN histórico de encftr0 0.0.2.9002, conservando coeficientes
  y resultados de referencia. Añade puntaje, identificador de método y contrato
  explícito de entradas; agrupación por año y validación de jefaturas.
- Conserva los cuatro nombres ftc0_* con deprecación. encftr0 0.1.0 delega aquí;
  la selección de variables de etiquetado ahora se respeta.
- Etiquetas del ICV separadas de la edición inmutable baseline-1.
- Guía bilingüe de cálculo y migración y ejemplo de 108 personas sintéticas.
- Paridad con endompy 0.3.0. No modifica ni certifica la metodología vigente de SIUBEN.

# encftr 0.9.0

- Diccionario ENCFT `baseline-1`: 636 definiciones verificadas. Registro de
  ediciones inmutables, selección exacta o por vigencia documentada y reutilización
  de variables que no cambian entre ediciones.
- Factores anual y semestral calculados con los trimestres observados por año.
  Los extractos sin períodos necesitan `periods = 4` o `periods = 2` explícito.
- Pobreza 2012 y 2022 con entradas explícitas, ingreso nominal y deflactado,
  conservación de casos sin clasificación y cálculos repetibles.
- Corrección de la conversión de moneda de los componentes históricos: se usa
  el calendario de tasas, evitando depender de la moneda de otra persona.
- Correcciones en alias educativos, ajuste de verano del trabajo infantil,
  agrupación por año y hogares sin jefatura identificada.
- IIH disponible con resultados por persona o por hogar y diagnóstico del modelo.
  Recalcular actualiza resultados anteriores.
- Documentación completa en español e inglés y ejemplos sintéticos compartidos
  con endompy 0.2.0.

Los cálculos históricos pueden cambiar por las correcciones anteriores. Registre
la versión del paquete, edición del diccionario, metodología y cobertura temporal
al reproducir un informe. Las tablas empaquetadas tienen cobertura finita; una
edición del cuestionario no actualiza las líneas de pobreza ni el modelo IIH.
