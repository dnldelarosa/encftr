# encftr 0.10.0

- Integrates historical ICV SIUBEN from encftr0 0.0.2.9002, preserving coefficients
  and reference outputs. Adds score, method identifier, explicit inputs,
  grouping by year and household-head validation.
- Retains four deprecated ftc0_* names. encftr0 0.1.0 delegates here; labeling
  now respects selected variables.
- Separate ICV labels preserve the immutable baseline-1 questionnaire.
- Bilingual calculation and migration guide with 108 synthetic person records.
- Parity with endompy 0.3.0. Does not update or certify current SIUBEN methodology.

# encftr 0.9.0

- ENCFT functional parity with encftr 0.9.0: education, labour, income components,
  household indicators, poverty 2012/2022 and the household income index (IIH).
- R functions paired with endompy 0.2.0 Python functions.
- Verified dictionary revisions through labelerpy >= 0.2.1; unchanged definitions
  are reused across editions. The bundled baseline has no asserted historic dates.
- Annual and semester weights use observed coverage. Calls without quarter fields
  must now declare periods=4 or periods=2 explicitly.
- Bundled rules, reference tables and synthetic examples; no R runtime required.
- Bilingual documentation and reproducible package/site validation scripts.

This release changes some calculations to correct defects in legacy helpers.
Persist package version, dictionary revision, poverty methodology and input coverage
with results. Model-based IIH and observed monetary poverty are separate measures.
