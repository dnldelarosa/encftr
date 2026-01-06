# El diccionario de las variables originales y las nuevas creadas en el paquete
# se mantienen en data-raw/dict.json

# Cargar diccionario desde JSON usando la nueva API
dict <- labeler::from_json("data-raw/dict.json")

usethis::use_data(dict, overwrite = TRUE)
