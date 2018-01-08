### LABORATORIO N°4 ###
### LÓGICA DIFUSA EN R PARTE 1 ###

#### EL CLIMA ####

# carga el paquete
library(sets)

# Configurar el universo
sets_options("universe", seq(1, 50, 0.1))

# Definimos las variables del sistema difuso

variables <- set(
  
  techo = fuzzy_partition(varnames = c(no.techado= 15, techado = 35), sd = 5.0),
  
  distancia = fuzzy_partition(varnames = c(cerca =10 , mediana= 25, lejano= 40), 
                             sd = 3.0),
  tamaño = fuzzy_partition(varnames = c(pequeño = 10,mediano = 30, grande = 45), sd = 5.5),
  
  precio = fuzzy_partition(varnames = c(barato = 15, caro = 40), sd = 5.5),
  
  disponibilidad = fuzzy_partition(varnames = c( ocupado= 15, disponible = 35), sd = 7),
  
  estado = fuzzy_partition(varnames = c(malo = 10, regular = 25, excelente =40 ), sd = 4.5),
  
  grass= fuzzy_partition(varnames = c(no.recomendable = 8, pocorecomendable = 24, recomendable = 40),
                      FUN = fuzzy_cone, radius = 10)
  
)

# Definimos las reglas difusas del sistema
reglas <- set(
  fuzzy_rule( disponibilidad %is% ocupado, grass  %is% no.recomendable),
  fuzzy_rule( estado %is% malo, grass  %is% no.recomendable),
  fuzzy_rule(disponibilidad %is% disponible && techo %is% no.techado &&
               precio %is% barato && distancia %is% cerca && estado %is% regular &&
               tamaño %is% pequeño, grass %is% no.recomendable),
  fuzzy_rule(disponibilidad %is% disponible && techo %is% no.techado &&
               precio %is% barato && distancia %is% cerca && estado %is% regular &&
               tamaño %is% pequeño, grass %is% no.recomendable),
  fuzzy_rule(disponibilidad %is% disponible && techo %is% no.techado &&
               precio %is% barato && distancia %is% cerca && estado %is% regular &&
               tamaño %is% pequeño, grass %is% no.recomendable),
  fuzzy_rule(disponibilidad %is% disponible && techo %is% no.techado &&
               precio %is% barato && distancia %is% cerca && estado %is% regular &&
               tamaño %is% pequeño, grass %is% no.recomendable),
  fuzzy_rule(disponibilidad %is% disponible && techo %is% no.techado &&
               precio %is% barato && distancia %is% cerca && estado %is% regular &&
               tamaño %is% pequeño, grass %is% no.recomendable),
  fuzzy_rule(temperatura %is% frio, clima %is% malo),
  fuzzy_rule(temperatura %is% aceptable || humedad %is% aceptable ||
               precipitacion %is% poca.lluvia, clima %is% aceptable),
  fuzzy_rule(temperatura %is% caliente && precipitacion %is% poca.lluvia,
             clima %is% aceptable),
  fuzzy_rule(temperatura %is% caliente && humedad %is% seco &&
               precipitacion %is% poca.lluvia, clima %is% aceptable)
  
)

# Ahora, construyamos el sistema (el modelo)
# Convierte las entradas a valores difusos
modelo <- fuzzy_system(variables, reglas)

# Mostramos las variables y las reglas del sistema
print(modelo)

# Mostramos el gráfico (trama) del sistema
plot(modelo)

## Probamos el sistema difuso ##
# Estos son algunos ejemplos para probar el sistema
# Ejemplo N°01
# Temperatura = 75, humedad = 0, y precipitación = 70
# fuzzy_inference: procesa la salida en función de las entradas

ejemplo.1 <- fuzzy_inference(modelo, list(temperatura = 75, humedad = 0,
                                         precipitacion = 70))

# Ahora, defuzzificamos el ejemplo para transformar los parámetros en un número real
gset_defuzzify(ejemplo.1, "centroid")

# Mostramos el gráfico
plot(ejemplo.1)

# Interpretando: De acuerdo con el sistema, el clima es 0.6 aceptable (ver la gráfica del clima)

# Ejemplo N°02
# Temperatura = 30, humedad = 0 y precipitación = 70
ejemplo.2 <- fuzzy_inference(modelo, list(techo = 30, precio = 0, distancia= 2, estado=34, tamaño=23,
                                         disponibilidad = 10))
plot(ejemplo.2)

# Interpretando: Al bajar la temperatura, el modelo reduce la cantidad de clima "aceptable"
# a alrededor de 0.4 y crea un nuevo "pulso" con maximos globales en 40, lo que significa que
# clima es 1.0 "malo"

# Resetear el universo
sets_options("universe", NULL)
