### SISTEMA DIFUSO PARA RECOMENDAR GRASS SINTETICO ###

#### grass sintetico####

# carga el paquete
library(sets)

# Configurar el universo
sets_options("universe", seq(1, 50, 0.1))

# Definimos las variables del sistema difuso

variables <- set(
  
  techo = fuzzy_partition(varnames = c(no.techado= 15, techado = 35),
                          sd = 5.0),
  
  distancia = fuzzy_partition(varnames = c(cerca =10 , mediana= 25, lejos= 40), 
                             sd = 3.0),
  
  tamaño = fuzzy_partition(varnames = c(pequeño = 10,mediano = 30, grande = 45), 
                           sd = 5.5),
  
  precio = fuzzy_partition(varnames = c(barato = 15, caro = 40), 
                           sd = 5.5),
  
  disponibilidad = fuzzy_partition(varnames = c( ocupado= 15, disponible = 35), 
                                   sd = 7),
  
  estado = fuzzy_partition(varnames = c(malo = 10, regular = 25, excelente =40 ),
                           sd = 4.5),
  
  grass= fuzzy_partition(varnames = c(no.recomendable = 8, pocorecomendable = 24, recomendable = 40),
                      FUN = fuzzy_cone, radius = 10)
  
)

# Definimos las reglas difusas del sistema
reglas <- set(
  fuzzy_rule( disponibilidad %is% ocupado||estado %is% malo, grass  %is% no.recomendable),
  
  fuzzy_rule( disponibilidad %is% disponible && tamaño %is% pequeño && 
                precio %is% caro && techo %is% no.techado, grass %is% no.recomendable),
  
  fuzzy_rule(disponibilidad %is% disponible && techo %is% no.techado &&
               precio %is% barato && distancia %is% cerca && estado %is% regular &&
               tamaño %is% pequeño, grass %is% no.recomendable),
  
  fuzzy_rule(disponibilidad %is% disponible && techo %is% techado &&
               precio %is% barato && distancia %is% cerca && estado %is% regular &&
               tamaño %is% pequeño, grass %is% recomendable),
  
 fuzzy_rule(disponibilidad %is% disponible && techo %is% techado &&
              precio %is% caro && distancia %is% cerca && estado %is% regular &&
             tamaño %is% pequeño, grass %is% pocorecomendable),
  
  fuzzy_rule(disponibilidad %is% disponible && techo %is% no.techado &&
               precio %is% barato && distancia %is% cerca && 
               estado %is% regular && tamaño %is% mediano,
             grass %is% recomendable),
  
  fuzzy_rule(disponibilidad %is% disponible && techo %is% no.techado &&
               precio %is% barato && distancia %is% lejos && estado %is% regular &&
               tamaño %is% mediano, grass %is% pocorecomendable),
  
  fuzzy_rule(disponibilidad %is% disponible && techo %is% no.techado &&
              precio %is% caro && distancia %is% lejos && estado %is% regular &&
              tamaño %is% mediano, grass %is% pocorecomendable),
  
  fuzzy_rule(disponibilidad %is% disponible && techo %is% techado &&
               precio %is% barato && distancia %is% lejos && estado %is% regular &&
               tamaño %is% mediano, grass %is% recomendable),
  
  fuzzy_rule(disponibilidad %is% disponible && techo %is% techado &&
               precio %is% caro && distancia %is% lejos && estado %is% regular &&
               tamaño %is% mediano, grass %is% recomendable),
  
  fuzzy_rule(disponibilidad %is% disponible && techo %is% techado &&
              precio %is% caro && distancia %is% cerca && estado %is% regular &&
              tamaño %is% mediano, grass %is% recomendable),
  
  fuzzy_rule(disponibilidad %is% disponible && techo %is% techado &&
               precio %is% caro && distancia %is% mediana && estado %is% regular &&
               tamaño %is% mediano, grass %is% pocorecomendable)

  
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

# fuzzy_inference: procesa la salida en función de las entradas





ejemplo1 <- fuzzy_inference(modelo, list(techo = 33, precio = 40, distancia= 25, estado=35,
                                         tamaño=40, disponibilidad = 35))
plot(ejemplo1)
ejemplo2 <- fuzzy_inference(modelo, list(techo =35 , precio = 35, distancia= 20, estado=20,
                                         tamaño=25, disponibilidad = 35))
plot(ejemplo2)
ejemplo3 <- fuzzy_inference(modelo, list(techo = 13, precio = 45, distancia= 16, estado=23,
                                         tamaño=30, disponibilidad = 10))
plot(ejemplo3)

ejemplo4 <- fuzzy_inference(modelo, list(techo = 15, precio = 15, distancia= 45, estado=25,
                                         tamaño= 30, disponibilidad = 35))
plot(ejemplo4)


