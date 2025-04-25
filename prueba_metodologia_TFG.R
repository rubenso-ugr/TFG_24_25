library(RandomFieldsUtils)
library(RandomFields)

mis.colores <- colorRampPalette(c("white", "blue", "lightgreen", "yellow", "red"))

N1 <- 200  # filas
N2 <- 200  # columnas
n <- 150   # número de realizaciones
alpha <- 0.1 # percentil
rea_to_show <- 1 #Realización a mostrar

# This is an arrange for when spatiotemporal random fields are simulated. There is no temporal horizon here.
T<- c(1,1,1)

# Modelo de Simulación
modelo1 <- RMgencauchy(alpha=2,beta=0.1,var=0.1,scale=1) #Cauchy
modelo2 <- RMnsst(phi=RMgencauchy(alpha=2,beta=0.1,var=0.1, scale=1),psi=RMstable(alpha=1.9,var=0.1,scale=1), delta=2) #Gneiting

# Definición de la malla de trabajo
x<-0:(N1-1)
y<-0:(N2-1)

modelo_elegido <- tolower(readline(prompt = "¿Que modelo quieres usar?(Por defecto Cauchy):\n1: Cauchy\n2: Gneiting"))

if (modelo_elegido == "2") {
  sim<-RFsimulate(model=modelo2, x=x, y=y, T=T, n=n)
} else {
  sim<-RFsimulate(model=modelo1, x=x, y=y, T=T, n=n)
}

#Recolección de todos los valores de todas las simulaciones realizadas
sim_values <- matrix(unlist(sim@data), nrow = N1 * N2, ncol = n)

# Obtenemos los mínimos de la realización escogida y el mínimo global. También la diferencia entre ambos
min_val_rea1 <- min(sim_values[, rea_to_show])
min_val <- min(sim_values)
diferencia <- min_val - min_val_rea1

#Obtención del máximo de la realización escogida
max_val_rea1 <- max(sim_values[, rea_to_show])

#Obtenemos los valores de la realización escogida haciendo que 0 sea el mínimo de todos
sim_values_pos_rea1 <- sim_values[, rea_to_show] - min_val_rea1

#Sumamos a todos los valores el mínimo absoluto para que el 0 sea el menor valor global
sim_values_pos <- sim_values - min_val  # ahora el mínimo de la primera realización es 0


#Tomamos la realización escogida
realization_matrix <- matrix(sim_values_pos[, rea_to_show], nrow = N1, ncol = N2)

# Dibujamos la realización escogida
filled.contour(x, y, realization_matrix,
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("Realización", rea_to_show),
               xlim =  c(0, N1),
               ylim =  c(0, N2))


#Guardamos el valor máximo global y de la primera realización.
max_val <- max(sim_values_pos)
max_val_rea1 <- max(sim_values_pos_rea1)


#Estimamos el umbral de riesgo (percentil 1 - alpha)
all_vals <- as.vector(sim_values_pos)
threshold <- quantile(all_vals, probs = 1 - alpha)

#Obtenemos el conjunto de excursión (asignamos 0 a valores por debajo del umbral en TODAS las realizaciones)
excursion_values <- sim_values_pos
excursion_values[sim_values_pos < threshold] <- 0

#Tomamos el conjunto de excursión de la realización escogida
excursion_matrix <- matrix(excursion_values[, rea_to_show], nrow = N1, ncol = N2)

# Dibujamos el conjunto de excursión de la realización escogida
filled.contour(x, y, excursion_matrix,
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("Conjunto de Excursión (Realización", rea_to_show, ")"),
               xlim =  c(0, N1),
               ylim =  c(0, N2),
               zlim = c(min_val_rea1 - min_val, max_val_rea1 - diferencia))

#----------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

# Definimos los parámetros para ventanas deslizantes
window_size <- 5        # tamaño de ventana 
overlap <- 1            # solapamiento entre ventanas 
step <- window_size - overlap
alpha2 <- 0.05  #Segundo percentil a usar

# Creamos una copia exacta de sim_values_pos para cada medida y metodologia a usar
refined_values_hist <- excursion_values  # VaR histórico
refined_values_param <- excursion_values # VaR paramétrico
refined_values_mc <- excursion_values    # VaR Monte Carlo
refined_values_es <- excursion_values    # Expected Shortfall

# Recorremos la malla con ventanas deslizantes
for (i in seq(1, N1, by = step)) {
  for (j in seq(1, N2, by = step)) {
    
    # Ajustamos dinámicamente los tamaños de la ventana en i y j para poder recorrer todos los pixeles
    actual_window_size_i <- min(window_size, N1 - i + 1)
    actual_window_size_j <- min(window_size, N2 - j + 1)
    
    # Índices espaciales de la ventana
    indices_i <- i:(i + actual_window_size_i - 1)
    indices_j <- j:(j + actual_window_size_j - 1)
    
    # Creamos la cuadrícula completa de coordenadas
    grid <- expand.grid(i = indices_i, j = indices_j)
    
    # Convertimos a índices lineales
    linear_indices <- (grid$i - 1) * N2 + grid$j
    
    # Extraemos los datos de la ventana (todas las realizaciones)
    window_data <- excursion_values[linear_indices, , drop = FALSE]
    
    mask_above_global <- window_data > 0
    
    if (any(mask_above_global)) {   # Comprobamos que haya algun valor que supere el umbral
      
      filtered_data <- window_data[mask_above_global] #Tomamos los valores de la ventana que han superado el primer umbral
      
      # VaR Histórico
      var_hist <- quantile(filtered_data, probs = 1 - alpha2) 
      #refined_values_hist[linear_indices, ][mask_above_global] <- var_hist
      
            # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
      tmp <- refined_values_hist[linear_indices, , drop = FALSE]
      tmp[mask_above_global] <- var_hist
      refined_values_hist[linear_indices, ] <- tmp
      
      # VaR Paramétrico
      mu <- mean(filtered_data)     # media
      sigma <- sd(filtered_data)    # desv. tipica
      z_alpha <- qnorm(1 - alpha2)  # cuantil de distrib. normal estandar a nivel de confianza 1- alpha2
      var_param <- mu + sigma * z_alpha
      #refined_values_param[linear_indices, ][mask_above_global] <- var_param
      
      
            # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
      tmp_param <- refined_values_param[linear_indices, , drop = FALSE]
      tmp_param[mask_above_global] <- var_param
      refined_values_param[linear_indices, ] <- tmp_param
      
      
      
      # VaR Monte Carlo (simulación normal simple con misma media y sigma)
      sim_mc <- rnorm(10000, mean = mu, sd = sigma)   # conjunto de simulaciones
      var_mc <- quantile(sim_mc, probs = 1 - alpha2)
      #refined_values_mc[linear_indices, ][mask_above_global] <- var_mc
      
            # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
      tmp_mc <- refined_values_mc[linear_indices, , drop = FALSE]
      tmp_mc[mask_above_global] <- var_mc
      refined_values_mc[linear_indices, ] <- tmp_mc
      
      
      # Expected Shortfall histórico
      es_val <- mean(filtered_data[filtered_data >= var_hist])
      #refined_values_es[linear_indices, ][mask_above_global] <- es_val
      
            # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
      tmp_es <- refined_values_es[linear_indices, , drop = FALSE]
      tmp_es[mask_above_global] <- es_val
      refined_values_es[linear_indices, ] <- tmp_es
      
      
    }
  }
}

# Creamos matrices 2D de cada resultado para una realización
matrix_VaR_hist <- matrix(refined_values_hist[, rea_to_show], nrow = N1, ncol = N2)
matrix_VaR_param <- matrix(refined_values_param[, rea_to_show], nrow = N1, ncol = N2)
matrix_VaR_mc <- matrix(refined_values_mc[, rea_to_show], nrow = N1, ncol = N2)
matrix_ES <- matrix(refined_values_es[, rea_to_show], nrow = N1, ncol = N2)

#Obtenemos el maximo de entre todas las simulaciones para que la comparación de colores sea correcta
max_z <- max(
  refined_values_hist,
  refined_values_param,
  refined_values_mc,
  refined_values_es,
  na.rm = TRUE  # por si acaso hay NA
)


# Dibujamos el conjunto de excursión de la realización escogida después de aplicar la metodología con diferentes medidas

filled.contour(x, y, matrix_VaR_hist,
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("VaR Histórico 95% - Realización", rea_to_show),
               xlim = c(0, N1),
               ylim = c(0, N2),
               zlim = c(0, max_z))


filled.contour(x, y, matrix_VaR_param,
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("VaR Paramétrico 95% - Realización", rea_to_show),
               xlim = c(0, N1),
               ylim = c(0, N2),
               zlim = c(0, max_z))

 filled.contour(x, y, matrix_VaR_mc,
                color.palette = mis.colores,
                asp = 1,
                axes = TRUE,
                frame.plot = 0,
                main = paste("VaR Monte Carlo 95% - Realización", rea_to_show),
                xlim = c(0, N1),
                ylim = c(0, N2),
                zlim = c(0, max_z))
 
 filled.contour(x, y, matrix_ES,
                color.palette = mis.colores,
                asp = 1,
                axes = TRUE,
                frame.plot = 0,
                main = paste("Expected Shortfall 95% - Realización", rea_to_show),
                xlim = c(0, N1),
                ylim = c(0, N2),
                zlim = c(0, max_z))
 
 
 
 
 #----------------------------------------------------------------------------------------------------------
 #-------------------Primer intento erroneo ---------------------------------------------------
 #----------------------------------------------------------------------------------------------------------
 #Definimos los parámetros para ventanas deslizantes
 window_size <- 50        # tamaño de ventana 
 overlap <- 10           # solapamiento entre ventanas 
 step <- window_size - overlap  # paso según el solapamiento
 alpha2 <- 0.05
 
 # Preguntamos si se quiere un umbral propio
 use_custom_alpha <- tolower(readline(prompt = "¿Quieres usar un umbral propio? (s/n): "))
 
 if (use_custom_alpha == "s") {
   custom_alpha_input <- readline(prompt = "Introduce el valor del umbral (por ejemplo, 0.6): ")
   custome_threshold <- as.numeric(custom_alpha_input)
 } else {
   cat("Usando percentil por defecto:", (1 - alpha2)*100, "%\n")
 }
 
 
 # Creamos una copia exacta de sim_values_pos
 refined_values <- excursion_values
 
 # Recorremos la malla con ventanas deslizantes
 for (i in seq(1, N1, by = step)) {
   for (j in seq(1, N2, by = step)) {
     
     # Ajustamos dinámicamente los tamaños de la ventana en i y j
     actual_window_size_i <- min(window_size, N1 - i + 1)
     actual_window_size_j <- min(window_size, N2 - j + 1)
     
     # Índices espaciales de la ventana
     indices_i <- i:(i + actual_window_size_i - 1)
     indices_j <- j:(j + actual_window_size_j - 1)
     
     # Creamos la cuadrícula completa de coordenadas (producto cartesiano)
     grid <- expand.grid(i = indices_i, j = indices_j)
     
     # Convertimos a índices lineales
     linear_indices <- (grid$i - 1) * N2 + grid$j
     
     # Extraemos los datos de la ventana (todas las realizaciones)
     window_data <- excursion_values[linear_indices, , drop = FALSE]
     
     if (use_custom_alpha != "s") {
       
       mask_above_global <- window_data > 0
       
       if (any(mask_above_global)) {
         
         filtered_data <- window_data[mask_above_global]
         
         local_threshold <- quantile(filtered_data, probs = 1 - alpha2)
         
         # Creamos la máscara lógica completa
         mask_below_threshold <- window_data < local_threshold
         
         # Asignamos 0 a los valores por debajo del umbral directamente en refined_values
         temp_window_data <- window_data
         temp_window_data[mask_below_threshold] <- 0
         
         # Guardamos en la matriz global
         refined_values[linear_indices, ] <- temp_window_data
         
       }
       
     }else{
       
       # Creamos la máscara lógica sobre los valores inferiores al umbral personalizado
       mask_below_custom_threshold <- window_data < custome_threshold
       
       # Creamos una copia temporal de la ventana para modificar
       temp_window_data <- window_data
       temp_window_data[mask_below_custom_threshold] <- 0
       
       # Guardamos la ventana modificada en la matriz global
       refined_values[linear_indices, ] <- temp_window_data
     }
     
     
   }
 }
 
 
 #Tomamos el conjunto de excursión de la primera realización
 realization_matrix2 <- matrix(refined_values[, rea_to_show], nrow = N1, ncol = N2)
 
 # Dibujamos el conjunto de excursión de la primera realización
 filled.contour(x, y, realization_matrix2,
                color.palette = mis.colores,
                asp = 1,
                axes = TRUE,
                frame.plot = 0,
                main = paste("Conjunto de Excursión Segundo (Realización", rea_to_show, ")"),
                xlim =  c(0, N1),
                ylim =  c(0, N2),
                zlim = c(min_val_rea1 - min_val, max_val_rea1 - diferencia))
 
