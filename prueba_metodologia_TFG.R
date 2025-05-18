library(RandomFieldsUtils)
library(RandomFields)
library(plot3D)


library(plotly)
library(magrittr)

mis.colores <- colorRampPalette(c("white", "blue", "lightgreen", "yellow", "red"))

N1 <- 50  # filas
N2 <- 50  # columnas
n <- 30   # número de realizaciones
alpha <- 0.15 # Primer percentil a usar
alpha2 <- 0.1  #Segundo percentil a usar
rea_to_show <- 1 #Realización a mostrar

T<-1:3

# Modelo de Simulación
#modelo1 <- RMgencauchy(alpha=2,beta=0.1,var=0.1,scale=1) #Cauchy
modelo2 <- RMnsst(phi=RMgencauchy(alpha=2,beta=0.1,var=0.1, scale=1),psi=RMstable(alpha=1.9,var=0.1,scale=1), delta=2) #Gneiting

# Definición de la malla de trabajo
x<-0:(N1-1)
y<-0:(N2-1)

#------------------------------Simulción condicionada -------------------------------------------
T<-1:1

model <- RMgencauchy(alpha=2, beta=0.3, scale=0.1, var=0.5)


# Datos de ejemplo
valores <- c(10, 20, 5, 30, 7,21, 32, 5, 28, 8, 10, 20, 5, 30, 7,21, 32, 5, 28, 8)
coord_x <- c(60, 10, 15, 20, 7, 15, 22, 45, 68, 88, 10, 95, 20, 5, 34, 88, 52, 1, 55, 54)
coord_y <- c(10, 95, 20, 5, 34, 88, 52, 1, 55, 54, 60, 10, 15, 20, 7, 15, 22, 45, 68, 88)

# Crear el data frame
data <- data.frame(
  variable1 = valores,
  coords.x1 = coord_y,
  coords.x2 = coord_x,
  coords.x3 = 1
)

# Processing grid for RFsimulate function
giv<-cbind(rep(x,each=length(y)), rep(y,length(x)),  
           rep(T,each=length(x)*length(y)))

sim<-RFsimulate(model=model,x=giv,data=data,n=n)

#------------------------------------------------------------------------------------------------


#Realizamos las simulaciones
sim<-RFsimulate(model=modelo2, x=x, y=y, T=1:4, n=n)

#Recolección de todos los valores de todas las simulaciones realizadas
sim_values <- matrix(unlist(sim@data), nrow = N1 * N2 * 4, ncol = n)

min_val <- min(sim_values)
max_val <- max(sim_values)
max_val_rea <- max(sim_values[, rea_to_show])



n_times <- 4

# Crear una lista para almacenar las matrices por tiempo
realizations_by_time <- vector("list", n_times)

# Extraer cada instante temporal para la realización seleccionada
for (t in 1:n_times) {
  start_row <- (t - 1) * N1 * N2 + 1
  end_row <- t * N1 * N2
  realizations_by_time[[t]] <- matrix(sim_values[start_row:end_row, rea_to_show], nrow = N1, ncol = N2)
}

#Tomamos la realización escogida
#realization_matrix <- matrix(sim_values[, rea_to_show], nrow = N1, ncol = N2)

# Dibujamos la realización escogida
filled.contour(x, y, realizations_by_time[[1]],
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("Instante1 Realización", rea_to_show),
               xlim =  c(0, N1),
               ylim =  c(0, N2),
               zlim = c(min_val, max_val_rea))

# Dibujamos la realización escogida
filled.contour(x, y, realizations_by_time[[2]],
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("Instante2 Realización", rea_to_show),
               xlim =  c(0, N1),
               ylim =  c(0, N2),
               zlim = c(min_val, max_val_rea))

# Dibujamos la realización escogida
filled.contour(x, y, realizations_by_time[[3]],
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("Instante3 Realización", rea_to_show),
               xlim =  c(0, N1),
               ylim =  c(0, N2),
               zlim = c(min_val, max_val_rea))

# Dibujamos la realización escogida
filled.contour(x, y, realizations_by_time[[4]],
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("Instante4 Realización", rea_to_show),
               xlim =  c(0, N1),
               ylim =  c(0, N2),
               zlim = c(min_val, max_val_rea))


n_colors <- 100
plotly_colors <- mis.colores(n_colors)

# Mapear colores a valores entre min_val y max_val
color_scale <- lapply(seq(min_val, max_val_rea, length.out = n_colors), function(val) {
  scaled_val <- (val - min_val) / (max_val_rea - min_val)
  list(scaled_val, plotly_colors[round(scaled_val * (n_colors - 1)) + 1])
})

#Dibujamos la realización escogida en 3D

fig <- plot_ly(z = ~t(realization_matrix)) %>% 
  add_surface(
    colorscale = color_scale,
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "white",
        project = list(z = TRUE)
      )
    )
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "Valor")
    )
  )

#Dibujamos la realización escogida en 3D
fig


# Calculamos la varianza por fila (cada píxel a través de las n simulaciones)
varianzas_vector <- apply(sim_values, 1, var)

# Reestructuramos a una matriz 
mapa_varianzas <- matrix(varianzas_vector, nrow = N1, ncol = N2)

#Dibujamos el mapa de varianas de las simulaciones realizadas
filled.contour(x, y, mapa_varianzas,
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("Mapa de varianzas"),
               xlim = c(0, N1),
               ylim = c(0, N2))


#Estimamos el umbral de riesgo (percentil 1 - alpha)
all_vals <- as.vector(sim_values)
threshold <- quantile(all_vals, probs = 1 - alpha)

#Obtenemos el conjunto de excursión (asignamos 0 a valores por debajo del umbral en TODAS las realizaciones)
excursion_values <- sim_values
excursion_values[sim_values < threshold] <- min_val

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
               zlim = c(min_val, max_val_rea))

#----------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

# Definimos los parámetros para ventanas deslizantes
window_size <- 3        # tamaño de ventana 
overlap <- 1            # solapamiento entre ventanas 
step <- window_size - overlap

# Creamos una matriz para cada medida y metodologia a usar
refined_values_hist <- matrix(NA, nrow = N1, ncol = N2)
refined_values_param <- matrix(NA, nrow = N1, ncol = N2) # VaR paramétrico
refined_values_mc <- matrix(NA, nrow = N1, ncol = N2)    # VaR Monte Carlo
refined_values_es <- matrix(NA, nrow = N1, ncol = N2)    # Expected Shortfall

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
    window_data <- sim_values[linear_indices, , drop = FALSE]
    
    mask_above_global <- window_data >= threshold
    
    if (any(mask_above_global)) {   # Comprobamos que haya algun valor que supere el umbral
      
      filtered_data <- window_data[mask_above_global] #Tomamos los valores de la ventana que han superado el primer umbral
      

      # VaR Histórico
      if (all(is.na(filtered_data)) || length(filtered_data) == 0) { #Comprobamos que hayan valores para poder realizar la estimación, en caso contrario tomamos el  valore del primer umbral
        var_hist <- threshold
      } else {
        var_hist <- quantile(filtered_data, probs = 1 - alpha2, na.rm = TRUE)
      }
      
      refined_values_hist[linear_indices] <- var_hist
      
      # VaR Paramétrico
      if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
        var_param <- threshold
      } else {
        mu <- mean(filtered_data, na.rm = TRUE)
        sigma <- sd(filtered_data, na.rm = TRUE)
        z_alpha <- qnorm(1 - alpha2)
        var_param <- mu + sigma * z_alpha
      }
      
      refined_values_param[linear_indices] <- var_param
      
      # VaR Monte Carlo
      if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
        var_mc <- threshold
      } else {
        sim_mc <- rnorm(10000, mean = mu, sd = sigma)
        var_mc <- quantile(sim_mc, probs = 1 - alpha2, na.rm = TRUE)
      }
      
      refined_values_mc[linear_indices] <- var_mc
      
      # Expected Shortfall Histórico
      if (all(is.na(filtered_data)) || length(filtered_data) == 0) {
        es_val <- threshold
      } else {
        es_val <- mean(filtered_data[filtered_data >= var_hist], na.rm = TRUE)
      }
      
      # Aplicamos el umbral obtenido a los valores de dentro de la ventana de todas las realizaciones que superaron el primer umbral 
      refined_values_es[linear_indices] <- es_val
      
      
    }
  }
}


#Obtenemos el maximo y el mínimo entre todas las simulaciones para que la comparación de colores sea correcta
max_z <- max(
  refined_values_hist,
  refined_values_param,
  refined_values_mc,
  refined_values_es,
  na.rm = TRUE  # por si acaso hay NA
)

min_z <- min(
  refined_values_hist,
  refined_values_param,
  refined_values_mc,
  refined_values_es,
  na.rm = TRUE  # por si acaso hay NA
)


# Dibujamos el mapa resultante tras aplicar la metodología con diferentes medidas

filled.contour(x, y, refined_values_hist,
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("VaR Histórico 95%"),
               xlim = c(0, N1),
               ylim = c(0, N2))


filled.contour(x, y, refined_values_param,
               color.palette = mis.colores,
               asp = 1,
               axes = TRUE,
               frame.plot = 0,
               main = paste("VaR Paramétrico 95%"),
               xlim = c(0, N1),
               ylim = c(0, N2))

 filled.contour(x, y, refined_values_mc,
                color.palette = mis.colores,
                asp = 1,
                axes = TRUE,
                frame.plot = 0,
                main = paste("VaR Monte Carlo 95%"),
                xlim = c(0, N1),
                ylim = c(0, N2))
 
 filled.contour(x, y, refined_values_es,
                color.palette = mis.colores,
                asp = 1,
                axes = TRUE,
                frame.plot = 0,
                main = paste("Expected Shortfall 95%"),
                xlim = c(0, N1),
                ylim = c(0, N2))
 
 
 # Dibujamos el mapa 3D resultante tras aplicar la metodología con diferentes medidas
 
 persp3D(x = x,
         y = y,
         z = refined_values_hist,
         zlim = c(min_z, max_z),
         col = mis.colores(300),
         theta = 30,
         phi = 20,
         r = 50,
         d = 0.1,
         expand = 0.5,
         ltheta = 90,
         main = sprintf("VaR Histórico %.0f%%", (1 - alpha2) * 100),
         lphi = 180,
         shade = 0.3,
         ticktype = "detailed",
         nticks = 5,
         cex.axis = 0.7,
         zlab = "Values")
 
 persp3D(x = x,
         y = y,
         z = refined_values_param,
         zlim = c(min_z, max_z),
         col = mis.colores(300),
         theta = 30,
         phi = 20,
         r = 50,
         d = 0.1,
         expand = 0.5,
         ltheta = 90,
         main = sprintf("VaR Paramétrico %.0f%%", (1 - alpha2) * 100),
         lphi = 180,
         shade = 0.3,
         ticktype = "detailed",
         nticks = 5,
         cex.axis = 0.7,
         zlab = "Values")
 
 persp3D(x = x,
         y = y,
         z = refined_values_mc,
         zlim = c(min_z, max_z),
         col = mis.colores(300),
         theta = 30,
         phi = 20,
         r = 50,
         d = 0.1,
         expand = 0.5,
         ltheta = 90,
         main = sprintf("VaR Monte Carlo %.0f%%", (1 - alpha2) * 100),
         lphi = 180,
         shade = 0.3,
         ticktype = "detailed",
         nticks = 5,
         cex.axis = 0.7,
         zlab = "Values")
 
 persp3D(x = x,
         y = y,
         z = refined_values_es,
         zlim = c(min_z, max_z),
         col = mis.colores(300),
         theta = 30,
         phi = 20,
         r = 50,
         d = 0.1,
         expand = 0.5,
         ltheta = 90,
         main = sprintf("Expected Shortfall %.0f%%", (1 - alpha2) * 100),
         lphi = 180,
         shade = 0.3,
         ticktype = "detailed",
         nticks = 5,
         cex.axis = 0.7,
         zlab = "Values")
 
 
 