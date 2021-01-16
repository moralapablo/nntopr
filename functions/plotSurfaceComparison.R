plotSurfaceComparison <- function(x1, x2, y, train_x1, train_x2, prediction, x1_big, x2_big, y_big, my_breaks, my_theta, my_phi, title1, title2){
  # Polynomial in input range
  par(mfrow=c(1,2))
  plot3D::persp3D(x1,
                  x2,
                  y,
                  theta = my_theta,
                  breaks = my_breaks,
                  colkey = FALSE,
                  phi = my_phi,
                  xlab = "x1", ylab = "x2",
                  zlab = "y"
  )
  plot3D::points3D(train_x1,
                   train_x2,
                   prediction,
                   colvar = NULL,
                   col = "black",
                   bg = "black",
                   size = 30,
                   pch = 23,
                   add = TRUE
  )
  # Polynomial Extended
  plot3D::persp3D(x1_big,
                  x2_big,
                  y_big,
                  theta = my_theta,
                  breaks = my_breaks,
                  phi = my_phi,
                  image = TRUE,
                  contour = TRUE,
                  xlab = "x1", ylab = "x2",
                  zlab = "y"
  )
  plot3D::points3D(train_x1,
                   train_x2,
                   prediction,
                   colvar = NULL,
                   col = "black",
                   bg = "black",
                   size = 30,
                   pch = 23,
                   add = TRUE
  )
}
