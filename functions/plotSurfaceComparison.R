plotSurfaceComparison <- function(x1, x2, y, train_x1, train_x2, prediction, x1_big, x2_big, y_big, my_breaks, my_theta, my_phi, title){
  # Polynomial in input range
  x <- c(1, 2, 4, 1, 3, 4)
  m <- matrix(x, ncol = 2)
  par(mar=c(1,2.5,1,2.5))
  layout(mat = m,
         heights = c(1,5,2),
         widths = c(2,2)
  )
  plot.new()
  text(0.5,0.5,title,cex=1.5,font=2)
  plot3D::persp3D(x1,
                  x2,
                  y,
                  theta = my_theta,
                  breaks = my_breaks,
                  colkey = FALSE,
                  phi = my_phi,
                  xlab = "x1", ylab = "x2",
                  zlab = "y",
                  main = "A) Range limited to input values"
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
                  colkey = FALSE,
                  phi = my_phi,
                  xlab = "x1", ylab = "x2",
                  zlab = "y",
                  main = "B) Extended range"
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
  plot3D::colkey(breaks = my_breaks, side = 1, length = 0.5, width = 2)
}
