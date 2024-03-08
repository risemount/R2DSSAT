polar <- function(df = NULL, x, y, k = 1){
  # x: yield of full irrigated
  # y: yield of drought condition
  if(is.null(df)){
    r <- sqrt(x^2+y^2)
    theta1 <- ifelse(x>0, (1-((atan(y/x)/pi)*180)/45), 1)
    theta <- ifelse(theta1<0,0,theta1)
    r = abs(r-max(r))/max(r)
    return(1-sqrt((1-k)*(1-r)*k*(1-theta)))  
  }else{
    r <- sqrt(df[,1]^2+df[,2]^2)
    theta1 <- ifelse(df[,1]>0, (1-((atan(df[,2]/df[,1])/pi)*180)/45), 1)
    theta <- ifelse(theta1<0,0,theta1)
    r = abs(r-max(r))/max(r)
    return(1-sqrt((1-r)^(2-k)*(1-theta)^(k))) 
  }
}
## This is just for testing
# mx = 0.8; my = 0.7
# polar <- function(df = NULL, x, y, k = 1, max.x = mx, max.y = my){
#   # x: yield of full irrigated
#   # y: yield of drought condition
#   if(is.null(df)){
#     r <- sqrt(x^2+y^2)
#     theta1 <- ifelse(x>0, (1-((atan(y/x)/pi)*180)/45), 1)
#     theta <- ifelse(theta1<0,0,theta1)
#     mr <- sqrt(max.x^2+max.y^2)
#     r = abs(r-mr)/mr
#     return(1-sqrt((1-k)*(1-r)*k*(1-theta)))  
#   }else{
#     r <- sqrt(df[,1]^2+df[,2]^2)
#     theta1 <- ifelse(df[,1]>0, (1-((atan(df[,2]/df[,1])/pi)*180)/45), 1)
#     theta <- ifelse(theta1<0,0,theta1)
#     mr <- sqrt(max.x^2+max.y^2)
#     r = abs(r-mr)/mr
#     return(1-sqrt((1-r)^(2-k)*(1-theta)^(k))) 
#   }
# }
##
risk.ratio <- function(x,y){
  # x: yield of full irrigated
  # y: yield of drought condition
  risk = ifelse(x == 0, 1, ifelse( ((x-y)/x) > 0, (x-y)/x, 0))
  return(risk)
}

# Grid points for plane
grid.points <- expand.grid((1-seq(0,1,0.005)), seq(0,1,0.005))[,c(2,1)]

# Risk
polar.mat <- matrix(polar(df = grid.points),nrow = sqrt(dim(grid.points)[1]), ncol = sqrt(dim(grid.points)[1]))
# Yield different ratio
ratio.mat <- matrix(risk.ratio(grid.points[,1], grid.points[,2]),nrow = sqrt(dim(grid.points)[1]), ncol = sqrt(dim(grid.points)[1]))
# Just calculate by Pythagorean theorem
sqrt.mat <- matrix(sqrt(grid.points[,1]^2+grid.points[,2]^2),nrow = sqrt(dim(grid.points)[1]), ncol = sqrt(dim(grid.points)[1]))

# Function: Visualize a matrix
imageM <- function(m, grid = max(dim(m)) <= 25, asp = (nrow(m)-1)/(ncol(m)-1), ...) {
  tf <- function(m) t(m)[, nrow(m):1]
  image(tf(m), asp=asp, axes = FALSE, col = hcl.colors(50, "YlOrRd", rev = TRUE),...)
  mAxis <- function(side, at, ...) # using 'j'
    axis(side, at=at, labels=as.character(j/200), col="gray", col.axis=1,...)
  n <- ncol(m); n1 <- n-1L; j <- seq(0,n1,length.out = 5); mAxis(1, at= j/n1)
  if(grid) abline(v = (0:n - .5)/n1, col="gray77", lty="dotted")
  n <- nrow(m); n1 <- n-1L; j <- seq(0,n1,length.out = 5); mAxis(2, at=j/n1, las=1)
  if(grid) abline(h = (0:n - .5)/n1, col="gray77", lty="dotted")
}

png(filename = "./figures/polar_heatmap.png", width = 1300, height = 1100)
par(mfrow = c(3,3), cex.main = 3, cex.axis = 1.6, cex.lab = 1.8, mgp=c(4,1,0), mar = c(5.5,6,4,2))
for(k in c(0, 0.3, 0.5, 0.7, 1, 1.3, 1.5, 1.7, 2)){
  polar.mat <- matrix(polar(df = grid.points,k = k),nrow = sqrt(dim(grid.points)[1]), ncol = sqrt(dim(grid.points)[1]))
  # Set up the layout to place the image and color bar side by side
  layout(matrix(c(1,2), nrow=1), widths=c(4,1), heights=c(1,0.2))
  imageM(polar.mat, main = paste("k = ", k), xlab = "Yield in full irrigated (Normalized)", ylab = "yield in drought condition (Normalized)")
  abline(0,1)
  # Generate a sequence of values covering the range of your data
  zlim <- range(polar.mat)
  z <- seq(zlim[1], zlim[2], length.out=length(heat.colors(50)))
  # Create a matrix for the color bar
  color.bar <- matrix(z, nrow=length(z), ncol=1)
  # Plot the color bar
  image(1, z, t(color.bar), col=hcl.colors(50, "YlOrRd", rev = TRUE), xaxt="n", yaxt="n", bty="n", ylab = "",
        main = "risk", xlab = ""); axis(2)
}
graphics.off()

# Yield different ratio
png(filename = "./figures/ratio_heatmap.png", height = 400, width = 400)
imageM(ratio.mat, main = "Yield different ratio", xlab = "Yield in full irrigated (Normalized)", ylab = "yield in drought condition (Normalized)")
abline(0,1)
graphics.off()

# Pythagorean theorem
png(filename = "./figures/sqrt_heatmap.png", height = 400, width = 400)
imageM(sqrt.mat, main = "Pythagorean theorem", xlab = "Yield in full irrigated (Normalized)", ylab = "yield in drought condition (Normalized)")
abline(0,1)
graphics.off()
