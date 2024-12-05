#install.packages("devtools")
#devtools::install_github("davidcsterratt/RTriangle", subdir="pkg")


k = 0
points = matrix(c(
    0, 1, 1, 0.5, 0.5,   0,
    1, 1, 0,   0, 0.5, 0.5
),ncol=2)
n = nrow(points)
segments = matrix(c(1:n,2:n,1),ncol=2)

plot(points,asp=1)
segments(points[segments[,1],1],points[segments[,1],2],points[segments[,2],1],points[segments[,2],2])

p = RTriangle::pslg(points,S = segments)
?pslg
ret = RTriangle::triangulate(p,a=0.03,q=30)
?triangulate
plot(ret$P,asp=1)
segments(ret$P[ret$T[,1],1],ret$P[ret$T[,1],2],ret$P[ret$T[,2],1],ret$P[ret$T[,2],2])
segments(ret$P[ret$T[,2],1],ret$P[ret$T[,2],2],ret$P[ret$T[,3],1],ret$P[ret$T[,3],2])
segments(ret$P[ret$T[,3],1],ret$P[ret$T[,3],2],ret$P[ret$T[,1],1],ret$P[ret$T[,1],2])

points(ret$P[unique(as.vector(ret$E[ret$EB == 1,])),],pch=16,cex=2)

points = ret$P
elements = ret$T
dofs = nrow(points)
RHS = np.zeros(dofs)
S = matrix(0,dofs, dofs)
M = matrix(0,dofs, dofs)

for (i in 1:nrow(elements)) {
    el = elements[i,]
    J = cbind(points[el[2],]-points[el[1],],points[el[3],]-points[el[1],])
    tmp1 = solve(t(J) %*% J)
    tmp2 = matrix(c(-1, 1, 0, -1, 0, 1),3,2)
    local_S = det(J) * tmp2 %*% tmp1 %*% t(tmp2)
    S[el,el] = S[el,el] + local_S
    local_M = det(J) * matrix(c(2, 1, 1, 1, 2, 1, 1, 1, 2),3,3) / 24
    M[el,el] = M[el,el] + local_M
}

image(S)
image(M)

sel = abs(points[,2])<0.0001



write.table(ret$P, "mesh/man_points.txt", row.names=FALSE,col.names=FALSE)
write.table(ret$T-1, "mesh/man_triangles.txt", row.names=FALSE,col.names=FALSE)


tab = ret$P
tab[] = sprintf("%.2f",tab)
tab = paste0("[",tab[,1],",",tab[,1],"]")
n = length(tab)
tab = matrix(tab,nrow=3)
tab[-(1:n)] = NA
apply(tab,2,paste0,collapse=",")
