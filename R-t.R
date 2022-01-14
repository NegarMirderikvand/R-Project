n<-10
x<-c(1,2,3,4,5,6)   #c is concatenate

y<-1:6
yy<-1:100

XX<-seq(1,110.5,0.5)
xx<-seq(20)

XX+xx
x+y

q<-log(x^2+3)
h<-cumsum(x)
f<-sort(x,decreasing = TRUE)
g<-crossprod(x,y)
max(x)
min(x)
range(x)
diff(x)
diff(range(x))
rep(1,5)
rep(c(1,2,3),5)
rep(TRUE,5)
rep(0
    ,10)
rep('a',5)
rep("a",10)
rep(FALSE,5)
x[1:4]
x[1:3]
x[c(2,4)]
dd<-c(rep(FALSE,10),rep(TRUE,10))
xx[dd]
x>3
x[x>3]

#remove some element
x<- x[-2]
x[-c(3,6)] 

#find 
which(x>3)
x[which(x>2 & x<4)]
which.max(x)
which.min(x)
x[-2]<-3
t(a)
mean(x)
mean(x,x.na=TRUE)
b<-cbind(x,y)
ro<-rbind()
diag(5)
diag(x)
diag(matrix(x,y))
upper.tri(x)
A<-matrix(rnorm(36,0,1),6,6)
B<-matrix(runif(36,6,18),6,6)
A%*%B
solve(A)#A^-1
x+y
x*y
x/y
t(A)
colMeans(A)
rowMeans(A)
mean(A)
colnames(A)
colnames(A)<-c("A","B","col3","col4")
rownames(A)<-c
A[1,2]
A[3,]
A[,2]
A[-1,]
A[-1,-2]
A[-c(2,3),]
apply(A, 1, mean) #row
apply(A,2,sd)  #col
apply(A,1,leangh)


#Categorical Variables nonnumeric

gender<- c("Female","male","Female","male")
is.factor(gender)
gender<-as.factor(gender)
is.factor(gender)

levels(gender)
levels(gender)<-c("F","M")
#Generate Levels
gl(2,2,labels = c("F","M"))
rep(gl(2,1,labels = c("female","male")),2)
as.numeric()
as.Date()
as.character()

N<-data.frame(Names=,Salary=)
N$Salary
str(N)
summary(N)

tapply(N$salary, N$gender, mean)
order(N$Salary)