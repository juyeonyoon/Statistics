# NO.1 

# (a)
X = matrix(c(1,1,1,1,1,1,1,2,3,5,5,7,1,3,3,4,4,5), ncol = 3)
y = matrix(c(2,4,5,8,8,9), ncol = 1)
X
y
t(X) %*% X

# Choleski 분해 : X^(T) X = L L^(T)

# 1) 강의 예제 함수를 이용한 하삼각행렬 L 구하기 (Choleski 분해이용)
choleskyfactorization = function(A){
  n = nrow(A)
  L = matrix(0,nrow=n,ncol=n)
  for (i in (1:n)){
    L[i,i] = A[i,i]
    if (i > 1){
      for (k in (1:(i-1))){
        L[i,i] = L[i,i] - L[i,k]*L[i,k]
      }
    }
    L[i,i] = (L[i,i])^(1/2)
    if (i < n){
      for (j in ((i+1):n)){
        L[j,i] = A[j,i]
        if (i > 1){
          for (k in (1:(i-1))){
            L[j,i] = L[j,i] - L[j,k]*L[i,k]
          }
        }
        L[j,i] = L[j,i]/L[i,i]
      }
    }
  }
  return(L)
}

A = t(X) %*% X

# 하삼각행렬 L
choleskyfactorization(A)

# 2) 내장함수 이용
A.chol = chol(A)
A.chol
# 하삼각행렬 L을 구하는 것임으로 transpose를 통해 구한 행렬 전치한다. 

# 하삼각행렬 L
t(A.chol)

######################################################################

# (b) 

# X^(T) X 의 고유값 구하기
eigen(A)$values

# D의 대각원소의 값 구하기 
X.svd = svd(X)
X.svd$d

# 고유치와 D의 대각원소의 값 비교
(X.svd$d)^2

# 비교한 결과 (D의 대각원소의 값)^2 = X^(T) X 의 고유값이다. 

######################################################################

# (c)

# 1) 회귀계수의 추정값 구하기(QR분해)
X.qr = qr(X)
X.qr
Q = qr.Q(X.qr)
R = qr.R(X.qr)

solve(R) %*% t(Q) %*% y

# 2) 회귀계수의 추정값 구하기(lm 함수)
x1 = X[,2]
x1
x2 = X[,3]
x2

lm(y ~ x1 + x2)

# QR 분해를 이용했을 때와 lm 함수 이용했을 때, 회귀계수의 추정값이 유사하다. 
