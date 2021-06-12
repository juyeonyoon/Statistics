# NO.1

# 0.2 & 0.3-0.1
0.2 == (0.3-0.1)
all.equal(0.2,(0.3-0.1))
# '==' -> 정확하게 같은지를 비교
# all.equal -> 실수의 비교, 오차범위내에서 두실수가 같은지 비교할때 사용한다. 
# floating point 연산에서는 표현 방식에 따라서 오차 생길 수 있다. 
# 따라서 '==' -> 정확한 값 나오지 X

# NO.2

evaluatefunctionsinc <- function(xmin, xmax, n){
  x = c(0)
  f = c(0)
  for(i in 0:n){
    x[i+1] = xmin + i*(xmax-xmin)/n
    f[i+1] = sin(x[i+1])/x[i+1]
  }
  plot(x,f,type = "l", col= "blue", xlab = "x", ylab = "function")
}
evaluatefunctionsinc(-10,10,100)
evaluatefunctionsinc(-10^(-20), 10^(-20),100)

evaluatefunctionsincwithcheck <- function(xmin, xmax, n, epsilon){
  x = c(0)
  f = c(0)
  for(i in (0:n)){
    x[i+1] = xmin + i*(xmax-xmin)/n
    if(abs(x[i+1]) > epsilon){
      f[i+1] = sin(x[i+1])/x[i+1]
    }
    else{
      f[i+1] = 1
    }
    plot(x, f, type = "l", col = "blue", xlab = "x", ylab = "function")
  }
} 
evaluatefunctionsincwithcheck(-10^(-20), 10^(-20),100,10^(-30))
# x = 0에서 불연속이고 함숫값이 존재하지 않는다. 

# NO.3

# 재귀 프로그램
fiborecursive = function(i){
  if(i <= 2){
    value = 1
  }
  else{
    return(fiborecursive(i-1)+fiborecursive(i-2))
  }
}

# 반복 프로그램
fiboiterative = function(i){
  if(i <= 2){
    value = 1
  }
  else{
    value1 = 1
    value2 = 1
    for(j in 3:i){
      value <- value1 + value2
      value1 <- value2
      value2 <- value
    }
  }
  return(value)
}
fiboiterative(10)
system.time(fiborecursive(10))
system.time(fiboiterative(10))
system.time(fiborecursive(20))
system.time(fiboiterative(20))
system.time(fiborecursive(30))
system.time(fiboiterative(30))
system.time(fiborecursive(40))
system.time(fiboiterative(40))
# i가 커질수록 재귀 프로그램이 반복 프로그램보다 실행되는 시간이 더 길다.
# 따라서 반복프로그램이 실행속도가 재귀 프로그램보다 더 빠르다는 것을 알 수 있다. 
