# NO.1
# 승산합동법을 이용해 U[0,1] 분포 따르는 난수 100개를 생성
a <- vector()
seed = 2020
for(i in 1:100){
  seed = (16807*seed) %% 2147483647
  a[i] = seed/2147483647
}
a

# 1) 균일분포 인지 아닌지 검정
df = data.frame(a = a)
df[df['a'] > 0.9] = 10
df[df['a'] <= 0.9 & df['a'] > 0.8] = 9
df[df['a'] <= 0.8 & df['a'] > 0.7] = 8
df[df['a'] <= 0.7 & df['a'] > 0.6] = 7
df[df['a'] <= 0.6 & df['a'] > 0.5] = 6
df[df['a'] <= 0.5 & df['a'] > 0.4] = 5
df[df['a'] <= 0.4 & df['a'] > 0.3] = 4
df[df['a'] <= 0.3 & df['a'] > 0.2] = 3
df[df['a'] <= 0.2 & df['a'] > 0.1] = 2
df[df['a'] <= 0.1] = 1

t = as.numeric(table(df$a))
t

chisq.test(t, p = rep(1/10,10))

# 결과값 data: t  X-squared = 13.6, df = 9, p-value = 0.1373
# p-value 값이 < 0.5 로 귀무가설을 기각할 수 없다. 따라서 균일분포 U[0,1]을 따른다고 할 수 있다. 

# 2) 독립성 검정 (산점도 확인)
scatter_a <- data.frame(x1 = rep(0,99), x2 = rep(0,99))
scatter_a[,1] <- a[-length(a)]
scatter_a[,2] <- a[-1]

tail(scatter_a)

plot(scatter_a)

# 산점도 확인 결과 어떠한 패턴도 보이지 않는다. 독립이라고 할 수 있다. 

# NO.2
# 총 6개의 부품 직렬로 연결. 이중 1개 이상이 고장나면 시스템은 꺼진다. 각 부품의 고장확률이 
#    0.2일 때 시스템이 꺼질 확률을 1000번의 모의실험을 통해 추정하고 이론적인 값과 비교하기

# 1) 이론적인 값: 1 - (0.8)^6 = 0.737856 ; 약 74% 

# 2) R코드 이용
a = 0
for(i in 1: 1000){
  x = sample(0:1, size = 6, replace = T, prob = c(0.8,0.2))
  if(sum(x) >= 1){
    a = a + 1
  }
}
a/1000

# 모의실험을 통해 추정하면 a/1000 = 0.737이 구해진다. 
# 따라서 약 74%로 이론적인 값과 비슷한 값이 구해진다. 

# NO.3 (뷔퐁의 바늘실험)
# 반복수를 각각 10,50,100,1000,5000으로 하여 뷔퐁의 바늘실험을 실시하여 얻은 추정값과 
#   이론적으로 계산된 확률과의 차이를 R 그래프로 나타내어 비교. 

# 1) 이론적인 값 (l = 15, d = 20 지정) 
#    : (2*l)/(pi*d) = (2*15)/(pi*20) = 0.477464... = 0.477

# 2) R코드 이용
Buffon = function(n, lofneedle, distance){
  lofneedle = lofneedle/2
  distance = distance/2
  r1 = runif(n)
  r2 = runif(n)
  prob = mean(r1*distance < lofneedle*sin(r2*pi))
  return(prob)
}
Buffon(10,15,20)
Buffon(50,15,20)
Buffon(100,15,20)
Buffon(1000, 15, 20)
Buffon(10000, 15, 20)

# 추정값과 이론적으로 계산된 확률과의 차이그래프 그리기

theory = rep(0.477,5)
r = c(0.4,0.42,0.43,0.473,0.4762)
plot(theory - r, type = 'o', col = 'blue', ylim = c(0,0.1),axes = F, ann = F)
axis(1, at=1:5,lab=c(10,50,100,1000,5000))
axis(2, ylim = c(0,0.1))
title(xlab="n", col.lab="black")

# 시행 횟수가 커질수록 차이가 거의 없어진다는 것을 알 수 있다. 
# 따라서 시행 횟수가 커질수록 이론값과 추정 값이 비슷한 결과를 가지게 된다. 