# このRコードは
# 浜田2020『その問題，やっぱり数理モデルが解決します』ベレ出版
# の計算を再現するためのコードです．

library("ggplot2")
library("MASS")

##　2章 #####################

# p.44
curve(dnorm(x),-4,4)

# p.47-48　
#　本文内の図はMathematicaで作成したので見た目が異なります

# 1つずつplot
curve(dnorm(x,mean=0,sd=1),-4,4)
curve(dnorm(x,mean=1,sd=1),-4,4)
curve(dnorm(x,mean=2,sd=1),-4,4)

# 重ねてplot
#　本文内の図はMathematicaで作成したので見た目が異なります
curve(dnorm(x,0,1),-4,4,lty=1,ylab="f(x)")
curve(dnorm(x,1,1),-4,4,lty=2,add=TRUE,ylab="")
curve(dnorm(x,2,1),-4,4,lty=3,add=TRUE,ylab="")
legend("topleft",legend=c("mean=0","mean=1","mean=2"),lty=c(1,2,3))



# p.50

curve(dnorm(x,mean=0,sd=0.75),-4,4)
curve(dnorm(x,mean=0,sd=1),-4,4)
curve(dnorm(x,mean=0,sd=1.5),-4,4)


# 重ねてplot
#　本文内の図はMathematicaで作成したので見た目が異なります
curve(dnorm(x,0,0.75),-4,4,lty=1,ylab="f(x)")
curve(dnorm(x,0,1),-4,4,lty=2,add=TRUE,ylab="")
curve(dnorm(x,0,1.5),-4,4,lty=3,add=TRUE,ylab="")
legend("topleft",legend=c("sd=0.75","sd=1","sd=1.5"),lty=c(1,2,3))

# 参考．ggplot2パッケージを使った例

x <- seq(-4,5,0.1)
m0 <- dnorm(x,0,1)
m1 <- dnorm(x,1,1)
data <- data.frame(x=x,y1=m0,y2=m1)
ggplot(data,aes(x))+
	geom_line(aes(y=y1))+ 
	geom_line(aes(y=y2),linetype="dashed")



##　3章 #####################

# p.61
f <- function(x){dnorm(x,mean=0,sd=0.1)}
integrate(f, 0, 0.15)

# p.66
integrate(f, -0.15, 0.15)

# p.67
integrate(f, -0.4, 0.4)


# p.69 花京院分布
f <- function(x){exp(-(log(x)^2))}
curve(f(x),0,10,lty=1,ylab="f(x)")

# 面積の計算
integrate(f, 0, 10)



##　9章 #####################

x=c(1,2,3,4)
y=c(3,5,6,4)

# p.202-203 OLS推定値の計算

# b^ の分子
sum((x-mean(x))*(y-mean(y)))

# b^ の分母
sum((x-mean(x))^2)

# b^
b=sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
b

# a^
a=mean(y)-b*mean(x)
a




##　10章 #####################

# 2集団の合併（混合分布のpdf） 
f <- function(x){0.5*dnorm(x,1,1)+0.5*dnorm(x,5,1)}
curve(f(x),-2,8,lty=1,ylab="f(x)")

# 2集団の合併
x=rnorm(1000,1,1)
y=rnorm(1000,5,1)
z=c(x,y)#2集団を合併
hist(z,breaks =15,probability = T)
curve(f(x),-2,8,lty=1,add=T, ylab="f(x)")

# 2集団の和の分布，X~N(1,1), Y~N(5,1), X+Yの分布
x=rnorm(1000,1,1)
y=rnorm(1000,5,1)
z=x+y #実現値を足す
hist(z,breaks =15,probability = T)
curve(dnorm(x,6,1),1,11,lty=3,add=TRUE,ylab="",xlab ="")



##　11章 #####################

# 乱数を使っているので本文とは異なる結果が出ます
# 繰り返し計算すると, 本文で示した例と同様の例が出力されます

n <- 1000
data1<-data.frame(x1=rnorm(n),x2=rnorm(n),
									x3=rnorm(n),x4=rnorm(n),
									x5=rnorm(n),x6=rnorm(n),
									x7=rnorm(n),x8=rnorm(n),
									x9=rnorm(n),x10=rnorm(n),y=rnorm(n))

out<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10,data=data1) #OLS推定
summary(out)#結果出力



##　12章 #####################
# p.253--p.255
# 乱数を使っているので本文とは多少異なる結果が出ます
# 売上y＝b1*x店員数＋b2*z他店の売り上げ＋U

omitted<-function(n,sd,r,b1,b2){
	mu  <- c(0,0)  #平均ベクトルの定義
	cov <- matrix(c(1, r, r, 1), 2, 2)#分散共分散行列の定義
	exv<-mvrnorm(n, mu, cov)#説明変数用数n個を生成
	u<-rnorm(n, mean = 0, sd)#攪乱項の生成
	y<- b1*exv[,1]+ b2*exv[,2]+u #真関数によるデータ生成
	data1<-data.frame(x=exv[,1],z=exv[,2],y)#data.frameを使って人工データを定義
	out1<-lm(y~x,data=data1)#zが欠落した推定
	out2<-lm(y~x+z,data=data1)#欠落変数がない推定
	print(summary(out1))#分析結果の出力
	print(summary(out2))
	print(cor(exv[,1],exv[,2]))#相関係数確認
}

#omitted(n,sd,r,b1,b2)
omitted(1000,1,0.8,0.5,-2)
