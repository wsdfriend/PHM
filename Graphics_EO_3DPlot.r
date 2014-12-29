#==========================================
#
#==========================================
Graphics_EO_3DPlot<-function(angelmat,Israd=F)
{
	library(rgl);
	
	count=dim(angelmat)[1];#获得向量的个数_
	newvector=matrix(rep(0,3),1,3,byrow=T);
	organvector=matrix(c(0,0,1,0,1,0,1,0,0),3,3);
	#判断角度数据是角度还是弧度表示_
	if(Israd==F)
		angelmat=angelmat*pi/180.0;
	#坐标转化_
	for(i in 1:count)
	{
		#构建旋转矩阵_
		R1=matrix(c(cos(angelmat[i,1]),0,-sin(angelmat[i,1]),0,1,0,sin(angelmat[i,1]),0,cos(angelmat[i,1])),3,3,byrow=T);
		R2=matrix(c(1,0,0,0,cos(angelmat[i,2]),-sin(angelmat[i,2]),0,sin(angelmat[i,2]),cos(angelmat[i,2])),3,3,byrow=T);
		R3=matrix(c(cos(angelmat[i,3]),-sin(angelmat[i,3]),0,sin(angelmat[i,3]),cos(angelmat[i,3]),0,0,0,1),3,3,byrow=T);
		R=R1%*%R2%*%R3;
		temp=R%*%organvector;
		newvector=rbind(newvector,temp);
	}
	newvector=newvector[-1,]
	#3D绘图
	cols=c(rep(palette()[2],3),rep(palette()[3],3),rep(palette()[4],3),rep(palette()[5],3),rep(palette()[6],3),rep(palette()[7],3))
	p=plot3d(newvector[,1],newvector[,2],newvector[,3],type="s",size=1.5,fit=F,xlab="X",ylab="Y",zlab="Z",col=cols,box=F);
	p=p+points3d(0,0,0,size=30);#标记添加原点
	#polygon3d(organvector[c(1:3,1),1],organvector[c(1:3,1),2],organvector[c(1:3,1),3],alpha=0.4);
	for(i in 1:count)
	{
		polygon3d(newvector[c((3*i-2):(3*i),3*i-2),1],newvector[c((3*i-2):(3*i),3*i-2),2],newvector[c((3*i-2):(3*i),3*i-2),3],alpha=0.5,col=palette()[i+1]);
	}
	print(palette()[2:7]);
}
interline<-function(v1,v2)
{
	return (as.vector(rbind(v1,v2)));
}