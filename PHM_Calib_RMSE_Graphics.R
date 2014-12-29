#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#函数名称：绘制校正后角点偏移量折线图
#函数说明：从指定路径文件获取残差数据，并绘制残差折线图
#函数参数：
#path_before 待绘制残差的数据文件完整路径
#row 棋盘行数 默认为6
#col 棋盘列数 默认为9
#preindexname 图例标示字段 默认为“F”
#函数返回: 用于绘制残差图的ggplot绘图句柄
#前置条件：文件应组织应按照[index],x,y的形式组织，且index需要从1开始
#前置条件：需要library(ggplot2)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Rms_ErrorGraphics<-function(path_before,row=6,col=9,preindexname="F")
{
	j=0;
	mat=read.csv(path_before);
	#检验是否含有索引列
	if(!colnames(mat)[1]=="index")
		{
			index=1:dim(mat)[1]
			mat=data.frame(index,mat)
		}
	#构建顶点矩阵
	vertex=matrix(nrow=(row+col),ncol=2,byrow=T);
	for(j in (1:row))
	{
		vertex[j,1]=(j-1)*col+1;
		vertex[j,2]=j*col;
	}
	for(j in (row+1):(row+col))
	{
		vertex[j,1]=j-row;
		vertex[j,2]=j-row+(row-1)*col 
	}
	#评定精度
	des=matrix(nrow=(col+row),ncol=col,byrow=T);
	for(j in 1:(row+col))#遍历顶点数组
	{
		#解算直线方程y=kx+b形式
		coe=matrix(c(mat[vertex[j,1],2],1,mat[vertex[j,2],2],1),nrow=2,byrow=T);
		con=matrix(c(mat[vertex[j,1],3],mat[vertex[j,2],3]),nrow=2,byrow=T);		
		t=solve(coe,con);
		#转化为ax+by+c形式
		a=t[1];b=-1;c=t[2];
		l=sqrt(a*a+b*b);
		#计算点到直线距离
		for(i in (vertex[j,1]:vertex[j,2]))
		{
			if(i%%col!=0)
				{des[j,i%%col]=abs(a*mat[i,2]+b*mat[i,3]+c)/l;}
			else
				{des[j,col]=abs(a*mat[i,2]+b*mat[i,3]+c)/l;}
		}		
	}

	desmat=des[1:row,];	
	row=dim(desmat)[1];col=dim(desmat)[2];
	Value=rep(0,(row*col));
	for(i in 1:row)
		for(j in 1:col)
			Value[(i-1)*col+j]=desmat[i,j];
	
	PointNo=rep(c(1:col),row);	
	LineNo=paste(preindexname,sort(rep(c(1:row),col)),sep="");
	Frm=data.frame(LineNo,PointNo,Value);	
  color=c(palette()[2:5],colors()[97],colors()[91]);
	p=ggplot(Frm,aes(x=PointNo,y=Value,group=LineNo,colour=LineNo,shape=LineNo,linetype=LineNo))+geom_line(size=1.2)+geom_point(size=6,fill="green");
  p=p+scale_shape_manual(values=c(18,25,17,18,16,15,15,15));
	p=p+scale_colour_manual(values=color);
  p=p+scale_linetype_manual(values=c("twodash","solid","solid","solid","solid","solid","solid","solid"));
	p=p+ylab("偏移量(pixel)")+xlab("点号");
	p=p+scale_x_continuous(breaks=1:col);
	p=p+labs(colour="线段编号",shape="线段编号",linetype="线段编号");
	#修改坐标轴
	p=p+theme(axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,vjust=0.4),axis.text=element_text(size=30,colour="black"),axis.line=element_line(size=1));
	#修改图例
	p=p+theme(legend.title=element_text(size=25),legend.text=element_text(size=25));
	#修改面板
	p=p+theme(panel.background=element_blank(),panel.grid.major=element_line(colour="black",size=0.3,linetype="dotted"),panel.grid.minor=element_line(colour="black",size=0.3,linetype="dotted"));
	#添加一个功能计算每条线的方差。并打印出方差向量
	frm=data.frame(c(1:row),apply(desmat,1,mean),apply(desmat,1,sd),apply(desmat,1,max))
	colnames(frm)=c("编号","期望值","标准差","极大值")
	print(frm);
	return (list(p,frm));
}

#=============================================
#函数名称：绘制角点连线均值直方对比图
#函数简述：根据两个残差数据框，计算均值残差，并绘制柱状图
#函数参数：frm1，frm2由Rms_ErrorGraphics()函数产生的残差统计数据框
#函数返回：用于绘图ggplot函数句柄
#=============================================
Rms_ErrorHistogram<-function(frm1,frm2,le=12)
{
  #引入函数包
  library(grid);
  library(ggplot2);
  print(le);
  frm1=data.frame(type=rep("处理前",dim(frm1)[1]),no=frm1[,1],e=frm1[,2]);#增加标示before
  frm2=data.frame(type=rep("处理后",dim(frm2)[1]),no=frm2[,1],e=frm2[,2]);#增加标示after
  frm=rbind(frm1,frm2);#组合一下
  inn<<-frm[order(frm[,2],decreasing=F),3];#按照线段顺序排序
  frm<<-data.frame(frm,inn);#组成绘图用数据框	
  p=ggplot(frm,aes(x=no,y=e,fill=type));#基础绘图
  p=p+geom_bar(stat="identity",position="dodge");#设为柱状图
  p=p+scale_fill_manual(values=c("#1874CD","#EE4000"))#更改填充颜色
  p=p+xlab("线段编号")+ylab("残差(pixel)")#增加坐标轴标签
  p=p+geom_text(aes(x=seq(0.75,by=0.5,length=12),y=inn,label=round(inn,2)),vjust=-0.05,size=10);#设置标签
  p=p+scale_x_continuous(breaks=1:dim(frm1)[1]);
  p=p+labs(fill="状态")
  p=p+ylim(c(0,max(inn)+0.2));
  #修改坐标轴
  p=p+theme(axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,vjust=0.4),axis.text=element_text(size=30,colour="black"),axis.line=element_line(size=1));
  #修改图例
  p=p+theme(legend.title=element_text(size=25),legend.text=element_text(size=25));
  #修改面板
  p=p+theme(panel.background=element_blank(),panel.grid.major=element_line(colour="black",size=0.3,linetype="dotted"),panel.grid.minor=element_line(colour="black",size=0.3,linetype="dotted"));
  return(p);
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#函数名称：绘制影像校正前，校正后角点位移量折线图，以及连线角点位移均值对比柱状图
#函数说明：通过制定路径下的残差文件，绘制两次残差曲线，并同屏显示
#函数参数：
#path_before 校正前残差文件路径
#path_after 校正后残差文件
#以下为可选参数
#col 棋盘行数 默认为6
#row 棋盘列数 默认为9
#isprint 是否打印结果数据入文件 默认False
#outpath 输出的文件路径，默认为工作空间.compareResult.txt
#返回值：无返回值，直接在屏幕输出。
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Rms_ErrorCompareGraphics<-function(path_before,path_after,col=6,row=9,isprint=F,outpath=getwd(),ress=500)
{
  #引入函数包
  library(grid);
  library(ggplot2);
  #绘制残差图像1
  print("校正前");
  p1=Rms_ErrorGraphics(path_before,row=col,col=row);
  g1=p1[[1]];f1=p1[[2]];#获取图像句柄与结果数据框
  #绘制残差图像2
  print("校正后");
  p2=Rms_ErrorGraphics(path_after,row=col,col=row);
  g2=p2[[1]];f2=p2[[2]];#获取图像句柄与结果数据框
  #绘制残差均值柱状图
  p3=Rms_ErrorHistogram(f1,f2,col*2);
  #p3=Rms_ErrorHistogram2();
  #逐差法分析两组数据
  print("较差数据");
  frm=data.frame(c(1:dim(f2)[1]),f2[,-1]-f1[,-1]);
  colnames(frm)[1]="编号";
  print(frm);
  
  pushViewport(viewport(layout=grid.layout(3,1)))
  print(g1,vp=viewport(layout.pos.row=1,layout.pos.col=1))
  print(g2,vp=viewport(layout.pos.row=2,layout.pos.col=1))
  print(p3,vp=viewport(layout.pos.row=3,layout.pos.col=1))
  print(p3);
  
  if(isprint)
  {
    sink(paste(outpath,"compareResult.txt"),append=T);
    print(f1);
    print(f2);
    print(frm);
    sink();
    
    png(paste(outpath,"compareResult.png"),width=10*ress,height=15*ress,res=ress);
    
    pushViewport(viewport(layout=grid.layout(3,1)))
    print(g1,vp=viewport(layout.pos.row=1,layout.pos.col=1))
    print(g2,vp=viewport(layout.pos.row=2,layout.pos.col=1))
    print(p3,vp=viewport(layout.pos.row=3,layout.pos.col=1))
    
    dev.off();
  }
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#函数名：Rms_pixel2line
#参数列表：
#filepath：合法的csv文件路径
#返回值：
#暂未确定
#前置条件：
#1.需要引入ggplot2包
#2.csv文件须按照[index],x,y的形式组织
#3.index需要从1开始
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Rms_pixel2line<-function(filepath,row1,col1)
{
  j=0;
  mat=read.csv(filepath);
  #检验是否含有索引列
  if(!colnames(mat)[1]=="index")
  {
    index=1:dim(mat)[1]
    mat=data.frame(index,mat)
  }
  #获取行列数
  row=row1;col=col1;
  #构建顶点矩阵
  vertex=matrix(nrow=(row+col),ncol=2,byrow=T);
  for(j in (1:row))
  {
    vertex[j,1]=(j-1)*col+1;
    vertex[j,2]=j*col;
  }
  for(j in (row+1):(row+col))
  {
    vertex[j,1]=j-row;
    vertex[j,2]=j-row+(row-1)*col 
  }
  #绘制基础点图
  sp=ggplot(mat,aes(x=x,y=y))+geom_point(shape=13,color="red",size=5);
  #修改坐标轴
  sp=sp+theme(axis.title.x=element_text(size=15),axis.title.y=element_text(size=15,vjust=0.4),axis.text=element_text(size=15,colour="black"),axis.line=element_line(size=1));
  #修改图例
  sp=sp+theme(legend.title=element_text(size=25),legend.text=element_text(size=25));
  #修改面板
  sp=sp+theme(panel.background=element_rect(color=colors()[13]),panel.grid.major=element_blank(),panel.grid.minor=element_blank());
  #绘制连线
  j=1;
  for(j in 1:(row+col))
  {
    sp=sp+geom_line(data=subset(mat,index==vertex[j,1]|index==vertex[j,2]),aes(x=x,y=y),color=colors()[113])
  }
  sp=sp+geom_text(aes(label=index),size=6,hjust=-0.8);
  #评定精度
  des=matrix(nrow=(col+row),ncol=col,byrow=T);
  for(j in 1:(row+col))#遍历顶点数组
  {
    #解算直线方程y=kx+b形式
    coe=matrix(c(mat[vertex[j,1],2],1,mat[vertex[j,2],2],1),nrow=2,byrow=T);
    con=matrix(c(mat[vertex[j,1],3],mat[vertex[j,2],3]),nrow=2,byrow=T);		
    t=solve(coe,con);
    #转化为ax+by+c形式
    a=t[1];b=-1;c=t[2];
    l=sqrt(a*a+b*b);
    #计算点到直线距离
    for(i in (vertex[j,1]:vertex[j,2]))
    {
      if(i%%col!=0)
      {des[j,i%%col]=abs(a*mat[i,2]+b*mat[i,3]+c)/l;}
      else
      {des[j,9]=abs(a*mat[i,2]+b*mat[i,3]+c)/l;}
    }		
  }
  return (list(sp,des))
}

#=============================================
#函数名称：绘制角点连线均值直方对比图
#函数简述：根据两个残差数据框，计算均值残差，并绘制柱状图
#函数参数：frm1，frm2由Rms_ErrorGraphics()函数产生的残差统计数据框
#函数返回：用于绘图ggplot函数句柄
#=============================================
Rms_ErrorHistogram2<-function(path)
{
  mat<-read.csv(path);
  print(mat);
  
  p=ggplot(mat,aes(x=no,y=e,fill=type));#基础绘图
  p=p+geom_bar(stat="identity",position="dodge");#设为柱状图
  p=p+scale_fill_manual(values=c("#1874CD","#EE4000"))#更改填充颜色
  p=p+xlab("线段编号")+ylab("残差(pixel)")#增加坐标轴标签
  inn1<<-mat$inn;
  p=p+geom_text(aes(x=seq(0.75,by=0.5,length=18),y=inn1,label=round(inn1,2)),vjust=-0.05,size=10);#设置标签
  #p=p+ggtitle("像素偏差均值图")#设置标题
  p=p+scale_x_continuous(breaks=1:9);
  p=p+labs(fill="状态")
  p=p+ylim(c(0,max(inn1)+0.2));
  #修改坐标轴
  p=p+theme(axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,vjust=0.4),axis.text=element_text(size=30,colour="black"),axis.line=element_line(size=1));
  #修改图例
  p=p+theme(legend.title=element_text(size=25),legend.text=element_text(size=25));
  #修改面板
  p=p+theme(panel.background=element_blank(),panel.grid.major=element_line(colour="black",size=0.3,linetype="dotted"),panel.grid.minor=element_line(colour="black",size=0.3,linetype="dotted"));
  print(p);
}