EntrinsicGraphics<-function(path,count=290,splitwindow="T")
{
	mat=read.csv(path);
	
	count=dim(mat)[1];
	x=c(1:count);

	pingjunshu=apply(mat,2,mean)
	fangcha=apply(mat,2,sd)
	zuida=apply(mat,2,max)
	zuixiao=apply(mat,2,min)
	
	if(splitwindow)
	{
		par(mfrow=c(3,1));
		plot(x=x,y=mat[,1],ylim=c(min(mat[,1]),max(mat[,1])),type="l",col="red",main="XÖá");
		abline(h=pingjunshu[1],col="black",lty=2);
		plot(x=x,y=mat[,2],ylim=c(min(mat[,2]),max(mat[,2])),type="l",col="green",main="yÖá");
		abline(h=pingjunshu[2],col="black",lty=2);
		plot(x=x,y=mat[,3],ylim=c(min(mat[,3]),max(mat[,3])),type="l",col="blue",main="zÖá");
		abline(h=pingjunshu[3],col="black",lty=2);
		par(mfrow=c(1,1));
	}
	else
	{
		ymax=max(mat);ymin=min(mat);
		par(mfrow=c(1,1));
		p=plot(x,ylim=c(ymin,ymax),type="n",main="EO_Data");
		p+lines(mat[,1],col="red");
		p+lines(mat[,2],col="blue");
		p+lines(mat[,3],col="green");
		print(p);
	}

	
	mingzi=c("x","y","z");
	frm=data.frame(mingzi,pingjunshu,fangcha,zuida,zuixiao);
	colnames(frm)=c("label","E","D","a","in");
	return(mat);
}
EntrinsicGraphics2<-function(path,count=290,splitwindow="T")
{
  library(ggplot2);
  library(grid);
  mat=read.csv(path);
  
  count=dim(mat)[1];
    if(count>4000)count=4000;
  NO=c(1:count);
    mat<-cbind(NO,mat[1:count,]);
  
    co=colors()[c(124,97,50)];
    p1<-ggplot(mat,aes(x=NO,y=x));
    p1=p1+geom_line(color=co[1]);
    p1=p1+ylab("Õñ·ù(¶È)")+xlab("Ê±Ðò±àºÅ\n")+ggtitle("(a) XÖáÊý¾Ý");  
    p1=p1+theme(axis.text.x=element_text(size=15,color="black"),axis.text.y=element_text(size=15,color="black"))
    p1=p1+theme(axis.title.x=element_text(size=12),axis.title.y=element_text(size=15))
    p1=p1+theme(plot.title=element_text(size=15));
    p1=p1+theme(panel.background=element_blank());
    p1=p1+theme(plot.title=element_text(hjust=0.5,vjust=-15));
    p1=p1+theme(axis.line=element_line(size=0.5));
  
    p2<-ggplot(mat,aes(x=NO,y=y));
    p2=p2+geom_line(color=co[2]);
    p2=p2+ylab("Õñ·ù(¶È)")+xlab("Ê±Ðò±àºÅ\n")+ggtitle("(b) YÖáÊý¾Ý"); 
    p2=p2+theme(axis.text.x=element_text(size=15,color="black"),axis.text.y=element_text(size=15,color="black"))
    p2=p2+theme(axis.title.x=element_text(size=12),axis.title.y=element_text(size=15))
    p2=p2+theme(plot.title=element_text(size=15));
    p2=p2+theme(panel.background=element_blank());
    p2=p2+theme(plot.title=element_text(hjust=0.5,vjust=-15));
    p2=p2+theme(axis.line=element_line(size=0.5));
  
    p3<-ggplot(mat,aes(x=NO,y=z));
    p3= p3+geom_line(color=co[3]);
    p3= p3+ylab("Õñ·ù(¶È)")+xlab("Ê±Ðò±àºÅ\n")+ggtitle("(c) ZÖáÊý¾Ý");  
    p3= p3+theme(axis.text.x=element_text(size=15,color="black"),axis.text.y=element_text(size=15,color="black"))
    p3= p3+theme(axis.title.x=element_text(size=12),axis.title.y=element_text(size=15))
    p3= p3+theme(plot.title=element_text(size=15));
    p3=p3+theme(panel.background=element_blank());
    p3=p3+theme(plot.title=element_text(hjust=0.5,vjust=-15));
    p3=p3+theme(axis.line=element_line(size=0.5));
  
    ress=100;
    path1=print(strsplit(path,".",fixed=T));
  
    print(path1[[1]][1])
    png(paste(path1[[1]][1],".png"),width=10*ress,height=6.18*ress,res=ress);
    pushViewport(viewport(layout=grid.layout(3,1)))
    print(p1,vp=viewport(layout.pos.row=1,layout.pos.col=1))
    print(p2,vp=viewport(layout.pos.row=2,layout.pos.col=1))
    print(p3,vp=viewport(layout.pos.row=3,layout.pos.col=1))
    dev.off();
}
EntrinsicGraphics3<-function(path,count=290,splitwindow="T")
{
  library(ggplot2);
  library(grid);
  
  print(path);
  paths=list.files(path,pattern=".txt",full.names=T);
  print(paths);
  
  for(q in 1:length(paths))
  {
    mat=read.csv(paths[q]);
    
    count=dim(mat)[1];
    if(count>4000)count=4000;
    NO=c(1:count);
    mat<-cbind(NO,mat[1:count,]);
    
    co=colors()[c(124,97,50)];
    
    p1<-ggplot(mat,aes(x=NO,y=x));
    p1=p1+geom_line(color=co[1]);
    p1=p1+ylab("Õñ·ù(¶È)")+xlab("Ê±Ðò±àºÅ\n")+ggtitle("(a) XÖáÊý¾Ý");  
    
    p2<-ggplot(mat,aes(x=NO,y=y));
    p2=p2+geom_line(color=co[2]);
    p2=p2+ylab("Õñ·ù(¶È)")+xlab("Ê±Ðò±àºÅ\n")+ggtitle("(b) YÖáÊý¾Ý"); 
    
    p3<-ggplot(mat,aes(x=NO,y=z));
    p3= p3+geom_line(color=co[3]);
    p3= p3+ylab("Õñ·ù(¶È)")+xlab("Ê±Ðò±àºÅ\n")+ggtitle("(c) ZÖáÊý¾Ý");  
    
    t=theme(axis.text.x=element_text(size=15,color="black"),axis.text.y=element_text(size=15,color="black"))
    t=t+theme(axis.title.x=element_text(size=12),axis.title.y=element_text(size=15))
    t=t+theme(plot.title=element_text(size=15));
    t=t+theme(panel.background=element_blank());
    t=t+theme(plot.title=element_text(hjust=0.5,vjust=-15));
    t=t+theme(axis.line=element_line(size=0.5));
    
    p1=p1+t;p2=p2+t;p3=p3+t;
    
    ress=100;
    path1=strsplit(paths[q],".",fixed=T);
    print(path1[[1]][1])
    png(paste(path1[[1]][1],".png"),width=10*ress,height=6.18*ress,res=ress);
    pushViewport(viewport(layout=grid.layout(3,1)))
    print(p1,vp=viewport(layout.pos.row=1,layout.pos.col=1))
    print(p2,vp=viewport(layout.pos.row=2,layout.pos.col=1))
    print(p3,vp=viewport(layout.pos.row=3,layout.pos.col=1))
    dev.off();
  
  }
  
}
