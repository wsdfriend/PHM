#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#�������ƣ�����У����ǵ�ƫ��������ͼ
#����˵������ָ��·���ļ���ȡ�в����ݣ������Ʋв�����ͼ
#����������
#path_before �����Ʋв�������ļ�����·��
#row �������� Ĭ��Ϊ6
#col �������� Ĭ��Ϊ9
#preindexname ͼ����ʾ�ֶ� Ĭ��Ϊ��F��
#��������: ���ڻ��Ʋв�ͼ��ggplot��ͼ���
#ǰ���������ļ�Ӧ��֯Ӧ����[index],x,y����ʽ��֯����index��Ҫ��1��ʼ
#ǰ����������Ҫlibrary(ggplot2)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Rms_ErrorGraphics<-function(path_before,row=6,col=9,preindexname="F")
{
	j=0;
	mat=read.csv(path_before);
	#�����Ƿ���������
	if(!colnames(mat)[1]=="index")
		{
			index=1:dim(mat)[1]
			mat=data.frame(index,mat)
		}
	#�����������
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
	#��������
	des=matrix(nrow=(col+row),ncol=col,byrow=T);
	for(j in 1:(row+col))#������������
	{
		#����ֱ�߷���y=kx+b��ʽ
		coe=matrix(c(mat[vertex[j,1],2],1,mat[vertex[j,2],2],1),nrow=2,byrow=T);
		con=matrix(c(mat[vertex[j,1],3],mat[vertex[j,2],3]),nrow=2,byrow=T);		
		t=solve(coe,con);
		#ת��Ϊax+by+c��ʽ
		a=t[1];b=-1;c=t[2];
		l=sqrt(a*a+b*b);
		#����㵽ֱ�߾���
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
	p=p+ylab("ƫ����(pixel)")+xlab("���");
	p=p+scale_x_continuous(breaks=1:col);
	p=p+labs(colour="�߶α��",shape="�߶α��",linetype="�߶α��");
	#�޸�������
	p=p+theme(axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,vjust=0.4),axis.text=element_text(size=30,colour="black"),axis.line=element_line(size=1));
	#�޸�ͼ��
	p=p+theme(legend.title=element_text(size=25),legend.text=element_text(size=25));
	#�޸����
	p=p+theme(panel.background=element_blank(),panel.grid.major=element_line(colour="black",size=0.3,linetype="dotted"),panel.grid.minor=element_line(colour="black",size=0.3,linetype="dotted"));
	#����һ�����ܼ���ÿ���ߵķ������ӡ����������
	frm=data.frame(c(1:row),apply(desmat,1,mean),apply(desmat,1,sd),apply(desmat,1,max))
	colnames(frm)=c("���","����ֵ","��׼��","����ֵ")
	print(frm);
	return (list(p,frm));
}

#=============================================
#�������ƣ����ƽǵ����߾�ֱֵ���Ա�ͼ
#�������������������в����ݿ򣬼����ֵ�в��������״ͼ
#����������frm1��frm2��Rms_ErrorGraphics()���������Ĳв�ͳ�����ݿ�
#�������أ����ڻ�ͼggplot�������
#=============================================
Rms_ErrorHistogram<-function(frm1,frm2,le=12)
{
  #���뺯����
  library(grid);
  library(ggplot2);
  print(le);
  frm1=data.frame(type=rep("����ǰ",dim(frm1)[1]),no=frm1[,1],e=frm1[,2]);#���ӱ�ʾbefore
  frm2=data.frame(type=rep("������",dim(frm2)[1]),no=frm2[,1],e=frm2[,2]);#���ӱ�ʾafter
  frm=rbind(frm1,frm2);#���һ��
  inn<<-frm[order(frm[,2],decreasing=F),3];#�����߶�˳������
  frm<<-data.frame(frm,inn);#��ɻ�ͼ�����ݿ�	
  p=ggplot(frm,aes(x=no,y=e,fill=type));#������ͼ
  p=p+geom_bar(stat="identity",position="dodge");#��Ϊ��״ͼ
  p=p+scale_fill_manual(values=c("#1874CD","#EE4000"))#���������ɫ
  p=p+xlab("�߶α��")+ylab("�в�(pixel)")#�����������ǩ
  p=p+geom_text(aes(x=seq(0.75,by=0.5,length=12),y=inn,label=round(inn,2)),vjust=-0.05,size=10);#���ñ�ǩ
  p=p+scale_x_continuous(breaks=1:dim(frm1)[1]);
  p=p+labs(fill="״̬")
  p=p+ylim(c(0,max(inn)+0.2));
  #�޸�������
  p=p+theme(axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,vjust=0.4),axis.text=element_text(size=30,colour="black"),axis.line=element_line(size=1));
  #�޸�ͼ��
  p=p+theme(legend.title=element_text(size=25),legend.text=element_text(size=25));
  #�޸����
  p=p+theme(panel.background=element_blank(),panel.grid.major=element_line(colour="black",size=0.3,linetype="dotted"),panel.grid.minor=element_line(colour="black",size=0.3,linetype="dotted"));
  return(p);
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#�������ƣ�����Ӱ��У��ǰ��У����ǵ�λ��������ͼ���Լ����߽ǵ�λ�ƾ�ֵ�Ա���״ͼ
#����˵����ͨ���ƶ�·���µĲв��ļ����������βв����ߣ���ͬ����ʾ
#����������
#path_before У��ǰ�в��ļ�·��
#path_after У����в��ļ�
#����Ϊ��ѡ����
#col �������� Ĭ��Ϊ6
#row �������� Ĭ��Ϊ9
#isprint �Ƿ��ӡ����������ļ� Ĭ��False
#outpath ������ļ�·����Ĭ��Ϊ�����ռ�.compareResult.txt
#����ֵ���޷���ֵ��ֱ������Ļ�����
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Rms_ErrorCompareGraphics<-function(path_before,path_after,col=6,row=9,isprint=F,outpath=getwd(),ress=500)
{
  #���뺯����
  library(grid);
  library(ggplot2);
  #���Ʋв�ͼ��1
  print("У��ǰ");
  p1=Rms_ErrorGraphics(path_before,row=col,col=row);
  g1=p1[[1]];f1=p1[[2]];#��ȡͼ�����������ݿ�
  #���Ʋв�ͼ��2
  print("У����");
  p2=Rms_ErrorGraphics(path_after,row=col,col=row);
  g2=p2[[1]];f2=p2[[2]];#��ȡͼ�����������ݿ�
  #���Ʋв��ֵ��״ͼ
  p3=Rms_ErrorHistogram(f1,f2,col*2);
  #p3=Rms_ErrorHistogram2();
  #��������������
  print("�ϲ�����");
  frm=data.frame(c(1:dim(f2)[1]),f2[,-1]-f1[,-1]);
  colnames(frm)[1]="���";
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
#��������Rms_pixel2line
#�����б���
#filepath���Ϸ���csv�ļ�·��
#����ֵ��
#��δȷ��
#ǰ��������
#1.��Ҫ����ggplot2��
#2.csv�ļ��밴��[index],x,y����ʽ��֯
#3.index��Ҫ��1��ʼ
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Rms_pixel2line<-function(filepath,row1,col1)
{
  j=0;
  mat=read.csv(filepath);
  #�����Ƿ���������
  if(!colnames(mat)[1]=="index")
  {
    index=1:dim(mat)[1]
    mat=data.frame(index,mat)
  }
  #��ȡ������
  row=row1;col=col1;
  #�����������
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
  #���ƻ�����ͼ
  sp=ggplot(mat,aes(x=x,y=y))+geom_point(shape=13,color="red",size=5);
  #�޸�������
  sp=sp+theme(axis.title.x=element_text(size=15),axis.title.y=element_text(size=15,vjust=0.4),axis.text=element_text(size=15,colour="black"),axis.line=element_line(size=1));
  #�޸�ͼ��
  sp=sp+theme(legend.title=element_text(size=25),legend.text=element_text(size=25));
  #�޸����
  sp=sp+theme(panel.background=element_rect(color=colors()[13]),panel.grid.major=element_blank(),panel.grid.minor=element_blank());
  #��������
  j=1;
  for(j in 1:(row+col))
  {
    sp=sp+geom_line(data=subset(mat,index==vertex[j,1]|index==vertex[j,2]),aes(x=x,y=y),color=colors()[113])
  }
  sp=sp+geom_text(aes(label=index),size=6,hjust=-0.8);
  #��������
  des=matrix(nrow=(col+row),ncol=col,byrow=T);
  for(j in 1:(row+col))#������������
  {
    #����ֱ�߷���y=kx+b��ʽ
    coe=matrix(c(mat[vertex[j,1],2],1,mat[vertex[j,2],2],1),nrow=2,byrow=T);
    con=matrix(c(mat[vertex[j,1],3],mat[vertex[j,2],3]),nrow=2,byrow=T);		
    t=solve(coe,con);
    #ת��Ϊax+by+c��ʽ
    a=t[1];b=-1;c=t[2];
    l=sqrt(a*a+b*b);
    #����㵽ֱ�߾���
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
#�������ƣ����ƽǵ����߾�ֱֵ���Ա�ͼ
#�������������������в����ݿ򣬼����ֵ�в��������״ͼ
#����������frm1��frm2��Rms_ErrorGraphics()���������Ĳв�ͳ�����ݿ�
#�������أ����ڻ�ͼggplot�������
#=============================================
Rms_ErrorHistogram2<-function(path)
{
  mat<-read.csv(path);
  print(mat);
  
  p=ggplot(mat,aes(x=no,y=e,fill=type));#������ͼ
  p=p+geom_bar(stat="identity",position="dodge");#��Ϊ��״ͼ
  p=p+scale_fill_manual(values=c("#1874CD","#EE4000"))#���������ɫ
  p=p+xlab("�߶α��")+ylab("�в�(pixel)")#�����������ǩ
  inn1<<-mat$inn;
  p=p+geom_text(aes(x=seq(0.75,by=0.5,length=18),y=inn1,label=round(inn1,2)),vjust=-0.05,size=10);#���ñ�ǩ
  #p=p+ggtitle("����ƫ���ֵͼ")#���ñ���
  p=p+scale_x_continuous(breaks=1:9);
  p=p+labs(fill="״̬")
  p=p+ylim(c(0,max(inn1)+0.2));
  #�޸�������
  p=p+theme(axis.title.x=element_text(size=32),axis.title.y=element_text(size=32,vjust=0.4),axis.text=element_text(size=30,colour="black"),axis.line=element_line(size=1));
  #�޸�ͼ��
  p=p+theme(legend.title=element_text(size=25),legend.text=element_text(size=25));
  #�޸����
  p=p+theme(panel.background=element_blank(),panel.grid.major=element_line(colour="black",size=0.3,linetype="dotted"),panel.grid.minor=element_line(colour="black",size=0.3,linetype="dotted"));
  print(p);
}