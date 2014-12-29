###############################################################
#函数名称_:空间后方交会直接解_迭代解法_
#函数说明_:利用空间后方交会直接解解算外方位元素_四元数表达_
#函数原理_:参考__数字摄影测量学第二版__125页公式_
#函数参数_：_io内方位元素矩阵_ ip像平面空间下的点坐标_ op物方空间下的点坐标_
#函数返回_:
#函数版本_:2014/4/4
###############################################################
Resection_direct_iter_solution2<-function(io,ip,op)
{
	#1 获取必要数据_
	f=sqrt(io[1,1]^2+io[2,2]^2);
	Alpha=atan2(ip[,1]-io[1,3],f);
	Beta=atan2(ip[,2]-io[2,3],sqrt(f^2+(ip[,1]-io[1,3])^2));
	
	cos_12=sin(Beta[1])*sin(Beta[2])+cos(Beta[1])*cos(Beta[2])*cos(Alpha[2]-Alpha[1]);
	cos_13=sin(Beta[1])*sin(Beta[3])+cos(Beta[1])*cos(Beta[3])*cos(Alpha[3]-Alpha[1]);
	cos_23=sin(Beta[2])*sin(Beta[3])+cos(Beta[2])*cos(Beta[3])*cos(Alpha[3]-Alpha[2]);
	#print(cos_12);print(cos_13);print(cos_23);
	
	tempdist=dist(op);
	s12=tempdist[1];s13=tempdist[2];s23=tempdist[3]; 
	#print(tempdist);
	
	#2 构建解算中间变量_
	F12=1.0/(2.0*(1-cos_12));F13=1.0/(2.0*(1-cos_13));F23=1.0/(2.0*(1-cos_23));
	G12=F12*cos_12;G13=F13*cos_13;G23=F23*cos_23; 
	a=matrix(c(F12*s12^2,F23*s23^2,F13*s13^2),3,1);
	A=matrix(c(1,-1,1,1,1,-1,-1,1,1),3,3,byrow=T);
	x=A%*%a;
	print("X");print(x);
	count=0;flag=0;x1=1;
	while(1==1)
	{
		s=sqrt(x);
		b=matrix(c(-G12*(s[1,1]-s[2,1])^2,
				   -G23*(s[2,1]-s[3,1])^2,
				   -G13*(s[3,1]-s[1,1])^2),3,1,byrow=T);
		print("B");print(b);
		te=A%*%a+A%*%b;
		print("X1");print(x1);
		print("te");print(te);
		if(all(te>0)==FALSE)  
		{break;}
		else
		{
			x1=te;
			if(count>1000000)          			 {flag=2;break;}
			else if(abs(x1[1,1]-x[1,1])<0.0001)  {flag=1;break;}     
			else                                 {x=x1;count=count+1;}
		}
	}
	if(count!=0)
	{ans=sqrt(x1[,1]);}
	else
	{ans=sqrt(x);}
	#print("Des:");print(ans);
	#2_ 计算旋转矩阵_
		#2.1_ 计算距离比值_
		size=ans/(sqrt(ip[,1]^2+ip[,2]^2+f^2));
		#2.2_ 计算改化坐标_
		temp=cbind(ip,rep(-f,3));a1=temp[1,]*size[1];a2=temp[2,]*size[2];a3=temp[3,]*size[3];
		point=rbind(a1,a2,a3);
		#2.3_ 带入单位四元数绝对定向直接解，解算_
		result=UQM_FindEntrinsicParam(point,op);
		#2.4_ 由旋转矩阵推导旋转角_
		R=result$rotion;
		v1=atan2(-R[1,3],R[3,3])*180/pi;v2=asin(-R[2,3])*180/pi;v3=atan2(R[2,1],R[2,2])*180/pi;	
		print("jianyan");print(t(R)%*%R);
		print("Rotion");print(R);
		#2.5_ 返回结果
		return (list(init=c(v1,v2,v3),Rotion=R));
}
Test_Resection_direct_iter_solution2<-function(pointpath,f,cx,cy)
{ 
	mat=read.csv(pointpath);
	ip=mat[1:3,1:2];
	op=mat[1:3,3:5];
	io=diag(1,3);
	io[1,1]=sqrt((f^2)/2);io[2,2]=io[1,1];
	io[1,3]=cx;io[2,3]=cy;
	print(io);
	print(ip);
	print(op);
	Resection_direct_iter_solution2(io,ip,op);
}
Test_Resection_direct_iter_solution2_bat<-function(point_dirpath,intrinsicpath)
{
	op=matrix(c(0,0,0,0,5,0,8,5,0),3,3,byrow=T);
	#print(op);
	io=read.csv(intrinsicpath);
	#print(io);
	filelist=list.files(point_dirpath,full.name=T,pattern="csv")
	for(i in 1:length(filelist))
	{
		print(filelist[i]);
		mat=read.csv(filelist[i]);
		ip=mat[c(1,46,54),-1];
		print(ip);
		resultlist=Resection_direct_iter_solution2(io,ip,op);
		sink(paste(point_dirpath,"EOresult.txt"));
		print(resultlist$init);
		sink();
		print(resultlist);
		rm(mat);
	}
}
Test_Resection_direct_iter_solution2_single<-function(point_Filepath,outputpath,intrinsicpath)
{
	op=matrix(c(0,0,0,0,5,0,8,5,0),3,3,byrow=T);
	io=read.csv(intrinsicpath);
	mat=read.csv(point_Filepath);
	ip=mat[c(1,46,54),-1];
	resultlist=Resection_direct_iter_solution2(io,ip,op);
	sink(paste(outputpath,"EOresult.txt"),append=T);
	print(resultlist$init);
	sink();
	print(resultlist$init);
	rm(mat);
}