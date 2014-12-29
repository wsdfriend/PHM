#################################################### 
# 利用单位四元数解算外部元素_
# 函数名UQM_FindEntrinsicParam_
# 参数_ point_ 像空间坐标点_
# 参数_ objectpoint_ 物方空间坐标点_
# 函数返回_ 列表
# scales 比例系数
# rotion 旋转矩阵
# trans 平移参数
##################################################

UQM_FindEntrinsicParam<-function(point,objectpoint)
{
#1.合法性检验_
if(dim(point)[1]!=dim(objectpoint)[1])
	{
		cat("_点数不匹配，请检查长度_");
		return (0);
	}
	# 2四元数解算_7_参数_
	#2.1 重心化_
	point_mean=apply(point,2,mean);
	objectpoint_mean=apply(objectpoint,2,mean);
	point=point-rbind(point_mean,point_mean,point_mean);#注意矩阵与向量之间的减法运算
	objectpoint=objectpoint-rbind(objectpoint_mean,objectpoint_mean,objectpoint_mean);
	#print(objectpoint);
	 #2.1 求解比例因子_
	 s=sqrt(sum(objectpoint^2)/sum(point^2));
	 #2.2 求解旋转矩阵_
	  #2.2.1 构建_M_阵元素_
	  sxx = sum(point[,1]*objectpoint[,1]);
	  sxy = sum(point[,1]*objectpoint[,2]);
	  sxz = sum(point[,1]*objectpoint[,3]);
	  syy = sum(point[,2]*objectpoint[,2]);
	  syx = sum(point[,2]*objectpoint[,1]);
	  syz = sum(point[,2]*objectpoint[,3]);
	  szz = sum(point[,3]*objectpoint[,3]);
	  szx = sum(point[,3]*objectpoint[,1]);
	  szy = sum(point[,3]*objectpoint[,2]);
	  #3.2.2 构建N矩阵
	  n=c(
		  sxx+syy+szz,syz-szy,szx-sxz,sxy-syx,
	      syz-szy,sxx-syy-szz,sxy+syx,szx+sxz,
		  szx-sxz,sxy+syx,-sxx+syy-szz,syz+szy,
		  sxy-syx,szx+sxz,syz+szy,-sxx-syy+szz
		  );
	  N=matrix(n,4,4,byrow=T);
	  #2.2.3 解算特征值_
	  resultlist=eigen(N);
	  #2.2.4 计算最大特征向量_
	  meige=resultlist[[2]][,which.max(resultlist[[1]])];#R返回的的是列向量组成的矩阵，故特征向量应为列向量
	  q0=meige[1];qx=meige[2];qy=meige[3];qz=meige[4];
	  #3.2.5 组合为旋转矩阵_
	  r1=c(1-2*qy^2-2*qz^2, 2*qx*qy-2*q0*qz, 2*qx*qz+2*q0*qy);
	  r2=c(2*qx*qy+2*q0*qz, 1-2*qx^2-2*qz^2, 2*qy*qz-2*q0*qx);
	  r3=c(2*qx*qz-2*q0*qy, 2*qy*qz+2*q0*qx, 1-2*qx^2-2*qy^2);
	  R=rbind(r1,r2,r3);
	 #3.3 求解平移变量_
	  #x=t(point_mean)-(as.numeric(s)*R)%*%t(objectpoint);
	#4 返回结果_
	resultlist=list(scales=s,rotion=R,trans=1);
	return(resultlist);
}

Test_UQM_FindEntrinsicParam<-function(ippath,oppath)
{
	ip=read.csv(ippath);
	op=read.csv(oppath);
	
	result=UQM_FindEntrinsicParam(ip,op);
	return(result);
}
