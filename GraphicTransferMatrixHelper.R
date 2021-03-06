#====================================================
#用于生成基本的图形变化矩阵的帮助器
#====================================================
#----------------------------------------------------

#===================================================
#函数名称：生成平移矩阵
#函数参数：dx，dy图像点需要平移的变化量
#函数返回：平移矩阵（齐次表达）
#===================================================
#----------------------------------------------------
GraphicTransferMatrixHelper_translation<-function(dx,dy)
{
  return (matrix(c(1,0,0,0,1,0,dx,dy,1),nrow=3,byrow=T));
}
#===================================================
#函数名称：生成旋转矩阵
#函数参数：angel图像点需要旋转的角度,Israd标示符，指定当前角度是否是弧度表示
#函数返回：旋转矩阵（齐次表达）
#===================================================
#----------------------------------------------------
GraphicTransferMatrixHelper_rotate<-function(angel,Israd=F)
{
  if(Israd==F) {angel<-(angel*pi)/180;}
  ca<-cos(angel);sa<-sin(angel);
  return (matrix(c(ca,sa,0,-sa,ca,0,0,0,1),nrow=3,byrow=T));
}
#===================================================
#函数名称：生成缩放矩阵
#函数参数：Sx，Sy图像点中x，y所需缩放的比例
#函数返回：旋转矩阵（齐次表达）
#===================================================
#----------------------------------------------------
GraphicTransferMatrixHelper_scale<-function(Sx,Sy)
{
  return (matrix(c(Sx,0,0,0,Sy,0,0,0,1),nrow=3,byrow=T));
}
#===================================================
#函数名称：生成错切矩阵
#函数参数：dx，dy图像点沿x、y轴方向的倾斜角度
#          Israd 标识符用于表示该角度是否由是弧度表示
#函数返回：错切矩阵（齐次表达）
#===================================================
#---------------------------------------------------
GraphicTransferMatrixHelper_cuoqie<-function(dx,dy,Israd=F)
{
  if(Israd==F) {dx=dx*180/pi;dy=dy*pi/180;}
  dx=tan(dx);dy=tan(dy);
  return(matrix(c(1,dy,0,dx,1,0,0,0,1),nrow=3,byrow=T));
}
#===================================================
#函数名称：生成图像沿x轴、y轴等特殊直线对称矩阵
#函数参数：type 待对称的特殊直线类型标识
#          1 x轴 2 y轴 3 45°对角线 4 -45°对角线 5 坐标原点
#函数返回：错切矩阵（齐次表达）
#===================================================
#---------------------------------------------------
GraphicTransferMatrixHelper_mirror<-function(type)
{
  if(type==1)
  {return(matrix(c(1,0,0,0,-1,0,0,0,1),nrow=3,byrow=T));}
  else if(type==2)
  {return(matrix(c(-1,0,0,0,1,0,0,0,1),nrow=3,byrow=T));}
  else if(type==3)
  {return(matrix(c(0,1,0,1,-0,0,0,0,1),nrow=3,byrow=T));}
  else if(type==4)
  {return(matrix(c(0,-1,0,-1,0,0,0,0,1),nrow=3,byrow=T));}
  else if(type==5)
  {return(matrix(c(-1,0,0,0,-1,0,0,0,1),nrow=3,byrow=T));}
  else
  {return(matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,byrow=T));}
}
#===================================================
#函数名称：生成图像沿点(u,v)逆时针旋转a角度的变化矩阵
#函数参数：(u,v)旋转时的固定点
#          a 图形逆时针旋转的角度
#          Israd 标识符用于表示当前旋转角度是否由弧度表示
#函数返回：变化矩阵（齐次表达）
#===================================================
#---------------------------------------------------
GraphicTransferMatrixHelper_rotatebypoint<-function(u,v,a,Israd=F)
{
  if(Israd==F) {a=a*pi/180;}
  ca=cos(a);sa=sin(a);
  return (matrix(c(ca,sa,0,-sa,ca,0,u*(1-ca)+v*sa,-u*sa+v*(1-ca),1),nrow=3,byrow=T));
}
#===================================================
#函数名称：生成图像沿直线ax+by+c=0对称变化矩阵
#函数参数：直线一般式的各项系数
#函数返回：变化矩阵（齐次表达）
#===================================================
#---------------------------------------------------
GraphicTransferMatrixHelper_mirrorbyline<-function(a,b,c)
{
  s<-atan2(-a,b);ca<-cos(2*s);sa<-sin(2*s);t=c/a;
  return(matrix(c(ca,sa,0,sa,-ca,0,t*(ca-1),t*sa,1),nrow=3,byrow=T));
}