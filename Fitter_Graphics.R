Filter_Graphics_getpath<-function()
{
  return (c("F:\\cyoubo_lab_eo\\diff_nuclear\\Data1\\DataR.csv",
            "F:\\cyoubo_lab_eo\\diff_nuclear\\Data1\\fitter14641.csv",
            "F:\\cyoubo_lab_eo\\diff_nuclear\\Data1\\fitter11111.csv"
            ))
  
}
Filter_Graphics_OneAxes_MuliState<-function(paths,count)
{
  cols=c("orange","blue","green","yellow","black","pink")
  datas=matrix(c(1:count),count,1,byrow=T);
    
  for(i in c(1:length(paths)))
    {
      col=read.csv(paths[i],header=T);
      datas=cbind(datas,col[1:count,1]);
    }
  
  dfm_data=data.frame(datas);
  print(dfm_data[1:4,]);

  plot(x=dfm_data[,1],y=dfm_data[,2],ylim=c(min(dfm_data[,2]),max(dfm_data[,2])),type="l",col="red",main="EO_Data_X",ylab="values");
  
  for(i in c(3:(length(paths)+1)))
  {
    print(i);
    lines(x=dfm_data[,1],y=dfm_data[,i],col=cols[i-1],lwd=2)
    
  }
  
  
  Max=apply(datas[-1,],2,max); Mix=apply(datas[-1,],2,min);  Ave=apply(datas[-1,],2,mean);  Sd=apply(datas[-1,],2,sd);
  resultfrm=data.frame(Max,Mix,Ave,Sd);
  
  print(resultfrm[-1,]);
  
  
}
