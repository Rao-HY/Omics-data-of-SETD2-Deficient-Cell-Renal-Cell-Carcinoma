library(ggplot2)
library(stringr)
library(grid)
Args <- commandArgs()
data=read.table(Args[6],header=T,sep="\t",quote = "",nrows=20)


pathCharNum=apply(data,1,nchar)[2,]

data[,2]=str_wrap(data[,2], width = 200)
maxPathCharNum = max(pathCharNum)
if (maxPathCharNum>200) {
  maxPathCharNum = 200
}
rowNumber = dim(data)[1]

yOrder=Args[7]
porder=factor(as.integer(rownames(data)),labels=rev(data[,2]))
if (yOrder=="pvalue_up"){
    dataOrder=data[order(data[,7],decreasing=FALSE),]
    porder=factor(as.integer(rownames(data)),labels=rev(dataOrder[,2]))
    data=dataOrder
} else if (yOrder=="pvalue_down") {
    dataOrder=data[order(data[,7],decreasing=T),]
    porder=factor(as.integer(rownames(data)),labels=rev(dataOrder[,2]))
    data=dataOrder
} else if (yOrder=="enrichment_up") {
    dataOrder=data[order(data[,9],decreasing=FALSE),]
    porder=factor(as.integer(rownames(data)),labels=rev(dataOrder[,2]))
    data=dataOrder
} else if (yOrder=="enrichment_down") {
    dataOrder=data[order(data[,9],decreasing=T),]
    porder=factor(as.integer(rownames(data)),labels=rev(dataOrder[,2]))
    data=dataOrder
} 

titleStr = paste("Top",rowNumber,"of",Args[8],"enrichment")
yTitle = paste(Args[8],"Term")

pp = ggplot(data,aes(data[,3]/data[,5],data[,2]))+ theme_bw()
pbubble = pp + geom_point(aes(y=rev(porder),size=data[,3],color=-1*log10(P.Value)))
legendHeigh = 0.5;


picHeigh = 2 + rowNumber*0.2
picWidth = maxPathCharNum*0.1 + 5

#图例字体大小设置
lengendSize = as.numeric(Args[15])
if (lengendSize == 0){
	lengendSize = 14;
	if(rowNumber<5) {
		lengendSize = 5+rowNumber*1
	}
}
#图例的大小

legendKeySize = as.numeric(Args[16])
if (legendKeySize == 0){
	legendKeySize = 0.3 + rowNumber*0.01;
}

# 图例
pr=pbubble + scale_colour_gradient(low="green",high="red") + theme(legend.position="right",legend.title=element_text(family="sans", size=lengendSize),legend.key.size=unit(legendKeySize,"cm")) 

#pr=pbubble + scale_colour_gradient(low="green",high="red") + theme(legend.position="right",legend.title=element_text(face="bold", family="sans", size=lengendSize),legend.key.size=unit(legendKeySize,"cm")) 
# 修改图例中圆圈的大小说明 
pr2=pr+labs(color=expression(-log[10](P-Value)),size="Gene number",family = "sans")
# 添加图的title 文字
pr3 = pr2+labs(x="Rich factor",y="",title=titleStr)

titleFontSize = as.numeric(Args[11])
axisFontSize = as.numeric(Args[12])

if (titleFontSize==0) {
	titleFontSize = 14+axisFontSize*0.05;
}

#axisFontSize = Args[12]
if (axisFontSize==0){
	axisFontSize = 16 - rowNumber*0.2;
	if (maxPathCharNum>80) {
		axisFontSize = 18 - rowNumber*0.2 - maxPathCharNum*0.01;
	}
}


xCoorFontSize = as.numeric(Args[13])
if (xCoorFontSize==0) {
	xCoorFontSize = axisFontSize-1;
}
yCoorFontSize = as.numeric(Args[14])
if (yCoorFontSize==0) {
	yCoorFontSize = axisFontSize-1;
}

# 设置X轴title文字样式
pr4= pr3 + theme(axis.title=element_text(family = "sans",size=axisFontSize)) 
# 设置X轴坐标文字的样式
pr5 = pr4 + theme(axis.text.x=element_text(family = "sans",size=xCoorFontSize)) 
# 设置Y轴坐标文字的样式
pr6 = pr5 + theme(axis.text.y=element_text(family = "sans",size=yCoorFontSize))

# 设置图title样式
pr7 = pr6 + theme(plot.title=element_text(family = "sans",hjust=0.5,size=titleFontSize)) 

#ggsave(pr7,file="D:\\tmp\\PATHWAY26-2.png",width=picWidth ,height= picHeigh ,dpi=100)
ggsave(pr7,file=Args[9],width=picWidth ,height= picHeigh ,dpi=500)
ggsave(pr7,file=Args[10],width=picWidth ,height= picHeigh ,dpi=500)
