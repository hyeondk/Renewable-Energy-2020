# 2019.07.23
# Survey sampling for renewable energy
#
getwd()
setwd("I:/data/")
getwd()
dirpath = "I:/data/" 

#----------------------------------------------------------------------------------------------
# Read a txt file
#
#  1.���ü�Ϸù�ȣ(8)	 2.������س⵵(4)     3.���������ڵ�(7)        4.â���⵵(4)          5.���������ڵ�(1)
#  6.���ü�����ڵ�(1)   7.�����з��ڵ�(1)   8.�ֻ������з��ڵ�(5)  9.���ٷ����հ�(5)   10.���ٷ��ڳ�(5) 
# 11.���ٷ��ڿ�(4)    12.�������հ�(5)      13.�����ڼ���(5)         14.�����ڼ���(4)       15.�����(8)

wd = c(8,4,7,4,1,1,1,5,5,5,4,5,5,4,8)  # the lengths of the iterms
raw = read.fwf("I:/data/data/2017�������ü����_ǥ����������(Step1).txt",widths = wd, header = FALSE)
filename = paste0(dirpath,"rawdata",".csv")
write.csv(raw,filename)

data = read.csv("I:/data/result/survey.csv",sep=",", header = TRUE)
data1 = read.csv("I:/data/result/survey1.csv",sep=",", header = TRUE)
data2 = read.csv("I:/data/result/survey2.csv",sep=",", header = TRUE)
data3 = read.csv("I:/data/result/survey3.csv",sep=",", header = TRUE)
data4 = read.csv("I:/data/result/survey4.csv",sep=",", header = TRUE)
# read exit
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# Read KSIC, the number of population and the number of samples
#
# (1) ksic  (2) 5�ι̸������ܼ� (3) 5�ι̸���ũ��ǥ���� (4) 10�ι̸������ܼ� (5) 10�ι̸���ũ��ǥ���� 
# (6) 50�ι̸������ܼ� (7) 50�ι̸���ũ��ǥ����  (8) 50���̻�����ܼ�  (9) 50���̻�ũ��ǥ����


ksic = read.csv("I:/data/result/KSIC.csv",sep=",", header=FALSE)

nksic = nrow(ksic)
mksic = ncol(ksic)



#---------------------------------------------------------------------------------------------
# sampling by KSIc
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 5�� �̸� ��ũ��ǥ�� �Ҵ�
#---------------------------------------------------------------------------------------------

ch_num = matrix(rep(0,nksic),nksic,1) 
  
for (i in 1:nksic)
{
  # Match ksic
  ch = data1[,8] == ksic[i,1]
  sample_data = data1[ch,]
  
  ch_num[i,1]= nrow(sample_data)  # ksic�� �����ܼ�
  
  
  # sort by V12 �����ڼ� and systematic sampling by V12
  sort_sample = sort(sample_data[,12],index.return=TRUE)
  if (ksic[i,3]==0) ksic[i,3]=1  # ǥ���� 0->1
  if (ksic[i,2]==0) ksic[i,2]=1  # �����ܼ� 0->1
  num_group   = floor(ksic[i,2]/ksic[i,3])  # ��ޱ���

  id_sample = sample(1:num_group,1)
  id_sample = c(id_sample,id_sample-1+seq.int(num_group-1,num_group*(ksic[i,3]),num_group))
  id_sample = c(id_sample,sample((num_group*(ksic[i,3]-1)+1):ksic[i,2],1))

  sort_sample_data = sample_data[sort_sample$ix,]
  sampled_data = sort_sample_data[id_sample,]      # Final data sampled by KSIC


  # Append all lists
  if (i == 1)
  {
    list_data = sampled_data
  }
  else
  {
    list_data = rbind(list_data,sampled_data)
  }
    rm(list=c("ch","sample_data","sort_sample","id_sample","sort_sample_data","sampled_data"))  
}

# Save all list
filename = paste0(dirpath,"list_less5",".csv")
write.csv(list_data,filename)


#---------------------------------------------------------------------------------------------
# 10�� �̸� ��ũ��ǥ�� �Ҵ�
#---------------------------------------------------------------------------------------------
ch_num = matrix(rep(0,nksic),nksic,1) 

for (i in 1:nksic)
{
  # Match ksic
  ch = data2[,8] == ksic[i,1]
  sample_data = data2[ch,]
  
  ch_num[i,1]= nrow(sample_data)  # ksic�� �����ܼ�
  
  
  # sort by V12 �����ڼ� and systematic sampling by V12
  sort_sample = sort(sample_data[,12],index.return=TRUE)
  if (ksic[i,5]==0) ksic[i,5]=1
  if (ksic[i,4]==0) ksic[i,4]=1
  num_group   = floor(ksic[i,4]/ksic[i,5])

  id_sample = sample(1:num_group,1)
  id_sample = c(id_sample,id_sample-1+seq.int(num_group-1,num_group*(ksic[i,5]),num_group))
  id_sample = c(id_sample,sample((num_group*(ksic[i,5]-1)+1):ksic[i,4],1))

  sort_sample_data = sample_data[sort_sample$ix,]
  sampled_data = sort_sample_data[id_sample,]      # Final data sampled by KSIC
  
  # Append all lists
  if (i == 1)
  {
    list_data = sampled_data
  }
  else
  {
    list_data = rbind(list_data,sampled_data)
  }
  rm(list=c("ch","sample_data","sort_sample","id_sample","sort_sample_data","sampled_data"))
}

# Save all list
filename = paste0(dirpath,"list_less10",".csv")
write.csv(list_data,filename)


#---------------------------------------------------------------------------------------------
# 50�� �̸� ��ũ��ǥ�� �Ҵ�
#---------------------------------------------------------------------------------------------
ch_num = matrix(rep(0,nksic),nksic,1) 

for (i in 1:nksic)
{
  # Match ksic
  ch = data3[,8] == ksic[i,1]
  sample_data = data3[ch,]
  
  ch_num[i,1]= nrow(sample_data)  # ksic�� �����ܼ�
  
  
  # sort by V12 �����ڼ� and systematic sampling by V12
  sort_sample = sort(sample_data[,12],index.return=TRUE)
  if (ksic[i,7]==0) ksic[i,7]=1
  if (ksic[i,6]==0) ksic[i,6]=1
  num_group   = floor(ksic[i,6]/ksic[i,7])

  id_sample = sample(1:num_group,1)
  id_sample = c(id_sample,id_sample-1+seq.int(num_group-1,num_group*(ksic[i,7]),num_group))
  id_sample = c(id_sample,sample((num_group*(ksic[i,7]-1)+1):ksic[i,6],1))

  sort_sample_data = sample_data[sort_sample$ix,]
  sampled_data = sort_sample_data[id_sample,]      # Final data sampled by KSIC
  
  # Append all lists
  if (i == 1)
  {
    list_data = sampled_data
  }
  else
  {
    list_data = rbind(list_data,sampled_data)
  }
  rm(list=c("ch","sample_data","sort_sample","id_sample","sort_sample_data","sampled_data"))
}

# Save all list
filename = paste0(dirpath,"list_less50",".csv")
write.csv(list_data,filename)


#---------------------------------------------------------------------------------------------
# 50�� �̻� ��ũ��ǥ�� �Ҵ�
#---------------------------------------------------------------------------------------------
ch_num = matrix(rep(0,nksic),nksic,1) 

for (i in 1:nksic)
{
  # Match ksic
  ch = data4[,8] == ksic[i,1]
  sample_data = data4[ch,]
  
  ch_num[i,1]= nrow(sample_data)  # ksic�� �����ܼ�
  
  
  # sort by V12 �����ڼ� and systematic sampling by V12
  sort_sample = sort(sample_data[,12],index.return=TRUE)
  if (ksic[i,9]==0) ksic[i,9]=1
  if (ksic[i,8]==0) ksic[i,8]=1
  num_group   = floor(ksic[i,8]/ksic[i,9])
  
  
  id_sample = sample(1:num_group,1)
  id_sample = c(id_sample,id_sample-1+seq.int(num_group-1,num_group*(ksic[i,9]),num_group))
  id_sample = c(id_sample,sample((num_group*(ksic[i,9]-1)+1):ksic[i,8],1))
  
  
  sort_sample_data = sample_data[sort_sample$ix,]
  sampled_data = sort_sample_data[id_sample,]      # Final data sampled by KSIC
  
  # Append all lists
  if (i == 1)
  {
    list_data = sampled_data
  }
  else
  {
    list_data = rbind(list_data,sampled_data)
  }
  rm(list=c("ch","sample_data","sort_sample","id_sample","sort_sample_data","sampled_data"))
}

# Save all list
filename = paste0(dirpath,"list_more50",".csv")
write.csv(list_data,filename)
