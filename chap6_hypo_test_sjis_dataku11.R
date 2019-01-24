#�܂��f�[�^��ǂݍ��݂܂��傤
#setwd("?/statistics-intro/")
#���߂̃R�c���w��
veh_data<-read.csv("data/data11ku.csv")
head(veh_data)

#��������
#�s�s�ƍx�O�Ńf�[�^���킯�܂��B�f�[�^�ǂݍ���
urban<-veh_data[veh_data$urb==1, ]#�s�s
suburb<-veh_data[veh_data$urb!=1, ]#�x�O

# t����
# �s�s�ƍx�O�ŕБ�����
t.test(urban$vehicles_owend,suburb$vehicles_owend, alternative = "greater")
# ����p�y��t = 3.4872�Ap-value = 0.000614�A0.0614%�Ŋ��p�ł���
boxplot(urban$vehicles_owend,
        suburb$vehicles_owend)

# �s�s�Ǝs���w�̐��ŗ�������
unique(veh_data$ward)

cent_urb_data<-veh_data[veh_data$ward=="cent_urb",]
city1_data<-veh_data[veh_data$ward=="city1",]

t.test(cent_urb_data$no_of_sta, 
       city1_data$no_of_sta, 
       var.equal = FALSE,
       alternative = "two.sided")
# ����p�y��t = 6.1998�Ap-value = 0.008182�A0.82%�Ŋ��p�ł���



# �l���ʐ��O���t�ŉw�����r����
boxplot(cent_urb_data$no_of_sta,
        city1_data$no_of_sta)

t.test(cent_urb_data$no_of_sta, 
       city1_data$no_of_sta,
       var.equal = FALSE,
       alternative = "two.sided")

#�q�X�g�O�������ׂĂ݂�
management_hr<-hr_data[hr_data$sales=="management", ]
par(mfrow=c(1,2))
hist(sales_hr$satisfaction_level)
hist(management_hr$satisfaction_level)
par(mfrow=c(1,1))
