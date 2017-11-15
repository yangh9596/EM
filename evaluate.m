clear;
clc;
load('car.mat');
load('theata.mat');

label_true = car(:,7);
label_pred = zeros(length(car),1);
prob = zeros(length(car),4);

for i = 1:length(car)
    prob(i,1) = Pyk(1)*df1(car(i,1),1)*df1(car(i,2),2)*df1(car(i,3),3)*df1(car(i,4),4)*df1(car(i,5),5)*df1(car(i,6),6);
    prob(i,2) = Pyk(2)*df2(car(i,1),1)*df2(car(i,2),2)*df2(car(i,3),3)*df2(car(i,4),4)*df2(car(i,5),5)*df2(car(i,6),6);
    prob(i,3) = Pyk(3)*df3(car(i,1),1)*df3(car(i,2),2)*df3(car(i,3),3)*df3(car(i,4),4)*df3(car(i,5),5)*df3(car(i,6),6);
    prob(i,4) = Pyk(4)*df4(car(i,1),1)*df4(car(i,2),2)*df4(car(i,3),3)*df4(car(i,4),4)*df4(car(i,5),5)*df4(car(i,6),6);
end
for i = 1:length(car)
    [~,label_pred(i)] = max(prob(i,:));
end
csvwrite('label_true.csv',label_true);
csvwrite('label_pred.csv',label_pred);


index1 = find(label_true == 1);
TP_1 = sum(label_pred(index1) == 1);
TotalPredicted_1 = sum(label_pred == 1);
TotalGoldLabel_1 = sum(label_true == 1);
precision1 = TP_1/TotalPredicted_1;
recall1 = TP_1/TotalGoldLabel_1;

index2 = find(label_true == 2);
TP_2 = sum(label_pred(index2) == 2);
TotalPredicted_2 = sum(label_pred == 2);
TotalGoldLabel_2 = sum(label_true == 2);
precision2= TP_2/TotalPredicted_2;
recall2 = TP_2/TotalGoldLabel_2;

index3 = find(label_true == 3);
TP_3 = sum(label_pred(index3) == 3);
TotalPredicted_3 = sum(label_pred == 3);
TotalGoldLabel_3 = sum(label_true == 3);
precision3= TP_3/TotalPredicted_3;
recall3 = TP_2/TotalGoldLabel_3;

index4 = find(label_true == 4);
TP_4 = sum(label_pred(index4) == 4);
TotalPredicted_4 = sum(label_pred == 4);
TotalGoldLabel_4 = sum(label_true == 4);
precision4= TP_4/TotalPredicted_4;
recall4 = TP_4/TotalGoldLabel_4;

precision = (precision1+precision2+precision3+precision4)/4
recall = (recall1+recall2+recall3+recall4)/4
F = 2*precision*recall/(precision+recall)
accuracy = sum(label_pred==label_true)/length(car)
