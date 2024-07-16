% conduct the same ILLA R unit test implemented in test-illa.R
subid = [1; 1; 1; 2; 2; 2; 2; 2; 2];
age = [linspace(50, 70, 3)'; linspace(50, 70, 6)'];
value = [2.14; 3.94; 6.04; 2.06; 4.04; 5.99; 8.15; 9.99; 12.2];
dt = 2;
val0 = 2;
maxi = 100;
[tout,tdrs] = ILLA(age,value,subid,dt,val0,maxi,0);
writetable(tout, "test_illa_matlab_tout_skern0.csv")
writetable(tdrs, "test_illa_matlab_tdrs_skern0.csv")