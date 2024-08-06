% conduct the same SILA R unit test implemented in test-sila.R
% add filepath for SILA functions to Matlab search path
path_sila = fileparts(mfilename('fullpath'));
addpath(fullfile(path_sila,'..','..','SILA-AD-Biomarker'))

subid = [1; 1; 1; 2; 2; 2; 2; 2; 2];
age = [linspace(50, 70, 3)'; linspace(50, 70, 6)'];
value = [2.14; 3.94; 6.04; 2.06; 4.04; 5.99; 8.15; 9.99; 12.2];
dt = 2;
val0 = 2;
maxi = 100;
[tsila,tdrs] = SILA(age,value,subid,dt,val0,maxi);

writetable(tsila, "test_sila_matlab_tsila.csv");
writetable(tdrs, "test_sila_matlab_tdrs.csv");
