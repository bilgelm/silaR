% add filepath for SILA functions to Matlab search path
path_sila = fileparts(mfilename('fullpath'));
addpath(fullfile(path_sila,'..','..','SILA-AD-Biomarker'))
addpath(fullfile(path_sila,'..','..','SILA-AD-Biomarker','demo'))

t = simulate_data();
writetable(t, "test_demo_matlab_simulated_data.csv")

[tsila,tdrs] = SILA(t.age,t.val,t.subid,0.25,21,200);
writetable(tsila, "test_demo_matlab_tsila.csv");
writetable(tdrs, "test_demo_matlab_tdrs.csv");