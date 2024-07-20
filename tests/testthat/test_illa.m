% conduct the same ILLA R unit test implemented in test-illa.R
function test_illa_with_skern(skern)
    arguments
        skern double {mustBeNonnegative}
    end

    % add filepath for SILA functions to Matlab search path
    path_sila = fileparts(mfilename('fullpath'));
    addpath(fullfile(path_sila,'..','..','SILA-AD-Biomarker'))

    subid = [1; 1; 1; 2; 2; 2; 2; 2; 2];
    age = [linspace(50, 70, 3)'; linspace(50, 70, 6)'];
    value = [2.14; 3.94; 6.04; 2.06; 4.04; 5.99; 8.15; 9.99; 12.2];
    dt = 2;
    val0 = 2;
    maxi = 100;
    [tout,tdrs] = ILLA(age,value,subid,dt,val0,maxi,skern);

    writetable(tout, strcat("test_illa_matlab_tout_skern", string(skern),".csv"));
    writetable(tdrs, strcat("test_illa_matlab_tdrs_skern", string(skern),".csv"));
end

test_illa_with_skern(0)
test_illa_with_skern(0.5)