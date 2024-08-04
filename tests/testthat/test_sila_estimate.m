% conduct the same SILA_estimate R unit test implemented in test-sila_estimate.R
function test_sila_estimate_aevent(align_event)
    arguments
        align_event {mustBeMember(align_event,["first","last","all"])}
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
    skern = 0;
    [tsila,~] = ILLA(age,value,subid,dt,val0,maxi,skern);
    tout = SILA_estimate(tsila,age,value,subid,align_event=align_event);

    writetable(tout, strcat("test_sila_estimate_matlab_aevent_",align_event,".csv"));
end

test_sila_estimate_aevent("last")
test_sila_estimate_aevent("first")