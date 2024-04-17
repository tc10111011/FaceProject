subjID = input('Please enter subject ID: ', 's');
numStimuli = 15;
combinations = nchoosek(1:numStimuli, 2);
shuffled = combinations(randperm(length(combinations)), :);% Randomly shuffle the pairs for presentation
for trialNum = 1:length(combinations) % the experiment
    currentTrial = shuffled(trialNum, :);
    lateralization = randi([0, 1]);  % 0 for left, 1 for right, randomly determine the lateralization (left or right)
    if lateralization == 0 % Store the positions of the stimuli for this trial
            lateralisedRecord(trialNum,1) = currentTrial(1);
            lateralisedRecord(trialNum,3) = currentTrial(2);
    else
            lateralisedRecord(trialNum,1) = currentTrial(2);
            lateralisedRecord(trialNum,3) = currentTrial(1);
    end 
    h1 = figure('WindowState','maximized');
    figure(h1)    
    subplot(1, 2, 1); % Display the first image on the left side
    imshow("FaceGenMLCM/" + num2str(lateralisedRecord(trialNum,1)) + ".png");
    subplot(1, 2, 2);  % Display the second image on the right side
    imshow("FaceGenMLCM/" + num2str(lateralisedRecord(trialNum,3)) + ".png");
    lateralisedRecord(trialNum, 2) = input('Left(1) or Right(2)'); 
    resultsFilename = [subjID '_experiment_data.mat'];
    save(resultsFilename,'lateralisedRecord');
    close(h1)         
end
disp('Over')
MLCMFood(:, 1) = lateralisedRecord(:,2);%new matrix to feed into MLCM
for trialNum = 1:length(lateralisedRecord)%coding the lighting
    for face = 1:2:3
        if lateralisedRecord(trialNum,face) <= 5 
            MLCMFood(trialNum, face+1) = 1;
        elseif lateralisedRecord(trialNum,face) >5 & lateralisedRecord(trialNum,face) <= 10
            MLCMFood(trialNum, face+1) = 2;
        else 
            MLCMFood(trialNum, face+1) = 3;
        end 
    end
end
for trialNum = 1:length(lateralisedRecord)%coding the stretch
    for face = 1:2:3
        for stretch = 1:5
            for multiple = 0:5:10
                if lateralisedRecord(trialNum,face) == multiple + stretch
                   MLCMFood(trialNum, face+2) = stretch;
                end
            end 
        end 
    end
end
save(resultsFilename,"MLCMFood")