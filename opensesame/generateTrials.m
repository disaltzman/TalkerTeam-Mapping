clear all
recycleTargets = 'no'; %options: yes (E1, E2), no (E3, E4)
mixedTargetTalker = 'one'; %options: both (E1, E3), one (E2, E4)

%% SETUP
% We will choose the set of distractors based on whether we are recycling
% targets
if strcmp(recycleTargets,'yes')==1
    mBallDistractors = ["M_cave.wav";"M_tile.wav";"M_done.wav";"M_bluff.wav";"M_cad.wav";"M_cling.wav";"M_depth.wav";"M_dime.wav";"M_gnash.wav";"M_greet.wav";"M_jaw.wav";"M_jolt.wav";"M_knife.wav";"M_lash.wav";"M_park.wav";"M_priest.wav";"M_reek.wav";"M_romp.wav"];
    mCaveDistractors = ["M_ball.wav";"M_tile.wav";"M_done.wav";"M_bluff.wav";"M_cad.wav";"M_cling.wav";"M_depth.wav";"M_dime.wav";"M_gnash.wav";"M_greet.wav";"M_jaw.wav";"M_jolt.wav";"M_knife.wav";"M_lash.wav";"M_park.wav";"M_priest.wav";"M_reek.wav";"M_romp.wav"];
    mTileDistractors = ["M_ball.wav";"M_cave.wav";"M_done.wav";"M_bluff.wav";"M_cad.wav";"M_cling.wav";"M_depth.wav";"M_dime.wav";"M_gnash.wav";"M_greet.wav";"M_jaw.wav";"M_jolt.wav";"M_knife.wav";"M_lash.wav";"M_park.wav";"M_priest.wav";"M_reek.wav";"M_romp.wav"];
    mDoneDistractors = ["M_ball.wav";"M_cave.wav";"M_tile.wav";"M_bluff.wav";"M_cad.wav";"M_cling.wav";"M_depth.wav";"M_dime.wav";"M_gnash.wav";"M_greet.wav";"M_jaw.wav";"M_jolt.wav";"M_knife.wav";"M_lash.wav";"M_park.wav";"M_priest.wav";"M_reek.wav";"M_romp.wav"];
    
    fBallDistractors = ["F_cave.wav";"F_tile.wav";"F_done.wav";"F_bluff.wav";"F_cad.wav";"F_cling.wav";"F_depth.wav";"F_dime.wav";"F_gnash.wav";"F_greet.wav";"F_jaw.wav";"F_jolt.wav";"F_knife.wav";"F_lash.wav";"F_park.wav";"F_priest.wav";"F_reek.wav";"F_romp.wav"];
    fCaveDistractors = ["F_ball.wav";"F_tile.wav";"F_done.wav";"F_bluff.wav";"F_cad.wav";"F_cling.wav";"F_depth.wav";"F_dime.wav";"F_gnash.wav";"F_greet.wav";"F_jaw.wav";"F_jolt.wav";"F_knife.wav";"F_lash.wav";"F_park.wav";"F_priest.wav";"F_reek.wav";"F_romp.wav"];
    fTileDistractors = ["F_ball.wav";"F_cave.wav";"F_done.wav";"F_bluff.wav";"F_cad.wav";"F_cling.wav";"F_depth.wav";"F_dime.wav";"F_gnash.wav";"F_greet.wav";"F_jaw.wav";"F_jolt.wav";"F_knife.wav";"F_lash.wav";"F_park.wav";"F_priest.wav";"F_reek.wav";"F_romp.wav"];
    fDoneDistractors = ["F_ball.wav";"F_cave.wav";"F_tile.wav";"F_bluff.wav";"F_cad.wav";"F_cling.wav";"F_depth.wav";"F_dime.wav";"F_gnash.wav";"F_greet.wav";"F_jaw.wav";"F_jolt.wav";"F_knife.wav";"F_lash.wav";"F_park.wav";"F_priest.wav";"F_reek.wav";"F_romp.wav"];
    
elseif strcmp(recycleTargets, 'no') == 1
    mBallDistractors = ["M_bluff.wav";"M_cad.wav";"M_cling.wav";"M_depth.wav";"M_dime.wav";"M_gnash.wav";"M_greet.wav";"M_jaw.wav";"M_jolt.wav";"M_knife.wav";"M_lash.wav";"M_park.wav";"M_priest.wav";"M_reek.wav";"M_romp.wav"];
    mCaveDistractors = ["M_bluff.wav";"M_cad.wav";"M_cling.wav";"M_depth.wav";"M_dime.wav";"M_gnash.wav";"M_greet.wav";"M_jaw.wav";"M_jolt.wav";"M_knife.wav";"M_lash.wav";"M_park.wav";"M_priest.wav";"M_reek.wav";"M_romp.wav"];
    mTileDistractors = ["M_bluff.wav";"M_cad.wav";"M_cling.wav";"M_depth.wav";"M_dime.wav";"M_gnash.wav";"M_greet.wav";"M_jaw.wav";"M_jolt.wav";"M_knife.wav";"M_lash.wav";"M_park.wav";"M_priest.wav";"M_reek.wav";"M_romp.wav"];
    mDoneDistractors = ["M_bluff.wav";"M_cad.wav";"M_cling.wav";"M_depth.wav";"M_dime.wav";"M_gnash.wav";"M_greet.wav";"M_jaw.wav";"M_jolt.wav";"M_knife.wav";"M_lash.wav";"M_park.wav";"M_priest.wav";"M_reek.wav";"M_romp.wav"];
    
    fBallDistractors = ["F_bluff.wav";"F_cad.wav";"F_cling.wav";"F_depth.wav";"F_dime.wav";"F_gnash.wav";"F_greet.wav";"F_jaw.wav";"F_jolt.wav";"F_knife.wav";"F_lash.wav";"F_park.wav";"F_priest.wav";"F_reek.wav";"F_romp.wav"];
    fCaveDistractors = ["F_bluff.wav";"F_cad.wav";"F_cling.wav";"F_depth.wav";"F_dime.wav";"F_gnash.wav";"F_greet.wav";"F_jaw.wav";"F_jolt.wav";"F_knife.wav";"F_lash.wav";"F_park.wav";"F_priest.wav";"F_reek.wav";"F_romp.wav"];
    fTileDistractors = ["F_bluff.wav";"F_cad.wav";"F_cling.wav";"F_depth.wav";"F_dime.wav";"F_gnash.wav";"F_greet.wav";"F_jaw.wav";"F_jolt.wav";"F_knife.wav";"F_lash.wav";"F_park.wav";"F_priest.wav";"F_reek.wav";"F_romp.wav"];
    fDoneDistractors = ["F_bluff.wav";"F_cad.wav";"F_cling.wav";"F_depth.wav";"F_dime.wav";"F_gnash.wav";"F_greet.wav";"F_jaw.wav";"F_jolt.wav";"F_knife.wav";"F_lash.wav";"F_park.wav";"F_priest.wav";"F_reek.wav";"F_romp.wav"];
end
    
    %% MIXED TRIALS
    
    %First, figure out which positions will contain targets
    %Targets can only appear in positions 2-15 and must be separated by at
    %least one distractor
    
    targets = zeros(96*2,4);
    for i = 1:48
        targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        while targets(i,4)-1 == targets(i,3) || targets(i,3)-1 == targets(i,2) || targets(i,2)-1 == targets(i,1)
            targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        end
    end
    
    %Next, figure out which positions must have distractors (based on which ones
    %have targets)
    allPos = [1:16];
    for i = 1:48
        distractors(i,:) = setdiff(allPos, targets(i,:));
    end
    
    %Create a matrix that will contain all the stimulus names
    stimuli = zeros(96,16); stimuli = string(stimuli);
    
    % And now we load stimuli
    % This will depend on whether both talkers produce the targets on a given
    % trial (each producing half)
    % or whether just one talker produces all the targets for a given trial
    % (and then distractors are used to make sure each talker produces the same
    % number of stimuli in a trial)
    if strcmp(mixedTargetTalker, 'both') == 1
        
        % trials with BALL target
        for i = 1:12
            mStim = randsample(targets(i,:),2);
            fStim = setdiff(targets(i,:), mStim);
            stimuli(i, mStim) = "M_ball.wav";
            stimuli(i, fStim) = "F_ball.wav";
            
            trialDistractors = [randsample(fBallDistractors,6, true);randsample(mBallDistractors,6, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        % trials with CAVE target
        for i = 13:24
            mStim = randsample(targets(i,:),2);
            fStim = setdiff(targets(i,:), mStim);
            stimuli(i, mStim) = "M_cave.wav";
            stimuli(i, fStim) = "F_cave.wav";
            
            trialDistractors = [randsample(fCaveDistractors,6, true);randsample(mCaveDistractors,6, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        
        % trials with TILE target
        for i = 25:36
            mStim = randsample(targets(i,:),2);
            fStim = setdiff(targets(i,:), mStim);
            stimuli(i, mStim) = "M_tile.wav";
            stimuli(i, fStim) = "F_tile.wav";
            
            trialDistractors = [randsample(fTileDistractors,6, true);randsample(mTileDistractors,6, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        
        % trials with DONE target
        for i = 37:48
            mStim = randsample(targets(i,:),2);
            fStim = setdiff(targets(i,:), mStim);
            stimuli(i, mStim) = "M_done.wav";
            stimuli(i, fStim) = "F_done.wav";
            
            trialDistractors = [randsample(fDoneDistractors,6, true);randsample(mDoneDistractors,6, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
    elseif strcmp(mixedTargetTalker, 'one') == 1
        
        % trials with BALL target
        for i = 1:6
            mStim = targets(i,:);
            stimuli(i, mStim) = "M_ball.wav";
            trialDistractors = [randsample(fBallDistractors,8, true);randsample(mBallDistractors,4, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        for i = 7:12
            fStim = targets(i,:);
            stimuli(i, fStim) = "F_ball.wav";
            trialDistractors = [randsample(fBallDistractors,4, true);randsample(mBallDistractors,8, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        % trials with CAVE target
        for i = 13:18
            mStim = targets(i,:);
            stimuli(i, mStim) = "M_cave.wav";
            trialDistractors = [randsample(fCaveDistractors,8, true);randsample(mCaveDistractors,4, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        for i = 19:24
            fStim = targets(i,:);
            stimuli(i, fStim) = "F_cave.wav";
            trialDistractors = [randsample(fCaveDistractors,4, true);randsample(mCaveDistractors,8, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        % trials with TILE target
        for i = 25:30
            mStim = targets(i,:);
            stimuli(i, mStim) = "M_tile.wav";
            trialDistractors = [randsample(fTileDistractors,8, true);randsample(mTileDistractors,4, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        for i = 31:36
            fStim = targets(i,:);
            stimuli(i, fStim) = "F_tile.wav";
            trialDistractors = [randsample(fTileDistractors,4, true);randsample(mTileDistractors,8, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        % trials with DONE target
        for i = 37:42
            mStim = targets(i,:);
            stimuli(i, mStim) = "M_done.wav";
            trialDistractors = [randsample(fDoneDistractors,8, true);randsample(mDoneDistractors,4, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        for i = 43:48
            fStim = targets(i,:);
            stimuli(i, fStim) = "F_done.wav";
            trialDistractors = [randsample(fDoneDistractors,4, true);randsample(mDoneDistractors,8, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
    end
    
    %% BLOCKED TRIALS: MALE
    %First, figure out which positions will contain targets
    %Targets can only appear in positions 2-15 and must be separated by at
    %least one distractor
    
    for i = 1:24
        targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        while targets(i,4)-1 == targets(i,3) || targets(i,3)-1 == targets(i,2) || targets(i,2)-1 == targets(i,1)
            targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        end
    end
    
    %Next, figure out which positions must have distractors
    for i = 1:24
        distractors(i,:) = setdiff(allPos, targets(i,:));
    end
    
    % trials with BALL target
    for i = 1:6
        stimuli(48+i, targets(i,:)) = "M_ball.wav";
        stimuli(48+i, distractors(i,:)) = randsample(mBallDistractors,12, true);
    end
    
    % trials with CAVE target
    for i = 7:12
        stimuli(48+i, targets(i,:)) = "M_cave.wav";
        stimuli(48+i, distractors(i,:)) = randsample(mCaveDistractors,12, true);
    end
    
    % trials with TILE target
    for i = 13:18
        stimuli(48+i, targets(i,:)) = "M_tile.wav";
        stimuli(48+i, distractors(i,:)) = randsample(mTileDistractors,12, true);
    end
    
    % trials with DONE target
    for i = 19:24
        stimuli(48+i, targets(i,:)) = "M_done.wav";
        stimuli(48+i, distractors(i,:)) = randsample(mDoneDistractors,12, true);
    end
    
    %% BLOCKED TRIALS: FEMALE
    %First, figure out which positions will contain targets
    %Targets can only appear in positions 2-15 and must be separated by at
    %least one distractor
    
    for i = 1:24
        targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        while targets(i,4)-1 == targets(i,3) || targets(i,3)-1 == targets(i,2) || targets(i,2)-1 == targets(i,1)
            targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        end
    end
    
    %Next, figure out which positions must have distractors
    for i = 1:24
        distractors(i,:) = setdiff(allPos, targets(i,:));
    end
    
    % trials with BALL target
    for i = 1:6
        stimuli(72+i, targets(i,:)) = "F_ball.wav";
        stimuli(72+i, distractors(i,:)) = randsample(fBallDistractors,12, true);
    end
    
    % trials with CAVE target
    for i = 7:12
        stimuli(72+i, targets(i,:)) = "F_cave.wav";
        stimuli(72+i, distractors(i,:)) = randsample(fCaveDistractors,12, true);
    end
    
    % trials with TILE target
    for i = 13:18
        stimuli(72+i, targets(i,:)) = "F_tile.wav";
        stimuli(72+i, distractors(i,:)) = randsample(fTileDistractors,12, true);
    end
    
    % trials with DONE target
    for i = 19:24
        stimuli(72+i, targets(i,:)) = "F_done.wav";
        stimuli(72+i, distractors(i,:)) = randsample(fDoneDistractors,12, true);
    end
