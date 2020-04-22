#  form that asks user for the directories containing file to be worked on, 
#  and for other information 

soundDir$ = "/Volumes/Shared/Experiments/Sahil/TalkerTeam/Stimuli/Ben"
textDir$ = "/Volumes/Shared/Experiments/Sahil/TalkerTeam/Stimuli/Ben"
outDir$ = "/Volumes/Shared/Experiments/Sahil/TalkerTeam/Stimuli/Ben"
tierName$ = "tier"
prefix$ = "Ben_" 
baseFile$ = "Ben_v2_2_7_2019"

# delete any existing record file
filedelete 'outDir$'/'baseFile$'_list.txt

numberOfFiles = 1
for ifile to numberOfFiles
	# Read in the Sound and TextGrid files
	Read from file... 'textDir$'/'baseFile$'.TextGrid
	Read from file: "'soundDir$'/'baseFile$'.wav"

	# Go through tiers and extract info
	selectObject: "TextGrid 'baseFile$'"
    nInterv = Get number of intervals: 1

	# loop through intervals and extract ones that contain text
	for j to nInterv
		selectObject: "TextGrid 'baseFile$'"
		lab$ = Get label of interval: 1, 'j'
			if lab$ != ""
				select TextGrid 'baseFile$'
				plus Sound 'baseFile$'
				Extract intervals where: 1, "no", "is equal to", "'lab$'"
           		Write to WAV file... 'outDir$'/'prefix$''lab$'.wav
				Remove
           endif
    endfor
endfor

# Complete object cleanup
select TextGrid 'baseFile$'
plus Sound 'baseFile$'
Remove