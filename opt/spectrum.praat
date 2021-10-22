form Spectrum analysis
    sentence soundfile 
    positive range 5000
endform
Read from file... 'soundfile$'
current_sound$ = selected$ ("Sound")
filedelete 'defaultDirectory$'/'current_sound$'.spectrum
filedelete 'defaultDirectory$'/'current_sound$'.bw
To Spectrum: "yes"  
step=1
repeat 
    res = Get real value in bin: 'step'
    fileappend 'defaultDirectory$'/'current_sound$'.spectrum 'res' 'newline$'
    freq=Get frequency from bin number: 'step'
    step=step+1
until 'freq' > 'range' 	
bw=Get bin width
fileappend 'defaultDirectory$'/'current_sound$'.bw 'bw' 
select all
Remove
