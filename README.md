# easypopr
An R package to run the population simulation program, EASYPOP

You can install the package in your R installation using the devtools package, with the command: 

     devtools::install_github( "popgengui/easpopr" )


You can use easypopr to setup (parameterize) an EASYPOP simulation with the R console entry,
     
     setup.easypop( <file_name> )

and following the prompts.  <file_name> should name a file to which the program can write the values you enter at the prompts.   You can then run EASYPOP using your entered values with, 

     run.easypop( <file_name> )

EASYPOP runs a simulation according to the values in the file <file_name>.


