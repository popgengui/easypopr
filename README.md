# easypopr
An R package to run the population simulation program, EASYPOP, authored by F. Balloux, (c.f. Balloux, Francois. "EASYPOP (version 1.7): a computer program for population genetics simulations." Journal of heredity 92.3 (2001): 301-302 ).

This version has revisions to the interface by T. Cosart.  You'll find the author's original user guide, as well as documentation detailing the revised input methods, and per-OS exacutables at

     https://drive.google.com/drive/folders/1ZbbVeYR1ix8rI43d2myc1Fg9LTz3qfA_?usp=drive_link

You don't need to download the executables to run our R package version.  You can install the package in your R installation using the devtools package, with the R console command: 

     devtools::install_github( "popgengui/easpopr" )

After loading easypopr with,

     library( "easypopr" )

you can use easypopr to setup (parameterize) an EASYPOP simulation with the R console command,
     
     setup_easypop( <file_name>, run=FALSE )

and following the prompts.  <file_name> should name a file to which the program can write the values you enter at the prompts.   To run EASYPOP automatically after the prompts, use TRUE as an optional second argument.  You can also run EASYPOP using your entered values from R with, 

     run_easypop( <file_name> )

EASYPOP runs a simulation according to the values in the file <file_name>.

To view plotted replicate mean, per-generation values of quantities in EASYPOP's equ output files, you can call,

    plot_easypop_replicate_equ_means( <vector of config file names>, <quantity> )

where the vector argument is one or more config files used by EASYPOP to run a simulation, and the quantity argument gives one of the column names  in the first line of an equ file (one of: Ho, Hs, Ht, Fis, Fst, or Fit), to see the per-generation replicate mean value of the quantity.

To view per-generation, individual replicate quantities you can call,
	
    plot_easypop_replicate_equ_values( <quantity> <data source> )

where data source is one of (1) a list of data frames as created by calling 'read.table' on a set of equ files, or (2) a vector of file paths to each .equ file you wish to read, or (3) one string giving the name of an easypop configuration file (as created, for example with a call to setup_easypop)  In case (3), the program assumes it will find equ results files as named in the configuration file.




