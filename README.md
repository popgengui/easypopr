# easypopr
An R package to run the population simulation program, EASYPOP, authored by F. Balloux, (c.f. Balloux, Francois. "EASYPOP (version 1.7): a computer program for population genetics simulations." Journal of heredity 92.3 (2001): 301-302 ).

This version has revisions to the interface by T. Cosart.  You'll find the author's original user guide, as well as documentation detailing the revised input methods, and per-OS executables at

     https://drive.google.com/drive/folders/1ZbbVeYR1ix8rI43d2myc1Fg9LTz3qfA_?usp=drive_link

## Installation
You don't need to download the executables to run our R package version.  You can install the package in your R installation using the devtools package, with the R console command: 

     devtools::install_github( "popgengui/easpopr" )

## First use
After loading easypopr with,

     library( "easypopr" )

you can use easypopr to setup (parameterize) an EASYPOP simulation with the R console command,
     
     setup_easypop( <file_name>, run=FALSE )

and following the prompts.  <file_name> should name a file to which the program can write the values you enter at the prompts.   To run EASYPOP automatically after the prompts, use TRUE as an optional second argument.  You can also run EASYPOP using your entered values from R with, 

     run_easypop( <file_name> )

EASYPOP runs a simulation according to the values in the file <file_name>. 

## Revising configurations
We also provide functions to read in an existing configuration file, 

     read_parameters_from_file( <file_name> )

which gives you a list of parameter names/values.  You can then change parameter values, and write a new configuration file (and optionally run it based on the parameter list with, 

     write_parameters_to_file( <ls.parameters>,  <s.file>, [b.run=FALSE] )

These read and write calls are meant to allow you to write an R script, for example, that automatically writes configuration files and runs corresponding simulations based on changing a particular parameter value a value to see the effect on Fst, Heterozygosity, etc.  Note also that you can simply open a configuration file in a text editor, change the parameter values , and (re)run it with the run_easypop call.  For details on manually revising a configuration file, see the notes in the text document "easypop.revised.usage.txt" available at the link given above.

## Automating multiple parameter tests
We also provide a function call that can run multiple simulations given a list of parameters, each with a list of settings:

     configure_multiple_easypop_runs( <l.settings>, <s.starting.config.file>, <s.filebase>,  [b.run = FALSE] )

For example, you have a configuration file already created, "my.cfg",  (using for example, the function setup_easypop), and, matching exactly the parameter names of interest in your cfg file, you make a list like this:

     l.settings = list( "number_populations" = c( 5, 10, 15 ), "mutation_rate" = c( 0.001, 0.005 ) )

The function call:

     configure_multiple_easypop_runs( l.settings, "my.cfg", "my.pop.mut.test", TRUE )

would result in 6 configuration files created, such that each of the 3 values for "number_populations" would be paired with each of the 2 "mutation_rate" values, with all other parameters set as in the "my.cfg" file.  The new 6 configuration files would be named "my.pop.mut.test_n.cfg", where n = 1 to 6. With the b.run argument set to TRUE, easypop would run a simulation on each, with output files named (and numbered) like the configuration files.  The l.settings argument can list an arbitrary number of parameter names (as seen in an easypop configuration file), each with an arbitrary number of values.  Note, as a warning, that a list (l.settings) with many parameters, each with many values can create a huge number of configurations.  Besides a huge number of output files, a large input may also interrupt the execution by violating R's stack limitation, as our function uses recursion to create the configurations.

## Plotting results

To view plotted replicate mean, per-generation values of quantities in EASYPOP's equ output files, you can call,

    plot_easypop_replicate_equ_means( <vector of config file names>, <quantity> )

where the vector argument is one or more config files used by EASYPOP to run a simulation, and the quantity argument (in quotes) gives one of the column names  in the first line of an equ file (one of: "Ho", "Hs", "Ht", "Fis", "Fst", or "Fit"), to see the per-generation replicate mean value of the quantity.

To view per-generation, individual replicate quantities you can call,
	
    plot_easypop_replicate_equ_values( <quantity>, <data source> )

where data source is one of (1) a list of data frames as created by calling 'read.table' on a set of equ files, or (2) a vector of file paths to each .equ file you wish to read, or (3) one string giving the name of an easypop configuration file (as created, for example with a call to setup_easypop).  In case (3), the program assumes it will find equ results files as named in the configuration file's "name_of_file" entry.




