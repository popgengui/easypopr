
#in this env we store variables for the 
#function, do.recursive.configs,
#to solve R's only-pass-by-value 
#limitation
ep.env = new.env( parent = baseenv() )

update.config.key = function( i.config.number, l.config )
{
	#this function, added 20240106, updates rows for a table file
	#that will show which values of the pertinent params are assoc
	#which which config file (number)
	s.params.that.change = get( "v.names.of.test.params", envir = ep.env )
	m.copy.current.set.configs = get( "m.current.set.configs", envir = ep.env )

	v.vals.this.config = unlist( lapply( X = s.params.that.change, 
					    FUN = function(x) l.config[[x]] ) ) 

	v.config.num.with.vals = c( i.config.number, v.vals.this.config )
	#new row describes current config:	
	m.copy.current.set.configs = rbind( m.copy.current.set.configs, 
					 	v.config.num.with.vals )

	assign( "m.current.set.configs", m.copy.current.set.configs, 
	       						envir = ep.env ) 
}#end update.config.key


write.config.key.table = function()
{
	#To be called just before returning from the last recursive call,
	#when all config files have been written.
	#This function needs no args, as the ep.env has been
	#assigned all the needed values (see fx, update.config.key):
	s.file.name = get( "s.config.key.file.name", envir = ep.env )
	m.config.key.table = get( "m.current.set.configs", envir = ep.env )
	v.param.names = get( "v.names.of.test.params", envir = ep.env )
	v.col.names = c( "config_file_number", v.param.names )

	colnames( m.config.key.table ) = v.col.names 

	if( file.exists( s.file.name ) )
	{
		#we won't stop the execution, as we don't consider this file's
		#absense as critical to using the output:
		print ( paste0( "The function write.config.key.table can't write the file named, ",
			       		s.file.name, 
			       		", because the file name is already in use." ) )
		return ( NULL )
	}
	else
	{
write.table( x = m.config.key.table , 
			    	file = s.file.name, 
				row.names = FALSE,
	       			quote = FALSE,
				sep = "\t" )
	}#end if file exists, else write table

}#end write.config.key.table


update.param.value = function( l.config, l.settings, idx.param, idx.value )
{
	l.updated = l.config

	v.names = names( l.settings )
	s.name = v.names[idx.param]
	v.val = l.settings[[s.name]][ idx.value ]
	l.updated[ s.name ] = v.val 

	return( l.updated )
}#end update.param.value

make.config = function (  l.config, s.filebase, b.run = FALSE )
{
	
	#on each last parm values, we know we have a uniq
	#config setting, so we write config list, or file, 
	#or file and run sim (??)
	i.current.config.count = get( "i.config.count", envir = ep.env )
	i.current.config.count = i.current.config.count + 1

	#update config count:
	assign( "i.config.count", i.current.config.count, envir = ep.env )

	s.filenames = paste0( s.filebase, "_", i.current.config.count )

	#have to update output file base name:
	#20240213 adding "rep" go the output name
	#as terminating with just the config count and a dot
	#breaks code that finds outputfiles for just one config
	#when config count exceeds 10^1
	l.config$name_of_file = paste0( s.filenames, ".rep." )

	#we add a new row to our key file that associates
	#a config number with a set of param values:
	update.config.key( i.current.config.count, l.config )

	write_parameters_to_file( l.config, 
		paste0( s.filebase, "_", i.current.config.count,  ".cfg" ), 
		b.run )

}#end make.config

do.recursive.configs = function( l.settings, s.filebase, b.run = FALSE )
{
	#this function creates single configurations by combining
	#multiple settings, that is, one config being the set of selected values
	#one for each param.  This alg accomodates an arbitrary number of parameters,
	#p1, p2, p3, with values p1.v1, p1.v2...p1vn, p2.v1, p1.v2...p2vn,
	#etc.  One config file is created (and optionally run) for each combination
	#of p1.vi, p2vj, .... etc.   The code proceeds recursively from p1, p2..pn,
	#then back up to pi-1 after all values of pi have been used in combo
        #with a given p(i-1).  Too many params with too many values may overwhelm
	#R's stack limitations, which seems to (thankfully) simply fail with a message,
	#without paralyzing the memory system.

	i.next.param.number = 0
	i.next.val.index = 0

	i.local.current.param.number = get( "i.current.param.number", 
					   		envir = ep.env )
	
	#by our recursive procedure, i.local.current.param.number 
	#reaches zero when all combinations of all param values 
	#have been used.
	if( i.local.current.param.number < 1 )
	{
		print( "done making configurations" )
		#added 20240206, to write a table
		#keying config numnber to param vals:
		write.config.key.table()
		return()
	}#end if param num < 1

	l.local.config = get( "l.config", envir = ep.env )
	v.local.val.indices = get( "v.current.value.indices", envir  = ep.env )
	v.local.val.totals = get( "v.param.value.totals", envir  = ep.env )

	i.total.vals.this.param = v.local.val.totals[ i.local.current.param.number ]
	i.current.value.index = v.local.val.indices[ i.local.current.param.number ]

	b.is.last.param = ( i.local.current.param.number == length( v.local.val.totals ) )
	b.all.values.used = ( i.current.value.index > i.total.vals.this.param )

	if( b.all.values.used )
	{
		i.next.param.number = i.local.current.param.number - 1
		i.next.val.index = 1
	}
	else
	{
		l.local.config = update.param.value( l.local.config, 
						    l.settings, 
						    i.local.current.param.number, 
						    i.current.value.index )

		i.next.val.index = i.current.value.index + 1

		#if not the last param, we move to the next
		if ( !b.is.last.param )
		{
			i.next.param.number = i.local.current.param.number + 1
		}
		else
		{
			#last param, we want to iterate over all last param vals:
			#without moving to previous params:
			i.next.param.number = i.local.current.param.number

			make.config( l.local.config, s.filebase, b.run )

		}#end if last param

	}#end if all values used, else not

	#update our "globals"
	v.local.val.indices[ i.local.current.param.number ] = i.next.val.index
	assign( "v.current.value.indices", v.local.val.indices , envir = ep.env )
	assign( "l.config", l.local.config, envir = ep.env )
	assign( "i.current.param.number", i.next.param.number, envir = ep.env )

	#recurse	
	do.recursive.configs( l.settings, s.filebase, b.run )	

}#end do.recursive.configs

#' configure_multiple_easypop_runs
#' 
#' given a list of parameter names, each with a vector of values, for each 
#' combination of settings, create an easypop configuration file
#' and, optionally run each file as it is created.  Configuration and output files
#' mames are numberd as created.  After all configurations are written (and, optionally, run), 
#' the program also writes  a tabular file with the given filebase and with extension, 
#' ".config.key.tsv"  This table gives in it's first column a config file number, and in 
#' columns 2..N, the values for the params that were in the l.settings list 
#' (see parameter descriptions).
#'
#' @param l.settings a list whose names are easypop config file parameter names, 
#'          and whose values are vectors of settings (values) for each parameter
#'
#' @param s.starting.config.file names an easypop config file to be used as the basis
#'        for the simulations, with revised settings as given by the first arg
#' @param s.filebase names the prefix used to name configuration files
#'        (*.cfg) and output files (*equ, *gen, etc).  The program also adds an 
#'        integer to the file names that indicates its order of creation.
#' @param b.run  optional, default = FALSE, if set to TRUE, as each config is created,
#'        a simulation is run based on the config
#'        
#' @return A data.frame containing run/config information.
#' @export
#'

configure_multiple_easypop_runs =function( l.settings, s.starting.config.file, s.filebase,  b.run = FALSE )
{
	v.param.value.totals =unlist( lapply( 1 : length( l.settings ), 
				  FUN = function(x) length( l.settings[[x]] ) ) ) 
	
	l.starting.config = read_parameters_from_file( s.starting.config.file )

	assign( "l.config", l.starting.config, envir = ep.env )

	assign( "v.current.value.indices", 
	       		rep( 1, length( l.settings ) ), 
			envir = ep.env ) 

	assign( "v.param.value.totals", v.param.value.totals, envir = ep.env )
	assign( "i.current.param.number", 1, envir = ep.env )
	assign( "i.config.count", 0, envir = ep.env )

	#these added 20240206, to give users a key to the settings in each config:
	assign( "m.current.set.configs", NULL, envir = ep.env ) 
	#this will help extract the current vals for test params from the 
	#current config
	assign( "v.names.of.test.params", names( l.settings ), envir = ep.env )
	assign( "s.config.key.file.name", paste0( s.filebase, ".config.key.tsv" ), 
	       								envir = ep.env )
	#end additions 20240206


	do.recursive.configs( l.settings, s.filebase, b.run = b.run )
	
	# pull in metadata
	config_count <- get("i.config.count", ep.env)
	
	info_table <- cbind(data.frame(config_file = paste0(s.filebase, "_", 1:config_count, ".cfg")),
	                    read.table(get("s.config.key.file.name", ep.env), header = TRUE, sep = "\t"))
	
	if(b.run){
	  outfiles <- lapply(1:config_count, 
	                     function(x) list.files(pattern = paste0(gsub("\\.", "\\\\.", s.filebase), "_", x, "\\..+.gen$")))
	  info_table$outfiles <- character(nrow(info_table))
	  for(i in 1:nrow(info_table)){
	    info_table$outfiles[i] <- I(outfiles[i])
	  }
	}
	
	return(info_table)

}#end ep_auto_config


#' Format easypopr genepop outfiles into snpRdata objects
#' 
#' Generates data files for the 'snpR' package given a vector of file names or 
#' config outputs from \code{\link{configure_multiple_easypop_runs}}. Requires
#' 'snpR' to be installed.
#'
#' @param files Either a vector of file names pointing to easypop/genepop
#' output files ending in either '.gen' or '.genepop' or a run info table
#' like that produced by \code{\link{configure_multiple_easypop_runs}}.
#' @param verbose logical, default FALSE. If TRUE, prints progress statements.
#'        
#' @return A nested list containing snpRdata objects in '$dat' and possibly
#'   '$meta' containing run metadata if provided an run info table.
#' @export
#' @examplesIf "snpR" %in% rownames(utils::installed.packages())
#' 
#' # pull in example config file
#' s.config.file = system.file( "extdata", 
#'                              "demo.standard.cfg", 
#'                              package = "easypopr") 
#' 
#' # run for two different numbers of populations
#' l.params.to.vary = list(
#'   "number_populations" = c(5, 10))
#' 
#' # run
#' info <- 
#'   configure_multiple_easypop_runs( l.settings =  l.params.to.vary, 
#'                                    s.starting.config.file = s.config.file, 
#'                                    s.filebase = "test_pops",
#'                                    b.run = TRUE )
#' 
#' # pull into snpR format
#' d <- format_snpR(info)
#' 
#' # check results
#' d$meta # metadata for each object
#' snpR::summarize_facets(d$dat[[1]], "genepop_pop") # five pops
#' snpR::summarize_facets(d$dat[[3]], "genepop_pop") # ten pops
#' 
#' # can also be run with a vector of file names:
#' d <- format_snpR(unlist(info$outfiles))
format_snpR <- function(files, verbose = FALSE){
  #============sanity checks==========
  # check installed
  if(!"snpR" %in% rownames(utils::installed.packages())){
    say <- paste0("Package 'snpR' not found.")
    cat(say, "")
    cat("Install? (y or n)\n")
    
    if(!interactive()){
      stop(say)
    }
    
    resp <- readLines(n = 1)
    resp <- tolower(resp)

    if(resp != "y"){
      stop(say)
    }
    
    if(!"remotes" %in% rownames(utils::installed.packages())){
      install.packages("remotes")
    }
    
    remotes::install_github("hemstrow/snpR")
  }
  
  if(is.character(files)){
    input_type <- "files"
    
    if(any(!file.exists(files))){
      missing_files <- which(!file.exists(files))
      stop(paste0("File(s) not found:\n\t", paste0(files[missing_files], collapse = "\n\t"), "\n"))
    }
    
    bad_extensions <- which(!(grepl("\\.gen$", files) | grepl("\\.genepop$", files)))
    if(length(bad_extensions) > 0){
      stop(paste0("File(s) have bad extensions. Extensions must be '.gen' or '.genepop':\n\t", paste0(files[bad_extensions], collapse = "\n\t"), "\n"))
    }
  }
  
  else if(is.data.frame(files)){
    input_type <- "info_table"
    
    if(!"outfiles" %in% colnames(files)){
      stop("A column named 'outfiles' is expected in a data.frame list of simulation outputs.\n")
    }
    
    outfiles <- unlist(files$outfiles)
    
    if(any(!file.exists(outfiles))){
      missing_files <- which(!file.exists(outfiles))
      stop(paste0("File(s) not found:\n\t", paste0(outfiles[missing_files], collapse = "\n\t"), "\n"))
    }
    
    bad_extensions <- which(!(grepl("\\.gen$", outfiles) | grepl("\\.genepop$", outfiles)))
    if(length(bad_extensions) > 0){
      stop(paste0("File(s) have bad extensions. Extensions must be '.gen' or '.genepop':\n\t", paste0(outfiles[bad_extensions], collapse = "\n\t"), "\n"))
    }
  }
  
  
  
  #==================read in the genepop file(s) listed===========
  # if not an info table, easy -- the wierd supress wrapping is to suppress some warnings that always happen when
  # reading in genepop files generated by easypopr 
  # (allelic identity, overlapping facet level, and incomplete final line warnings)
  if(input_type == "files"){
    obj <- snpR:::.suppress_specific_warning(
      snpR:::.suppress_specific_warning(
        snpR:::.suppress_specific_warning(lapply(files, function(f){
          if(verbose){cat("Reading: ", f, "\n")}
          return(snpR::read_genepop(f))
        }), 
        "incomplete final"), 
        "levels are duplicated"), 
      "allelic identities")
    
    obj <- list(dat = obj)
  }
  
  # otherwise we grab metadata as well
  else if(input_type == "info_table"){

    # set up storage
    obj <- vector("list", length(outfiles))
    prog <- 1
    
    meta_table <- data.frame(config_file = character(length(obj)),
                             config_file_number = numeric(length(obj)))
    meta_table <- cbind(meta_table, files[rep(1, nrow(meta_table)),-c(1:2)])
    meta_table$outfiles <- NULL
    meta_table$rep <- 0
    meta_table$outfile <- ""
    
    # read in each file and store metadata
    if(verbose){cat("Reading parameters.\n")}
    for(i in 1:nrow(files)){
      if(verbose){cat("Set:", i, "\n")}
      i_info <- files[i,which(colnames(files) != "outfiles")]
      for(j in 1:length(files[1,"outfiles"][[1]])){
        tfile <- files[i,"outfiles"][[1]][j]
        if(verbose){cat("\t", tfile, "\n")}
        j_info <- cbind(i_info, rep = as.numeric(gsub("\\..+$", "", gsub(".+rep\\.", "", tfile))))
        j_info$outfile <- tfile
        
        obj[[prog]] <- snpR:::.suppress_specific_warning(
          snpR:::.suppress_specific_warning(
            snpR:::.suppress_specific_warning(snpR::read_genepop(tfile), 
                                              "incomplete final"), 
            "levels are duplicated"), 
          "allelic identities")
        
        
        meta_table[prog,] <- j_info
        prog <- prog + 1
      }
    }
    
    obj <- list(dat = obj, meta = meta_table)
  }
  
  # done
  return(obj)
}



















