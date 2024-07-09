#ep.analysis.R

#20231214
#This code for processing EASYPOP (ep) output is based 
#on my scripting in my project directory, easypop.revised.testing, 
#used to compare output of the revised easypop
#program versus the original.

is.absolute.path=function( s.path )
{
	#to help find equ files associated
	#with a config file's "name_of_file" entry, 
	#this function checks a path to see if it's 
	#an absolute.
	
	#Note that after a few tests in R it looks 
	#like R in windows 10 can resolve abs paths 
	#in unix-style, or with init backslash (escaped) 
	#without drive spec, or with an init drive spec

	s.abs.path.unix.first.char = "/"
	b.path.is.abs = FALSE
	s.first.char = substr( s.path, 1, 1 )

	#I think R on all platforms can resolve from the
	#old unix first char:
	if( s.first.char == s.abs.path.unix.first.char )
	{
		b.path.is.abs = TRUE
	}
	else if ( .Platform$OS.type == "windows" )
	{
		
		if ( s.first.char == "\\" )         
		{
			b.path.is.abs = TRUE
		}#end if first char backslach
		else
		{
			s.drive.pattern = "^[A-Z,a-z]:"
			i.hit = grep( pattern = s.drive.pattern, x = s.path )

			if( i.hit == 1 )
			{
				b.path.is.abs = TRUE
			}
		}#end if starts with backslash, else drive ref
	}#end if first char "/" else if on windows, 
	 #check for other abs path first char formats

	return ( b.path.is.abs )

}#end is.absolute.path

get.list.equ.files=function( s.file.pattern, s.path )
{

	v.files = list.files( path=s.path, pattern = s.file.pattern );

	if( length( v.files ) == 0 )
	{
		stop( paste( "error in get.list.equ.files:  no files found in path,",
			    	s.path, ", and file pattern,", s.file.pattern ) )

	}#end if length is zero

	return( v.files )

}#end get.list.equ.files

get.mean.equ.file = function( v.files, s.path )
{
	#v.files should be a list of existing
	#*equ files from ep output.  We assume
	#the tabuular files are tab-separated,
	#with the first line a header with col names.

	df.means=NULL;
	i.file.count=0;
	
	for( s.file in v.files )
	{
		i.file.count = i.file.count + 1;

		s.file.and.path=paste( s.path, s.file, sep="/" )

		if( i.file.count == 1 )
		{
			df.means=read.table( s.file.and.path, header=T, sep="\t" )
		}
		else
		{
			df.temp=read.table( s.file.and.path, header=T, sep="\t" )
	
			for( s.name in names( df.temp ) )
			{
				df.means[[s.name]] = df.means[[s.name]] + df.temp[[s.name]];
			}#end for each name

		}#end if first file ... else not first
	}#end for each file

	for( s.name in names( df.means ) )
	{
		df.means[[s.name]] = df.means[[s.name]]/i.file.count;
	}#end for each name

	return( df.means );

}#end fx get.mean.equ.file

files.are.all.of.type.equ = function( v.file.names )
{
	#motivation:  we'd like to check the data.source input
	#for the plot_easypop_replicate_equ_values 
	#when the user sends in a single string,
	#hence our init usage for s.file.name arg is expected to 
	#be a single string (an R character obj with length=1), 
	#but we check all char types if multiple char items are sent 

	#We make only a prima facia, name-based test that 
	#all files are easypop *.equ results file.  Maybe we'll 
	#add some analysis of contents if this gives false 
	#positives too frequently.
	b.all.are.type.equ = FALSE

	i.num.file.names = length( v.file.names )

	v.match=grep( pattern = "\\.equ$", x = v.file.names ) 

	#with a match we expect a single string (i.e. in R's
	#lingo a length=1 character type:
	if( length( v.match ) == i.num.file.names )
	{
		b.all.are.type.equ = TRUE
	}#end if every char item matched the name test

	return( b.all.are.type.equ )
		
}#end file.is.type.equ

#' Plot the per-generation, per-replicate values from a set of EASYPOP equ output files
#'
#' For each item in v.data.source (see the argument descriptions below for details),
#' each replicate is plotted showing the per-generation values for the quantity 
#' (e.g. Fst) associated with the column name given by argument, s.colname.   
#' The function returns a call to ggplot2::ggplot.  If you call the function without assigning 
#' the return to a variable, a plot graphic is shown. If you assign the call to
#' a holder variable, you will have a list comprising the ggplot components, which can be 
#' plotted, for example, with a call to plot().
#'
#' @param   s.colname  a (quoted) column name from the equ file's first line, one of,
#' "Ho", "Hs", "Ht", "Fis", "Fst", or "Fit".
#'
#' @param   v.data.source one of:  (1) a list of data frames, as created by calling
#'   \code{\link[utils]{read.table}} on a set of *.equ files, or (2) a vector of file
#'   paths to each .equ file you wish to read, or (3) one string giving the name
#'   of one EASYPOP configuration file (as created, for example, with a call to setup_easypop). 
#'   Note that for option (2) the vector of file paths can be generated with
#'   the \code{\link[base]{file.path}} using the \code{full.names = TRUE} argument.
#'   In case (3), the program looks for equ files whose prefixes match the "name_of_file" value
#'   given in the configuration file
#' @export

plot_easypop_replicate_equ_values = function( s.colname, v.data.source  )
{
  
  ldf.data.frames = NULL

  #20231221.  We preserve the code that analyzes the orig fx arg, ldf.data.frames
  #variable, which we first assign, either trivially to our replacement arg,
  #v.data.source, or, in the special case we want to add (see below), we convert
  #a config file name into a list of equ files, then assign it to
  #ldf.data.frames for processing.

  #if a single string (R's type==character, length  == 1 ) is sent 
  #in, we check to see if its not an *equ file, in which case
  #we assume it's an easypop config file, from which we want 
  #to extract equ output file names:

  if( is.character ( v.data.source ) 
     && length( v.data.source )  == 1
     && !files.are.all.of.type.equ( v.data.source ) )
  {
	  s.outfile.base=get.results.file.base.name.from.config.file( v.data.source )
	  s.config.file.path = dirname( v.data.source )
  	  l.files = get.list.equ.files.from.results.base.name( s.config.file.path,
							       		s.outfile.base )
	  ldf.data.frames = paste( l.files$equpath, l.files$equnames, sep="/" )
  }
  else#in this case, we assume its already in a form that is ready to be processed below
  {  
	  ldf.data.frames = v.data.source
  }#end if single string, non-equ file name, else either a data df or an equ file name

  if(!is.data.frame(ldf.data.frames[[1]])){
    check <- unlist(lapply(ldf.data.frames, file.exists))
    if(any(!check)){
      stop(paste0("Some files in lf.data.frames not found: \n",
                  paste0(ldf.data.frames[-check], collapse = "\n")))
    }
    
    names(ldf.data.frames) <- basename(ldf.data.frames)
    ldf.data.frames <- lapply(ldf.data.frames, function(x) read.table(x, header = TRUE))
  }
  else{
    names(ldf.data.frames) <- paste0("run", 1:length(ldf.data.frames))
  }

  ldf.data.frames <- data.table::rbindlist(ldf.data.frames, idcol = "run")
  ldf.data.frames <- ldf.data.frames[,c("run", "gen", s.colname)]

   p <- ggplot2::ggplot(ldf.data.frames, ggplot2::aes_string(x = "gen", y = s.colname, color = "run")) +
	  ggplot2::geom_line(show.legend = FALSE) +
	  ggplot2::theme_bw()
	
   return(p)
	
}#end fx plot_easypop_replicate_equ_values

get.results.file.base.name.from.config.file = function( s.config.file )
{
	#this function assumes the file named by the argument
	#is a valid ep config file, each line a key/value pair
	#separated by a colon<tab>

	#this key in the config file is
	#followed by the base name of
	#output files
	s.key.pattern = "^name_of_file:"
	vs.file.lines = readLines( s.config.file )
	s.outfile.base=NULL

	for( s.line in vs.file.lines )
	{
		v.idx = grep( pattern = s.key.pattern, x = s.line )
		if( length( v.idx ) == 1 )
		{
			ls.split = strsplit( s.line, ":\t" )
			s.outfile.base = ls.split[[1]][2]
		}#end if one index

	}#end for each s.line

	if( is.null( s.outfile.base ) )
	{
		stop( paste( "error, function get.results.file.base.name.from.config.file:",
			    	"no file base name was found in the file,",
				s.config.file ) )
	}#end if no name found
	
	   
	return( s.outfile.base )
	
}#end get.results.file.base.name.from.config.file

get.list.equ.files.from.results.base.name=function( s.config.file.path, s.outfile.base )
{	
	#20240208
	#Problem when R console's wd is not the same as the config file, so that
	#the outfile base needs a path prepended.  Remaining problem is when
	#the user enters an absolute path for the name_of_file param.

	#for the list.files fx divide into the actual file base and the path
       	#that leads to it. fx dirname will output a dot if there is no preceeding
	#path, which is the correct value for cwd (default) when using  list.files

	s.path.only = NULL
	s.file.only = basename( s.outfile.base )
	b.base.name.abs = is.absolute.path( s.outfile.base )

	if(  b.base.name.abs )
	{
		s.path.only = dirname( s.outfile.base )
	}
	else
	{
		s.path.only = s.config.file.path
	}#end if outfile base is abs path, else not

	s.equ.file.pattern=paste( s.file.only, ".*equ", sep="" )

	v.equ.files = get.list.equ.files( s.equ.file.pattern, s.path.only )
	
	return( list( "equnames" = v.equ.files, "equpath" = s.path.only ) )

}#end get.list.equ.files.from.results.base.name

#' Plot the per-generation mean values of a set of EASYPOP equ files
#'
#' For each EASYPOP configuration file given in the vector argument,
#' plot the per-generation, replicate mean values for the 
#' equ output files. The column to plot is given by the s.colname argument 
#' (see argument desriptions).
#' The value of the "name_of_file" entry in each configuration file is used to
#' locate the *equ files.
#' @param vs.config.files vector of names of EASYPOP configuration files
#' @param s.colname quoted string names one of the columns of an equ file,
#' as given in the first line of an equ file, one of,
#' "Ho", "Hs", "Ht", "Fis", "Fst", or "Fit".
#'
#' @export
 
plot_easypop_replicate_equ_means = function( vs.config.files, s.colname ) {

	ldf.means = list()

	i.last.num.gens=NULL

	for( s.file in vs.config.files )
	{

		s.outfile.base = get.results.file.base.name.from.config.file( s.file )

		s.config.file.path = dirname( s.file )

		l.equ.files = get.list.equ.files.from.results.base.name( s.config.file.path, 
										s.outfile.base )		

		df.means = get.mean.equ.file( l.equ.files$equnames, l.equ.files$equpath ) 

		#20240213 we add a check to make sure
		#all configurations have the same number
		#of generations. If not ggplot command 
		#(or is it the data.table command?)
		#will fail.  Not able right now to 
		#get allowance for variable num gens
		i.current.num.gens=dim( df.means )[1]

		if( is.null( i.last.num.gens ) )
		{
			i.last.num.gens = i.current.num.gens
		}
		else
		{
			if( i.current.num.gens != i.last.num.gens )
			{
				s.msg=paste0( "In plot_easypop_replicate_equ_means, ",
					     "the program currently can't plot runs from ",
					     "multiple configurations unless the number of ",
					     "generations run is identical in all of the configurations." )
				stop( s.msg )
			}#end if unequal num gens

			i.current.num.gens = i.last.num.gens
		}# end if i.last.num.gens is null, else not

		ldf.means[[s.outfile.base]] = df.means 
	}#end for each config file

	#20240118 we borrow from Will's ggplot code above, but add
	#a legend that shows which config file generated which line:

	v.output.file.basenames=names(ldf.means)

    	names(ldf.means) <- paste0("run", 1:length(ldf.means))
  	ldf.means <- data.table::rbindlist(ldf.means, idcol = "run")
	ldf.means$config.file = vs.config.files

	ldf.means <- ldf.means[,c( "run",  "gen", s.colname)]

	p <- ggplot2::ggplot(ldf.means, ggplot2::aes_string(x = "gen", y = s.colname, color = "run")) +
	  ggplot2::geom_line(show.legend = TRUE) +
	  ggplot2::theme_bw() + 
	  ggplot2::scale_color_hue( labels = v.output.file.basenames )
	
   	return(p)

}#end plot.easypop.replicate.means


