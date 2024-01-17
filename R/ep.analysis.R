#ep.analysis.R

#20231214
#This code for processing EASYPOP (ep) output is based 
#on my scripting in my project directory, easypop.revised.testing, 
#used to compare output of the revised easypop
#program versus the original.

get.list.equ.files=function( s.file.pattern, s.path )
{

	v.files = list.files( path=s.path, pattern = s.file.pattern );

	if( length( v.files ) == 0 )
	{
		stop( paste( "error in get.list.equ.files:  no files found in path, ",
			    	s.path, "and file pattern,", s.file.pattern ) )

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

#' plot_easypop_replicate_equ_values
#'
#' for each item in v.data.source (see below for param v.data.source, 
#' each is plotted showing the per-generation values for the quantity (e.g. Fst), 
#' associated with the column name given by arg s.colname and returns them in a 
#' vector length i.num.values, of user-entered values
#'
#' @param   s.colname  a (quoted) column name from the equ file's first line, one of,
#' "Ho", "Hs", "Ht", "Fis", "Fst", or "Fit".

#' @param   v.data.source one of:  (1) a list of data frames, as created by calling
#'   \code{\link[utils]{read.table}} on a set of *.equ file, or (2) a vector of file
#'   paths to each .equ file you wish to read, or (3) one string giving the name
#'   of an easypop configuration file (as created, for example, with a call to setup_easypop). 
#'   Note that for option (2) the vector of file paths can be generated with
#'   the \code{\link[base]{file.path}} using the \code{full.names = TRUE} argument.
#'   In case (3), the program looks for equ files matching the "name_of_file" value
#'   listed in the configuration file
#'  
#' 
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
  	  l.files = get.list.equ.files.from.results.base.name( s.outfile.base )
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

get.list.equ.files.from.results.base.name=function( s.outfile.base )
{	
	#for the list.files fx divide into the actual file base and the path
       	#that leads to it. fx dirname will output a dot if there is no preceeding
	#path, which is the correct value for cwd (default) when using  list.files
	s.file.only = basename( s.outfile.base )
	s.path.only = dirname( s.outfile.base )

	s.equ.file.pattern=paste( s.file.only, ".*equ", sep="" )
	v.equ.files = get.list.equ.files( s.equ.file.pattern, s.path.only )
	
	return( list( "equnames" = v.equ.files, "equpath" = s.path.only ) )
}#end get.list.equ.files.from.results.base.name

#' plot_easypop_replicate_equ_means
#'
#' for each easypop config file given in the vector arg,
#' Plot the per-generation, replicate mean values for the 
#' equ output files, the column given by the s.colname arg.
#' The name_of_file entry  in each config file is used to
#' locate the *equ files
#' @param vs.config.files vector of names of ep config files
#' @param s.colname quoted string names one of the columns of an equ file,
#' as given in the first line of an equ file, one of,
#' "Ho", "Hs", "Ht", "Fis", "Fst", or "Fit".
#
#'@export
 
plot_easypop_replicate_equ_means = function( vs.config.files, s.colname ) {

	ldf.means = list()

	for( s.file in vs.config.files )
	{
		s.outfile.base = get.results.file.base.name.from.config.file( s.file )
		l.equ.files = get.list.equ.files.from.results.base.name( s.outfile.base )		
		df.means = get.mean.equ.file( l.equ.files$equnames, l.equ.files$equpath ) 
		ldf.means[[s.outfile.base]] = df.means 
	}#end for each config file


	return( plot_easypop_replicate_equ_values( s.colname, ldf.means ) )

}#end plot.easypop.replicate.means


