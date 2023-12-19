#ep.analysis.R

#20231214
#This code for processing EASYPOP (ep) output is based 
#on my scripting in my project directory, easypop.revised.testing, 
#used to compare output of the revised easypop
#program versus the original.

get.list.equ.files=function( s.file.pattern, s.path )
{

	v.files = list.files( path=s.path, pattern = s.file.pattern );

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

#' plot.equ.values
#' 
#' for each data frame given in arg ldf.data.frames, list of items, each representing 
#' an ep output *equ file,  a line is plotted showing the per-generation values for the
#' quantity (e.g. Fst), associated with the column name given by arg s.colname
#' and returns them in a vector length i.num.values, of user-entered values
#'
#' @param   s.colname  a column name from the equ file's first line
#' @param   ldf.data.frames a list of data frames, as created by calling read.table
#' on an *.equ file.
#

plot.equ.values = function( s.colname, ldf.data.frames )
{

	i.num.frames=length( ldf.data.frames )

	lv.values=NULL

	#we plot 1:length of y-values,
	#assuming per-generation starting with one,
	#hence min always one
	f.overall.xmax = -Inf

	f.overall.ymin = Inf
	f.overall.ymax = -Inf

	for( s.name in names( ldf.data.frames ) )
	{

		v.values = ldf.data.frames[[s.name]][[s.colname]]
		i.num.rows = length( v.values )
	
		f.xmax.this=i.num.rows
		f.ymin.this=min(v.values)
		f.ymax.this=max(v.values)

		f.overall.xmax = ifelse( f.xmax.this > f.overall.xmax, f.xmax.this, f.overall.xmax )
		f.overall.ymin = ifelse( f.ymin.this < f.overall.ymin, f.ymin.this, f.overall.ymin )
		f.overall.ymax = ifelse( f.ymax.this > f.overall.ymax, f.ymax.this, f.overall.ymax )

		lv.values[[s.name]] = v.values

	}#and for each frame, get values, compute overall mins/maxes

	plot( 1:f.overall.xmax, 
		1:f.overall.xmax,
		main=s.colname,
		xlab="generation",
		ylab=s.colname, 
		xlim=c( 1, f.overall.xmax), 
		ylim=c( f.overall.ymin, f.overall.ymax ), 
		type='n' );

	i.this.line.color=0

	for( s.name in names( lv.values ) )
	{
		v.yvals=lv.values[[s.name]] 
		i.this.line.color = i.this.line.color + 1

		lines( 1:length( v.yvals ), v.yvals, col=i.this.line.color ); 

	}#for each name

	legend( x="bottomright", 
	       legend=names( ldf.data.frames ),
	       col = 1:i.num.frames, lty=1, 
	       lwd=3, bty="n", bg="transparent" )

}#end fx plot.equ.values

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

#' plot_ep_replicate_equ_means
#'
#' for each easypop config file given in the vector arg,
#' plot the per-generation, replicate mean values for the 
#' equ output files, the column given by the s.colname arg 
#' the name_of_file entry  in each config file is used to
#' locate the *equ files
#' @param vs.config.files vector of names of ep config files
#' @param s.colname names one of the columns of an equ file,
#' as given in the first line of an equ file, one of,
#' Ho, Hs, Ht, Fis, Fst, or Fit.
#
#'@export
 
plot_ep_replicate_equ_means = function( vs.config.files, s.colname ) {

	ldf.means = list()

	for( s.file in vs.config.files )
	{
		s.outfile.base = get.results.file.base.name.from.config.file( s.file )
		#for the list.files fx divide into the actual file base and the path
	       	#that leads to it. fx dirname will output a dot if there is no preceeding
		#path, which is the correct value for cwd (default) when using  list.files
		s.file.only = basename( s.outfile.base )
		s.path.only = dirname( s.outfile.base )

		s.equ.file.pattern=paste( s.file.only, ".*equ", sep="" )
		v.equ.files = get.list.equ.files( s.equ.file.pattern, s.path.only )

		df.means = get.mean.equ.file( v.equ.files, s.path.only ) 
		ldf.means[[s.outfile.base]] = df.means 
		
	}#end for each config file

	plot.equ.values( s.colname, ldf.means )

}#end plot.ep.replicate.means
