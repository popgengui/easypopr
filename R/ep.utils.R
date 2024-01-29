
#in this env we store variables for the 
#function, do.recursive.configs,
#to solve R's only-pass-by-value 
#limitation
ep.env = new.env( parent = baseenv() )

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
	l.config$name_of_file = paste0( s.filenames, "." )

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
#' given a list of parameter names, each with a vector of values,
#' for each combination of settings, create an easypop configuration file
#' and, optionally run each file as it is created.
#'
#' @param l.settings a list whose names are easypop config file parameter names, 
#'          and whose values are vectors of settings (values) for each parameter
#'
#' @param s.starting.config.file names an easypop config file to be used as the basis
#'        for the simulations, with revised settings as given by the first arg
#'
#' @param b.run  optional, default = FALSE, if set to TRUE, as each config is created,
#'        a simulation is run based on the config
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


	do.recursive.configs( l.settings, s.filebase, b.run = b.run ) 

}#end ep_auto_config



