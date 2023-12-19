#This is a "master" list of all (possible) easypop parameters
#This list is used below to parameter data types to properly
#write them to file.  As of 20231210, this list is not used
#to store user values, hence the "value" field is not relevant.
ALL.EP.PARAMS.WITH.TYPE=list(	
		"ploidy"
			 = list( "name" = "ploidy:", "valtype" = "integer", "value" = NULL ),
		"proportion_recombination"
			 = list( "name" = "proportion_recombination:", "valtype" = "numeric", "value" = NULL ),
		"two_sexes"
			 = list( "name" = "two_sexes:", "valtype" = "logical", "value" = NULL ),
		"random_mating"
			 = list( "name" = "random_mating:", "valtype" = "numeric", "value" = NULL ),
		"proportion_clonal_mating"
			 = list( "name" = "proportion_clonal_mating:", "valtype" = "numeric", "value" = NULL ),
		"proportion_selfing_of_nonclones"
			 = list( "name" = "proportion_selfing_of_nonclones:", "valtype" = "numeric", "value" = NULL ),
		"mating_system"
			 = list( "name" = "mating_system:", "valtype" = "integer", "value" = NULL ),
		"proportion_extra_pair_copulation"
			 = list( "name" = "proportion_extra_pair_copulation:", "valtype" = "numeric", "value" = NULL ),
		"proportion_matings_by_subordinate_males"
			 = list( "name" = "proportion_matings_by_subordinate_males:", "valtype" = "numeric", "value" = NULL ),
		"number_populations"
			 = list( "name" = "number_populations:", "valtype" = "integer", "value" = NULL ),
		"same_number_individuals_each_population"
			 = list( "name" = "same_number_individuals_each_population:", "valtype" = "logical", "value" = NULL ),
		"number_of_individuals"
			 = list( "name" = "number_of_individuals:", "valtype" = "integer", "value" = NULL ),
		"per_population_individual_counts"
			 = list( "name" = "per_population_individual_counts:", "valtype" = "array.integer", "value" = NULL ),
		"per_population_number_of_females"
			 = list( "name" = "per_population_number_of_females:", "valtype" = "array.integer", "value" = NULL ),
		"per_population_number_of_males"
			 = list( "name" = "per_population_number_of_males:", "valtype" = "array.integer", "value" = NULL ),
		"per_population_number_of_queens"
			 = list( "name" = "per_population_number_of_queens:", "valtype" = "array.integer", "value" = NULL ),
		"per_population_number_of_workers"
			 = list( "name" = "per_population_number_of_workers:", "valtype" = "array.numeric", "value" = NULL ),
		"number_females_each_population"
			 = list( "name" = "number_females_each_population:", "valtype" = "integer", "value" = NULL ),
		"number_males_each_population"
			 = list( "name" = "number_males_each_population:", "valtype" = "integer", "value" = NULL ),
		"number_queens_each_population"
			 = list( "name" = "number_queens_each_population:", "valtype" = "integer", "value" = NULL ),
		"number_workers_each_population"
			 = list( "name" = "number_workers_each_population:", "valtype" = "integer", "value" = NULL ),
		"same_migration_scheme_all_simulation"
			 = list( "name" = "same_migration_scheme_all_simulation:", "valtype" = "logical", "value" = NULL ),
		"migration_model"
			 = list( "name" = "migration_model:", "valtype" = "integer", "value" = NULL ),
		"migration_model_second_scheme"
			 = list( "name" = "migration_model_second_scheme:", "valtype" = "integer", "value" = NULL ),
		"number_populations_first_population_group"
			 = list( "name" = "number_populations_first_population_group:", "valtype" = "integer", "value" = NULL ),
		"number_populations_first_population_group_second_scheme"
			 = list( "name" = "number_populations_first_population_group_second_scheme:", "valtype" = "integer", "value" = NULL ),
		"number_of_archipelagos"
			 = list( "name" = "number_of_archipelagos:", "valtype" = "integer", "value" = NULL ),
		"number_of_archipelagos_second_scheme"
			 = list( "name" = "number_of_archipelagos_second_scheme:", "valtype" = "integer", "value" = NULL ),
		"same_number_populations_each_archipelago"
			 = list( "name" = "same_number_populations_each_archipelago:", "valtype" = "logical", "value" = NULL ),
		"same_number_populations_each_archipelago_second_scheme"
			 = list( "name" = "same_number_populations_each_archipelago_second_scheme:", "logical" = "numeric", "value" = NULL ),
		"per_archipelago_number_of_populations"
			 = list( "name" = "per_archipelago_number_of_populations:", "valtype" = "array.integer", "value" = NULL ),
		"per_archipelago_number_of_populations_second_scheme"
			 = list( "name" = "per_archipelago_number_of_populations_second_scheme:", "valtype" = "array.integer", "value" = NULL ),
		"proportion_migration_within_groups"
			 = list( "name" = "proportion_migration_within_groups:", "valtype" = "numeric", "value" = NULL ),
		"proportion_migration_within_groups_second_scheme"
			 = list( "name" = "proportion_migration_within_groups_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"proportion_migration_between_groups"
			 = list( "name" = "proportion_migration_between_groups:", "valtype" = "numeric", "value" = NULL ),
		"proportion_migration_between_groups_second_scheme"
			 = list( "name" = "proportion_migration_between_groups_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"proportion_female_migration_within_groups"
			 = list( "name" = "proportion_female_migration_within_groups:", "valtype" = "numeric", "value" = NULL ),
		"proportion_female_migration_within_groups_second_scheme"
			 = list( "name" = "proportion_female_migration_within_groups_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"proportion_female_migration_between_groups"
			 = list( "name" = "proportion_female_migration_between_groups:", "valtype" = "numeric", "value" = NULL ),
		"proportion_female_migration_between_groups_second_scheme"
			 = list( "name" = "proportion_female_migration_between_groups_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"proportion_male_migration_within_groups"
			 = list( "name" = "proportion_male_migration_within_groups:", "valtype" = "numeric", "value" = NULL ),
		"proportion_male_migration_within_groups_second_scheme"
			 = list( "name" = "proportion_male_migration_within_groups_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"proportion_male_migration_between_groups"
			 = list( "name" = "proportion_male_migration_between_groups:", "valtype" = "numeric", "value" = NULL ),
		"proportion_male_migration_between_groups_second_scheme"
			 = list( "name" = "proportion_male_migration_between_groups_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"proportion_migration"
			 = list( "name" = "proportion_migration:", "valtype" = "numeric", "value" = NULL ),
		"proportion_migration_second_scheme"
			 = list( "name" = "proportion_migration_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"proportion_female_migration"
			 = list( "name" = "proportion_female_migration:", "valtype" = "numeric", "value" = NULL ),
		"proportion_female_migration_second_scheme"
			 = list( "name" = "proportion_female_migration_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"proportion_male_migration"
			 = list( "name" = "proportion_male_migration:", "valtype" = "numeric", "value" = NULL ),
		"proportion_male_migration_second_scheme"
			 = list( "name" = "proportion_male_migration_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"number_of_dimensions_defining_the_space"
			 = list( "name" = "number_of_dimensions_defining_the_space:", "valtype" = "integer", "value" = NULL ),
		"number_of_dimensions_defining_the_space_second_scheme"
			 = list( "name" = "number_of_dimensions_defining_the_space_second_scheme:", "valtype" = "integer", "value" = NULL ),
		"per_population_per_dimension_coordinates"
			 = list( "name" = "per_population_per_dimension_coordinates:", "valtype" = "matrix.numeric", "value" = NULL ),
		"per_population_per_dimension_coordinates_second_scheme"
			 = list( "name" = "per_population_per_dimension_coordinates_second_scheme:", 
							"valtype" = "matrix.numeric", "value" = NULL ),
		"mean_dispersal_distance"
			 = list( "name" = "mean_dispersal_distance:", "valtype" = "numeric", "value" = NULL ),
		"mean_dispersal_distance_second_scheme"
			 = list( "name" = "mean_dispersal_distance_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"mean_female_dispersal_distance"
			 = list( "name" = "mean_female_dispersal_distance:", "valtype" = "numeric", "value" = NULL ),
		"mean_female_dispersal_distance_second_scheme"
			 = list( "name" = "mean_female_dispersal_distance_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"mean_male_dispersal_distance"
			 = list( "name" = "mean_male_dispersal_distance:", "valtype" = "numeric", "value" = NULL ),
		"mean_male_dispersal_distance_second_scheme"
			 = list( "name" = "mean_male_dispersal_distance_second_scheme:", "valtype" = "numeric", "value" = NULL ),
		"number_of_loci"
			 = list( "name" = "number_of_loci:", "valtype" = "integer", "value" = NULL ),
		"free_recombination_between_loci"
			 = list( "name" = "free_recombination_between_loci:", "valtype" = "logical", "value" = NULL ),
		"recombination_rate_between_adjacent_loci"
			 = list( "name" = "recombination_rate_between_adjacent_loci:", "valtype" = "numeric", "value" = NULL ),
		"all_loci_same_mutation_scheme"
			 = list( "name" = "all_loci_same_mutation_scheme:", "valtype" = "logical", "value" = NULL ),
		"per_locus_mutation_rate"
			 = list( "name" = "per_locus_mutation_rate:", "valtype" = "array.numeric", "value" = NULL ),
		"mutation_rate"
			 = list( "name" = "mutation_rate:", "valtype" = "numeric", "value" = NULL ),
		"per_locus_mutation_model"
			 = list( "name" = "per_locus_mutation_model:", "valtype" = "array.integer", "value" = NULL ),
		"mutation_model"
			 = list( "name" = "mutation_model:", "valtype" = "integer", "value" = NULL ),	
		"proportion_kam_mutation_events"
			 = list( "name" = "proportion_kam_mutation_events:", "valtype" = "numeric", "value" = NULL ),
		"locus_numbers_and_proportions_kam_mutation_events"
			 = list( "name" = "locus_numbers_and_proportions_kam_mutation_events:", "valtype" = "list.numeric", "value" = NULL ),
		"proportion_double_step_mutation_events"
			 = list( "name" = "proportion_double_step_mutation_events:", "valtype" = "numeric", "value" = NULL ),
		"locus_numbers_and_proportions_double_step_mutation_events"
			 = list( "name" = "locus_numbers_and_proportions_double_step_mutation_events:", 
					"valtype" = "list.numeric", "value" = NULL ),
		"per_locus_number_possible_allelic_states"
			 = list( "name" = "per_locus_number_possible_allelic_states:", "valtype" = "array.integer", "value" = NULL ),
		"number_possible_allelic_states"
			 = list( "name" = "number_possible_allelic_states:", "valtype" = "integer", "value" = NULL ),
		"variability_initial_population"
			 = list( "name" = "variability_initial_population:", "valtype" = "integer", "value" = NULL ),
		"number_of_generations"
			 = list( "name" = "number_of_generations:", "valtype" = "integer", "value" = NULL ),
		"number_of_generations_before_migration_scheme_change"
			 = list( "name" = "number_of_generations_before_migration_scheme_change:", "valtype" = "integer", "value" = NULL ),
		"complete_dataset_in_dat_and_gen_files"
			 = list( "name" = "complete_dataset_in_dat_and_gen_files:", "valtype" = "logical", "value" = NULL ),
		"number_populations_in_result_files"
			 = list( "name" = "number_populations_in_result_files:", "valtype" = "integer", "value" = NULL ),
		"number_females_per_population_in_result_files"
			 = list( "name" = "number_females_per_population_in_result_files:", "valtype" = "integer", "value" = NULL ),
		"number_males_per_population_in_result_files"
			 = list( "name" = "number_males_per_population_in_result_files:", "valtype" = "integer", "value" = NULL ),
		"number_individuals_in_each_population_in_result_files"
			 = list( "name" = "number_individuals_in_each_population_in_result_files:", "valtype" = "integer", "value" = NULL ),	
		"file_giving_pedigrees"
			 = list( "name" = "file_giving_pedigrees:", "valtype" = "logical", "value" = NULL ),
		"name_of_file"
			 = list( "name" = "name_of_file:", "valtype" = "character", "value" = NULL ),
		"number_of_replicates"= list( "name" = "number_of_replicates:", "valtype" = "integer", "value" = NULL ) )



FALSE.AS.INT = as.integer( 0 )
TRUE.AS.INT = as.integer( 1 )

#parameter limits from the easypop.config.file.parser.h:
MAX_NUMBER_POPULATIONS = as.integer ( 1000 )
MAX_NUMBER_INDIVIDUALS_IN_POP = as.integer( 1e5 )
MAX_NUMBER_DIMENSIONS = as.integer( 1e5 )
MAX_NUMBER_LOCI = as.integer ( 1e7 )
MAX_NUMBER_ALLELIC_STATES = as.integer ( 999 )
MAX_NUMBER_GENERATIONS = as.integer( 1e7 )
MAX_NUMBER_REPLICATES = as.integer( 999 )
MAX_NUMBER_CHARS_FILENAME = as.integer( 5000 )
MAX_NUMBER_ITEMS_IN_ARRAY = as.integer( 1e5 )
MAX_NUMBER_MIGRATION_SCHEME = as.integer( 6 )
MAX_NUMBER_MUTATION_MODEL = as.integer( 4 )

#not sure what out of bounds is here, so just
#using a large number to allow practically any value:
MAX_DISPERSAL_DISTANCE = 1e50

#when the user is entering an array,
#the propmt string will sub in the
#loop number (1-base) for this placeholder:
INDEX.HOLDER.STRING = "iiindexii"

MIGRATION_SCHEME_PROMPT = 
	paste( "migration model?",
		"1 = 1-dimension stepping stone;", 
		"2 = 2-dimension stepping stone (only with a square",
		"\tnumber of populations, e.g. 9, 16,..144..);",
		"3 = island model;",
		"4 = hierarchical stepping stone ('contact zone');",
		"5 = hierarchical island ;",
		"6 = spatial models;", sep = "\n"  )


MUTATION_MODEL_LIST = 
	paste( 	"1= Kam,(same probability to mutate to any allelic state)",
			"2= Ssm, (single step mutation model)",
			"3= Mixed model of Ssm with a proportion of Kam mutation events)",
			"4= Mixed model of Ssm with a proportion of static double step mutation events)", 
		       	sep="\n" );

lr.conversion.functions=list ( "integer" = as.integer, "numeric" = as.numeric, "character" = as.character );

format.vals.as.nonscientific.strings=function( v.numeric )
{
	#expects a numeric vector,
	#returns a vector of string reps
	#of the numbers without using scientific
	#notation.

	v.strings = format( v.numeric, scientific = FALSE )
   
	return( v.strings )
}#end format.vals.as.nonscientific.strings

#' prompt.for.values.and.return.user.entries
#' 
#' prompts user at the console for parameter values, stores
#' and returns them in a vector length i.num.values, of user-entered values
#'
#' @param   s.prompt, printed to console on each iteration to get another value
#' @param   i.num.values, number of entries to get from the user.
#' @param   s.type.of.values, string, one of "integer", "numeric", or "character"
#' @param   v.valid.range  min, max values (inclusive) of valid values. If type "character", then NULL or list of valid strings)

prompt.for.values.and.return.user.entries=function( s.prompt, i.num.values, s.type.of.values, v.valid.range  )
 {

	#returned to caller:
	v.values.entered=NULL	

	if ( !s.type.of.values %in%  c( "integer", "numeric", "character" ) )
	 {
		 s.msg=paste(  "In function promp.for.values,",
		      	 "arg s.type.of.values passed invalid value:",
			 s.type.of.values,
			 ".  Expecting one of",
			 "\"integer\", \"numeric\", or \"character\"" );
		stop( s.msg )

	 }#end if invalid type
	
	b.is.number = ( s.type.of.values == "integer" || s.type.of.values == "numeric" )		 

	i.value.counter=0
		 
	for( idx in 1:i.num.values )
	{
		i.value.counter=i.value.counter+1

		v.scan.buff = NULL;

		b.is.in.range=FALSE;



		#if the prompt has the place holder, this will replace it
		#with the ordinal in the series of values to be prompted for:
		#The place holder should be missing if there is only 1 value to
		#be entered.
		s.prompt.with.index.number =  sub( pattern = INDEX.HOLDER.STRING,
						    replacement = as.character( idx ),
						    x = s.prompt )

		while( !b.is.in.range  )
		{
			#prompt may contain newlines, which "print"
			#will rep as "\n", hence we use cat, and so
			#will need to add an endline to get a new
			#prompt for the user's entry:
			cat( paste( s.prompt.with.index.number, "\n" )  )

			v.scan.buff = scan( nmax=1, what = s.type.of.values, 
					     flush = TRUE,  multi.line = FALSE )
			
			#we use tryCatch to return NULL if the conversion fails.
			#note that failed conversions of numbers can return NA
			#and generate only a warning (??), so we make handlers for
			#both both errors and warnings, in both cases returning NULL
			i.result.of.try = tryCatch( 
						{	#the value assigned to the tryCatch call if no error or warning: 
							lr.conversion.functions[[s.type.of.values]]( v.scan.buff[1] );
						},	  
						error = function ( e )
						{
							return( NULL )
						},
						warning = function ( w )
						{
							return( NULL )
						}
			)#end call to try catch

			if ( is.null( i.result.of.try ) )
			{
				b.is.in.range = FALSE
			}
			else if( !b.is.number )
			{	
				#character strings valid if range vector is NULL
				#or, if range non-null, the user entry matches an
				#entry in the v.valid.range vector
				if( is.null( v.valid.range ) )
				{
					b.is.in.range = TRUE
				}#end if v.valid.range is null
				else
				{
					b.is.in.range = FALSE

					if ( i.result.of.try %in% v.valid.range )
					{
						b.is.in.range = TRUE
					}#end if string in list
				}#end if NULL valid.range else we have strings in v.valid.rage
				
			}
			else
			{

		
				b.is.in.range = ( i.result.of.try >= v.valid.range[1] && i.result.of.try <= v.valid.range[2] )
			}#end if conversion result is NULL, else character, else numeric
	
		}#end while value entered is not in range


		v.values.entered=c( v.values.entered,i.result.of.try );

	}#end for each value

	return( v.values.entered )

}#end prompt.for.values.and.return.user.entries

#' get.selfing.parameters
#' 
#' prompt user for selfing parameters, return a list with user-entered values.

get.selfing.parameters=function( )
{
	#param names and values returned
	#to update the param list:
	lv.selfing=list()

	#intermediate value
	#used to get recomb 
	#and proportion of selfing (propauto)
	clonality = NULL
	grecombination = NULL
	gpropauto = NULL

	#reused to get results from prompt
	v.user.values=NULL


	v.user.values = prompt.for.values.and.return.user.entries(
		 "Proportion of clonal reproduction (between 0 and 1)",
		 1, "numeric", c( 0, 1)  );
	clonality = v.user.values[1]

	lv.selfing[["proportion_clonal_mating"]] = clonality

	if( clonality != 1.0 )
	{
		v.user.values = prompt.for.values.and.return.user.entries(   
			"Proportion of selfing (between 0 and 1)\nof the individuals not born as clones",
			1, "numeric", c( 0,1 ) );

		gpropauto = v.user.values[1]
	}
	else
	{
		gpropauto = 0.0 #random mating
	}#end if non clonality not 1.0, else 

	lv.selfing[["proportion_selfing_of_nonclones" ]] = gpropauto
	
	return ( lv.selfing )

}#end get.selfing.parameters



#' get.mating.parameters
#' 
#' prompts user for mating parameters and returns user-entered values as a list.
#'
#' @param   gploidy, integer giving ploidy
#' @param   g2sex, boolean, 1 or 0, whether there are 2 sexes.

get.mating.parameters=function ( gploidy, g2sex )
{

	#return param name and value list:
	lv.mating=list()
	
	v.user.values=NULL
	gmatesyst = NULL
	gpropmono = NULL
	gproppoly = NULL
	
	if (gploidy == 2 && g2sex==1)
	{
		s.prompt = paste( "Mating system?\n",
				 "1= Random mating\n", 
				 "2= Polygyny\n", 
				 "3= Monogyny")

		v.user.values = prompt.for.values.and.return.user.entries( 
						s.prompt, 1, "integer", c( 1, 3 ) )
		
		gmatesyst = v.user.values[1]

		lv.mating[["mating_system"]] = gmatesyst

		if( gmatesyst == 3 )
		{
			v.user.values = prompt.for.values.and.return.user.entries( 
				"Proportion of extra pair copulations (between 0 and 1)",
				1, "numeric", c( 0, 1 ) )

			gpropmono = v.user.values[1]	

			lv.mating[["proportion_extra_pair_copulation"]] = gpropmono 
		}
		else if ( gmatesyst  == 2 )
		{
			v.user.values = prompt.for.values.and.return.user.entries( 
				"Proportion of matings by subordinate males (between 0 and 1)",
				1, "numeric", c( 0,1 ) )

			gproppoly = v.user.values[1]

			lv.mating[["proportion_matings_by_subordinate_males"]] = gproppoly
		}
	
	}#end if diploidy and two sexes

	return( lv.mating )

}#end get.mating.parameters


#' get.reproduction.parameters
#' 
#' prompts user for reproduction parameters and returns user-entered values as a list.
#'

get.reproduction.parameters=function()
{
	lv.repro = list()

	#returned from get.selfing.parameters
	#if user wants cloning
	lv.selfing = NULL;

	#values for recording for config
	#struct, and some only as conditionals
	#needed for correctly prompting 
	gploidy=NULL
	grecombination=NULL
	g2sex=NULL
	gploidyfem=NULL
	gploidymal=NULL
	gpropauto=NULL	

	v.user.values = prompt.for.values.and.return.user.entries( 
				"Ploidy level ? (0=haplo-diploid; 1=haploid; 2=diploid)",
				1, "integer", c( 0, 2 ) )

	gploidy=v.user.values[1]

	lv.repro[["ploidy"]] = gploidy
	
	if (gploidy == 0)
	{

		print("Note: colonies will be referred to as populations !");
		g2sex=1
		lv.repro[["two_sexes"]] = g2sex
	}
	else if (gploidy == 1)
	{
		g2sex=0;
		gploidyfem=1;
		v.user.values = prompt.for.values.and.return.user.entries( 
				"Proportion of recombination (between 0 and 1)",
						1, "numeric", c( 0, 1 ) );

		grecombination = v.user.values[1];

		#record param proportion recombination:
		lv.repro[["proportion_recombination"]] = grecombination;
		lv.repro[["two_sexes"]] = g2sex 
 	}
	else if ( gploidy == 2 )
	{
		gploidyfem=2;
		gploidymal=2; #/*si hermaphrodite, cette valeur ne sera pas utilised*/
		#assume 2 sexes, prompt in case user wants hermaphrodite case
		g2sex=1;

		v.user.values = prompt.for.values.and.return.user.entries( 	
		"Two sexes?:y/n", 1, "character", c( "y", "n" ) ); 
		s.val = v.user.values[1]
		if( s.val == "n" )
		{
			g2sex = 0;
		}
		else
		{
			g2sex = 1;
		}#end if no else yes


		if( g2sex==0  )
		{

			v.user.values = prompt.for.values.and.return.user.entries(
					"Random mating?:y/n",1 ,"character", c( "y", "n" ) )
			s.random.mating = v.user.values[1]	
			i.random.mating = ifelse( s.random.mating == "y", TRUE.AS.INT, FALSE.AS.INT )

			lv.selfing[["random_mating"]] =  i.random.mating
			
			if( i.random.mating == FALSE.AS.INT )
			{	
				lv.selfing = get.selfing.parameters()
				lv.repro=append( lv.repro, lv.selfing )
			}#end  if no random mating, get selfing params

		}#end if g2sex == 0

		lv.repro[["two_sexes"]] = g2sex 

	}#end if ploidy == 0, else 1, else 2

	return( lv.repro )

}#end get.reproduction.parameters



#' get.population.parameters
#' 
#' prompts user for population parameters and returns user-entered values as a list.
#'
#' @param   gploidy, integer giving ploidy
#' @param   g2sex, boolean, 1 or 0, whether there are 2 sexes.

get.population.parameters = function( gploidy, g2sex )
{
	gnbpop = NULL
	lv.pop = list()

	v.user.values = prompt.for.values.and.return.user.entries( 
			"Number of populations ?",
			1, "integer", c( 1, MAX_NUMBER_POPULATIONS ) );

	gnbpop = v.user.values[1];
	
	lv.pop[["number_populations"]] = gnbpop

	if ( gnbpop > 1 )
	{

		v.user.values = prompt.for.values.and.return.user.entries( 
			"Same number of individuals in each populations ?:y/n",
			1, "character", c( "y", "n" ) );

		s.unif.pop.size =  v.user.values[1];
	}
	else
	{
		s.unif.pop.size = "y"
	}#end if > 1


	i.unif.pop.size = ifelse( 
		s.unif.pop.size == "y", TRUE.AS.INT, FALSE.AS.INT )	

	lv.pop[["same_number_individuals_each_population"]] = i.unif.pop.size

	
	#first, we deal with non-haplodiploids
	#( ploidy not zero)

	if( gploidy != 0 )
	{
		#if single sex
		if( g2sex == FALSE.AS.INT )
		{
			if ( i.unif.pop.size == FALSE.AS.INT )
			{
				#since we are prompting for more than one value,
				#we add the string that will be replaced
				#by the pop number in the prompt fx:
				s.prompt.with.index.holder = paste( 
					"enter number of individuals of population",
					INDEX.HOLDER.STRING )

				v.user.values = prompt.for.values.and.return.user.entries( 
						s.prompt.with.index.holder, 
						gnbpop, "integer", 
						c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )

				s.per.pop.size=paste( v.user.values, collapse="," )

				lv.pop[["per_population_individual_counts"]] = s.per.pop.size
					
			}
			else #uniformly sized populations
			{
				v.user.values = prompt.for.values.and.return.user.entries( 
						"Number of individuals ?",
						1, "integer", c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )

				lv.pop[["number_of_individuals"]] = v.user.values[1]
			}#if non-uniform pop sizes, else uniform
		}
		else #two_sexes case
		{
			#two sexes, non-uniform pop sizes:	
			if ( i.unif.pop.size == FALSE.AS.INT )
			{
				s.prompt.with.index.holder = paste( 
						"enter number of females of population",
						INDEX.HOLDER.STRING )
				v.user.values = prompt.for.values.and.return.user.entries( 
						s.prompt.with.index.holder,
						gnbpop, "integer", 
						c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )
				s.per.pop.female.count=paste( v.user.values, collapse = "," )
				lv.pop[["per_population_number_of_females"]] = s.per.pop.female.count 


				s.prompt.with.index.holder = paste( 
						"enter number of males of population",
						INDEX.HOLDER.STRING )
				v.user.values = prompt.for.values.and.return.user.entries( 
						s.prompt.with.index.holder,
						gnbpop, "integer", 
						c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )

				s.per.pop.male.count=paste( v.user.values, collapse = "," )

				lv.pop[["per_population_number_of_males"]] = s.per.pop.male.count 

			}
			else #two sexes, uniform pop sizes
			{
				v.user.values = prompt.for.values.and.return.user.entries( 
							"Number of females in each population ?",
							1, "integer", c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )
				i.females.per.pop=v.user.values[1]
				lv.pop[["number_females_each_population"]] = i.females.per.pop


				v.user.values = prompt.for.values.and.return.user.entries( 
							"Number of males in each population ?",
							1, "integer", c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )
				i.males.per.pop=v.user.values[1]

				lv.pop[["number_males_each_population"]] = i.females.per.pop

			}#end if nonuniform, else uniform pop sizes
		}#end if single, else two sexes

	}
	else #haplodiploidsy
	{
		for(  s.indtype in c( "queens", "males", "workers" ) )
		{
			if ( i.unif.pop.size == FALSE.AS.INT )
			{
				s.prompt.with.index.holder = paste( 
								"enter number of" , 
								s.indtype, 
								"of population",
								INDEX.HOLDER.STRING )

				v.user.values = prompt.for.values.and.return.user.entries( 
						s.prompt.with.index.holder,
						gnbpop, "integer", 
						c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )

				s.per.pop.count=paste( v.user.values, collapse = "," )

				s.key=paste( "per_population_number_of_", s.indtype, sep="" )

				lv.pop[[s.key]] = s.per.pop.count 
			}
			else #uniform value for each type
			{
				s.prompt=paste( "Number of ", s.indtype, " in each population?", sep="" )
				v.user.values = prompt.for.values.and.return.user.entries(
						s.prompt, 1, "integer", c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )

				s.key=paste( "number_", s.indtype, "_each_population", sep="" )

				lv.pop[[s.key]] = v.user.values[1]

			}#end if uniform pop size else not
		}#end for each type of indiv, get  count
	}#end if non-haplodip, else haplodip

	return( lv.pop )
}#end get.population.parameters 

get.migr.proportions.non.hier=function( g2sex, b.second.scheme )
{
	lv.props=list()
	v.user.values=NULL
	v.user.values.a=NULL

	if( g2sex == FALSE.AS.INT )
	{
	
		v.user.values = prompt.for.values.and.return.user.entries( 	
			"proportion of migration?(between 0 and 1)",
			1, "numeric", c( 0,1 ) )
		
		if( !b.second.scheme )
		{
			lv.props[["proportion_migration"]] = v.user.values[1]
		}
		else
		{
			lv.props[["proportion_migration_second_scheme"]] = v.user.values[1]
		}#end if first, else second scheme
	}
	else
	{
		v.user.values = prompt.for.values.and.return.user.entries( 	
			"proportion of female migration?(between 0 and 1)",
			1, "numeric", c( 0,1 ) )

		v.user.values.a = prompt.for.values.and.return.user.entries( 	
			"proportion of male migration?(between 0 and 1)",
			1, "numeric", c( 0,1 ) )

		if( !b.second.scheme )
		{
			lv.props[["proportion_female_migration"]] = v.user.values[1]	
			lv.props[["proportion_male_migration"]] = v.user.values.a[1]	
		}
		else
		{
			lv.props[["proportion_female_migration_second_scheme"]] = v.user.values[1]
			lv.props[["proportion_male_migration_second_scheme"]] = v.user.values.a[1]	
		}#end if first else second scheme
		
	}#end if one sex else two

	return (lv.props )
}#end get.migr.proportions.non.hier

get.migr.proportions.hier = function( g2sex, b.second.scheme )
{
	lv.props=list()
	v.user.values=NULL
	v.user.values.a=NULL
	v.user.values.b=NULL
	v.user.values.c=NULL

	if ( g2sex == FALSE.AS.INT )
	{
		
		v.user.values = prompt.for.values.and.return.user.entries( 	
			"proportion of migration within groups?(between 0 and 1)",
			1, "numeric", c( 0,1 ) )
	
		v.user.values.a = prompt.for.values.and.return.user.entries( 	
			"proportion of migration between groups?(between 0 and 1)",
			1, "numeric", c( 0,1 ) )

		f.sum.within.and.between=v.user.values[1] + v.user.values.a[1]

		if ( f.sum.within.and.between > 1.0 )
		{
			stop( "The sum of migration within and between must not exceed one" )
		}#end if props sum to greater than one

		if( !b.second.scheme )
		{
			lv.props[["proportion_migration_within_groups"]] = v.user.values[1]
			lv.props[["proportion_migration_between_groups"]] = v.user.values.a[1]

		}
		else
		{
			lv.props[["proportion_migration_within_groups_second_scheme"]] = v.user.values[1]
			lv.props[["proportion_migration_between_groups_second_scheme"]] = v.user.values.a[1]
		}#end if first else second scheme
	}
	else
	{
			
		v.user.values = prompt.for.values.and.return.user.entries( 	
			"proportion of female migration within groups?(between 0 and 1)",
			1, "numeric", c( 0,1 ) )
	
		v.user.values.a = prompt.for.values.and.return.user.entries( 	
			"proportion of female migration between groups?(between 0 and 1)",
			1, "numeric", c( 0,1 ) )

		f.sum.within.and.between=v.user.values[1] + v.user.values.a[1]

		if ( f.sum.within.and.between > 1.0 )
		{
			stop( "The sum of migration within and between must not exceed one" )
		}#end if props sum to greater than one

		
		v.user.values.b = prompt.for.values.and.return.user.entries( 	
			"proportion of male migration within groups?(between 0 and 1)",
			1, "numeric", c( 0,1 ) )
	
		v.user.values.c = prompt.for.values.and.return.user.entries( 	
			"proportion of male migration between groups?(between 0 and 1)",
			1, "numeric", c( 0,1 ) )


		f.sum.within.and.between=v.user.values.b[1] + v.user.values.c[1]

		if ( f.sum.within.and.between > 1.0 )
		{
			stop( "The sum of migration within and between must not exceed one" )
		}#end if props sum to greater than one

		if( !b.second.scheme )
		{
			lv.props[["proportion_female_migration_within_groups"]] = v.user.values[1]
			lv.props[["proportion_female_migration_between_groups"]] = v.user.values.a[1]
			lv.props[["proportion_male_migration_within_groups"]] = v.user.values.b[1]
			lv.props[["proportion_male_migration_between_groups"]] = v.user.values.c[1]
		}
		else
		{
			lv.props[["proportion_female_migration_within_groups_second_scheme"]] = v.user.values[1]
			lv.props[["proportion_female_migration_between_groups_second_scheme"]] = v.user.values.a[1]
			lv.props[["proportion_male_migration_within_groups_second_scheme"]] = v.user.values.b[1]
			lv.props[["proportion_male_migration_between_groups_second_scheme"]] = v.user.values.c[1]

		}#end if first, else second scheme


	}#end if one sex, else two

	return( lv.props )

}#end get.migr.proportions.hier

get.migr.one.dim.step = function( g2sex, gnbpop, b.second.scheme )
{
	lv.params = list()

	lv.props = get.migr.proportions.non.hier( g2sex, b.second.scheme )

	lv.params = append( lv.params, lv.props )

	return( lv.params )
}#end get.migr.one.dim.step

get.migr.two.dim.step = function( g2sex, gnbpop, b.second.scheme )
{
	lv.params = list()
	f.root=sqrt( gnbpop )
	i.root = as.integer( f.root )

	i.square=i.root*i.root

	if( i.square != gnbpop )
	{
		stop( "the two dimensional stepping stone needs a square number of populations !");
	}#end if not integer root

	lv.props=get.migr.proportions.non.hier( g2sex, b.second.scheme )
	
	lv.params=append( lv.params, lv.props )

	return( lv.params )
}#end get.migr.two.dim.step

get.migr.island = function( g2sex, gnbpop, b.second.scheme )
{
	lv.params = list()
	lv.props=get.migr.proportions.non.hier( g2sex, b.second.scheme )
	lv.params=append( lv.params, lv.props )
	return( lv.params )
}#end get.migr.island

get.migr.hier.step = function( g2sex, gnbpop, b.second.scheme )
{

	lv.params = list()
	gfirstrace = NULL
	gfirstrace2 = NULL

	v.user.values = prompt.for.values.and.return.user.entries( 	
		"How many populations form the first group of populations",
		1, "integer", c( 1, MAX_NUMBER_POPULATIONS ) );


	if ( !b.second.scheme )
	{
		gfirstrace = v.user.values[1]
		lv.params[["number_populations_first_population_group"]] = gfirstrace
	}
	else
	{
		gfirstrace2 = v.user.values[1]
		lv.params[["number_populations_first_population_group_second_scheme"]] = gfirstrace2
	}# end if first scheme else second

	lv.props = get.migr.proportions.hier( g2sex, b.second.scheme )	

	lv.params=append( lv.params, lv.props )


	return( lv.params )

}#end get.migr.hier.step

get.migr.hier.island = function( g2sex, gnbpop, b.second.scheme )
{
	lv.params = list()
	gfirstrace=NULL
	gfirstrace2=NULL

	v.user.values = prompt.for.values.and.return.user.entries( 	
		"Number of archipelagos?",
		1, "integer", c( 1, MAX_NUMBER_POPULATIONS ) );

	i.num.arch=v.user.values[1]

	if( !b.second.scheme )
	{

		gfirstrace = i.num.arch
		lv.params[["number_of_archipelagos"]] = gfirstrace
	}
	else
	{
		gfirstrace2 = i.num.arch
		lv.params[["number_of_archipelagos_second_scheme"]] = gfirstrace2
	}#end if first else second scheme

	v.user.values = prompt.for.values.and.return.user.entries( 
		 "Same number of populations in each archipelago:y/n",
		 1, "character", c( "y", "n" ) )

	s.unif.pops.per.arch = v.user.values[1]	

	i.unif.pops.per.arch  = ifelse( 
		s.unif.pops.per.arch == "y", TRUE.AS.INT, FALSE.AS.INT )

	#recored wheter uniform pops per arch:
	if( !b.second.scheme )
	{
		lv.params[["same_number_populations_each_archipelago"]] = i.unif.pops.per.arch
	}
	else
	{
		lv.params[["same_number_populations_each_archipelago_second_scheme"]] = i.unif.pops.per.arch
	}#end if first else second scheme

	
	if( i.unif.pops.per.arch == FALSE.AS.INT )
	{
		

		s.prompt.with.index.holder = paste( 
					"enter number of populations of archipelago",
					INDEX.HOLDER.STRING )

		v.user.values = prompt.for.values.and.return.user.entries( 
			s.prompt.with.index.holder,
			i.num.arch,
			"integer",
			c( 1, MAX_NUMBER_POPULATIONS ) )

		s.per.arch.num.pops=paste( v.user.values, collapse="," )

		if ( !b.second.scheme )
		{

			lv.params[["per_archipelago_number_of_populations"]] = s.per.arch.num.pops
		}
		else
		{
			lv.params[["per_archipelago_number_of_populations_second_scheme"]] = s.per.arch.num.pops
			
		}#end if first, else second, scheme

	}
	else
	{
		
		#note that if there are to be uniform number of pops
		#in each archipelage, the program does not look for a user
		#entered valud but just divides the number
	      	#of populations (gnbpop) by the number of archipelagos,
		#so no user entry needed.  We do test that the total for pops
	        #is evenly dividable by total archipelagos	

		i.pops.per.arch = as.integer( gnbpop/i.num.arch )
		i.recalc.gnbpop = i.pops.per.arch*i.num.arch

		if( i.recalc.gnbpop != gnbpop )
		{
			stop( "Your values do not match the number of populations!" )
		}#end if pops can't be equally divided into archs
	}#end if unif pops per arch else not

	lv.props = get.migr.proportions.hier( g2sex, b.second.scheme )	

	lv.params=append( lv.params, lv.props )

	return( lv.params )

}#end get.migr.hier.island

get.migr.spatial = function( g2sex, gnbpop, b.second.scheme )
{
	lv.params = list()
	
	v.user.values=NULL
	i.num.dimen=NULL
	f.mean.dispersal=NULL
	f.mean.female.dispersal=NULL
	f.mean.male.dispersal=NULL
	
	#we represent our matrix of per-pop,
	#per-dimenstion values as a semi-colon
	#delimted list of comma-separated coordinates
	#as a string
	s.coord.matrix=NULL

	#first we get migration proportions:	
	lv.proportions=get.migr.proportions.non.hier( g2sex, b.second.scheme )

	lv.params = append( lv.params, lv.proportions )

	v.user.values = prompt.for.values.and.return.user.entries( 
				"How many dimensions define the space(at least one)",
				1, "integer", c( 1, MAX_NUMBER_DIMENSIONS ) )

	i.num.dimen = v.user.values[1]	
	
	#here we need a matrix, hence we prompt per-pop-per-dimension

	for( i.pop in 1:gnbpop )
	{
		s.prompt = paste( "enter coordinate ", INDEX.HOLDER.STRING,
				 " of population ", i.pop )

		v.user.values = prompt.for.values.and.return.user.entries( 
				s.prompt, i.num.dimen, "integer", 
				c( 1, MAX_NUMBER_DIMENSIONS ) )

		#if number is very small, default R will write in scientific
		#notation -- not readable by easypop, so:	
		v.user.values = format.vals.as.nonscientific.strings( v.user.values )

		s.coords.this.pop=paste( v.user.values, collapse="," )

		if( i.pop == 1 )
		{
			s.coord.matrix = s.coords.this.pop
		}
		else
		{
			s.coord.matrix = paste( s.coord.matrix, s.coords.this.pop, sep=";" )
		}#end if first pop, else not

	}#end for each pop get a set of per-dimension coords

	if( !b.second.scheme )
	{
		lv.params[["per_population_per_dimension_coordinates"]] = s.coord.matrix

	}
	else
	{
		lv.params[["per_population_per_dimension_coordinates_second_scheme"]] = s.coord.matrix
	}#end if first else second scheme

	if( g2sex == FALSE.AS.INT )
	{

		v.user.values = s.prompt.with.index.holder = paste( 
				"enter mean dispersal distance",
				1, "numeric", c( 0, MAX_DISPERSAL_DISTANCE ) )

		f.mean.dispersal = v.user.values[1]

		if( !b.second.scheme )
		{
			lv.params[["mean_dispersal_distance"]] = f.mean.dispersal
		}
		else
		{
			lv.params[["mean_dispersal_distance_second_scheme"]] = f.mean.dispersal
		}#end if first else second scheme
	}
	else
	{

		v.user.values = prompt.for.values.and.return.user.entries( 
					"enter mean female dispersal distance",
					1, "numeric", c( 0, MAX_DISPERSAL_DISTANCE ) )

		f.mean.female.dispersal = v.user.values[1]

		v.user.values = prompt.for.values.and.return.user.entries( 
					"enter mean male dispersal distance",
					1, "numeric", c( 0, MAX_DISPERSAL_DISTANCE ) )
		f.mean.male.dispersal = v.user.values[1]

		if( !b.second.scheme )
		{
			
			lv.params[["mean_female_dispersal_distance"]] = f.mean.female.dispersal
			lv.params[["mean_male_dispersal_distance"]] = f.mean.male.dispersal

		}
		else
		{
			lv.params[["mean_female_dispersal_distance_second_scheme"]] = f.mean.female.dispersal
			lv.params[["mean_male_dispersal_distance_second_scheme"]] = f.mean.male.dispersal

		}#end if first else second scheme

	}#end if one else two sexes

	return( lv.params )

}#end get.migr.spatial

#' get.migration.scheme.details
#' 
#' prompts user for details of migration scheme parameters and returns user-entered values as a list.
#' this function is called only if the number of pops exceeds 2 (else they are trivially assigned island in 
#' the fx, get.migration.parameters. 
#' @param   g2sex, boolean, 1 or 0, whether there are 2 sexes.
#' @param   gnbpop, integer, number of populations.

get.migration.scheme.details = function( g2sex, gnbpop, b.second.scheme )
{

	lv.scheme = list()
	lv.migr.params = NULL

	lv.user.values = prompt.for.values.and.return.user.entries( 
				MIGRATION_SCHEME_PROMPT,
				1, "integer", c( 1, MAX_NUMBER_POPULATIONS ) ) 

	gtypemigr = lv.user.values[1] 

	lv.scheme[["migration_model"]] = gtypemigr

	if( gtypemigr == 1 )
	{
		lv.migr.params = get.migr.one.dim.step( g2sex, gnbpop, b.second.scheme )
	}
	else if( gtypemigr == 2 )
	{
		lv.migr.params = get.migr.two.dim.step(  g2sex, gnbpop, b.second.scheme )
	}
	else if( gtypemigr == 3 )
	{
		lv.migr.params = get.migr.island( g2sex, gnbpop, b.second.scheme )  
	}
	else if( gtypemigr == 4 )
	{
		lv.migr.params = get.migr.hier.step( g2sex, gnbpop, b.second.scheme )
	}
	else if( gtypemigr == 5 )
	{
		lv.migr.params = get.migr.hier.island( g2sex, gnbpop, b.second.scheme )
	}
	else if( gtypemigr == 6 )
	{
		lv.migr.params = get.migr.spatial( g2sex, gnbpop, b.second.scheme )
	}
	else
	{
		s.msg=( paste( "error: unknown migration model number, ", gtypemigr ) )
		stop( s.msg )
	} #end if type 1,2...6, else error

	lv.scheme=append( lv.scheme, lv.migr.params )

	return( lv.scheme )

}#end get.migration.scheme.details

#' get.migration.parameters
#' 
#' prompts user for migration scheme parameters and returns user-entered values as a list.
#'
#' @param   g2sex, boolean, 1 or 0, whether there are 2 sexes.
#' @param   gnbpop, integer, number of populations.

get.migration.parameters = function( g2sex, gnbpop )
{

	lv.migr=list()
	lv.scheme = list()
	gtypemigr = NULL

	if ( gnbpop <= 2 )
	{
		#default for 1 or 2 pops is  island
		lv.migr[["migration_model"]] = 3
		gtypemigr = 3

		if( gnbpop == 1 )
		{
			#ep uses vars labeled "female" when under haploidy and needs
			#a value:	
			lv.migr[["proportion_female_migration"]] = 0.0
			if( g2sex == TRUE.AS.INT )
			{
				#2-sexes, we also need to set maile migr
				#to zero for 1-pop scenario:
				lv.migr[["proportion_male_migration"]] = 0
			}#end if 2 sexes
			return( lv.migr )
		}#end if one pop only return after trivial migration assignments

	}#end if only one or two pops, set as island model

	v.user.values = prompt.for.values.and.return.user.entries( 
			"Same migration scheme over all simulation? (y/n)",
			1, "character", c( "y", "n" ) );

	s.unif.migr.scheme = v.user.values[1]

	i.unif.migr.scheme = ifelse( 
		 s.unif.migr.scheme == "y", TRUE.AS.INT, FALSE.AS.INT )	

	lv.migr[["same_migration_scheme_all_simulation"]] = i.unif.migr.scheme

	if( i.unif.migr.scheme == TRUE.AS.INT )
	{
		lv.scheme = get.migration.scheme.details( g2sex, gnbpop, FALSE );
	}
	else	
	{

		print("migration model for the first part of the simulation?");
		lv.scheme.one = get.migration.scheme.details( g2sex, gnbpop, FALSE )
		print("migration model for the second part of the simulation?");
		lv.scheme.two = get.migration.scheme.details( g2sex, gnbpop, TRUE )

		lv.scheme = append( lv.scheme, lv.scheme.one )
		lv.scheme = append( lv.scheme, lv.scheme.two )

	}#end if one scheme only, else two schemes

	lv.migr = append( lv.migr, lv.scheme )

	return( lv.migr )
}#end get.migration.parameters

get.mutation.rate = function( i.num.loci, i.unif.mut.scheme )
{
	lv.mutation.rates = list()
	v.user.values = NULL

	if( i.num.loci > 1 && i.unif.mut.scheme == FALSE.AS.INT )
	{

		s.prompt = paste( "Mutation rate of locus", 
			 		INDEX.HOLDER.STRING,
			       	   "? (between 0 and 1, e.g. 0.0001)" )

		v.user.values = prompt.for.values.and.return.user.entries( 
			s.prompt, i.num.loci, "numeric" , c( 0.0, 1.0 ) )
	
		#avoid writing floats as scientific notation:	
		v.user.values = format.vals.as.nonscientific.strings( v.user.values )

		s.per.locus.mut.rate=paste( v.user.values, collapse = "," )

		lv.mutation.rates[["per_locus_mutation_rate"]] = s.per.locus.mut.rate
		
	}#end if at least 2 loci and want non-uniform mut rate
	else
	{
		v.user.values = prompt.for.values.and.return.user.entries(
			"Mutation rate? (between 0 and 1, e.g. 0.0001)",
			1, "numeric", c( 0.0, 1.0 ) )

		f.mut.rate = v.user.values[1]

		lv.mutation.rates[["mutation_rate"]] = f.mut.rate
		
	}#end if want per-locus mut scheme, else not

	return( lv.mutation.rates )

}#end get.mutation.rate

get.mixed.mutation.scheme.details = function( s.type )
{
	lv.mut.details = list()
	v.user.values = NULL
	s.prompt = NULL

	if( s.type == "kam" )
	{

		s.prompt = "Proportion of Kam mutation events (between 0 and 1)"
		s.key = "proportion_kam_mutation_events"
	}
	else if ( s.type == "step" )
	{
		s.prompt = "Proportion of double step mutation events (between 0 and 1)"
		s.key = "proportion_double_step_mutation_events"

	}#end if kam else step type

	v.user.values =  prompt.for.values.and.return.user.entries( 
		s.prompt, 1, "numeric", c( 0.0, 1.0 ) )

	
	f.prop = v.user.values[1]

	lv.mut.details[[s.key]] = f.prop

	return( lv.mut.details )

}#end get.mixed.mutation.scheme.details

get.mutation.scheme = function( i.num.loci, i.unif.mut.scheme )
{
	lv.mut.scheme = list()
	lv.mixed.models = list()
	i.this.scheme = NULL
	v.user.values = NULL

	if ( i.num.loci > 1 && i.unif.mut.scheme == FALSE.AS.INT )
	{

		for ( i.locus in 1:i.num.loci )
		{

			s.prompt = paste( "Mutation model for locus ", i.locus, 
					 "?", MUTATION_MODEL_LIST,  sep="" )

			v.user.values = prompt.for.values.and.return.user.entries( 
				s.prompt, 1, "integer", c( 1, MAX_NUMBER_MUTATION_MODEL ) )

			i.this.scheme = v.user.values[1]

			if( i.locus == 1 )
			{
				lv.mut.scheme[["per_locus_mutation_model"]] = i.this.scheme
			}
			else
			{
				lv.mut.scheme[["per_locus_mutation_model"]]= paste( 
					lv.mut.scheme[["per_locus_mutation_model"]], 
					i.this.scheme, sep="," )
			}#end if first locus else not			

			if( i.this.scheme %in% c(3,4) )
			{
				lv.this.scheme.details = NULL

				#for per-locus, mixed mutation schemes we need a special kind of list that
				#tells ep which loci numbers are associated with which mixed scheme proportion.
				#So, after the ep config file format, we make a list for each of the two mixed models 
				#(if the user chooses).  The list is of the form l,p;l,p;... with l giving the 
				#locus number and p, the proportion, the two vals separated by a comma. 
				#These pairs are separated by semi-colons (when >1 locus has the same model)
				if ( i.this.scheme == 3 )
				{
					lv.this.scheme.details = get.mixed.mutation.scheme.details( "kam" )
					#the call to get.mixed.mutation.scheme.details() returns the "single"
					#parameter key name, which we use to extract the proportion value:
					s.key.for.single="proportion_kam_mutation_events"
					#because were recording per locus scheme details,
					#we need to use the list parameter name:
					s.key.for.list="locus_numbers_and_proportions_kam_mutation_events"

				}
				else
				{
					lv.this.scheme.details = get.mixed.mutation.scheme.details( "step" )
					s.key.for.single = "proportion_double_step_mutation_events"
					s.key.for.list ="locus_numbers_and_proportions_double_step_mutation_events"
				}#end if scheme 3 else 4

				f.prop =lv.this.scheme.details[[s.key.for.single]]
				
				#non-singleton entries containing floats
				#need this conversion before the string
				#conversions (of singletons) in fx, 
				#get.parameter.list.for.ep.processing
				#easypop won't  correctly read in string reps
				#of floats in R's scientific notation:
				s.prop = format( f.prop, scientific = FALSE )

				if ( !( s.key.for.list %in% names( lv.mixed.models ) ) )
				{
					lv.mixed.models[[s.key.for.list]] = 
						paste( i.locus, s.prop, sep="," )
				}				
				else #list exists, so we append this loci,prop pair
				{
					s.duple = paste( i.locus, s.prop, sep="," )

					s.current.list = lv.mixed.models[[s.key.for.list]]
					
					lv.mixed.models[[s.key.for.list]] = 
						paste( s.current.list, s.duple, sep=";" )
				}#end if first (possibly only) mixed model of this kind, 
				 #else add to existing list...
			}#end if selected scheme is mixed, need a proportion and to record loci-num/prop pair

		}#end for each locus

		lv.mut.scheme = append( lv.mut.scheme, lv.mixed.models )
	}
	else
	{
		s.prompt = paste( "Mutation model?", MUTATION_MODEL_LIST, sep = "\n" )

		v.user.values = prompt.for.values.and.return.user.entries( 
			s.prompt, 1, "integer", c( 1, MAX_NUMBER_MUTATION_MODEL ) )

		i.this.scheme = v.user.values[1]

		lv.mut.scheme[[ "mutation_model" ]] = i.this.scheme
		
		if( i.this.scheme %in% c( 3,4) )
		{
			lv.this.scheme.details = NULL
			s.mixed.type = ifelse( i.this.scheme == 3, "kam", "step" )

			lv.this.scheme.details = get.mixed.mutation.scheme.details( s.mixed.type )
			lv.mut.scheme = append( lv.mut.scheme, lv.this.scheme.details ) 

		}#end if mixed scheme, get proportion
	
	}#if non-unif mutscheme else uniform

	return ( lv.mut.scheme )

}#end get.mutation.scheme

get.allele.parameters = function( i.num.loci, i.unif.mut.scheme )
{
	lv.allele = list()
	v.user.values = NULL


	if( i.num.loci > 1 && i.unif.mut.scheme == FALSE.AS.INT )
	{
		s.prompt = paste( "Number of possible allelic states of locus,", INDEX.HOLDER.STRING, "? (below 1000)" )

		v.user.values = prompt.for.values.and.return.user.entries( 
			s.prompt, i.num.loci, "integer", 
			c( 1, MAX_NUMBER_ALLELIC_STATES ) )

		s.per.locus.allelic.states=paste( v.user.values, collapse="," )
		lv.allele[["per_locus_number_possible_allelic_states"]]= s.per.locus.allelic.states

	}
	else
	{

		s.prompt = paste( "Number of possible allelic states? (below 1000)" )

		v.user.values = prompt.for.values.and.return.user.entries( 
			s.prompt, 1, "integer", c( 1, MAX_NUMBER_ALLELIC_STATES ) )

		lv.allele[["number_possible_allelic_states"]] = v.user.values[1] 

	}#end if non-unif mut scheme, else uniform


	s.prompt = paste( "Variability of the initial population",
			 "1= Maximal, (randomly assigned alleles)",
			 "2= Minimal, (all individuals start with the same allele",
			 sep = "\n" )

	v.user.values = prompt.for.values.and.return.user.entries( 
		s.prompt, 1, "integer", c( 1,2 ) )

	lv.allele[["variability_initial_population"]] = v.user.values[1]

	return( lv.allele )

}#end get.allele.parameters

get.generation.parameters=function( i.unif.migr.scheme )
{
	lv.generations = list()
	v.user.values = NULL

	s.prompt = "Number of generations ?" 
	v.user.values = prompt.for.values.and.return.user.entries( 
		s.prompt, 1, "integer" , c( 1, MAX_NUMBER_GENERATIONS ) )
	i.num.gens = v.user.values[1]
	lv.generations[["number_of_generations"]] = i.num.gens

	if( i.unif.migr.scheme == FALSE.AS.INT )
	{
		s.prompt = "Number of generations before the migration scheme changes (< number of generations)?"

		v.user.values = prompt.for.values.and.return.user.entries(
			s.prompt, 1, "integer", c( 1, (i.num.gens - 1) ) )

		lv.generations[["number_of_generations_before_migration_scheme_change"]] = v.user.values[1]

 	}#end if non-uniform migrationscheme

	return( lv.generations )
}#end get.generation.parameters

#' get.genetic.parameters
#' 
#' prompts user for genetic parameters and returns user-entered values as a list.
#'
get.genetic.parameters = function( )
{
	lv.genetics = list()
	v.user.values = NULL

	v.user.values = prompt.for.values.and.return.user.entries( 
		"Number of loci?", 1, "integer", c( 1, MAX_NUMBER_LOCI ) )

	i.num.loci = v.user.values[1]
	
	lv.genetics[["number_of_loci"]] = i.num.loci	

	v.user.values = prompt.for.values.and.return.user.entries( 
		"Free recombination between loci?: y/n",
		1, "character", c( "y", "n" ) )

	s.free.recomb = v.user.values[1]
	i.free.recomb = ifelse( s.free.recomb=="y", TRUE.AS.INT, FALSE.AS.INT )
	lv.genetics[["free_recombination_between_loci"]] = i.free.recomb

	if(  i.free.recomb == FALSE )
	{
		s.prompt = paste( "Recombination rate between adjacent loci (ie between locus n and n+1)",
					"The recombination rate must be comprised between 0.0 and 0.5",
					sep = "\n" );
		v.user.values = prompt.for.values.and.return.user.entries( 
			s.prompt, 1, "numeric", c( 0.0, 0.5 ) )

		f.recomb = v.user.values[1]
		lv.genetics[["recombination_rate_between_adjacent_loci"]] = f.recomb
	}#end if non-free recomb

	v.user.values = prompt.for.values.and.return.user.entries( 
				"Do all loci have the same mutation scheme?:y/n",
				1, "character", c( "y", "n" ) )
	
	s.unif.mut.scheme = v.user.values[1]
	i.unif.mut.scheme = ifelse( s.unif.mut.scheme == "y", 
			 	TRUE.AS.INT, FALSE.AS.INT )

	lv.genetics[["all_loci_same_mutation_scheme"]] = i.unif.mut.scheme 

	lv.mutrates = get.mutation.rate( i.num.loci, i.unif.mut.scheme )
	lv.genetics = append( lv.genetics, lv.mutrates )

	lv.mut.scheme = get.mutation.scheme( i.num.loci, i.unif.mut.scheme )
	lv.genetics = append( lv.genetics, lv.mut.scheme )

	lv.allele = get.allele.parameters( i.num.loci, i.unif.mut.scheme )
	lv.genetics = append( lv.genetics, lv.allele )

	return ( lv.genetics )
}#end get.genetic.parameters

get.output.parameters = function( i.number.of.populations, i.two.sexes )
{
	lv.output = list()
	v.user.values = NULL
	i.complete.data = NULL
	i.pops.out = NULL

	s.prompt = "Do you want the complete dataset in the '.dat' and '.gen' result files ?:y/n"
	v.user.values = prompt.for.values.and.return.user.entries( 
		s.prompt, 1, "character", c( "y", "n" ) )
	s.complete.data = v.user.values[1]
	i.complete.data = ifelse( s.complete.data == "y", TRUE.AS.INT, FALSE.AS.INT )

	lv.output[["complete_dataset_in_dat_and_gen_files"]] = i.complete.data 

	if( i.complete.data == FALSE.AS.INT )
	{
		s.prompt = "How many populations in the result files (less than simulated populations)"
		v.user.values = prompt.for.values.and.return.user.entries( 
			s.prompt, 1, "integer", c( 1, i.number.of.populations-1 ) )
		i.pops.out = v.user.values[1]
		lv.output[["number_populations_in_result_files"]] = i.pops.out

		if( i.two.sexes == TRUE.AS.INT )
		{

			s.prompt = "How many females in each population (less than simulated females)"

			v.user.values = prompt.for.values.and.return.user.entries( 
				s.prompt, 1, "integer", c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )

			lv.output[["number_females_per_population_in_result_files"]] = v.user.values[1]


			s.prompt = "How many males in each population (less than simulated females)"

			v.user.values = prompt.for.values.and.return.user.entries( 
				s.prompt, 1, "integer", c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )

			lv.output[["number_males_per_population_in_result_files"]] = v.user.values[1]

		}
		else
		{
			s.prompt = "How many individuals in each population (less than simulated individuals)"

			v.user.values = prompt.for.values.and.return.user.entries( 
				s.prompt, 1, "integer" , c( 1, MAX_NUMBER_INDIVIDUALS_IN_POP ) )

			lv <- output[["number_individuals_in_each_population_in_result_files"]] = v.user.values[1]	

		}#end if two sexes, else one
	}#end if subset

	s.prompt = paste( "Do you want a file giving the complete pedigree of the simulation ?:y/n",
			"(Please notice that this file can be very huge and will slow down simulations)",
			sep = "\n");

	v.user.values = prompt.for.values.and.return.user.entries( 
		s.prompt, 1, "character", c( "y", "n" ) )

	s.pedigree = v.user.values[1]
	i.pedigree = ifelse( s.pedigree == "y", TRUE.AS.INT, FALSE.AS.INT )

	lv.output[["file_giving_pedigrees"]] = i.pedigree 

	s.prompt = "Name of the file?" 

	v.user.values = prompt.for.values.and.return.user.entries( 
		s.prompt, 1, "character", NULL )

	lv.output[["name_of_file"]] = v.user.values[1]

	s.prompt = "Number of replicates? (between 1 and 999)"

	v.user.values = prompt.for.values.and.return.user.entries( 
		s.prompt, 1, "integer", c( 1, MAX_NUMBER_REPLICATES ) )

	lv.output[["number_of_replicates"]] = v.user.values[1]

	return ( lv.output )

}#end get.output.parameters

get.ep.parameters=function()
 {
	lv.param.vals=list()

	lv.repro=get.reproduction.parameters()
	lv.param.vals=append( lv.param.vals, lv.repro )


	lv.mating=get.mating.parameters( lv.param.vals$ploidy, lv.param.vals$two_sexes )
	lv.param.vals=append( lv.param.vals, lv.mating )


	lv.pop=get.population.parameters( lv.param.vals$ploidy, lv.param.vals$two_sexes )
	lv.param.vals=append( lv.param.vals, lv.pop )

	lv.migr = get.migration.parameters( lv.param.vals$two_sexes, lv.param.vals$number_populations ) 
	lv.param.vals=append( lv.param.vals, lv.migr )


	lv.genetics = get.genetic.parameters( )
	lv.param.vals=append( lv.param.vals, lv.genetics )

	lv.generations = get.generation.parameters( lv.param.vals[["same_migration_scheme_all_simulation"]] )
	lv.param.vals = append( lv.param.vals, lv.generations )


	lv.output = get.output.parameters( lv.param.vals$number_populations, lv.param.vals$two_sexes )
	lv.param.vals = append( lv.param.vals, lv.output )

	return(lv.param.vals );	

}#end get.ep.parameters 

get.parameter.list.for.ep.processing = function( lv.param.vals, lv.ep.list.with.types )
{

	# arg ep.list.with.types is the list of all parameters,
	# with the names (matching those in our para.vals list)
	# as keya, and a list of properties for each, one of which 
	# is "valtype" (c.f. param.list.R )
	#
	# Note that valtypes are:
	# "integer" "numeric" "logical" "array.integer" 
	# "array.numeric" "matrix.numeric" "list.numeric" 
	# "character"   	
	lv.param.list = list()

	#these types should already be formatted as strings,
	#represented so as to match the ep expected config
	#file value type 
	vs.types.already.character = c( "array.integer",
					"array.numeric",
					"matrix.numeric",
					"list.numeric",
      					"character" )

	for( s.name in names( lv.param.vals ) )
	{
		v.val = lv.param.vals[[s.name]]
		s.valtype = lv.ep.list.with.types[[s.name]][["valtype"]]

		v.converted.value = NULL

		if( s.valtype == "logical" )
		{
			if( v.val == TRUE.AS.INT )
			{
				v.converted.value = "y"
			}
			else if ( v.val == FALSE.AS.INT )
			{
				v.converted.value = "n"
			}
			else
			{
				stop( "error:  found value of logical param",
					s.name, ", ", v.val, ", expecting ",
					TRUE.AS.INT, " or ", FALSE.AS.INT, 
					sep = "" )	
			}#end if true, else false, else invalid
		}
		else if( s.valtype == "integer" )
		{
			v.converted.value = as.character( v.val )
		}
		else if( s.valtype == "numeric" )
		{
			v.converted.value = 
				format( v.val, scientific = FALSE )
		}
		else if( s.valtype %in% vs.types.already.character )
		{
			v.converted.value = v.val
		}
		else
		{
			stop( "error:  format type not recognized: ",
				s.valtype )
		}#end if logical, else if integer, ....else...
	

		lv.param.list[[ s.name ]] = v.converted.value 
	}#end for each param name 

	return( lv.param.list )

}#end get.parameter.list.for.ep.processing

check.for.inclusion.in.config.file = function( s.param.name, lv.param.list )
{
	#special cases for params which we entered in our main param list
	#in our R code, as it was needed to set other parmeters, in multiple functions,
	#but which, under certain contexts, should not written to the config file,
        #because it's a value inferred under some contexts. 

	#As of 20231209, we have only found this check needed for the
	#"two_sexes" parameter value, which our R code passes around to more than
	#one fx, so that its universal inclusion in the list is needed as we prompt the user. 
	#For reading in and running the sim, however, it is not needed under non-diploidy.
	#and should not be shown, as it might mislead the user into thinking that changing 
	#the two_sexes value under non-diploidy might affect the sim, which it would not, 
	#as ep automatically sets two_sexes to false under haploidy and true 
	#under haplo-diploidy.	

	b.include=TRUE

	if( s.param.name == "two_sexes" )
	{
		if( lv.param.list[[ "ploidy" ]] != 2 )
		{
			b.include = FALSE
		}#end if ploid not two
	}#end if param name is 	two_sexes

	return( b.include)
	
}#end check.for.inclusion.in.config.file

print.param.list = function( lv.param.list, s.filename )
{
	vs.lines=c()

	for( s.name in names( lv.param.list ) )
	{
		b.include = check.for.inclusion.in.config.file( s.name, lv.param.list )
	
		if( b.include )
		{	
			s.line  = paste( s.name, ":\t", lv.param.list[[s.name]], sep="" )	
			vs.lines = c( vs.lines, s.line )
		}#end if we are to include this parameter in the config file

	}#end for each name

	s.combined.lines = paste( vs.lines, collapse="\n" )	

	o.file.conn = file( s.filename )

	writeLines( s.combined.lines, o.file.conn )

	close(o.file.conn)

}#end print.param.list


#' setup_easypop
#'
#' Prompts the user for parameter values used to run an EASYPOP simulation. and
#' writes the values to a file named by the argument.
#'
#' @param s.file.name string giving the name of a new file to which the program
#'   can write the parameter values entered at the prompts.
#' @param run logical, default FALSE. If TRUE, easypop will automatically be run
#'   on the after parameter file creation is complete.
#' @export
setup_easypop = function( s.file.name, run = FALSE)
{

	#before prompting for params,
	#make sure the file name is not
	#already in use:
	b.exists=file.exists( s.file.name )
	
	if( b.exists )
	{
		stop( "error:  the file", s.file.name,
		     	"already exists.  Please provide ",
			"a file name not already in use." )

	}#end if file exists

	lv.params = get.ep.parameters()

	lv.as.list = get.parameter.list.for.ep.processing(
			lv.params, ALL.EP.PARAMS.WITH.TYPE )

	print.param.list( lv.as.list, s.file.name )

	print( "finished writing parameter values" )
	
	if(run){
	  run_easypop(s.file.name)
	}

}#end setup_easypop

#' run_easypop
#' 
#' Runs an EASYPOP simulation using the parameters given in the file
#' named by the argument.
#'
#' @param s.file.name string giving the name of an existing file holding EASYPOP simulation parameters 
#' @export

run_easypop = function ( s.file.name )
{
  s.file.name <- normalizePath(s.file.name)
	if( file.exists(Sys.getenv("EASYPOP.EXECUTABLE")) )
	{
		system ( paste( Sys.getenv("EASYPOP.EXECUTABLE"), "read", s.file.name ) )
	}
	else
	{
		print( "in fx run_easypop, program does not have an executable name. try manually locating the easypop executable with locate_easypop()" )
	}#end if executable exists else

}#end run_easypop

get.bin.subdir = function ( )
{
    s.this.os = Sys.info()[ 'sysname' ]

    s.bin.subdir = NULL

    if( s.this.os == "Linux" )
    { 
	s.bin.subdir = "linux" 
    }
    else if ( s.this.os ==  "Darwin" )
    {
	s.bin.subdir = "mac"
    }
    else if ( s.this.os == "Windows" )
    {
	    s.bin.subdir = "windows"
    }
    else
    {
	    print( paste( "Easypop package can't locate the correct ", 
				"subdirectory for the executable.",
				"Sys.info sysname returns", s.bin.subdir ) )
    }#end if linux, else darwin

    return( s.bin.subdir )

}#end get.bin.subdir

#' Manually locate an EASYPOP executable
#' 
#' @param path character, a path to the EASYPOP executable.
#' 
#' @export
locate_easypop <- function(path){
  path <- normalizePath(path)
  
  # see if there is anything at this location
  if(!file.exists(path)){
    stop("File: ", path, " does not exist.\n")
  }

  # see if this looks like easypop
  suppressWarnings(test <- try(system(path, intern = TRUE), silent = TRUE))
  if(is("try-error", test[1])){
    stop(test)
  }
  else{
    if(grepl("EASY", test[1])){
      Sys.setenv(EASYPOP.EXECUTABLE = path)
      return(TRUE)
    }
    else{
      stop(paste0("This doesn't look like an EASYPOP executable--here is the result of system(normalizePath(path)):",
                  "\n",
                  paste0(test, collapse = "\n")))
    }
  }
}
