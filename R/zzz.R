.onLoad = function (libname, pkgname) 
{
  #Note that this function assumes that the package "bin" dir
  #has per-os subdirectories, each of which contains exactly one
  #file, the easypop executable.
  
  #if the call to list files to get exec
  #name fails we'll get a character(0)
  #return, so we can check for it with:
  MIN.EXEC.NAME.LEN=1
  
  s.pack.loc=find.package( "easypopr" )
  s.bin.subdir = get.bin.subdir()	
  s.path.to.exec = paste(  s.pack.loc, "bin", s.bin.subdir, sep="/" )
  s.exec.name = list.files(  s.path.to.exec  )
  s.full.path.with.exec = paste( s.path.to.exec, s.exec.name, sep= "/" )
  
  if( length( s.exec.name ) < MIN.EXEC.NAME.LEN )
  {
    print( "package easypopr can't find the easypop executable; path can be manually indicated with locate_easypop()" );
  }
  else
  {
    Sys.setenv(EASYPOP.EXECUTABLE = s.full.path.with.exec)
    print( paste( "setting the package easypop executable to",
                  basename( s.exec.name ) ) )
  }#end if no exec found, else notify of name
  
}#end .onLoad
