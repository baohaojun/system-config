all:
	if ls *.csproj; then vc8make vcbuild /u *.csproj debug; vc8make vcbuild /u *.csproj release; fi
	if ls *.vcproj; then vc8make vcbuild /u *.vcproj debug; vc8make vcbuild /u *.vcproj release; fi
