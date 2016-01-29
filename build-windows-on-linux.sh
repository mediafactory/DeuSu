#!/bin/bash
echo "Starting Cross-Compile for Windows on Linux of DeuSu..."
echo "-------------------------------------------------------"
mkdir bin
mkdir bin/units
rm bin/units/*.o
rm bin/units/*.ppu
rm bin/units/*.rst
set -e

compiler="fpc -Mdelphi -Twin64 -O3 -Xs -XX -vewnh"
compiler=$compiler" -Fusrc/dep/indy10/Lib/*"
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# You will probably need to change the path in the following
# line to match your setup!
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
compiler=$compiler" -Fu/usr/local/lib/fpc/3.0.0/units/x86_64-win64/*"
compiler=$compiler" -FEbin -FUbin/units"


function compile {
    $compiler $1.dpr
    echo -e '\n'
}


compile src/RobotNew
compile src/searchservernew
compile src/ImportUrls
compile src/ImportAlexa
compile src/CleanUrlsTxt
compile src/PrepareRobot
compile src/ImportData
compile src/Sleep
compile src/Parser
compile src/GenDb
compile src/cgi/query

# The following two only compile with Delphi. Needs fixing.
#compile Robot
#compile SearchServer

cd ..

echo "++++++++++++++++++++++++++"
echo "+                        +"
echo "+ Build was successful ! +"
echo "+                        +"
echo "++++++++++++++++++++++++++"
