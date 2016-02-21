#!/bin/bash
echo "Starting Linux build of DeuSu..."
echo "--------------------------------"
mkdir bin
mkdir bin/units
rm bin/units/*.o
rm bin/units/*.ppu
rm bin/units/*.rst
set -e

compiler="fpc -Mdelphi -Tlinux -O3 -vewnh"
compiler=$compiler" -Fusrc/dep/indy10/Lib/*"
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# You will probably need to change the path in the following
# line to match your setup!
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
compiler=$compiler" -Fu/usr/local/lib/fpc/3.0.0/units/x86_64-linux/*"
compiler=$compiler" -FEbin -FUbin/units"


function compile {
    $compiler $1
    echo -e '\n'
}


compile src/RobotNew.dpr
compile src/searchservernew.dpr
compile src/ImportUrls.dpr
compile src/ImportAlexa.dpr
compile src/CleanUrlsTxt.dpr
compile src/PrepareRobot.dpr
compile src/ImportData.dpr
compile src/Sleep.dpr
compile src/Parser.dpr
compile src/GenDb.dpr
compile src/cgi/query.dpr
compile src/compressrwi.dpr

# The following two only compile with Delphi. Needs fixing.
#compile Robot
#compile SearchServer

cd ..

echo "++++++++++++++++++++++++++"
echo "+                        +"
echo "+ Build was successful ! +"
echo "+                        +"
echo "++++++++++++++++++++++++++"
