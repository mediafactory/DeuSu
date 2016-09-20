### DeuSu

This is a web search-engine that can easily hold 2 billion web-pages
in its search-index. If you want to see what it can do, see
https://deusu.org/

The above website runs on an Intel i7-3770K with 32gb RAM and two 500gb SSDs
running Windows Server, and another similar computer with just one 500gb SSD
runnng Ubuntu Linux. These two servers are webserver and query-server.

There are another two similar servers which work as crawler and for
index-generation.

The search-index on https://deusu.org currently holds about 2 billion WWW-pages.
On average a query takes about 250ms. The transfer-speed from SSD into
RAM is the limiting-factor for query-time. Even 600mb/s can be slow sometimes. :)

The software was originally written in Delphi (=Pascal). The latest Delphi
version I have is XE2, and I don't know if it will compile and run in newer
versions of Delphi. Please see the notes below about compiling with FreePascal
for Linux and Windows.

New development will be done for FreePascal only! This is what I use nowadays.

Sorry for the quality of most of the code. Big parts of it were written
15 years ago when I was still young and stupid. :)

[Note 12-Jul-2014: I had to dive deep into the Indy-sourcecode the last few days.
I feel a LOT better about the quality of my own sourcecode now... :) ]

#### Linux

**Note 29-Jan-2016: Due to a bug in the Indy library "searchservernew" won't work correctly on Linux
at the moment! I haven't found a fix or workaround for this yet.**

**Note 31-Jan-2016: I have implemented a (temporary?) fix to Indy that fixes
the above-mentioned bug. We will have to see if Indy applies that bugfix
or if they find a different way to fix it.**

As of 12-Jul-2014 the master branch will compile with FreePascal on Linux.
All development since then has been on done on FreePascal. I use Linux as a
development-machine and compile for Linux and for Windows on that machine.

To compile with FreePascal you may need to change a pathname in build-linux.sh
and build-windows-on-linux.sh so that FPC will find all the necessary files.

#### Compiling with FreePascal for Windows

As of 09-Feb-2015 the master branch will compile with FreePascal for Windows.

On Windows use "build-windows-fpc.bat" to compile.

You can even use FPC to cross-compile on Linux to Windows. In that case use
"build-windows-on-linux.sh". You will have to manually compile FPC to enable
it to cross-compile of course. This is a bit tricky... :)
See http://wiki.freepascal.org/Cross_compiling_for_Win32_under_Linux
for how to do that.

In both cases you will probably have to adjust some compiler-options
in the the .bat/.sh file to let FPC know about the path to its libraries.
Somehow the default FPC install never set the paths correctly for me.
