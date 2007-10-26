#------------------------------------------------------------ {{{1 ----------
#   Description: Options setable by the Ada plugin
#           $Id: ada_options.vim 774 2007-09-17 09:11:59Z krischik $
#     Copyright: Copyright (C) 2007 Martin Krischik
#       Licence: GNU General Public License
#    Maintainer: Martin Krischik
#       $Author: krischik $
#         $Date: 2007-09-17 11:11:59 +0200 (Mo, 17 Sep 2007) $
#       Version: 4.5
#     $Revision: 774 $
#      $HeadURL: https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ada_options.vim $
#       History: 25.10.2007 MK 
#---------------------------------------------------------------------------
#   Copyright (C) 2007  Martin Krischik
# 
#   This file is part of Ada_Demo.
# 
#   Ada_Demo is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
# 
#   Ada_Demo is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
# 
#   You should have received a copy of the GNU General Public License
#   along with Ada_Demo.  If not, see <http://www.gnu.org/licenses/>.
#------------------------------------------------------------ }}}1 ----------

Project_File 	:= Ada_Demo.gpr
Source_Files 	:= $(wildcard Source/*.ad?)
Library_Files 	:= SDK/libTakeCmd.a

SDK/libTakeCmd.a: SDK/TakeCmd.def
	cd SDK && gnatdll -k -e TakeCmd.def -d TakeCmd.dll

pentium4-Release\lib\ada_demo.dll:		\
    	${Project_File}				\
	${Source_Files}				\
	${Library_Files}
	gnat make -P Ada_Demo.gpr -XStyle=Release -XTarget=pentium4

pentium4-Debug\lib\ada_demo.dll:		\
    	${Project_File}				\
	${Source_Files}				\
	${Library_Files}
	echo ${Source_Files}
	gnat make -P Ada_Demo.gpr -XStyle=Debug -XTarget=pentium4

#------------------------------------------------------------ {{{1 ----------
#vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=0 noexpandtab :
#vim: set textwidth=0 filetype=make foldmethod=marker nospell :
