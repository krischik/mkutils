############################################################# {{{1 ##########
#   Description: Makefile for Ada plugin
#           $Id$
#     Copyright: Copyright (C) 2007 Martin Krischik
#       Licence: GNU General Public License
#    Maintainer: Martin Krischik
#       $Author$
#         $Date$
#       Version: 4.5
#     $Revision$
#      $HeadURL$
#       History: 25.10.2007 MK
############################################################################
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
############################################################# }}}1 ##########

.PHONY: pretty
.PHONY: all

Project_File	:= Ada_Demo.gpr
Source_Files	:= $(wildcard Source/*.ad?)
Library_Files	:= SDK/libTakeCmd.a

all:							\
	pentium4-Release\lib\ada_demo.dll		\
	pentium4-Debug\lib\ada_demo.dll

pretty:
	gnat pretty -P Ada_Demo.gpr

SDK/TakeCmd.def: %[JPPATH]/TakeCmd.dll
	dll2def %[JPPATH]/TakeCmd.dll >SDK/TakeCmd.def

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
	gnat make -P Ada_Demo.gpr -XStyle=Debug -XTarget=pentium4

#------------------------------------------------------------ {{{1 ----------
#vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=0 noexpandtab :
#vim: set textwidth=0 filetype=make foldmethod=marker nospell :
