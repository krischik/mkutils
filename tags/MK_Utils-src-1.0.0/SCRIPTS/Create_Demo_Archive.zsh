#!/bin/zsh
############################################################# {{{1 ##########
#   Description: Options setable by the Ada plugin
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

setopt X_Trace;
setopt No_Verbose;
setopt SH_Word_Split;
setopt Err_Exit;
setopt CSH_Null_Glob;

declare SVN_Server="https://mkutils.googlecode.com/svn";
declare Module="Ada_Demo";
declare Version="2.2.0";

svn copy											\
	"${SVN_Server}/trunk"							\
	"${SVN_Server}/tags/${Module}-${Version}"		\
	-m"tag release ${Module} ${Version}"			;

pushd /tmp
	svn export										\
		"${SVN_Server}/tags/${Module}-${Version}"	\
		"${Module}-${Version}"						;
	tar --create --bzip2							\
		--file="${Module}-${Version}.tar.bz2"		\
		"${Module}-${Version}"						;
	7z a -t7z										\
		"${Module}-${Version}.7z"					\
		"${Module}-${Version}"						;
	7z a -tzip										\
		"${Module}-${Version}.zip"					\
		"${Module}-${Version}"						;
	rm --recursive --force "${Module}-${Version}"	;
popd

############################################################# {{{1 ##########
# vim: textwidth=0 nowrap tabstop=4 shiftwidth=4 softtabstop=4 noexpandtab
# vim: filetype=zsh encoding=utf-8 fileformat=unix
