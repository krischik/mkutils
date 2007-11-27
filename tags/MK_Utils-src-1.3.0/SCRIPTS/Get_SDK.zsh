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

setopt No_X_Trace;
setopt No_Verbose;
setopt SH_Word_Split;
setopt Extended_Glob;
setopt Err_Exit;
setopt CSH_Null_Glob;

autoload -U zfinit
zfinit

zfopen www.jpsoft.com anonymous krischik@users.sourceforge.net
zftype binary

pushd /tmp
	zfget sdk/sdk.zip
popd

zfclose

pushd ../SDK
   7z x -y /tmp/sdk.zip
popd


############################################################# {{{1 ##########
# vim: textwidth=0 nowrap tabstop=4 shiftwidth=4 softtabstop=4 noexpandtab
# vim: filetype=zsh encoding=utf-8 fileformat=unix
