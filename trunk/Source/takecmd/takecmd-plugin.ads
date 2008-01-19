----------------------------------------------------------------------------
--  Description: Options setable by the Ada plugin
--          $Id$
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author$
--        $Date$
--      Version: 4.5
--    $Revision$
--     $HeadURL:
--  https://mkutils.googlecode.com/svn/trunk/Source/takecmd/takecmd-plugin.ads $
--      History: 25.10.2007 MK Initial Release
--               29.10.2007 MK Added Threading, parameter names closer to
--                             C original
--         Help: http://www.jpsoftwiki.com/wiki/index.php?title=Plugin/Ada
-----------------------------------------------------------------------------
--  Copyright (C) 2007 Martin Krischik
--
--  This file is part of Ada_Demo.
--
--  Ada_Demo is free software: you can redistribute it and/or modify it under the terms of the
--  GNU General Public License as published by the Free Software Foundation, either version 3
--  of the License, or (at your option) any later version.
--
--  Ada_Demo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
--  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along with Ada_Demo. If
--  not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------

pragma License (Gpl);
pragma Ada_05;

with Interfaces.C;
with Win32;

package TakeCmd.Plugin is

   use type Interfaces.C.int;

   ---------------------------------------------------------------------------
   --  PluginInfo structure - returned by plugin in response to GetPluginInfo() call from
   --  command processor Note that the strings should all be Unicode; if your PlugIn is
   --  compiled for ASCII you'll need to use the MultiByteToWideChar API to convert the
   --  strings before passing them back to 4NT / TC (Not the case in Ada)
   --
   type Plugin_Info is record
      pszDll         : Win32.PCWSTR;         --  name of the DLL
      pszAuthor      : Win32.PCWSTR;         --  author's name
      pszEmail       : Win32.PCWSTR;         --  author's email
      pszWWW         : Win32.PCWSTR;         --  author's web page
      pszDescription : Win32.PCWSTR;         --  (brief) description of plugin
      pszFunctions   : Win32.PCWSTR;         --  comma-delimited list of
                                             --  functions in the plugin
                                             --  r internal vars, @ for var
                                             --  s, * for keystroke function, otherwise
                                             --  it's a command
      nMajor         : Interfaces.C.int;     --  plugin's major version #
      nMinor         : Interfaces.C.int;     --  plugin's minor version #
      nBuild         : Interfaces.C.int;     --  plugin's build #
      hModule        : Interfaces.C.long;    --  module handle
      pszModule      : Win32.PCWSTR;         --  module name
   end record;

   type LP_Plugin_Info is access Plugin_Info;

   pragma Convention (Convention => C, Entity => Plugin_Info);
   pragma Convention (Convention => C, Entity => LP_Plugin_Info);

   ---------------------------------------------------------------------------
   --  structure passed to plugin functions to monitor keystrokes. A keystroke function can be
   --  named anything, but must prefix a * to its name in the function list (pszFunctions,
   --  above). (Supported in version 8 only)
   --
   --  If the keystroke plugin handled the keystroke and doesn't want pass it back to 4NT /
   --  TC, it should set nKey = 0 The command processor will call the keystroke function with
   --  all parameters set to 0 just before accepting input for each new command line.
   --
   --  The string pointers are Unicode
   --
   type Key_Info is record
      nKey        : Interfaces.C.int; --  key entered
      nHomerow    : Interfaces.C.int; --  start row
      nHomeColumn : Interfaces.C.int; --  start column
      nRow        : Interfaces.C.int; --  current row in window
      nColumn     : Interfaces.C.int; --  current column in window
      pszLine     : Win32.PCWSTR;     --  command line
      pszCurrent  : Win32.PCWSTR;     --  pointer to position in line
      fRedraw     : Interfaces.C.int; --  if != 0, redraw the lin
   end record;

   pragma Convention (Convention => C, Entity => Key_Info);

   ---------------------------------------------------------------------------
   --  Returning from the PlugIn:
   --
   --  For internal variables and variable functions, copy the result string over
   --  pszArguments. The maximum string length for internal variables and variable functions
   --  is 2K (2047 characters + null byte).
   --
   subtype Buffer is Win32.WCHAR_Array (1 .. 2 ** 11);

   ---------------------------------------------------------------------------
   --
   --  There is a special return value (0xFEDCBA98) that tells the parser to assume that the
   --  plugin decided not to handle the variable/function/ command. The parser then continues
   --  looking for a matching internal, then external. Note that you can use this return value
   --  to have your plugin modify the command line and then pass it on to an existing internal
   --  variable/function/command!
   --
   --  Ada strickly checks signed and unsigned integers - hence we have to use the true
   --  (negative) error code.
   --
   Did_Not_Process : Interfaces.C.int := -16#1234568#;

end TakeCmd.Plugin;

----------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
