------------------------------------------------------------- {{{1 ----------
--  Description: Options setable by the Ada plugin
--          $Id: ada_options.vim 774 2007-09-17 09:11:59Z krischik $
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author: krischik $
--        $Date: 2007-09-17 11:11:59 +0200 (Mo, 17 Sep 2007) $
--      Version: 4.5
--    $Revision: 774 $
--     $HeadURL: https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ada_options.vim $
--      History: 25.10.2007 MK
----------------------------------------------------------------------------
--  Copyright (C) 2007  Martin Krischik
--
--  This file is part of Ada_Demo.
--
--  Ada_Demo is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Ada_Demo is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Ada_Demo.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------- }}}1 ----------

pragma License (Gpl);

with Interfaces.C;
with Win32;

package TakeCmd.Plugin is

   use type Interfaces.C.int;

   --  If for whatever reason you decide not to handle the function, variable
   --  or command when called from 4NT/TC, then return this special value and
   --  4NT/TC will continue to look for a matching function, variable or
   --  command as it normally would.
   --
   --  Ada strickly checks signed and unsigned integers - hence we have to use
   --  the true (negative) error code.
   ---
   Did_Not_Process : Interfaces.C.int := -16#1234568#;

   --  This Record defines the PluginInfo structure which is returned by the
   --  plugin in response to a GetPluginInfo() call from 4NT/TC.
   type Plugin_Info is record
      DLLName       : Win32.PCWSTR;
      Author        : Win32.PCWSTR;
      AuthorEmail   : Win32.PCWSTR;
      AuthorWebSite : Win32.PCWSTR;
      Description   : Win32.PCWSTR;
      Implements    : Win32.PCWSTR;
      MajorVer      : Interfaces.C.int;
      MinorVer      : Interfaces.C.int;
      BuildNum      : Interfaces.C.int;
      ModuleHandle  : Interfaces.C.long;
      ModuleName    : Win32.PCWSTR;
   end record;

   type LP_Plugin_Info is access Plugin_Info;

   pragma Convention (Convention => C, Entity => Plugin_Info);
   pragma Convention (Convention => C, Entity => LP_Plugin_Info);

   --  This record defines the structure passed to plugin functions to monitor
   --  keystrokes. A keystroke function can be named anything, but must prefix
   --  a * to its name in the function list (Implements, above). (Supported in
   --  version 8 only)
   type Key_Info is record
      KeyName         : Interfaces.C.int; --  key entered }
      Homerow         : Interfaces.C.int; --  start row }
      HomeColumn      : Interfaces.C.int; --  start column }
      Row             : Interfaces.C.int; --  current row in window }
      Column          : Interfaces.C.int; --  current column in window }
      ConmmandLine    : Win32.PCWSTR;     --  Pointer to command line }
      CurrentPosition : Win32.PCWSTR;     --  pointer to position in line }
   end record;

   type LP_Key_Info is access Key_Info;

   pragma Convention (Convention => C, Entity => Key_Info);
   pragma Convention (Convention => C, Entity => LP_Key_Info);

end TakeCmd.Plugin;

------------------------------------------------------------- {{{1 ----------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab     :
--  vim: set textwidth=0 filetype=ada foldmethod=expr nospell          :
