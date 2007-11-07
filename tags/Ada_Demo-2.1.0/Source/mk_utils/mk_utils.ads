------------------------------------------------------------- {{{1 ----------
--  Description: Options setable by the Ada plugin
--          $Id: mk_utils.ads 16 2007-10-31 17:08:47Z
--  krischik@users.sourceforge.net $
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author$
--        $Date$
--      Version: 4.5
--    $Revision$
--     $HeadURL:
--  https://mkutils.googlecode.com/svn/trunk/Source/mk_utils/mk_utils.ads $
--      History: 25.10.2007 MK Initial Release
--               29.10.2007 MK Added Threading, parameter names closer to
--                             C original
--         Help:
--  http://www.jpsoftwiki.com/wiki/index.php?title=MK_Utils_%28plugin%29
----------------------------------------------------------------------------
--  Copyright (C) 2007 Martin Krischik
--
--  This file is part of MK_Utils.
--
--  MK_Utils is free software: you can redistribute it and/or modify it under the terms of the
--  GNU General Public License as published by the Free Software Foundation, either version 3
--  of the License, or (at your option) any later version.
--
--  MK_Utils is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
--  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along with MK_Utils. If
--  not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------- }}}1 ----------

pragma License (Gpl);

with Interfaces.C;
with Win32;
with TakeCmd.Plugin;

package MK_Utils is

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC after loading the plugin. The API requires a return of 0, but as the
   --  function is declared as a boolean we must, somewhat counter-intuitively, return
   --  "false".
   --
   function Initialize_Plugin return  Win32.BOOL;

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC when shutting down, if EndProcess = 0, only the plugin is being
   --  closed; if EndProcess = 1, then 4NT/TC is shutting down. The API requires a return of
   --  0, but as the function is declared as a boolean we must, somewhat counter-intuitively,
   --  return "false".
   --
   function Shutdown_Plugin (End_Process : in Win32.BOOL) return Win32.BOOL;

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC (after the call to "InitializePlugin") to get information from the
   --  plugin, primarily for the names of functions, variables & commands. All that is
   --  necessary is to return a pointer to the PluginInfo structure that was populated when
   --  the Plugin loaded.
   --
   function Get_Plugin_Info return  TakeCmd.Plugin.LP_Plugin_Info;

   ---------------------------------------------------------------------------
   --  Change Owner of a File
   --
   function C_Change_Owner (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   function C_Show_Owner (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

private

   pragma Export (Convention => Stdcall, Entity => C_Change_Owner, External_Name => "CHOWN");

   pragma Export
     (Convention => Stdcall,
      Entity => C_Show_Owner,
      External_Name => "SHOWOWNER");

end MK_Utils;

------------------------------------------------------------- {{{1 ----------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
