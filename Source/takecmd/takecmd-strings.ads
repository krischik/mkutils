--------------------------------------------------------------------------
--  Description: Trace facility for 4NT / Take Command Plugins
--          $Id$
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author$
--        $Date$
--      Version: 4.5
--    $Revision$
--     $HeadURL$
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
--  GNU General Public License as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--
--  Ada_Demo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
--  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along with Ada_Demo. If
--  not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------- }}}1 ----------

pragma License (Gpl);
pragma Ada_05;

with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Maps;

with TakeCmd.Plugin;

package TakeCmd.Strings is
   ---------------------------------------------------------------------------
   --
   --  Searches for all occurences of text "Search" and Inserts text "Insert" after the found
   --  text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Count    : Count of replaces done
   --
   procedure Append_All
     (Source   : in out Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      Search   : in Wide_String;
      New_Item : in Wide_String;
      Mapping  : in Ada.Strings.Wide_Maps.Wide_Character_Mapping :=
      Ada.Strings.Wide_Maps.Identity;
      Count    : out Natural);

   ---------------------------------------------------------------------------
   --
   --  Convert the string to Ada and if desired remove spaces from the begin and end. Also the
   --  string can be converted to upper case if casing is not important
   --
   --  Arguments   : String to be converted
   --  Keep_Null   : Keep the null character at the end of the string
   --  To_Upper    : Convert to upper case
   --  Trim_Spaces : remove unneded spaces.
   --
   function To_Ada
     (Arguments   : in Win32.PCWSTR;
      Keep_Null   : in Boolean := False;
      To_Upper    : in Boolean := False;
      Trim_Spaces : in Boolean := False)
      return        Wide_String;

   pragma Pure_Function (To_Ada);

   function To_Ada
     (Arguments   : in Win32.WCHAR_Array;
      Keep_Null   : in Boolean := False;
      To_Upper    : in Boolean := False;
      Trim_Spaces : in Boolean := False)
      return        Wide_String;

   pragma Pure_Function (To_Ada);

   procedure To_Ada
     (Arguments   : in Win32.PCWSTR;
      Buffer      : out Wide_String;
      Keep_Null   : in Boolean := False;
      To_Upper    : in Boolean := False;
      Trim_Spaces : in Boolean := False);

   ---------------------------------------------------------------------------
   --
   --  Convert the string to Win 32.
   --
   --  Arguments   : String to be converted.
   --  To_Upper    : Convert to upper case
   --  Trim_Spaces : remove unneded spaces.
   --
   function To_Win (Arguments : in Wide_String) return TakeCmd.Plugin.Buffer;

   pragma Pure_Function (To_Win);

   function To_Win (Arguments : in String) return TakeCmd.Plugin.Buffer;

   function To_Win
     (Arguments   : in Win32.PCWSTR;
      To_Upper    : in Boolean := False;
      Trim_Spaces : in Boolean := False)
      return        TakeCmd.Plugin.Buffer;

private

end TakeCmd.Strings;

----------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
