------------------------------------------------------------------------------
--  Description: Options setable by the Ada plugin
--          $Id: mk_utils-c_show_owner.ada 38 2007-11-13 19:01:29Z
--  krischik@users.sourceforge.net $
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author$
--        $Date$
--      Version: 4.5
--    $Revision$
--     $HeadURL:
--  https://mkutils.googlecode.com/svn/trunk/Source/mk_utils/mk_utils-c_show_owner.ada $
--      History: 30.10.2007 MK Initial Release
------------------------------------------------------------------------------
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
------------------------------------------------------------------------------

pragma License (Gpl);

with System;

with Ada.Strings.Wide_Fixed;

with Win32.Winnt;
with Win32.Advapi;
with Win32.Windef;
with Win32.Winerror;
with Win32.Winbase;

---------------------------------------------------------------------------
--
--  Get Filename for a Temporary File
--
separate (MK_Utils)
function F_Temp_File (Arguments : in TakeCmd.Plugin.Buffer) return Interfaces.C.int is
   Trace : constant TakeCmd.Trace.Object :=
      TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
   pragma Unreferenced (Trace);

   use type Interfaces.C.unsigned;
   use type Win32.DWORD;

   Prefix      : aliased constant Wide_String :=
      TakeCmd.Strings.To_Ada
        (Arguments   => Arguments,
         Keep_Null   => True,
         To_Upper    => False,
         Trim_Spaces => True);
   Max_Path    : constant Natural             := Arguments'Length - Prefix'Length - 14;
   Path        : aliased Win32.WCHAR_Array (1 .. Max_Path);
   Path_Length : constant Win32.DWORD         :=
      Win32.Winbase.GetTempPathW
        (nBufferLength => Interfaces.C.unsigned_long (Max_Path),
         lpBuffer      => Win32.Addr (Path));
   pragma Warnings (Off, Path);
begin
   if Path_Length = 0 or else Path_Length > Win32.DWORD (Max_Path) then
      TakeCmd.Trace.Raise_Exception
        (Raising => TakeCmd.Win32_Error'Identity,
         Message => "GetTempPathW: " &
                    Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                    ". Can't determine temp directory.",
         Entity  => TakeCmd.Trace.Entity,
         Source  => TakeCmd.Trace.Source);
   elsif Win32.Winbase.GetTempFileNameW
            (lpPathName     => Win32.Addr (Path),
             lpPrefixString => Win32.Addr (Prefix),
             uUnique        => 0,
             lpTempFileName => Win32.Addr (Arguments)) =
         0
   then
      TakeCmd.Trace.Raise_Exception
        (Raising => TakeCmd.Win32_Error'Identity,
         Message => "GetTempFileNameW: " &
                    Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                    ". Can't create temp file name.",
         Entity  => TakeCmd.Trace.Entity,
         Source  => TakeCmd.Trace.Source);
   end if;

   return Win32.Winerror.NO_ERROR;
exception
   when An_Exception : others =>
      TakeCmd.Trace.Write_Error (An_Exception);
      return -2;
end F_Temp_File;

------------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
