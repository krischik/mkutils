------------------------------------------------------------------------------
--  Description: Options setable by the Ada plugin
--          $Id: mk_utils.ads 58 2007-12-02 15:06:05Z krischik@users.sourceforge.net $
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author: krischik@users.sourceforge.net $
--        $Date: 2007-12-02 16:06:05 +0100 (So, 02 Dez 2007) $
--      Version: 4.5
--    $Revision: 58 $
--     $HeadURL: https://mkutils.googlecode.com/svn/trunk/Source/mk_utils/mk_utils.ads $
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
------------------------------------------------------------------------------

pragma License (Gpl);
pragma Ada_05;

with Win32.Winbase;
with Ada.Unchecked_Conversion;

package body Win32.Set_Privilege is
   use type Win32.DWORD;

   function Fix_The_Silly_Bug is new Ada.Unchecked_Conversion (
      Source => Win32.Winnt.ULARGE_INTEGER,
      Target => Win32.Winnt.LARGE_INTEGER);

   ------------
   -- Create --
   ------------

   function Create (Privilege : in Win32.Winnt.TEXT) return Object is
   begin
      return Retval : Object do
         if Win32.Winbase.OpenProcessToken
               (ProcessHandle => Win32.Winbase.GetCurrentProcess,
                DesiredAccess => Win32.Winnt.TOKEN_ADJUST_PRIVILEGES,
                TokenHandle   => Retval.Token'Unchecked_Access) /=
            Win32.TRUE
         then
            raise Win32_Error with "OpenProcessToken: " &
                                   Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                                   ".";
         end if;

         if Win32.Winbase.LookupPrivilegeValue
               (lpSystemName => null,
                lpName       => Win32.Addr (Privilege),
                lpLuid       => Retval.Luid'Unchecked_Access) /=
            Win32.TRUE
         then
            raise Win32_Error with "LookupPrivilegeValue: " &
                                   Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                                   ".";
         end if;

         Enable_Privilege : declare
            Tp : aliased Win32.Winnt.TOKEN_PRIVILEGES :=
              (PrivilegeCount => 1,
               Privileges     =>
               Win32.Winnt.LUID_AND_ATTRIBUTES_ARRAY'(0 =>
               Win32.Winnt.LUID_AND_ATTRIBUTES'(Luid       => Fix_The_Silly_Bug
                                                                (Retval.Luid),
                                                Attributes =>
               Win32.Winnt.SE_PRIVILEGE_ENABLED)));
         begin
            if Win32.Winbase.AdjustTokenPrivileges
                  (TokenHandle          => Retval.Token,
                   DisableAllPrivileges => FALSE,
                   NewState             => Tp'Unchecked_Access,
                   BufferLength         => Tp'Size / System.Storage_Unit,
                   PreviousState        => null,
                   ReturnLength         => null) /=
               Win32.TRUE
            then
               raise Win32_Error with "AdjustTokenPrivileges: " &
                                      Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                                      ".";
            end if;
         end Enable_Privilege;
      end return;
   end Create;

   procedure Finalize (This : in out Object) is
   begin
      Disable_Privilege : declare
         Tp : aliased Win32.Winnt.TOKEN_PRIVILEGES :=
           (PrivilegeCount => 1,
            Privileges     =>
            Win32.Winnt.LUID_AND_ATTRIBUTES_ARRAY'(0 =>
            Win32.Winnt.LUID_AND_ATTRIBUTES'(Luid       => Fix_The_Silly_Bug (This.Luid),
                                             Attributes => 16#0#)));
      begin
         if Win32.Winbase.AdjustTokenPrivileges
               (TokenHandle          => This.Token,
                DisableAllPrivileges => FALSE,
                NewState             => Tp'Unchecked_Access,
                BufferLength         => Tp'Size / System.Storage_Unit,
                PreviousState        => null,
                ReturnLength         => null) /=
            Win32.TRUE
         then
            raise Win32_Error with "AdjustTokenPrivileges: " &
                                   Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                                   ".";
         end if;
      end Disable_Privilege;

      if Win32.Winbase.CloseHandle (This.Token) /= Win32.TRUE then
         raise Win32_Error with "CloseHandle: " &
                                Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                                ".";
      end if;
   end Finalize;

end Win32.Set_Privilege;

------------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
