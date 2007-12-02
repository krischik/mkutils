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
--     $HeadURL$
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
--  Show Owner of a File
--
separate (MK_Utils)
function C_Show_Owner (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
   Trace : constant TakeCmd.Trace.Object :=
      TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
   pragma Unreferenced (Trace);

   use Ada.Strings.Wide_Fixed;
   use type Win32.Winnt.SECURITY_INFORMATION;

   procedure Process (Directory_Entry : in Win32.WCHAR_Array);

   function Max is new Win32.Windef.Max (T => Integer, ">" => ">");

   Buffer       : aliased constant TakeCmd.Function_Buffer :=
      TakeCmd.Strings.To_Win
        (Arguments   => Arguments,
         To_Upper    => False,
         Trim_Spaces => True);
   Screen_Width : constant Integer                         := Integer (TakeCmd.GetScrCols);

   procedure Process (Directory_Entry : in Win32.WCHAR_Array) is
      Trace : constant TakeCmd.Trace.Object :=
         TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
      pragma Unreferenced (Trace);

      procedure Lookup_Account_Sid
        (Sid           : in Win32.Winnt.PSID;
         Name          : access Win32.Advapi.UNAME;
         Name_Length   : access Win32.DWORD;
         Domain        : access Win32.Advapi.UNAME;
         Domain_Length : access Win32.DWORD;
         Name_Use      : access Win32.Winnt.SID_NAME_USE);

      pragma Warnings (Off, "variable ""Dummy_Handle"" is assigned but never read");

      Result              : Win32.DWORD;
      Owner               : aliased Win32.Winnt.PSID := System.Null_Address;
      Owner_Name          : aliased Win32.Advapi.UNAME;
      Owner_Name_Length   : aliased Win32.DWORD      := Owner_Name'Length;
      Owner_Domain        : aliased Win32.Advapi.UNAME;
      Owner_Domain_Length : aliased Win32.DWORD      := Owner_Domain'Length;
      Owner_Type          : aliased Win32.Winnt.SID_NAME_USE;
      Security_Descriptor : aliased Win32.Winnt.PSECURITY_DESCRIPTOR;
      Dummy_Handle        : Win32.Windef.HLOCAL;

      pragma Warnings (On, "variable ""Dummy_Handle"" is assigned but never read");

      procedure Lookup_Account_Sid
        (Sid           : in Win32.Winnt.PSID;
         Name          : access Win32.Advapi.UNAME;
         Name_Length   : access Win32.DWORD;
         Domain        : access Win32.Advapi.UNAME;
         Domain_Length : access Win32.DWORD;
         Name_Use      : access Win32.Winnt.SID_NAME_USE)
      is
         Trace : constant TakeCmd.Trace.Object :=
            TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
         pragma Unreferenced (Trace);

         Success : Win32.BOOL;
      begin
         Success :=
            Win32.Winbase.LookupAccountSidW
              (lpSystemName           => null,
               Sid                    => Sid,
               Name                   => Win32.Addr (Name.all),
               cbName                 => Name_Length.all'Unchecked_Access,
               ReferencedDomainName   => Win32.Addr (Domain.all),
               cbReferencedDomainName => Domain_Length.all'Unchecked_Access,
               peUse                  => Name_Use.all'Unchecked_Access);
         if Success = Win32.FALSE then
            TakeCmd.Trace.Raise_Exception
              (Raising => TakeCmd.Win32_Error'Identity,
               Message => "LookupAccountSidW: " &
                          Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                          ".",
               Entity  => TakeCmd.Trace.Entity,
               Source  => TakeCmd.Trace.Source);
         end if;

         return;
      end Lookup_Account_Sid;
   begin
      TakeCmd.Trace.Write (Directory_Entry);
      Result :=
         Win32.Advapi.GetNamedSecurityInfoW
           (pObjectName          => Win32.Addr (Directory_Entry),
            ObjectType           => Win32.Advapi.SE_FILE_OBJECT,
            SecurityInfo         => Win32.Winnt.OWNER_SECURITY_INFORMATION,
            ppsidOwner           => Owner'Access,
            ppsidGroup           => null,
            ppDacl               => null,
            ppSacl               => null,
            ppSecurityDescriptor => Security_Descriptor'Access);

      TakeCmd.Trace.Write_Dump (Address => Owner'Address, Size => Owner'Size);
      if Result = Win32.Winerror.NO_ERROR then
         Dummy_Handle := Win32.Winbase.LocalFree (Security_Descriptor);
         Lookup_Account_Sid
           (Sid           => Owner,
            Name          => Owner_Name'Access,
            Name_Length   => Owner_Name_Length'Access,
            Domain        => Owner_Domain'Access,
            Domain_Length => Owner_Domain_Length'Access,
            Name_Use      => Owner_Type'Access);
         declare
            Directory_Entry_Length : constant Natural :=
               Natural (Win32.Winbase.lstrlenW (Win32.Addr (Directory_Entry)));
         begin
            TakeCmd.Q_Put_String (Directory_Entry);
            TakeCmd.Q_Put_String
              (Max ((Screen_Width / 2) - Directory_Entry_Length, 1) * " ");
            TakeCmd.Q_Put_String (Owner_Domain);
            TakeCmd.Q_Put_String (Win32.WCHAR_Array'("\"));
            TakeCmd.Q_Put_String (Owner_Name);
            TakeCmd.Q_Put_String (Win32.WCHAR_Array'(" ("));
            TakeCmd.Q_Put_String (Win32.Winnt.SID_NAME_USE'Image (Owner_Type));
            TakeCmd.Q_Put_String (Win32.WCHAR_Array'(")"));
            TakeCmd.CrLf;
         end;
      else
         TakeCmd.Trace.Raise_Exception
           (Raising => TakeCmd.Win32_Error'Identity,
            Message => "GetNamedSecurityInfoW: " & Win32.DWORD'Image (Result) & ".",
            Entity  => TakeCmd.Trace.Entity,
            Source  => TakeCmd.Trace.Source);
      end if;
   end Process;

begin
   TakeCmd.Trace.Write (Arguments);
   TakeCmd.Trace.Write (Buffer);

   TakeCmd.Wildcard_Search (Directory_Pattern => Buffer, Process => Process'Access);

   return Win32.Winerror.NO_ERROR;
exception
   when An_Exception : others =>
      TakeCmd.Q_Put_String (Ada.Exceptions.Exception_Information (An_Exception));
      TakeCmd.CrLf;
      return -2;
end C_Show_Owner;

------------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=96 filetype=ada foldmethod=expr spell spelllang=en_GB:
