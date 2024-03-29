------------------------------------------------------------------------------
--  Description: Options setable by the Ada plugin
--          $Id: mk_utils-c_change_owner.ada 38 2007-11-13 19:01:29Z
--  krischik@users.sourceforge.net $
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author$
--        $Date$
--      Version: 4.5
--    $Revision$
--     $HeadURL:
--  https://mkutils.googlecode.com/svn/trunk/Source/mk_utils/mk_utils-c_change_owner.ada $
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

with Extensible;
with System;
with Win32.Advapi;
with Win32.Winerror;
with Win32.Winnt;
with Win32.Winbase;
with Win32.Set_Privilege;
with Ada.Characters.Wide_Latin_1;

--------------------------------------------------------------------------
--
--  Change Owner of a File
--
separate (MK_Utils)
function C_Change_Owner (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
   Trace : constant TakeCmd.Trace.Object :=
      TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
   pragma Unreferenced (Trace);

   use type Interfaces.C.unsigned_short;

   Sid : aliased Win32.Winnt.SID;

   package Extensible_SID is new Extensible (
      Fixed_Rec => Win32.Winnt.SID,
      Extensible_Elem => Win32.ULONG,
      Position_Of_Extensible_Array => Sid.SubAuthority'Position / System.Storage_Unit);

   Parameter   : constant TakeCmd.Strings.String_Vectors.Vector :=
      TakeCmd.Strings.To_Parameter (Arguments);
   Name        : aliased constant Wide_String                   :=
      Parameter.Element (1) & Ada.Characters.Wide_Latin_1.NUL;
   File        : aliased constant Wide_String                   :=
      Parameter.Element (2) & Ada.Characters.Wide_Latin_1.NUL;
   Sid_Size    : aliased Win32.ULONG                            := 0;
   Name_Use    : aliased Win32.Winnt.SID_NAME_USE;
   Domain_Size : aliased Win32.ULONG                            := 0;
   Success     : Win32.BOOL;
   Result      : Win32.DWORD;
   Error       : Win32.DWORD;
   Token       : aliased Win32.Winnt.HANDLE;
   Privilege   : Win32.Set_Privilege.Object                     :=
      Win32.Set_Privilege.Create (Win32.Winnt.SE_TAKE_OWNERSHIP_NAME);
   pragma Unreferenced (Privilege);
begin
   TakeCmd.Trace.Write (Arguments);
   TakeCmd.Trace.Write (Name);
   TakeCmd.Trace.Write (File);

   Success :=
      Win32.Winbase.LookupAccountNameW
        (lpSystemName           => null,
         lpAccountName          => Win32.Addr (Name),
         Sid                    => System.Null_Address,
         cbSid                  => Sid_Size'Unchecked_Access,
         ReferencedDomainName   => null,
         cbReferencedDomainName => Domain_Size'Unchecked_Access,
         peUse                  => Name_Use'Unchecked_Access);
   Error   := Win32.Winbase.GetLastError;
   if Error = Win32.Winerror.ERROR_INSUFFICIENT_BUFFER then
      With_Real_Size : declare
         Extended_Sid : aliased Extensible_SID.Extended_Ptr :=
            Extensible_SID.Allocate (Extensible_SID.Big_Range (Sid_Size));
         Domain_Name  : aliased Win32.WCHAR_Array (1 .. Integer (Domain_Size));
      begin
         if Win32.Winbase.LookupAccountNameW
               (lpSystemName           => null,
                lpAccountName          => Win32.Addr (Name),
                Sid                    =>
                   Extensible_SID.Fixed_Part (Extended_Sid).all'Address,
                cbSid                  => Sid_Size'Unchecked_Access,
                ReferencedDomainName   => Win32.Addr (Domain_Name),
                cbReferencedDomainName => Domain_Size'Unchecked_Access,
                peUse                  => Name_Use'Unchecked_Access) =
            Win32.TRUE
         then
            TakeCmd.Trace.Write (Wide_String'("Success"));
            Result :=
               Win32.Advapi.SetNamedSecurityInfoW
                 (pObjectName  => Win32.Addr (File),
                  ObjectType   => Win32.Advapi.SE_FILE_OBJECT,
                  SecurityInfo => Win32.Winnt.OWNER_SECURITY_INFORMATION,
                  psidOwner    => Extensible_SID.Fixed_Part (Extended_Sid).all'Address,
                  psidGroup    => System.Null_Address,
                  pDacl        => null,
                  pSacl        => null);
            if Result /= Win32.Winerror.NO_ERROR then
               TakeCmd.Trace.Raise_Exception
                 (Raising => TakeCmd.Win32_Error'Identity,
                  Message => "GetNamedSecurityInfoW: " & Win32.DWORD'Image (Result) & ".",
                  Entity  => TakeCmd.Trace.Entity,
                  Source  => TakeCmd.Trace.Source);
            end if;

         else
            TakeCmd.Trace.Raise_Exception
              (Raising => TakeCmd.Win32_Error'Identity,
               Message => "LookupAccountNameW: " &
                          Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                          ".",
               Entity  => TakeCmd.Trace.Entity,
               Source  => TakeCmd.Trace.Source);
         end if;
      end With_Real_Size;
   else
      TakeCmd.Trace.Raise_Exception
        (Raising => TakeCmd.Win32_Error'Identity,
         Message => "LookupAccountNameW: " & Win32.DWORD'Image (Error) & ".",
         Entity  => TakeCmd.Trace.Entity,
         Source  => TakeCmd.Trace.Source);
   end if;

   return Win32.Winerror.NO_ERROR;
exception
   when An_Exception : others =>
      TakeCmd.Q_Put_String (Ada.Exceptions.Exception_Information (An_Exception));
      TakeCmd.CrLf;
      return -2;
end C_Change_Owner;

------------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
