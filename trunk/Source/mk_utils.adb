-------------------------------------------------------------- {{{1 ----------
--  Description: Options setable by the Ada plugin
--          $Id$
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
--  MK_Utils is free software: you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation, either version 3 of the License, or (at your option)
--  any later version.
--
--  MK_Utils is distributed in the hope that it will be useful, but WITHOUT
--  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
--  more details.
--
--  You should have received a copy of the GNU General Public License along
--  with MK_Utils. If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------- }}}1 ----------

pragma License (Gpl);

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with TakeCmd;

with Win32.Winbase;
with Win32.Advapi;
with Win32.Winnt;
with Win32.Windef;

pragma Elaborate_All (TakeCmd);

package body MK_Utils is

   use type Win32.WCHAR_Array;
   use type Win32.BOOL;
   use type Interfaces.C.int;
   use type Interfaces.C.unsigned_long;

   DLL_Name       : aliased constant Win32.WCHAR_Array :=
      "MK_Utils" & Win32.Wide_Nul;
   Author         : aliased constant Win32.WCHAR_Array :=
      "Martin Krischik" & Win32.Wide_Nul;
   Author_Email   : aliased constant Win32.WCHAR_Array :=
      "krischik@users.sourceforge.net" & Win32.Wide_Nul;
   Author_WebSite : aliased constant Win32.WCHAR_Array :=
      "http://mkutils.googlecode.com" & Win32.Wide_Nul;
   Description    : aliased constant Win32.WCHAR_Array :=
      "Utilities for 4NT and Take Command." & Win32.Wide_Nul;
   Implements     : aliased constant Win32.WCHAR_Array :=
      "CHOWN" & Win32.Wide_Nul;

   Plugin_Info : TakeCmd.Plugin.LP_Plugin_Info := null;

   ---------------------------------------------------------------------------
   --  Change Owner of a File
   --
   function C_Change_Owner
     (Arguments : in Win32.PCWSTR)
      return      Interfaces.C.int
   is
      Result : Interfaces.C.int := TakeCmd.Plugin.Did_Not_Process;
   begin

      return Result;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         TakeCmd.CrLf;
         return -2;
   end C_Change_Owner;

   ---------------------------------------------------------------------------
   --  Show Owner of a File
   --
   function C_Show_Owner
     (Arguments : in Win32.PCWSTR)
      return      Interfaces.C.int
   is
      Result              : Win32.DWORD;
      Success             : Win32.BOOL;
      Owner               : aliased Win32.Winnt.PSID;
      Security_Descriptor : aliased Win32.Winnt.PSECURITY_DESCRIPTOR;
      Name                : aliased Win32.Advapi.UNAME;
      Name_Length         : aliased Win32.DWORD := Name'Length;
      Domain              : aliased Win32.Advapi.UNAME;
      Domain_Length       : aliased Win32.DWORD := Domain'Length;
      Name_Use            : aliased Win32.Winnt.SID_NAME_USE;
      Dummy_Handle        : Win32.Windef.HLOCAL;
   begin
      Result :=
         Win32.Advapi.GetNamedSecurityInfoW
           (pObjectName          => Arguments,
            ObjectType           => Win32.Advapi.SE_FILE_OBJECT,
            SecurityInfo         => Win32.Winnt.OWNER_SECURITY_INFORMATION,
            ppsidOwner           => Owner'Access,
            ppsidGroup           => null,
            ppDacl               => null,
            ppSacl               => null,
            ppSecurityDescriptor => Security_Descriptor'Access);
      if Result /= 0 then
         null;
      else
         Dummy_Handle := Win32.Winbase.LocalFree (Security_Descriptor);
         Success      :=
            Win32.Winbase.LookupAccountSidW
              (lpSystemName           => null,
               Sid                    => Owner,
               Name                   => Win32.Addr (Name),
               cbName                 => Name_Length'Unchecked_Access,
               ReferencedDomainName   => Win32.Addr (Domain),
               cbReferencedDomainName => Domain_Length'Unchecked_Access,
               peUse                  => Name_Use'Unchecked_Access);
         if Success /= 0 then
            TakeCmd.Q_Put_String (Name);
            TakeCmd.CrLf;
         end if;
      end if;

      return Interfaces.C.int (Result);
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         TakeCmd.CrLf;
         return -2;
   end C_Show_Owner;

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC (after the call to "InitializePlugin") to get
   --  information from the plugin, primarily for the names of functions,
   --  variables & commands. All that is necessary is to return a pointer to
   --  the PluginInfo structure that was populated when the Plugin loaded.
   --
   function Get_Plugin_Info return  TakeCmd.Plugin.LP_Plugin_Info is
      use type TakeCmd.Plugin.LP_Plugin_Info;
   begin
      if Plugin_Info = null then
         Plugin_Info :=
            new TakeCmd.Plugin.Plugin_Info'
           (pszDll         => Win32.Addr (DLL_Name),
            pszAuthor      => Win32.Addr (Author),
            pszEmail       => Win32.Addr (Author_Email),
            pszWWW         => Win32.Addr (Author_WebSite),
            pszDescription => Win32.Addr (Description),
            pszFunctions   => Win32.Addr (Implements),
            nMajor         => 1,
            nMinor         => 0,
            nBuild         => 0,
            hModule        => 0,
            pszModule      => null);
      end if;
      return Plugin_Info;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         TakeCmd.CrLf;
         return null;
   end Get_Plugin_Info;

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC after loading the plugin. The API requires a return of
   --  0, but as the function is declared as a boolean we must, somewhat
   --  counter-intuitively, return "false".
   --
   function Initialize_Plugin return  Win32.BOOL is
   begin
      return Win32.FALSE;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         TakeCmd.CrLf;
         return Win32.TRUE;
   end Initialize_Plugin;

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC when shutting down, if EndProcess = 0, only the plugin
   --  is being closed; if EndProcess = 1, then 4NT/TC is shutting down. The
   --  API requires a return of 0, but as the function is declared as a
   --  boolean we must, somewhat counter-intuitively, return "false".
   function Shutdown_Plugin
     (End_Process : in Win32.BOOL)
      return        Win32.BOOL
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation (
         Object => TakeCmd.Plugin.Plugin_Info,
         Name => TakeCmd.Plugin.LP_Plugin_Info);
      pragma Unreferenced (End_Process);
      use type TakeCmd.Plugin.LP_Plugin_Info;
   begin
      if Plugin_Info /= null then
         Deallocate (Plugin_Info);
      end if;
      return Win32.FALSE;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         TakeCmd.CrLf;
         return Win32.TRUE;
   end Shutdown_Plugin;

end MK_Utils;

------------------------------------------------------------- {{{1 ----------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
