------------------------------------------------------------- {{{1 ----------
--  Description: Options setable by the Ada plugin
--          $Id: win32-advapi.ads 16 2007-10-31 17:08:47Z
--  krischik@users.sourceforge.net $
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author$
--        $Date$
--      Version: 4.5
--    $Revision$
--     $HeadURL:
--  https://mkutils.googlecode.com/svn/trunk/Source/takecmd/win32-advapi.ads $
--      History: 25.10.2007 MK Initial Release
--               29.10.2007 MK Added Threading, parameter names closer to
--                             C original
----------------------------------------------------------------------------
--  Copyright (C) 2007 Martin Krischik
--
--  This file is part of Ada_Demo.
--
--  Ada_Demo is free software: you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation, either version 3 of the License, or (at your option)
--  any later version.
--
--  Ada_Demo is distributed in the hope that it will be useful, but WITHOUT
--  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
--  more details.
--
--  You should have received a copy of the GNU General Public License along
--  with Ada_Demo. If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------- }}}1 ----------

pragma License (Modified_Gpl);
pragma Ada_05;

with Win32;
with Win32.Winnt;

package Win32.Advapi is
   pragma Linker_Options ("-lADVAPI32");

   UNAMEMAXSIZE : constant := 256;

   subtype UNAME is Win32.WCHAR_Array (1 .. UNAMEMAXSIZE);

   type SE_OBJECT_TYPE is (
      SE_UNKNOWN_OBJECT_TYPE,
      SE_FILE_OBJECT,
      SE_SERVICE,
      SE_PRINTER,
      SE_REGISTRY_KEY,
      SE_LMSHARE,
      SE_KERNEL_OBJECT,
      SE_WINDOW_OBJECT,
      SE_DS_OBJECT,
      SE_DS_OBJECT_ALL,
      SE_PROVIDER_DEFINED_OBJECT,
      SE_WMIGUID_OBJECT,
      SE_REGISTRY_WOW64_32KEY);

   for SE_OBJECT_TYPE use
     (SE_UNKNOWN_OBJECT_TYPE     => 0,
      SE_FILE_OBJECT             => 1,
      SE_SERVICE                 => 2,
      SE_PRINTER                 => 3,
      SE_REGISTRY_KEY            => 4,
      SE_LMSHARE                 => 5,
      SE_KERNEL_OBJECT           => 6,
      SE_WINDOW_OBJECT           => 7,
      SE_DS_OBJECT               => 8,
      SE_DS_OBJECT_ALL           => 9,
      SE_PROVIDER_DEFINED_OBJECT => 10,
      SE_WMIGUID_OBJECT          => 11,
      SE_REGISTRY_WOW64_32KEY    => 12);

   pragma Convention (Convention => C, Entity => SE_OBJECT_TYPE);

   function GetNamedSecurityInfoW
     (pObjectName          : in Win32.PCWSTR;
      ObjectType           : in SE_OBJECT_TYPE;
      SecurityInfo         : in Win32.Winnt.SECURITY_INFORMATION;
      ppsidOwner           : access Win32.Winnt.PSID;
      ppsidGroup           : access Win32.Winnt.PSID;
      ppDacl               : access Win32.Winnt.PACL;
      ppSacl               : access Win32.Winnt.PACL;
      ppSecurityDescriptor : access Win32.Winnt.PSECURITY_DESCRIPTOR)
      return                 Win32.DWORD;

private

   pragma Import
     (Convention => Stdcall,
      Entity => GetNamedSecurityInfoW,
      External_Name => "GetNamedSecurityInfoW");

end Win32.Advapi;

------------------------------------------------------------- {{{1 ----------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
