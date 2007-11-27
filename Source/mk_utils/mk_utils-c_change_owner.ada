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

with Win32.Winerror;

--------------------------------------------------------------------------
--
--  Change Owner of a File
--
separate (MK_Utils)
function C_Change_Owner (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
   Trace : constant TakeCmd.Trace.Object :=
      TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
   pragma Unreferenced (Trace);

   Buffer : aliased constant TakeCmd.Plugin.Buffer :=
      TakeCmd.Strings.To_Win
        (Arguments   => Arguments,
         To_Upper    => False,
         Trim_Spaces => True);
begin
   TakeCmd.Trace.Write (Arguments);
   TakeCmd.Trace.Write (Buffer);
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
