------------------------------------------------------------------------------
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

---------------------------------------------------------------------------
--
--  Write Help for Commandline Options parsed from Trace
--
separate (MK_Utils)
function C_Help (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
   Trace : constant TakeCmd.Trace.Object :=
      TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
   pragma Unreferenced (Trace);
begin
   TakeCmd.Trace.Write (Arguments);
   TakeCmd.CrLf;
   TakeCmd.CrLf;
   TakeCmd.Q_Put_String (Win32.WCHAR_Array'("MK_Utils internal commands:"));
   TakeCmd.CrLf;
   TakeCmd.CrLf;
   TakeCmd.Q_Put_String ("   " & X_Help & "                                  This help.");
   TakeCmd.CrLf;
   TakeCmd.Q_Put_String
     ("   " & X_Change_Owner & "            (Filename)     change owner of a file");
   TakeCmd.CrLf;
   TakeCmd.Q_Put_String
     ("   " & X_Show_Owner & "              (Filepattern)  display owner files.");
   TakeCmd.CrLf;
   TakeCmd.CrLf;
   TakeCmd.Q_Put_String (Win32.WCHAR_Array'("MK_Utils internal variables:"));
   TakeCmd.CrLf;
   TakeCmd.CrLf;
   TakeCmd.Q_Put_String (Win32.WCHAR_Array'("MK_Utils internal functions:"));
   TakeCmd.CrLf;
   TakeCmd.CrLf;
   TakeCmd.Q_Put_String
     ("  @" & X_Temp_File & "               (Prefix)       generate temporary file name");
   TakeCmd.CrLf;
   TakeCmd.CrLf;
   return 0;
exception
   when An_Exception : others =>
      TakeCmd.Trace.Write_Error (An_Exception);
      return -2;
end C_Help;

------------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
