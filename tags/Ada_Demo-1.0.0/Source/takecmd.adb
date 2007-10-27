------------------------------------------------------------- {{{1 ----------
--  Description: Options setable by the Ada plugin
--          $Id$
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author$
--        $Date$
--      Version: 4.5
--    $Revision$
--     $HeadURL:
--  https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ada_opti
--  ons.vim $
--      History: 25.10.2007 MK
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

pragma License (Gpl);

with Win32.Winnls;

package body TakeCmd is

   use type Win32.WCHAR_Array;
   use type Interfaces.C.size_t;

   ------------------
   -- Q_Put_String --
   ------------------

   procedure Q_Put_String (Text_To_Display : in Win32.WCHAR_Array) is
      Win_Text : aliased constant Win32.WCHAR_Array :=
         Text_To_Display & Win32.Wide_Nul;
   begin
      QPuts (Win32.Addr (Win_Text));
      return;
   end Q_Put_String;

   procedure Q_Put_String (Text_To_Display : in Interfaces.C.wchar_array) is
   begin
      Q_Put_String (Win32.WCHAR_Array (Text_To_Display));
      return;
   end Q_Put_String;

   procedure Q_Put_String (Text_To_Display : in Wide_String) is
      C_Text : Interfaces.C.wchar_array (1 .. Text_To_Display'Length + 1);
      Count  : Interfaces.C.size_t;
   begin
      Interfaces.C.To_C
        (Item       => Text_To_Display,
         Target     => C_Text,
         Count      => Count,
         Append_Nul => True);
      Q_Put_String (C_Text);
      return;
   end Q_Put_String;

   procedure Q_Put_String (Text_To_Display : in Win32.CHAR_Array) is
      Wite_Text : Win32.WCHAR_Array (1 .. Text_To_Display'Length + 1);
      Result    : Win32.INT;

      pragma Warnings (Off, Result);
      pragma Warnings (Off, Wite_Text);
   begin
      Result :=
         Win32.Winnls.MultiByteToWideChar
           (CodePage       => Win32.Winnls.CP_ACP,
            dwFlags        => Win32.Winnls.MB_PRECOMPOSED,
            lpMultiByteStr => Win32.Addr (Text_To_Display),
            cchMultiByte   => Text_To_Display'Length,
            lpWideCharStr  => Win32.Addr (Wite_Text),
            cchWideChar    => Wite_Text'Length);
      return;
   end Q_Put_String;

   procedure Q_Put_String (Text_To_Display : in Interfaces.C.char_array) is
   begin
      Q_Put_String (Win32.CHAR_Array (Text_To_Display));
      return;
   end Q_Put_String;

   procedure Q_Put_String (Text_To_Display : in String) is
      C_Text : Interfaces.C.char_array (1 .. Text_To_Display'Length + 1);
      Count  : Interfaces.C.size_t;
   begin
      Interfaces.C.To_C
        (Item       => Text_To_Display,
         Target     => C_Text,
         Count      => Count,
         Append_Nul => True);
      Q_Put_String (C_Text);
      return;
   end Q_Put_String;

end TakeCmd;

------------------------------------------------------------- {{{1 ----------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab     :
--  vim: set textwidth=0 filetype=ada foldmethod=expr nospell          :
