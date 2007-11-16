----------------------------------------------------------------------------
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
--      History: 25.10.2007 MK Initial Release
--               29.10.2007 MK Added Threading, parameter names closer to
--                             C original
--         Help: http://www.jpsoftwiki.com/wiki/index.php?title=Plugin/Ada
----------------------------------------------------------------------------
--  Copyright (C) 2007 Martin Krischik
--
--  This file is part of Ada_Demo.
--
--  Ada_Demo is free software: you can redistribute it and/or modify it under the terms of the
--  GNU General Public License as published by the Free Software Foundation, either version 3
--  of the License, or (at your option) any later version.
--
--  Ada_Demo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
--  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along with Ada_Demo. If
--  not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------

pragma License (Gpl);
pragma Ada_05;

with Win32.Winnls;
with Win32.Winbase;
with Win32.Winnt;

with TakeCmd.Trace;

package body TakeCmd is

   use type Win32.WCHAR_Array;
   use type Interfaces.C.size_t;
   use type Interfaces.C.int;
   use type Interfaces.C.unsigned_long;

   ------------------
   -- Q_Put_String --
   ------------------

   procedure Q_Put_String (Text_To_Display : in Win32.WCHAR_Array) is
      Win_Text : aliased constant Win32.WCHAR_Array := Text_To_Display & Win32.Wide_Nul;
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
      C_Text : Interfaces.C.wchar_array (1 .. Text_To_Display'Length + 2);
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
      Wide_Text : Win32.WCHAR_Array (1 .. Text_To_Display'Length + 1);
      Result    : Win32.INT;

      pragma Warnings (Off, Wide_Text);
   begin
      Result :=
         Win32.Winnls.MultiByteToWideChar
           (CodePage       => Win32.Winnls.CP_ACP,
            dwFlags        => Win32.Winnls.MB_PRECOMPOSED,
            lpMultiByteStr => Win32.Addr (Text_To_Display),
            cchMultiByte   => Text_To_Display'Length,
            lpWideCharStr  => Win32.Addr (Wide_Text),
            cchWideChar    => Wide_Text'Length);
      if Result /= 0 then
         Q_Put_String (Wide_Text);
      end if;
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

   procedure Wildcard_Search
     (Directory_Pattern : in Win32.WCHAR_Array;
      Process           : not null access procedure
        (Directory_Entry : in Win32.WCHAR_Array))
   is
      --  Trace : constant TakeCmd.Trace.Object := TakeCmd.Trace.Function_Trace
      --  (TakeCmd.Trace.Entity); pragma Unreferenced (Trace);

      Directory_Length : constant Interfaces.C.int := PathLength (Directory_Pattern);
      Directory        : aliased Win32.WCHAR_Array (1 .. Integer (Directory_Length) + 1);
      Pattern          : aliased File_Name;
      Dummy            : Win32.PWSTR;
      Files_Found      : Natural                   := 0;

      pragma Warnings (Off, Dummy);
      pragma Warnings (Off, Directory);
      pragma Warnings (Off, Pattern);
   begin
      Dummy := PathPart (pszName => Directory_Pattern, pszPath => Win32.Addr (Directory));
      Dummy :=
         FilenamePart
           (pszName         => Directory_Pattern,
            pszFilenamePart => Win32.Addr (Pattern));

      Fix_Pattern : declare
         Pattern_Length : constant Interfaces.C.int :=
            Win32.Winbase.lstrlenW (Win32.Addr (Pattern));
      begin
         if Pattern_Length = 0 then
            Pattern := (1 => '*', others => Win32.Wide_Nul);
         end if;
      end Fix_Pattern;

      Find_Files : declare
         use type Win32.Winnt.HANDLE;
         use type Win32.BOOL;

         Data       : aliased Win32.Winbase.WIN32_FIND_DATAW;
         Search_All : aliased constant Win32.WCHAR_Array :=
            Directory (1 .. Directory'Last - 1) & "*" & Win32.Wide_Nul;
         Handle     : constant Win32.Winnt.HANDLE        :=
            Win32.Winbase.FindFirstFileW
              (lpFileName     => Win32.Addr (Search_All),
               lpFindFileData => Data'Unchecked_Access);
      begin
         if Handle = Win32.Winbase.INVALID_HANDLE_VALUE then
            TakeCmd.Trace.Raise_Exception
              (Raising => Win32_Error'Identity,
               Message => ": FindFirstFileW: " &
                          Win32.DWORD'Image (Win32.Winbase.GetLastError) &
                          ". ",
               Entity  => TakeCmd.Trace.Entity,
               Source  => TakeCmd.Trace.Source);
         else
            Process_Files : loop
               if WildcardComparison
                     (pszWildName => Win32.Addr (Pattern),
                      pszFileName => Win32.Addr (Data.cFileName),
                      fExtension  => 0,
                      fBrackets   => 1) =
                  0
               then
                  Process_File : declare
                     File : aliased constant Win32.WCHAR_Array :=
                        Directory (1 .. Directory'Last - 1) & Data.cFileName;
                  begin
                     Process (File);
                     Files_Found := Files_Found + 1;
                  end Process_File;
               end if;
               exit Process_Files when Win32.Winbase.FindNextFileW
                                          (hFindFile      => Handle,
                                           lpFindFileData => Data'Unchecked_Access) /=
                                       Win32.TRUE;
            end loop Process_Files;
         end if;
      end Find_Files;

      if Files_Found = 0 then
         TakeCmd.Trace.Raise_Exception
           (Raising => Name_Error'Identity,
            Message => "No Files found.",
            Entity  => TakeCmd.Trace.Entity,
            Source  => TakeCmd.Trace.Source);
      end if;
      return;
   end Wildcard_Search;

end TakeCmd;

----------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=96 filetype=ada foldmethod=expr spell spelllang=en_GB:
