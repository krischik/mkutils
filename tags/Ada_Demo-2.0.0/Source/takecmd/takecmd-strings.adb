--------------------------------------------------------------------------
----
---
--  Description: Trace facility for 4NT / Take Command Plugins
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
-----------------------------------------------------------------------------
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

with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps.Wide_Constants;
with Win32.Winbase;
with Interfaces.C;

package body TakeCmd.Strings is
   use type Interfaces.C.int;

   ---------------------------------------------------------------------------
   --
   --  Searches for all occurences of text "Search" and Inserts text "Insert" after the found
   --  text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Count    : Count of replaces done
   --
   procedure Append_All
     (Source   : in out Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      Search   : in Wide_String;
      New_Item : in Wide_String;
      Mapping  : in Ada.Strings.Wide_Maps.Wide_Character_Mapping :=
         Ada.Strings.Wide_Maps.Identity;
      Count    : out Natural)
   is
      --
      --  Offset from which we start. 0 means search from the first character
      --
      Offset : Natural := Natural'First;
      --
      --  Lenght of the full string
      --
      Len : constant Natural := Ada.Strings.Wide_Unbounded.Length (Source);
   begin
      --
      --  nothing found yet
      --
      Count := Natural'First;

      Search_Next : loop
         --
         --  Last Found Item was at the end of the String
         --
         exit Search_Next when Len = 0 or else Offset >= Len;

         Next_Sub_String : declare
            --
            --  We slice from the Offset on to the end. One might be suprised to learn that
            --  Sub_String'First might not be 1
            --
            Sub_String : constant Wide_String :=
               Ada.Strings.Wide_Unbounded.Slice
                 (Source => Source,
                  Low    => Offset + 1,
                  High   => Len);
            --
            --  We search for Pattern
            --
            Low  : constant Natural :=
               Ada.Strings.Wide_Fixed.Index
                 (Source  => Sub_String,
                  Pattern => Search,
                  Going   => Ada.Strings.Forward,
                  Mapping => Mapping);
            High : constant Natural := Low + Search'Length;
         begin
            --
            --  Exit Loop when we havn't found anything
            --
            exit Search_Next when Low = 0;

            if New_Item'Length > Sub_String'Last - High + 1
              or else New_Item /= Sub_String (High .. High + New_Item'Length - 1)
            then
               --
               --  We insert one character after the end of the found string when the new text
               --  does not allready follow. This can of course only happen when there are
               --  enouch characters behind the found string to contain the new text.
               --
               Ada.Strings.Wide_Unbounded.Insert
                 (Source   => Source,
                  Before   => High + Offset + 1 - Sub_String'First,
                  New_Item => New_Item);
               --
               --  Found one.
               --
               Count := Natural'Succ (Count);
            end if;
            --
            --  We set the offset to the end of the found string.
            --
            Offset := Offset + High;
         end Next_Sub_String;
      end loop Search_Next;
   end Append_All;

   ---------------------------------------------------------------------------
   --
   --  Convert the string to Ada and if desired remove spaces from the begin and end. Also the
   --  string can be converted to upper case if casing is not important
   --
   --  Arguments   : String to be converted
   --  To_Upper    : Convert to upper case
   --  Trim_Spaces : remove unneded spaces.
   --
   function To_Ada
     (Arguments   : in Win32.PCWSTR;
      Keep_Null   : in Boolean := False;
      To_Upper    : in Boolean := False;
      Trim_Spaces : in Boolean := False)
      return        Wide_String
   is
      Arguments_Length : constant Natural     :=
         Natural (Win32.Winbase.lstrlenW (Arguments));
      Buffer           : Wide_String (1 .. Arguments_Length + 1);
      Dummy            : constant Win32.PWSTR :=
         Win32.Winbase.lstrcpynW
           (lpString1  => Win32.Addr (Buffer),
            lpString2  => Arguments,
            iMaxLength => Buffer'Length);
      pragma Unreferenced (Dummy);
      pragma Warnings (Off, Buffer);
   begin
      if To_Upper then
         Ada.Strings.Wide_Fixed.Translate
           (Source  => Buffer,
            Mapping => Ada.Strings.Wide_Maps.Wide_Constants.Upper_Case_Map);
      end if;
      if Trim_Spaces then
         if Keep_Null then
            return Ada.Strings.Wide_Fixed.Trim
                     (Source => Buffer (Buffer'First .. Buffer'Last),
                      Side   => Ada.Strings.Both);
         else
            return Ada.Strings.Wide_Fixed.Trim
                     (Source => Buffer (Buffer'First .. Buffer'Last - 1),
                      Side   => Ada.Strings.Both);
         end if;
      else
         if Keep_Null then
            return Buffer (Buffer'First .. Buffer'Last);
         else
            return Buffer (Buffer'First .. Buffer'Last - 1);
         end if;
      end if;
   end To_Ada;

   function To_Ada
     (Arguments   : in TakeCmd.Plugin.Buffer;
      Keep_Null   : in Boolean := False;
      To_Upper    : in Boolean := False;
      Trim_Spaces : in Boolean := False)
      return        Wide_String
   is
      Arguments_Length : constant Natural     :=
         Natural (Win32.Winbase.lstrlenW (Win32.Addr (Arguments)));
      Buffer           : Wide_String (1 .. Arguments_Length + 1);
      Dummy            : constant Win32.PWSTR :=
         Win32.Winbase.lstrcpynW
           (lpString1  => Win32.Addr (Buffer),
            lpString2  => Win32.Addr (Arguments),
            iMaxLength => Buffer'Length);
      pragma Unreferenced (Dummy);
      pragma Warnings (Off, Buffer);
   begin
      if To_Upper then
         Ada.Strings.Wide_Fixed.Translate
           (Source  => Buffer,
            Mapping => Ada.Strings.Wide_Maps.Wide_Constants.Upper_Case_Map);
      end if;
      if Trim_Spaces then
         if Keep_Null then
            return Ada.Strings.Wide_Fixed.Trim
                     (Source => Buffer (Buffer'First .. Buffer'Last),
                      Side   => Ada.Strings.Both);
         else
            return Ada.Strings.Wide_Fixed.Trim
                     (Source => Buffer (Buffer'First .. Buffer'Last - 1),
                      Side   => Ada.Strings.Both);
         end if;
      else
         if Keep_Null then
            return Buffer (Buffer'First .. Buffer'Last);
         else
            return Buffer (Buffer'First .. Buffer'Last - 1);
         end if;
      end if;
   end To_Ada;

   procedure To_Ada
     (Arguments   : in Win32.PCWSTR;
      Buffer      : out Wide_String;
      Keep_Null   : in Boolean := False;
      To_Upper    : in Boolean := False;
      Trim_Spaces : in Boolean := False)
   is
      Dummy : constant Win32.PWSTR :=
         Win32.Winbase.lstrcpynW
           (lpString1  => Win32.Addr (Buffer),
            lpString2  => Arguments,
            iMaxLength => Buffer'Length);

      pragma Unreferenced (Dummy);
      pragma Warnings (Off, Buffer);
   begin
      if not Keep_Null then
         Buffer (Buffer'Last) := Ada.Strings.Wide_Space;
      end if;
      if To_Upper then
         Ada.Strings.Wide_Fixed.Translate
           (Source  => Buffer,
            Mapping => Ada.Strings.Wide_Maps.Wide_Constants.Upper_Case_Map);
      end if;
      if Trim_Spaces then
         Ada.Strings.Wide_Fixed.Trim
           (Source  => Buffer (Buffer'First .. Buffer'Last - 1),
            Side    => Ada.Strings.Both,
            Justify => Ada.Strings.Left,
            Pad     => Ada.Strings.Wide_Space);
      end if;
      return;
   end To_Ada;

   ---------------------------------------------------------------------------
   --
   --  Convert the string to Win 32.
   --
   --  Arguments   : String to be converted
   --
   function To_Win (Arguments : in Wide_String) return TakeCmd.Plugin.Buffer is
      Dummy : Win32.PWSTR;

      pragma Warnings (Off, Dummy);
   begin
      return Result : aliased TakeCmd.Plugin.Buffer do
         pragma Warnings (Off, Result);
         Dummy                         :=
            Win32.Winbase.lstrcpynW
              (lpString1  => Win32.Addr (Result),
               lpString2  => Win32.Addr (Arguments),
               iMaxLength => Arguments'Length + 1);
         Result (Arguments'Length + 1) := Win32.Wide_Nul;
      end return;
   end To_Win;

end TakeCmd.Strings;
