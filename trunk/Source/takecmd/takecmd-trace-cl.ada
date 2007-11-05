----------------------------------------------------------------------------
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

with Ada.Strings.Maps;
with TakeCmd.Strings;

---------------------------------------------------------------------------
--
--  Protect all global data.
--
separate (TakeCmd.Trace)
protected body Cl is

   ------------------------------------------------------------------------
   --
   --  Trace is On
   --
   function Get_On return Boolean is
   begin
      return State.On;
   end Get_On;

   ------------------------------------------------------------------------
   --
   --  Get Trace line counter
   --
   function Get_Sequence return Natural is
   begin
      return Sequence;
   end Get_Sequence;

   ------------------------------------------------------------------------
   --
   --  Determine the threadId of the current thread
   --
   procedure Get_Thread_ID (Retval : out Thread_ID) is
      Thread_Name : constant String := TaskID.Image (TaskID.Current_Task);
   begin
      if Threads.Contains (Thread_Name) then
         Retval := Threads.Element (Thread_Name);
      else
         Retval := Thread_ID'(Thread_No => Thread_No, Indent => 0);
         Threads.Insert (Key => Thread_Name, New_Item => Retval);

         Thread_No := Natural'Succ (Thread_No);

         if State.On then
            Write_Formatted_String
              (Text   => "New Thread : " & Thread_Name,
               Marker => Marker_Special);
         end if;
      end if;
   end Get_Thread_ID;

   ------------------------------------------------------------------------
   --
   --  Trace Destination
   --
   function Get_Trace_Location return Destination is
   begin
      return State.Location;
   end Get_Trace_Location;

   ------------------------------------------------------------------------
   --
   --  Trace is On
   --
   function Get_Verbose return Boolean is
   begin
      return State.Verbose;
   end Get_Verbose;

   ------------------------------------------------------------------------
   --
   --  Trace with line numbers.
   --
   function Get_Write_Line_Number return Boolean is
   begin
      return State.Write_Line_Number;
   end Get_Write_Line_Number;

   ------------------------------------------------------------------------
   --
   --  Trace with thread profex and optional line numbers.
   --
   function Get_Write_Prefix return Boolean is
   begin
      return State.Write_Prefix;
   end Get_Write_Prefix;

   ------------------------------------------------------------------------
   --
   --  Icrement Sequence by one
   --
   procedure Inc_Sequence is
   begin
      Sequence := Natural'Succ (Sequence);
   end Inc_Sequence;

   ------------------------------------------------------------------------
   --
   --  Set Filename for Trace File
   --
   procedure Set_Filename (New_Filename : in String) is
   begin
      if IO.Is_Open (Filehandle) then
         IO.Close (Filehandle);
      end if;

      Filename := S_U.To_Unbounded_String (New_Filename);
   end Set_Filename;

   ------------------------------------------------------------------------
   --
   --  Trace is On
   --
   procedure Set_On (On : Boolean) is
   begin
      State.On := On;
   end Set_On;

   ------------------------------------------------------------------------
   --
   --  Determine the threadId of the current thread
   --
   procedure Set_Thread_ID (New_Value : in Thread_ID) is
      Thread_Name : constant String := TaskID.Image (TaskID.Current_Task);
   begin
      if Threads.Contains (Thread_Name) then
         Threads.Replace (Key => Thread_Name, New_Item => New_Value);
      else
         Threads.Insert (Key => Thread_Name, New_Item => New_Value);
      end if;
   end Set_Thread_ID;

   ------------------------------------------------------------------------
   --
   --  Trace Destination
   --
   procedure Set_Trace_Location (Location : in Destination) is
   begin
      State.Location := Location;
   end Set_Trace_Location;

   ------------------------------------------------------------------------
   --
   --  Trace is On
   --
   procedure Set_Verbose (Verbose : Boolean) is
   begin
      State.Verbose := Verbose;
   end Set_Verbose;

   ------------------------------------------------------------------------
   --
   --  Trace with line numbers.
   --
   procedure Set_Write_Line_Number (Write_Line_Number : Boolean) is
   begin
      State.Write_Line_Number := Write_Line_Number;
   end Set_Write_Line_Number;

   ------------------------------------------------------------------------
   --
   --  Trace with thread profex and optional line numbers.
   --
   procedure Set_Write_Prefix (Write_Prefix : Boolean) is
   begin
      State.Write_Prefix := Write_Prefix;
   end Set_Write_Prefix;

   ------------------------------------------------------------------------
   --
   --  Write Formated Text
   --
   --  Text   : Text to be written
   --  Marker : Marker to be used
   procedure Write_Formatted_String (Text : in String; Marker : in String) is
      use Ada.Strings.Unbounded;

      Thread : Thread_ID;
   begin
      Get_Thread_ID (Thread);

      if Marker (Marker'First) = Marker_Outdent (Marker_Outdent'First)
        and then Thread.Indent >= Indent_Level
      then
         Thread.Indent := Thread.Indent - Indent_Level;
      end if;

      Format : declare
         StrOut    : Unbounded_String := To_Unbounded_String (Marker & Text);
         StrPrefix : Unbounded_String := Thread.Indent * ' ';
         StrLF     : constant String  := (1 => C_L1.LF);
         Count     : Natural          := Natural'First;
      begin
         if State.Write_Prefix then
            Prefix : declare
               StrThread_ID : constant String :=
                  S_F.Head (Natural'Image (Thread.Thread_No), 5);
               StrLineNo    : constant String := S_F.Head (Natural'Image (Get_Sequence), 5);
            begin
               StrPrefix := StrLineNo & ":" & StrThread_ID & ":" & StrPrefix;
            end Prefix;
         end if;

         TakeCmd.Strings.Append_All
           (Source   => StrOut,
            Search   => StrLF,
            New_Item => To_String (StrPrefix),
            Mapping  => Ada.Strings.Maps.Identity,
            Count    => Count);
         StrOut := StrPrefix & StrOut;

         Write_String (To_String (StrOut));
      end Format;

      Inc_Sequence;

      if Marker (Marker'First) = Marker_Indent (Marker_Indent'First) then
         Thread.Indent := Thread.Indent + Indent_Level;
      end if;

      Set_Thread_ID (Thread);

   end Write_Formatted_String;

   ------------------------------------------------------------------------
   --
   --  Write Text
   --
   --  Text to be written
   procedure Write_String (Text : in String) is
      use Ada.Text_IO;
   begin
      case State.Location is
         when Queue =>
            null;
         when Standard_Error =>
            Put_Line (Standard_Error, Text);
         when Standard_Output =>
            Put_Line (Standard_Output, Text);
         when File =>
            if not Is_Open (Filehandle) then
               Create
                 (File => Filehandle,
                  Mode => Out_File,
                  Name => S_U.To_String (Filename),
                  Form => "shared=yes");
            end if;

            Put_Line (Filehandle, Text);
            Flush (Filehandle);
      end case;
   end Write_String;
end Cl;
