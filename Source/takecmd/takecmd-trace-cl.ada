-------------------------------------------------------------- {{{1 ----------
--  Description: Options setable by the Ada plugin
--          $Id: takecmd-plugin.ads 15 2007-10-31 08:27:40Z
--  krischik@users.sourceforge.net $
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author: krischik@users.sourceforge.net $
--        $Date: 2007-10-31 09:27:40 +0100 (Mi, 31 Okt 2007) $
--      Version: 4.5
--    $Revision: 15 $
--     $HeadURL:
--  https://mkutils.googlecode.com/svn/trunk/Source/takecmd-plugin.ads $
--      History: 25.10.2007 MK Initial Release
--               29.10.2007 MK Added Threading, parameter names closer to
--                             C original
-----------------------------------------------------------------------------
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
-------------------------------------------------------------- }}}1 ----------

pragma License (Modified_Gpl);
pragma Ada_05;

with Ada.Text_IO;
with Ada.Task_Identification;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with Ada.Strings.Maps;
with Ada.Characters.Handling;
with Ada.Characters.Conversions;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

--  with GNAT.Traceback.Symbolic;

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
   --  Initialize Trace.
   --
   procedure Initialize is
      Value : aliased TakeCmd.Buffer;
   begin
      if TakeCmd.QueryOptionValue
            (pszOption => Win32.Addr (Trace_Opt),
             pszValue  => Value'Access) =
         0
      then
         if Value = Trace_Opt_On then
            State.On := True;
            --  elsif Value = Trace_Opt_NoPrefix then
            --  State.On                := True;
            --  State.Write_Prefix      := False;
            --  State.Write_Line_Number := False;
         end if;
      end if;
      if TakeCmd.QueryOptionValue
            (pszOption => Win32.Addr (Trace_Opt_To),
             pszValue  => Value'Access) =
         0
      then
         if Value = Trace_Opt_To_Err1
           or else Value = Trace_Opt_To_Err2
         then
            State.Location := Standard_Error;
         elsif Value = Trace_Opt_To_Std1
           or else Value = Trace_Opt_To_Std2
         then
            State.Location := Standard_Output;
         elsif Value = Trace_Opt_To_File then
            State.Location := File;
         elsif Value = Trace_Opt_To_Queue1
           or else Value = Trace_Opt_To_Queue2
         then
            State.Location := Queue;
         end if;
      end if;
      if TakeCmd.QueryOptionValue
            (pszOption => Win32.Addr (Trace_Opt_To),
             pszValue  => Value'Access) =
         0
      then
         --  if Value'Length > 0 then
         --  Set_Filename (Value);
         --  end if;
         null;
      end if;
      if TakeCmd.QueryOptionValue
            (pszOption => Win32.Addr (Trace_Verbose),
             pszValue  => Value'Access) =
         0
      then
         --  if Value  0 then
         State.Verbose := True;
         --  end if;
      end if;
   end Initialize;

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
               StrLineNo    : constant String :=
                  S_F.Head (Natural'Image (Get_Sequence), 5);
            begin
               StrPrefix := StrLineNo &
                            ":" &
                            StrThread_ID &
                            ":" &
                            StrPrefix;
            end Prefix;
         end if;

         Append_All
           (Source   => StrOut,
            Search   => StrLF,
            New_Item => To_String (StrPrefix),
            Mapping  => Maps.Identity,
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
