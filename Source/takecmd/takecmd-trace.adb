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

--
--  4NT / Take Commmand Plugin Library Trace
--
package body TakeCmd.Trace is

   ---------------------------------------------------------------------------
   --
   use type System.Storage_Elements.Storage_Offset;
   use type System.Storage_Elements.Storage_Element;
   use type System.Address;
   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Ada.Text_IO.File_Type;
   use type Win32.WCHAR_Array;
   use type Interfaces.C.int;
   ---------------------------------------------------------------------------
   --
   package S_U renames Ada.Strings.Unbounded;
   package S_F renames Ada.Strings.Fixed;
   package Maps renames Ada.Strings.Maps;
   package C_L1 renames Ada.Characters.Latin_1;
   package C_H renames Ada.Characters.Handling;
   package IO renames Ada.Text_IO;
   package TaskID renames Ada.Task_Identification;
   package Sys renames System;
   package Sys_SE renames System.Storage_Elements;

   --   package G_TB       renames GNAT.Traceback.Symbolic;

   ---------------------------------------------------------------------------
   --
   package Address_IO is new Ada.Text_IO.Modular_IO (
      Num => System.Storage_Elements.Integer_Address);

   ---------------------------------------------------------------------------
   --
   --  Location          : Trace Destination
   --  On                : Trace is On
   --  Write_Line_Number : Trace with line numbers.
   --  Write_Prefix      : Trace with thread profex and optional line numbers.
   --  Verbose           : Verbose operation.
   --
   type States is record
      Location          : Destination := Standard_Error;
      On                : Boolean     := False;
      Write_Line_Number : Boolean     := True;
      Write_Prefix      : Boolean     := True;
      Verbose           : Boolean     := False;
   end record;

   ---------------------------------------------------------------------------
   --
   --  More or less to try out a record layout - mind you the C++ original
   --  used similar techniques - only they where not at all as elegant.
   --
   --  Location          Trace Destination
   --  On                Trace is On
   --  Write_Line_Number Trace with line numbers.
   --  Write_Prefix      Trace with thread profex and optional line numbers.
   --  Verbose           Verbose operation.
   for States use record
      Location          at 0 range 0 .. 3;
      On                at 0 range 4 .. 4;
      Write_Line_Number at 0 range 5 .. 5;
      Write_Prefix      at 0 range 6 .. 6;
      Verbose           at 0 range 7 .. 7;
   end record;

   ---------------------------------------------------------------------------
   --
   --  Thread_No : Each Thread has a number. A number is shorter then string.
   --  Indent    : Function indeting is counded separate for every thread
   type Thread_ID is record
      Thread_No : Natural := Natural'First;
      Indent    : Natural := Natural'First;
   end record;

   package Thread_ID_Map is new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type => String,
      Element_Type => Thread_ID,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "=" => "=");

   ---------------------------------------------------------------------------
   --
   --  Protect all global data.
   --
   protected Cl is
      --
      --  Initialize Trace.
      --
      procedure Initialize;

      --
      --  Icrement Trace line counter by one
      --
      procedure Inc_Sequence;

      --
      --  Get Trace line counter
      --
      function Get_Sequence return Natural;

      procedure Set_Filename (New_Filename : in String);

      --
      --  Determine the threadId of the current thread
      --
      procedure Get_Thread_ID (Retval : out Thread_ID);

      --
      --  Determine the threadId of the current thread
      --
      procedure Set_Thread_ID (New_Value : in Thread_ID);

      --
      --  Trace is On
      --
      function Get_On return Boolean;

      --
      --  Trace is On
      --
      procedure Set_On (On : Boolean);

      --
      --  Trace is On
      --
      function Get_Verbose return Boolean;

      --
      --  Trace is On
      --
      procedure Set_Verbose (Verbose : Boolean);

      --
      --  Trace with line numbers.
      --
      function Get_Write_Line_Number return Boolean;

      --
      --  Trace with line numbers.
      --
      procedure Set_Write_Line_Number (Write_Line_Number : Boolean);

      --
      --  Trace with thread profex and optional line numbers.
      --
      function Get_Write_Prefix return Boolean;

      --
      --  Trace with thread profex and optional line numbers.
      --
      procedure Set_Write_Prefix (Write_Prefix : Boolean);

      --
      --  Trace Destination
      --
      function Get_Trace_Location return Destination;

      --
      --  Trace Destination
      --
      procedure Set_Trace_Location (Location : in Destination);

      --
      --  Write Formated Text
      --
      --  Text   : Text to be written
      --  Marker : Marker to be used
      procedure Write_Formatted_String
        (Text   : in String;
         Marker : in String);

      --
      --  Write Text
      --
      --  Text : Text to be written
      procedure Write_String (Text : in String);

   private

      --
      --  Trace line counter
      --
      Sequence : Natural := Natural'First;
      --
      --  Filename of Trace if Destination ist File
      --
      Filename : S_U.Unbounded_String :=
         S_U.To_Unbounded_String ("Trace.Out");
      --
      --  The original IBM design opened and closed the File all the time.
      --  However, Ada.Text_IO won't allow that and of course, it is slow.
      --
      Filehandle : IO.File_Type;
      --
      --  Last Thread ID used
      --
      Thread_No : Natural := Natural'First;
      --
      --  Current Indenting Level for each thread
      --
      Threads : Thread_ID_Map.Map;
      --
      --  Status of Trace
      --
      State : States;

      pragma Inline (Inc_Sequence);
      pragma Inline (Get_Sequence);
      pragma Inline (Set_Filename);
      pragma Inline (Get_On);
      pragma Inline (Set_On);
      pragma Inline (Set_Verbose);
      pragma Inline (Get_Write_Line_Number);
      pragma Inline (Set_Write_Line_Number);
      pragma Inline (Get_Write_Prefix);
      pragma Inline (Set_Write_Prefix);
      pragma Inline (Get_Trace_Location);
      pragma Inline (Set_Trace_Location);
   end Cl;
   ---------------------------------------------------------------------------
   ---
   --
   --  Searches for all occurences of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Count    : Count of replaces done
   procedure Append_All
     (Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity;
      Count    : out Natural);

   ---------------------------------------------------------------------------
   ---

   --
   --  Indent Level
   --
   Indent_Level : constant Natural := 2;
   --
   --  Commandline options
   --
   Trace_Verbose       : aliased constant Win32.WCHAR_Array := "verbose";
   Trace_Opt           : aliased constant Win32.WCHAR_Array := "TRACE";
   Trace_Opt_On        : aliased constant Win32.WCHAR_Array := "ON";
   Trace_Opt_To        : aliased constant Win32.WCHAR_Array := "TRACETO";
   Trace_Opt_To_Err1   : aliased constant Win32.WCHAR_Array := "STDERR";
   Trace_Opt_To_Err2   : aliased constant Win32.WCHAR_Array := "ERR";
   Trace_Opt_To_Std1   : aliased constant Win32.WCHAR_Array := "STDOUT";
   Trace_Opt_To_Std2   : aliased constant Win32.WCHAR_Array := "OUT";
   Trace_Opt_To_File   : aliased constant Win32.WCHAR_Array := "FILE";
   Trace_Opt_To_Queue1 : aliased constant Win32.WCHAR_Array := "QUEUE";
   Trace_Opt_To_Queue2 : aliased constant Win32.WCHAR_Array := "PMPRINTF";
   Trace_Opt_File      : aliased constant Win32.WCHAR_Array := "TRACEFILE";

   Marker_Std     : constant String := (1 => C_L1.Greater_Than_Sign);
   Marker_Special : constant String := (1 => C_L1.Exclamation);
   Marker_Outdent : constant String := (1 => C_L1.Minus_Sign);
   Marker_Indent  : constant String := (1 => C_L1.Plus_Sign);

   ---------------------------------------------------------------------------
   --
   --  Protect all global data.
   --
   protected body Cl is separate;

   ---------------------------------------------------------------------------
   ---
   --
   --  Searches for all occurences of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Count    : Count of replaces done
   procedure Append_All
     (Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity;
      Count    : out Natural)
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;

      --
      --  Offset from which we start. 0 means search from the first character
      --
      Offset : Natural := Natural'First;
      --
      --  Lenght of the full string
      --
      Len : constant Natural := Length (Source);
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
            --  We slice from the Offset on to the end. One might be suprised
            --  to learn that Sub_String'First might not be 1
            --
            Sub_String : constant String :=
               Slice (Source => Source, Low => Offset + 1, High => Len);
            --
            --  We search for Pattern
            --
            Low  : constant Natural :=
               Index
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
              or else New_Item /=
                      Sub_String (High .. High + New_Item'Length - 1)
            then
               --
               --  We insert one character after the end of the found string
               --  when the new text does not allready follow. This can of
               --  course only happen when there are enouch characters behind
               --  the found string to contain the new text.
               --
               Insert
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
   --
   --  Copy Instanz.
   --
   --  This :  Object itself.
   procedure Adjust (This : in out Object) is
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String
           (Text   => This.Trace_Name,
            Marker => Marker_Indent);
      end if;
   end Adjust;

   ---------------------------------------------------------------------------
   --
   --  Assert a Condition. If the condition is not true create a trace entry
   --  describing the assertion and then raise an exception.
   --
   --  Condition : Condition which should be true
   --  Raising   : Exeption which is raised
   --  Message   : Free form Message
   --  Entity    : Location destriptor. Suggested content: AdaCL.Trace.Entity
   --  Source    : Location destriptor. Suggested content: AdaCL.Trace.Source
   procedure Assert
     (Condition : in Boolean;
      Raising   : in Ada.Exceptions.Exception_Id;
      Message   : in String := "No Message given.";
      Entity    : in String := "No Entity given.";
      Source    : in String := "No Source given.")
   is
   begin
      if not Condition then
         Raise_Exception
           (Raising => Raising,
            Message => Message,
            Entity  => Entity,
            Source  => Source);
      end if;
   end Assert;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Disable_Trace is
   begin
      Cl.Set_On (False);
   end Disable_Trace;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Disable_Verbose is
   begin
      Cl.Set_Verbose (False);
   end Disable_Verbose;

   ---------------------------------------------------------------------------
   --
   --  Don't Write Line numbers
   --
   procedure Disable_Write_Line_Number is
   begin
      Cl.Set_Write_Line_Number (False);
   end Disable_Write_Line_Number;

   ---------------------------------------------------------------------------
   --
   --  Disable the Write prefix
   --
   procedure Disable_Write_Prefix is
   begin
      Cl.Set_Write_Prefix (False);
   end Disable_Write_Prefix;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Enable_Trace is
   begin
      Cl.Set_On (True);
   end Enable_Trace;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Enable_Verbose is
   begin
      Cl.Set_Verbose (True);
   end Enable_Verbose;

   ---------------------------------------------------------------------------
   --
   --  Write Line numbers
   --
   procedure Enable_Write_Line_Number is
   begin
      Cl.Set_Write_Line_Number (True);
   end Enable_Write_Line_Number;

   ---------------------------------------------------------------------------
   --
   --  Enable the Write prefix
   --
   procedure Enable_Write_Prefix is
   begin
      Cl.Set_Write_Prefix (True);
   end Enable_Write_Prefix;

   ---------------------------------------------------------------------------
   --
   --  Trace end of function
   --
   --  This : Object itself.
   procedure Finalize (This : in out Object) is
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String
           (Text   => This.Trace_Name,
            Marker => Marker_Outdent);
      end if;
   end Finalize;

   ---------------------------------------------------------------------------
   --
   --  Functrace is not quite as usefull as the C++ version. The reason are
   --  the missing constructors and destructors in Ada. With Controlled types
   --  you can't limit to just one call to Initialize and one to Finalize
   --  There are allways some extra Adjust with matching. Finalize.
   --
   function Function_Trace (
      --  Name of the function calls to be traced.
Name : String) return Object is
      Retval : constant Object (Name'Length) :=
        (Inherited.Controlled with
         Name_Length => Name'Length,
         Trace_Name  => Name);
   begin
      --
      --  The Initialize method is not realy a replacement for a proper
      --  contructor.
      --
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String
           (Text   => Retval.Trace_Name,
            Marker => Marker_Indent);
      end if;

      return Retval;
   end Function_Trace;

   ---------------------------------------------------------------------------
   --
   --  check is trace is Enabled
   --
   function Is_Trace_Enabled return Boolean is
   begin
      return Cl.Get_On;
   end Is_Trace_Enabled;

   ---------------------------------------------------------------------------
   --
   --  check is trace is Enabled
   --
   function Is_Verbose_Enabled return Boolean is
   begin
      return Cl.Get_Verbose;
   end Is_Verbose_Enabled;

   ---------------------------------------------------------------------------
   --
   --  check if Line numbers are written
   --
   function Is_Write_Line_Number_Enabled return Boolean is
   begin
      return Cl.Get_Write_Line_Number;
   end Is_Write_Line_Number_Enabled;

   ---------------------------------------------------------------------------
   --
   --  Check the Write prefix flag
   --
   function Is_Write_Prefix_Enabled return Boolean is
   begin
      return Cl.Get_Write_Prefix;
   end Is_Write_Prefix_Enabled;

   ---------------------------------------------------------------------------
   --
   --  Trace the given exeption details and then raise the exception.
   --
   --  Raising : Exeption which is raised Message : Free form Message Entity :
   --  Location destriptor. Suggested content: AdaCL.Trace.Entity Source :
   --  Location destriptor. Suggested content: AdaCL.Trace.Source
   procedure Raise_Exception
     (Raising : in Ada.Exceptions.Exception_Id;
      Message : in String := "No Message given";
      Entity  : in String := "No Entity given.";
      Source  : in String := "No Source given.")
   is
      use Ada.Exceptions;
   begin
      Write ("Raise Exception " & Exception_Name (Raising));
      Write ("   with Message " & Message);
      Write ("   for Entity   " & Entity);
      Write ("   in Source    " & Source);

      Raise_Exception
        (E       => Raising,
         Message =>
            Message &
            " Entity :" &
            Entity &
            "." &
            " Source :" &
            Source &
            ".");
      --
      --  GNAT designer forgot to add pragma No_Return to
      --  Ada.Exceptions.Raise_Exception.
      --
      raise Constraint_Error;
   end Raise_Exception;

   ---------------------------------------------------------------------------
   --
   --  Check the Trace Destination
   --
   function Trace_Destination return Destination is
   begin
      return Cl.Get_Trace_Location;
   end Trace_Destination;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   procedure Write (A_String : in String) is
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (Text => A_String, Marker => Marker_Std);
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an Address.
   --
   --  A_String : String to be written
   procedure Write (A_String : in String; An_Address : in Sys.Address) is
   begin
      if Is_Trace_Enabled then
         Write_Address : declare

            Address_Text : String (1 .. 3 + 8 + 1);

         begin
            Address_IO.Put
              (To   => Address_Text,
               Item => Sys_SE.To_Integer (An_Address),
               Base => 16);

            Cl.Write_Formatted_String
              (Text   => A_String & Address_Text,
               Marker => Marker_Std);
         end Write_Address;
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_Unbounded : String to be written
   procedure Write (A_Unbounded : in S_U.Unbounded_String) is
      use Ada.Strings.Unbounded;
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String
           (Text   => To_String (A_Unbounded),
            Marker => Marker_Std);
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   procedure Write (An_Exception : in Ada.Exceptions.Exception_Occurrence) is
      use Ada.Exceptions;
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String
           (Text   => Exception_Information (An_Exception),
            Marker => Marker_Special);
         --              Cl.Write_Formatted_String (Text   =>
         --  G_TB.Symbolic_Traceback (An_Exception),
         --                                         Marker => Marker_Special);
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   --  An_Entity    : Procedure in which the exception was caught
   --  A_Source     : Source File in which Entity is located.
   procedure Write
     (An_Exception : in Ada.Exceptions.Exception_Occurrence;
      An_Entity    : in String;
      A_Source     : in String)
   is
      use Ada.Exceptions;
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String
           (Text   => Exception_Information (An_Exception),
            Marker => Marker_Special);
         Cl.Write_Formatted_String
           (Text   => "Function: " & An_Entity,
            Marker => Marker_Special);
         Cl.Write_Formatted_String
           (Text   => "Source: " & A_Source,
            Marker => Marker_Special);
         --       Cl.Write_Formatted_String (
         --          Text   => G_TB.Symbolic_Traceback (An_Exception),
         --          Marker => Marker_Special);
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write Help for Commandline Options parsed from Trace
   --
   procedure Write_Commandline_Help is
   begin
      --  IO.New_Line;
      --  IO.Put_Line ("Trace options:");
      --  IO.New_Line;
      --  IO.Put_Line
      --  ("    --" & Trace_Verbose & "            verbose operation.");

      --  IO.Put_Line
      --  ("    --" &
      --  Trace_Opt &
      --  "=" &
      --  Trace_Opt_On &
      --  "           activate trace.");
      --  IO.Put_Line
      --  ("    --" &
      --  Trace_Opt &
      --  "=" &
      --  Trace_Opt_NoPrefix &
      --  "     activate trace without prefix.");
      --  IO.New_Line;
      --  IO.Put_Line
      --  ("    --" &
      --  Trace_Opt_To &
      --  "=" &
      --  Trace_Opt_To_Err1 &
      --  "     trace in stderr.");
      --  IO.Put_Line
      --  ("    --" &
      --  Trace_Opt_To &
      --  "=" &
      --  Trace_Opt_To_Std1 &
      --  "     trace in stdout.");
      --  IO.Put_Line
      --  ("    --" &
      --  Trace_Opt_To &
      --  "=" &
      --  Trace_Opt_To_File &
      --  "       trace to file.");
      --  IO.New_Line;
      --  IO.Put_Line
      --  ("    --" & Trace_Opt_File & "=" & "Filename" & " trace file.");
      --  IO.New_Line;
      return;
   end Write_Commandline_Help;

   --
   --  Create a memory dump
   --
   --  String to be written
   procedure Write_Dump
     (An_Address : in Sys.Address;
      A_Size     : in Sys_SE.Storage_Count)
   is
   begin
      Write ("Address         : ", An_Address);
      Write ("Lenght          :" & Sys_SE.Storage_Count'Image (A_Size));

      if Is_Trace_Enabled then
         Dump : declare
            package Byte_IO is new IO.Modular_IO (
               Num => Sys_SE.Storage_Element);

            use Ada.Strings.Fixed;

            Data : Sys_SE.Storage_Array (0 .. A_Size - 1);
            for Data'Address use An_Address;
            pragma Import (Ada, Data);

            Line_Len     : constant := 16;
            Address_Len  : constant := 8;
            Byte_Len     : constant := 2;
            Byte_Offset  : constant := 18;   --  Dump  [01234567]
            ASCII_Offset : constant := Byte_Offset +
                                       Line_Len * (Byte_Len + 1) +
                                       1;
            Text_Len     : constant := ASCII_Offset + Line_Len;

            Byte_Text    : String (1 .. 3 + Byte_Len + 1);
            Address_Text : String (1 .. 3 + Address_Len + 1);
            Text         : String (1 .. Text_Len);
            Line         : Sys_SE.Storage_Offset := Data'First;
            Col          : Sys_SE.Storage_Offset := Data'First;
            Char         : Character;
            Byte_Col     : Integer;
         begin
            Dump_Line : while Line <= Data'Last loop
               Address_IO.Put
                 (To   => Address_Text,
                  Item => Sys_SE.To_Integer (An_Address + Line),
                  Base => 16);

               if Address_Text (4) = '#' then
                  Address_Text (4) := '0';
               end if;

               Move
                 (Source => "Dump  [" & Address_Text (4 .. 11) & "]: ",
                  Target => Text);

               Col      := 0;
               Byte_Col := Byte_Offset;

               Dump_Column : while Col < Line_Len
                 and then          Col + Line < A_Size
               loop
                  Byte_IO.Put
                    (To   => Byte_Text,
                     Item => Data (Line + Col),
                     Base => 16);

                  if Byte_Text (4) = '#' then
                     Byte_Text (4) := '0';
                  end if;

                  Text (Byte_Col .. Byte_Col + 1) := Byte_Text (4 .. 5);

                  Char := Character'Val (Data (Line + Col));

                  if C_H.Is_Graphic (Char) then
                     Text (Natural (ASCII_Offset + Col))  := Char;
                  else
                     Text (Natural (ASCII_Offset + Col))  := '.';
                  end if;

                  Col      := Col + 1;
                  Byte_Col := Byte_Col + (Byte_Len + 1);
               end loop Dump_Column;

               Cl.Write_Formatted_String
                 (Text   => Text,
                  Marker => Marker_Std);
               Line := Line + Line_Len;
            end loop Dump_Line;
         end Dump;
      end if;
   end Write_Dump;

   ---------------------------------------------------------------------------
   --
   --  Create a memory dump. This Dump takes size in bits.
   --
   --  An_Address :  String to be written
   --  A_Size     :  Size in Storage_Elements.
   procedure Write_Dump
     (An_Address : in System.Address;
      A_Size     : in Integer)
   is
      Size : Sys_SE.Storage_Count :=
         Sys_SE.Storage_Count (A_Size / Sys.Storage_Unit);
   begin
      if (A_Size mod Sys.Storage_Unit) /= 0 then
         Size := Size + 1;
      end if;

      Write_Dump (An_Address, Size);
   end Write_Dump;

   ---------------------------------------------------------------------------
   ---

   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   procedure Write_Error (A_String : in String) is
   begin
      if not Is_Trace_Enabled
        or else Trace_Destination /= Standard_Error
      then
         IO.Put_Line (IO.Standard_Error, A_String);
      end if;

      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (Text => A_String, Marker => Marker_Std);
      end if;
   end Write_Error;

   ---------------------------------------------------------------------------
   ---

   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_Unbounded : String to be written
   procedure Write_Error (A_Unbounded : in S_U.Unbounded_String) is
      use Ada.Strings.Unbounded;
   begin
      if not Is_Trace_Enabled
        or else Trace_Destination /= Standard_Error
      then
         IO.Put_Line (IO.Standard_Error, To_String (A_Unbounded));
      end if;

      if Is_Trace_Enabled then
         Cl.Write_Formatted_String
           (Text   => To_String (A_Unbounded),
            Marker => Marker_Std);
      end if;
   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   procedure Write_Error
     (An_Exception : in Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
   begin
      if not Is_Trace_Enabled
        or else Trace_Destination /= Standard_Error
      then
         IO.Put_Line
           (IO.Standard_Error,
            Exception_Information (An_Exception));
         --              IO.Put_Line (IO.Standard_Error,
         --  G_TB.Symbolic_Traceback (An_Exception));
      end if;

      Write (An_Exception);
   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception :  String to be written
   --  An_Entity    :  Procedure in which the exception was caught
   --  A_Source     :  Source File in which Entity is located.
   procedure Write_Error
     (An_Exception : in Ada.Exceptions.Exception_Occurrence;
      An_Entity    : in String;
      A_Source     : in String)
   is
      use Ada.Exceptions;
   begin
      if not Is_Trace_Enabled
        or else Trace_Destination /= Standard_Error
      then
         IO.New_Line (IO.Standard_Error);
         IO.Put (IO.Standard_Error, Exception_Information (An_Exception));
         IO.Put_Line (IO.Standard_Error, "Function: " & Entity);
         IO.Put_Line (IO.Standard_Error, "Source: " & Source);
         --              IO.Put_Line (IO.Standard_Error,
         --  G_TB.Symbolic_Traceback (An_Exception));
      end if;

      Write
        (An_Exception => An_Exception,
         An_Entity    => An_Entity,
         A_Source     => A_Source);

   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  When verbose is aktivated then an empty line is written to
   --  Standart_Output
   --
   procedure Write_Info is
   begin
      if Is_Verbose_Enabled then
         IO.New_Line (IO.Standard_Output);
      end if;
   end Write_Info;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output
   --  as well.
   --
   --  A_String : String to be written
   procedure Write_Info (A_String : in String) is
   begin
      if Is_Verbose_Enabled
        and then (not Is_Trace_Enabled
                 or else Trace_Destination /= Standard_Output)
      then
         IO.Put_Line (IO.Standard_Output, A_String);
      end if;

      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (Text => A_String, Marker => Marker_Std);
      end if;
   end Write_Info;

   ---------------------------------------------------------------------------
   --
   --  When verbose is aktivated then the character is written to
   --  Standart_Output.
   --
   --  A_Character : String to be written
   procedure Write_Info (A_Character : in Character) is
   begin
      if Is_Verbose_Enabled then
         IO.Put (IO.Standard_Output, A_Character);
      end if;
   end Write_Info;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output
   --  as well.
   --
   --  A_Unbounded : String to be written
   procedure Write_Info (A_Unbounded : in S_U.Unbounded_String) is
      use Ada.Strings.Unbounded;
   begin
      if Is_Verbose_Enabled
        and then (not Is_Trace_Enabled
                 or else Trace_Destination /= Standard_Output)
      then
         IO.Put_Line (IO.Standard_Output, To_String (A_Unbounded));
      end if;

      if Is_Trace_Enabled then
         Cl.Write_Formatted_String
           (Text   => To_String (A_Unbounded),
            Marker => Marker_Std);
      end if;
   end Write_Info;

   ---------------------------------------------------------------------------
   --
   --  Write to queue - not supported yet.
   --
   procedure Write_To_File is
   begin
      Cl.Set_Trace_Location (File);
   end Write_To_File;

   ---------------------------------------------------------------------------
   --
   --  Set Filename for Trace File
   --
   procedure Write_To_File (New_Filename : in String) is
   begin
      Cl.Set_Filename (New_Filename);
      Cl.Set_Trace_Location (File);
   end Write_To_File;

   ---------------------------------------------------------------------------
   --
   --  Write to queue - not supported yet.
   --
   procedure Write_To_Queue is
   begin
      Cl.Set_Trace_Location (Queue);
   end Write_To_Queue;

   ---------------------------------------------------------------------------
   --
   --  Write to Standart Error
   --
   procedure Write_To_Standard_Error is
   begin
      Cl.Set_Trace_Location (Standard_Error);
   end Write_To_Standard_Error;

   ---------------------------------------------------------------------------
   --
   --  Write to Standart Error
   --
   procedure Write_To_Standard_Output is
   begin
      Cl.Set_Trace_Location (Standard_Output);
   end Write_To_Standard_Output;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   procedure Write_Wide (A_String : in Wide_String) is
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String
           (Text   => Ada.Characters.Conversions.To_String (A_String),
            Marker => Marker_Std);
      end if;
   end Write_Wide;

begin
   Cl.Initialize;
   pragma Debug (Enable_Trace);
   pragma Debug (Write_To_File);
end TakeCmd.Trace;
