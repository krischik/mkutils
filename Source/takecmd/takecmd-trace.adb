--------------------------------------------------------------------------
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
--  GNU General Public License as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
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

with Ada.Wide_Text_IO;
with Ada.Wide_Characters.Unicode;
with Ada.Characters.Conversions;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Unbounded;

with Win32;
with TakeCmd;
with TakeCmd.Strings;

--
--  4NT / Take Commmand Plugin Library Trace
--
package body TakeCmd.Trace is
   ---------------------------------------------------------------------------
   --
   use type System.Storage_Elements.Storage_Offset;
   use type System.Storage_Elements.Storage_Element;
   use type Interfaces.C.int;
   use type Win32.WCHAR_Array;

   ---------------------------------------------------------------------------
   --
   package Address_IO is new Ada.Wide_Text_IO.Modular_IO (
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
      Location          : Destination := Console;
      On                : Boolean     := False;
      Write_Line_Number : Boolean     := True;
      Write_Prefix      : Boolean     := True;
      Verbose           : Boolean     := False;
   end record;

   ---------------------------------------------------------------------------
   --
   --  Thread_No : Each Thread has a number. A number is shorter then string.
   --  Indent    : Function indeting is counded separate for every thread
   --
   type Thread_ID is record
      Thread_No : Natural := Natural'First;
      Indent    : Natural := Natural'First;
   end record;

   ---------------------------------------------------------------------------
   --
   --  Liste aller Threads - well, those which have ever called on of the Write functions.
   --
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
      ------------------------------------------------------------------------
      --
      --  Initialize_Plugin: Read initial setup from environment
      --
      procedure Initialize_Plugin;

      ------------------------------------------------------------------------
      --
      --  Shutdown Plugin: close trace file - of open
      --
      procedure Shutdown_Plugin;

      ------------------------------------------------------------------------
      --
      --  Icrement Trace line counter by one
      --
      procedure Inc_Sequence;

      ------------------------------------------------------------------------
      --
      --  Get Trace line counter
      --
      function Get_Sequence return Natural;

      ------------------------------------------------------------------------
      --
      --  Set Filename for Trace File
      --
      procedure Set_Filename (New_Filename : in String);

      ------------------------------------------------------------------------
      --
      --  Get Filename for Trace File
      --
      function Get_Filename return String;

      ------------------------------------------------------------------------
      --
      --  Determine the threadId of the current thread
      --
      procedure Get_Thread_ID (Retval : out Thread_ID);

      ------------------------------------------------------------------------
      --
      --  Determine the threadId of the current thread
      --
      procedure Set_Thread_ID (New_Value : in Thread_ID);

      ------------------------------------------------------------------------
      --
      --  Trace is On
      --
      function Get_On return Boolean;

      ------------------------------------------------------------------------
      --
      --  Trace is On
      --
      procedure Set_On (On : Boolean);

      ------------------------------------------------------------------------
      --
      --  Trace is On
      --
      function Get_Verbose return Boolean;

      ------------------------------------------------------------------------
      --
      --  Trace is On
      --
      procedure Set_Verbose (Verbose : Boolean);

      ------------------------------------------------------------------------
      --
      --  Trace with line numbers.
      --
      function Get_Write_Line_Number return Boolean;

      ------------------------------------------------------------------------
      --
      --  Trace with line numbers.
      --
      procedure Set_Write_Line_Number (Write_Line_Number : Boolean);

      ------------------------------------------------------------------------
      --
      --  Trace with thread profex and optional line numbers.
      --
      function Get_Write_Prefix return Boolean;

      ------------------------------------------------------------------------
      --
      --  Trace with thread profex and optional line numbers.
      --
      procedure Set_Write_Prefix (Write_Prefix : Boolean);

      ------------------------------------------------------------------------
      --
      --  Trace Destination
      --
      function Get_Trace_Location return Destination;

      ------------------------------------------------------------------------
      --
      --  Trace Destination
      --
      procedure Set_Trace_Location (Location : in Destination);

      ------------------------------------------------------------------------
      --
      --  Write Formated Text
      --
      --  Text   : Text to be written
      --  Marker : Marker to be used
      procedure Write_Formatted_String (Text : in Wide_String; Marker : in Wide_String);

      ------------------------------------------------------------------------
      --
      --  Write Text
      --
      --  Text : Text to be written
      procedure Write_String (Text : in Wide_String);

   private

      ------------------------------------------------------------------------
      --
      --  Trace line counter
      --
      Sequence : Natural := Natural'First;

      ------------------------------------------------------------------------
      --
      --  Filename of Trace if Destination ist File
      --
      Filename : Ada.Strings.Unbounded.Unbounded_String;

      ------------------------------------------------------------------------
      --
      --  The original IBM design opened and closed the File all the time. However, Ada.Text_IO
      --  won't allow that and of course, it is slow.
      --
      Filehandle : Ada.Wide_Text_IO.File_Type;

      ------------------------------------------------------------------------
      --
      --  Last Thread ID used
      --
      Thread_No : Natural := Natural'First;

      ------------------------------------------------------------------------
      --
      --  Current Indenting Level for each thread
      --
      Threads : Thread_ID_Map.Map;

      ------------------------------------------------------------------------
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
   --
   --  Check if parameter is on of off
   --
   function Is_On_Off (Arguments : in Win32.PCWSTR) return Boolean;
   function Is_On_Off (Value : in Boolean) return Plugin.Buffer;

   ------------------------------------------------------------------------
   --
   --  Determine the thread number of the current thread
   --
   function Get_Thread_Number return Natural;

   ---------------------------------------------------------------------------
   --
   --  Indent Level
   --
   Indent_Level : constant Natural := 2;
   --
   --  options
   --
   Trace_Opt_On         : constant Wide_String := "ON";
   Trace_Opt_Off        : constant Wide_String := "OFF";
   Trace_Opt_To_Err1    : constant Wide_String := "STDERR";
   Trace_Opt_To_Err2    : constant Wide_String := "ERR";
   Trace_Opt_To_Std1    : constant Wide_String := "STDOUT";
   Trace_Opt_To_Std2    : constant Wide_String := "OUT";
   Trace_Opt_To_File    : constant Wide_String := "FILE";
   Trace_Opt_To_Console : constant Wide_String := "CONSOLE";
   --
   --  Trace Marker
   --
   Marker_Std     : constant Wide_String := ">";
   Marker_Special : constant Wide_String := "!";
   Marker_Outdent : constant Wide_String := "-";
   Marker_Indent  : constant Wide_String := "+";

   ---------------------------------------------------------------------------
   --
   --  Protect all global data.
   --
   protected body Cl is separate;

   ---------------------------------------------------------------------------
   --
   --  Copy Instanz.
   --
   --  This :  Object itself.
   --
   procedure Adjust (This : in out Object) is
   begin
      This.Index := Integer'Succ (This.Index);
      return;
   end Adjust;

   ---------------------------------------------------------------------------
   --
   --  Assert a Condition. If the condition is not true create a trace entry describing the
   --  assertion and then raise an exception.
   --
   --  Condition : Condition which should be true
   --  Raising   : Exeption which is raised
   --  Message   : Free form Message
   --  Entity    : Location destriptor. Suggested content: AdaCL.Trace.Entity
   --  Source    : Location destriptor. Suggested content: AdaCL.Trace.Source
   --
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
   function C_Enable (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
   begin
      Cl.Set_On (Is_On_Off (Arguments));

      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end C_Enable;

   ---------------------------------------------------------------------------
   --
   --  Set Filename for Trace File
   --
   function C_File (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
      Buffer : constant Wide_String :=
         Strings.To_Ada
           (Arguments   => Arguments,
            Keep_Null   => False,
            To_Upper    => False,
            Trim_Spaces => True);
   begin
      Cl.Set_Filename (Ada.Characters.Conversions.To_String (Buffer));
      Cl.Set_Trace_Location (File);
      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end C_File;

   ---------------------------------------------------------------------------
   --
   --  Write Help for Commandline Options parsed from Trace
   --
   function C_Help (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
      pragma Unreferenced (Arguments);
   begin
      CrLf;
      CrLf;
      Q_Put_String (Win32.WCHAR_Array'("Trace Commands:"));
      CrLf;
      CrLf;
      Q_Put_String ("   " & X_Verbose & "         (TRUE|FALSE)   enable verbose operation.");
      CrLf;
      Q_Put_String ("   " & X_Enable & "          (TRUE|FALSE)   activate trace.");
      CrLf;
      Q_Put_String ("   " & X_Write_Prefix & "     (TRUE|FALSE)   enable trace prefix.");
      CrLf;
      Q_Put_String
        ("   " & X_Write_Line_Number & " (TRUE|FALSE)   enable trace line numbers.");
      CrLf;
      Q_Put_String
        ("   " & X_To & "              (CONSOLE|FILE) set trace to file or console.");
      CrLf;
      Q_Put_String ("   " & X_File & "            (Filename)     set trace file name.");
      CrLf;
      CrLf;
      Q_Put_String (Win32.WCHAR_Array'("Trace internal Variables:"));
      CrLf;
      CrLf;
      Q_Put_String ("  _" & X_Verbose & "         (TRUE|FALSE)   verbose operation?");
      CrLf;
      Q_Put_String ("  _" & X_Enable & "          (TRUE|FALSE)   trace activated?");
      CrLf;
      Q_Put_String ("  _" & X_Write_Prefix & "     (TRUE|FALSE)   trace prefix enabled?");
      CrLf;
      Q_Put_String
        ("  _" & X_Write_Line_Number & " (TRUE|FALSE)   trace line numbers enabled?");
      CrLf;
      Q_Put_String ("  _" & X_To & "              (CONSOLE|FILE) trace to file or console?");
      CrLf;
      Q_Put_String ("  _" & X_File & "            (Filename)     current trace file name?");
      CrLf;
      CrLf;
      Q_Put_String (Win32.WCHAR_Array'("When the plugin is loaded the following"));
      CrLf;
      Q_Put_String (Win32.WCHAR_Array'("environment variables are read for inital setup:"));
      CrLf;
      CrLf;
      Q_Put_String ("  %" & X_Verbose & "         (TRUE|FALSE)   enable verbose operation.");
      CrLf;
      Q_Put_String ("  %" & X_Enable & "          (TRUE|FALSE)   activate trace.");
      CrLf;
      Q_Put_String ("  %" & X_Write_Prefix & "     (TRUE|FALSE)   enable trace prefix.");
      CrLf;
      Q_Put_String
        ("  %" & X_Write_Line_Number & " (TRUE|FALSE)   enable trace line numbers.");
      CrLf;
      Q_Put_String
        ("  %" & X_To & "              (CONSOLE|FILE) set trace to file or console.");
      CrLf;
      Q_Put_String ("  %" & X_File & "            (Filename)     set trace file name.");
      CrLf;
      CrLf;
      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end C_Help;

   ---------------------------------------------------------------------------
   --
   --  Set Trace Destination
   --
   function C_To (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
      Buffer : constant Wide_String :=
         Strings.To_Ada
           (Arguments   => Arguments,
            Keep_Null   => False,
            To_Upper    => True,
            Trim_Spaces => True);
   begin
      if Buffer = Trace_Opt_To_Err1 or else Buffer = Trace_Opt_To_Err2 then
         Cl.Set_Trace_Location (Standard_Error);
      elsif Buffer = Trace_Opt_To_Std1 or else Buffer = Trace_Opt_To_Std2 then
         Cl.Set_Trace_Location (Standard_Output);
      elsif Buffer = Trace_Opt_To_File then
         Cl.Set_Trace_Location (File);
      elsif Buffer = Trace_Opt_To_Console then
         Cl.Set_Trace_Location (Console);
      else
         Cl.Set_Trace_Location (Destination'Wide_Value (Buffer));
      end if;

      return 0;
   end C_To;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   function C_Verbose (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
   begin
      Cl.Set_Verbose (Is_On_Off (Arguments));

      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end C_Verbose;

   ---------------------------------------------------------------------------
   --
   --  Write an Wide_String using writeFormattedString after adding the appropriate padding for
   --  indentation.
   --
   function C_Write (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
   begin
      if Cl.Get_On then
         Write (Arguments);
      end if;

      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end C_Write;

   ---------------------------------------------------------------------------
   --
   --  Don't Write Line numbers
   --
   function C_Write_Line_Number (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
   begin
      Cl.Set_Write_Line_Number (Is_On_Off (Arguments));

      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end C_Write_Line_Number;

   ---------------------------------------------------------------------------
   --
   --  Disable the Write prefix
   --
   function C_Write_Prefix (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
   begin
      Cl.Set_Write_Prefix (Is_On_Off (Arguments));

      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end C_Write_Prefix;

   ---------------------------------------------------------------------------
   --
   --  Trace end of function
   --
   --  This : Object itself.
   --
   procedure Finalize (This : in out Object) is
   begin
      if This.Index = 2 and then Cl.Get_On then
         Cl.Write_Formatted_String
           (Text   => Ada.Characters.Conversions.To_Wide_String (This.Trace_Name),
            Marker => Marker_Outdent);
      end if;

      return;
   end Finalize;

   ---------------------------------------------------------------------------
   --
   --  Functrace is not quite as usefull as the C++ version. The reason are the missing
   --  constructors and destructors in Ada. With Controlled types you can't limit to just one
   --  call to Initialize and one to Finalize There are allways some extra Adjust with matching.
   --  Finalize.
   --
   --  Name : Name of the function calls to be traced.
   --
   function Function_Trace (Name : String) return Object is
      Retval : constant Object (Name'Length) :=
        (Inherited.Controlled with
         Name_Length => Name'Length,
         Trace_Name  => Name,
         Index       => 0);
   begin
      --
      --  The Initialize method is not realy a replacement for a proper contructor.
      --
      if Cl.Get_On then
         Cl.Write_Formatted_String
           (Text   => Ada.Characters.Conversions.To_Wide_String (Retval.Trace_Name),
            Marker => Marker_Indent);
      end if;

      return Retval;
   end Function_Trace;

   ------------------------------------------------------------------------
   --
   --  Determine the thread number of the current thread
   --
   function Get_Thread_Number return Natural is
      Thread : Thread_ID;
   begin
      Cl.Get_Thread_ID (Thread);
      return Thread.Thread_No;
   end Get_Thread_Number;

   ---------------------------------------------------------------------------
   --
   --  Initialize_Plugin: Read initial setup from environment
   --
   procedure Initialize_Plugin is
   begin
      Cl.Initialize_Plugin;

      TakeCmd.Trace.Write (Wide_String'("Trace: initialized OK!"));

      return;
   end Initialize_Plugin;

   ---------------------------------------------------------------------------
   --
   --  Check if parameter is on of off
   --
   function Is_On_Off (Arguments : in Win32.PCWSTR) return Boolean is
      Buffer : constant Wide_String :=
         Strings.To_Ada
           (Arguments   => Arguments,
            Keep_Null   => False,
            To_Upper    => True,
            Trim_Spaces => True);
      Result : Boolean;
   begin
      if Buffer = Trace_Opt_On then
         Result := True;
      elsif Buffer = Trace_Opt_Off then
         Result := False;
      else
         Result := Boolean'Wide_Value (Buffer);
      end if;

      return Result;
   end Is_On_Off;

   function Is_On_Off (Value : in Boolean) return Plugin.Buffer is
   begin
      return Arguments : Plugin.Buffer do
         Arguments := Strings.To_Win (Boolean'Wide_Image (Value));
      end return;
   end Is_On_Off;

   ---------------------------------------------------------------------------
   --
   --  Trace the given exeption details and then raise the exception.
   --
   --  Raising : Exeption which is raised Message : Free form Message Entity : Location
   --  destriptor. Suggested content: AdaCL.Trace.Entity Source : Location destriptor. Suggested
   --  content: AdaCL.Trace.Source
   --
   procedure Raise_Exception
     (Raising : in Ada.Exceptions.Exception_Id;
      Message : in String := "No Message given";
      Entity  : in String := "No Entity given.";
      Source  : in String := "No Source given.")
   is
      use Ada.Exceptions;
   begin
      Write ("Raise Exception " & Wide_Exception_Name (Raising));
      Write ("   with Message " & Message);
      Write ("   for Entity   " & Entity);
      Write ("   in Source    " & Source);

      Raise_Exception
        (E       => Raising,
         Message => Message & " Entity: " & Entity & "." & " Source: " & Source & ".");
      --
      --  If Raising is null-exception then we could reach here. But we schould not raise the
      --  null-exception.
      --
      raise Program_Error;
   end Raise_Exception;

   ---------------------------------------------------------------------------
   --
   --  Shutdown Plugin: close trace file - of open
   --
   procedure Shutdown_Plugin is
   begin
      Cl.Shutdown_Plugin;

      return;
   end Shutdown_Plugin;

   ---------------------------------------------------------------------------
   --
   --  check is trace is Enabled
   --
   function V_Enable (Arguments : access Plugin.Buffer) return Interfaces.C.int is
   begin
      Arguments.all := Is_On_Off (Cl.Get_On);
      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end V_Enable;

   ---------------------------------------------------------------------------
   --
   --  Get Filename for Trace File
   --
   function V_File (Arguments : access Plugin.Buffer) return Interfaces.C.int is
   begin
      Arguments.all := Strings.To_Win (Cl.Get_Filename);
      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end V_File;

   ---------------------------------------------------------------------------
   --
   --  Check the Trace Destination
   --
   function V_To (Arguments : access Plugin.Buffer) return Interfaces.C.int is
   begin
      Arguments.all := Strings.To_Win (Destination'Wide_Image (Cl.Get_Trace_Location));

      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end V_To;

   ---------------------------------------------------------------------------
   --
   --  check is trace is Enabled
   --
   function V_Verbose (Arguments : access Plugin.Buffer) return Interfaces.C.int is
   begin
      Arguments.all := Is_On_Off (Cl.Get_Verbose);

      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end V_Verbose;

   ---------------------------------------------------------------------------
   --
   --  check if Line numbers are written
   --
   function V_Write_Line_Number (Arguments : access Plugin.Buffer) return Interfaces.C.int is
   begin
      Arguments.all := Is_On_Off (Cl.Get_Write_Line_Number);

      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end V_Write_Line_Number;

   ---------------------------------------------------------------------------
   --
   --  Check the Write prefix flag
   --
   function V_Write_Prefix (Arguments : access Plugin.Buffer) return Interfaces.C.int is
   begin
      Arguments.all := Is_On_Off (Cl.Get_Write_Prefix);

      return 0;
   exception
      when An_Exception : others =>
         Trace.Write_Error (An_Exception);
         return -2;
   end V_Write_Prefix;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the appropriate padding for
   --  indentation.
   --
   --  A_String : String to be written
   --
   procedure Write (A_String : in Wide_String) is
   begin
      if Cl.Get_On then
         Cl.Write_Formatted_String (Text => A_String, Marker => Marker_Std);
      end if;
   end Write;

   procedure Write (A_String : in String) is
   begin
      if Cl.Get_On then
         Write (Ada.Characters.Conversions.To_Wide_String (A_String));
      end if;
   end Write;

   procedure Write (A_String : in Win32.PCWSTR) is
   begin
      if Cl.Get_On then
         Write
           (Strings.To_Ada
               (Arguments   => A_String,
                Keep_Null   => False,
                To_Upper    => False,
                Trim_Spaces => False));
      end if;

      return;
   end Write;

   procedure Write (A_String : in Win32.WCHAR_Array) is
   begin
      if Cl.Get_On then
         Write
           (Strings.To_Ada
               (Arguments   => A_String,
                Keep_Null   => False,
                To_Upper    => False,
                Trim_Spaces => False));
      end if;

      return;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an Address.
   --
   --  A_String : String to be written
   --
   procedure Write (A_String : in Wide_String; An_Address : in System.Address) is
   begin
      if Cl.Get_On then
         Write_Address : declare
            Address_Text : Wide_String (1 .. 3 + 8 + 1);
         begin
            Address_IO.Put
              (To   => Address_Text,
               Item => System.Storage_Elements.To_Integer (An_Address),
               Base => 16);

            Cl.Write_Formatted_String (Text => A_String & Address_Text, Marker => Marker_Std);
         end Write_Address;
      end if;

      return;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   --
   procedure Write (An_Exception : in Ada.Exceptions.Exception_Occurrence) is
      use Ada.Exceptions;
   begin
      if Cl.Get_On then
         Cl.Write_Formatted_String
           (Text   =>
               Ada.Characters.Conversions.To_Wide_String
                 (Exception_Information (An_Exception)),
            Marker => Marker_Special);
         --  Cl.Write_Formatted_String (
         --  Text   => G_TB.Symbolic_Traceback (An_Exception),
         --  Marker => Marker_Special);
      end if;

      return;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   --  An_Entity    : Procedure in which the exception was caught
   --  A_Source     : Source File in which Entity is located.
   --
   procedure Write
     (An_Exception : in Ada.Exceptions.Exception_Occurrence;
      An_Entity    : in String;
      A_Source     : in String)
   is
      use Ada.Exceptions;
   begin
      if Cl.Get_On then
         Cl.Write_Formatted_String
           (Text   =>
               Ada.Characters.Conversions.To_Wide_String
                 (Exception_Information (An_Exception)),
            Marker => Marker_Special);
         Cl.Write_Formatted_String
           (Text   => "Function: " & Ada.Characters.Conversions.To_Wide_String (An_Entity),
            Marker => Marker_Special);
         Cl.Write_Formatted_String
           (Text   => "Source: " & Ada.Characters.Conversions.To_Wide_String (A_Source),
            Marker => Marker_Special);
      end if;

      return;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Create a memory dump
   --
   procedure Write_Dump
     (Address       : in System.Address;
      Element_Count : in System.Storage_Elements.Storage_Count)
   is
      use System.Storage_Elements;
   begin
      if Cl.Get_On then
         Write ("Lenght          :" & Storage_Count'Image (Element_Count));
         Write ("Address         : ", Address);

         Dump : declare
            package Byte_IO is new Ada.Wide_Text_IO.Modular_IO (Num => Storage_Element);

            Data : Storage_Array (0 .. Element_Count - 1);
            for Data'Address use Address;
            pragma Import (Ada, Data);

            Line_Len     : constant := 16;
            Address_Len  : constant := 8;
            Byte_Len     : constant := 2;
            Byte_Offset  : constant := 18;   --  Dump  [01234567]
            ASCII_Offset : constant := Byte_Offset + Line_Len * (Byte_Len + 1) + 1;
            UTF_Offset   : constant := ASCII_Offset + Line_Len + 1;
            Text_Len     : constant := UTF_Offset + Line_Len / 2;

            Byte_Text    : Wide_String (1 .. 3 + Byte_Len + 1);
            Address_Text : Wide_String (1 .. 3 + Address_Len + 1);
            Text         : Wide_String (1 .. Text_Len);
            Line         : Storage_Offset := Data'First;
            Col          : Storage_Offset := Data'First;
            Char         : Wide_Character;
            Byte_Col     : Integer;
         begin
            Dump_Line : while Line <= Data'Last loop
               Address_IO.Put
                 (To   => Address_Text,
                  Item => To_Integer (Address + Line),
                  Base => 16);

               if Address_Text (4) = '#' then
                  Address_Text (4) := '0';
               end if;

               Ada.Strings.Wide_Fixed.Move
                 (Source => "Dump  [" & Address_Text (4 .. 11) & "]: ",
                  Target => Text);

               Col      := 0;
               Byte_Col := Byte_Offset;

               Dump_Column : while Col < Line_Len and then Col + Line < Element_Count loop
                  Byte_IO.Put (To => Byte_Text, Item => Data (Line + Col), Base => 16);

                  if Byte_Text (4) = '#' then
                     Byte_Text (4) := '0';
                  end if;

                  Text (Byte_Col .. Byte_Col + 1) := Byte_Text (4 .. 5);

                  Char := Wide_Character'Val (Data (Line + Col));

                  if Ada.Wide_Characters.Unicode.Is_Non_Graphic (Char) then
                     Text (Natural (ASCII_Offset + Col))  := '.';
                  else
                     Text (Natural (ASCII_Offset + Col))  := Char;
                  end if;

                  if Col mod 2 /= 0 then
                     Char :=
                        Wide_Character'Val
                          (Integer (Data (Line + Col - 1)) +
                           256 * Integer (Data (Line + Col)));
                     if Ada.Wide_Characters.Unicode.Is_Non_Graphic (Char) then
                        Text (Natural (UTF_Offset + Col / 2))  := Char;
                     else
                        Text (Natural (UTF_Offset + Col / 2))  := Char;
                     end if;
                  end if;

                  Col      := Col + 1;
                  Byte_Col := Byte_Col + (Byte_Len + 1);
               end loop Dump_Column;

               Cl.Write_Formatted_String (Text => Text, Marker => Marker_Std);
               Line := Line + Line_Len;
            end loop Dump_Line;
         end Dump;
      end if;

      return;
   end Write_Dump;

   ---------------------------------------------------------------------------
   --
   --  Create a memory dump. This Dump takes size in bits.
   --
   --  An_Address :  String to be written
   --  A_Size     :  Size in Storage_Elements.
   --
   procedure Write_Dump (Address : in System.Address; Size : in Integer) is
      use System.Storage_Elements;

      Element_Count : Storage_Count := Storage_Count (Size / System.Storage_Unit);
   begin
      if Cl.Get_On then
         Write ("Size            :" & Integer'Image (Size));

         if (Size mod System.Storage_Unit) /= 0 then
            Element_Count := Element_Count + 1;
         end if;

         Write_Dump (Address, Element_Count);
      end if;
      return;
   end Write_Dump;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the appropriate padding for
   --  indentation.
   --
   --  A_String : String to be written
   --
   procedure Write_Error (A_String : in Wide_String) is
   begin
      if Cl.Get_On then
         Cl.Write_Formatted_String (Text => A_String, Marker => Marker_Std);
      end if;

      if not Cl.Get_On or else Cl.Get_Trace_Location /= Console then
         Q_Put_String (A_String);
      end if;

      return;
   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   --
   procedure Write_Error (An_Exception : in Ada.Exceptions.Exception_Occurrence) is
      use Ada.Exceptions;
   begin
      Write (An_Exception);

      if (not Cl.Get_On or else Cl.Get_Trace_Location /= Console)
        and then Get_Thread_Number = 0
      then
         Q_Put_String (Ada.Exceptions.Exception_Information (An_Exception));
         CrLf;
      end if;

      return;
   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception :  String to be written
   --  An_Entity    :  Procedure in which the exception was caught
   --  A_Source     :  Source File in which Entity is located.
   --
   procedure Write_Error
     (An_Exception : in Ada.Exceptions.Exception_Occurrence;
      An_Entity    : in String;
      A_Source     : in String)
   is
      use Ada.Exceptions;
   begin
      Write (An_Exception => An_Exception, An_Entity => An_Entity, A_Source => A_Source);

      if (not Cl.Get_On or else Cl.Get_Trace_Location /= Console)
        and then Get_Thread_Number = 0
      then
         CrLf;
         Q_Put_String
           (Ada.Characters.Conversions.To_Wide_String (Exception_Information (An_Exception)));
         CrLf;
         Q_Put_String ("Function: " & Ada.Characters.Conversions.To_Wide_String (Entity));
         CrLf;
         Q_Put_String ("Source: " & Ada.Characters.Conversions.To_Wide_String (Source));
      end if;

      return;
   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  When verbose is aktivated then an empty line is written to Standart_Output
   --
   procedure Write_Info is
   begin
      if Cl.Get_Verbose and then Get_Thread_Number = 0 then
         CrLf;
      end if;

      return;
   end Write_Info;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the appropriate padding for
   --  indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output as well.
   --
   --  A_String : String to be written
   --
   procedure Write_Info (A_String : in Wide_String) is
   begin
      if Cl.Get_On then
         Cl.Write_Formatted_String (Text => A_String, Marker => Marker_Std);
      end if;

      if Cl.Get_Verbose
        and then (not Cl.Get_On or else Cl.Get_Trace_Location /= Console)
        and then Get_Thread_Number = 0
      then
         Q_Put_String (A_String);
         CrLf;
      end if;

      return;
   end Write_Info;

end TakeCmd.Trace;

----------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
