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
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:

pragma License (Gpl);
pragma Ada_05;

with System;
with System.Storage_Elements;

with Ada.Exceptions;

with TakeCmd.Plugin;

with GNAT.Source_Info;

private with Ada.Finalization;

package TakeCmd.Trace is
   ---------------------------------------------------------------------------
   --
   --  Name_Length : Lenght of trace String
   --
   type Object (<>) is tagged limited private;

   ---------------------------------------------------------------------------
   --
   --  Trace Destination
   --
   type Destination is (Console, Standard_Error, Standard_Output, File);

   for Destination'Size use 8;

   ---------------------------------------------------------------------------
   --
   --  Renamed to keep dependencies to GNAT specific libraries to a minimum.
   --
   --  Return the name of the current subprogram, package, task, entry or protected
   --  subprogram. The string is in exactly the form used for the declaration of the entity
   --  (casing and encoding conventions), and is considered to be a static string constant.
   --
   --  Note: if this function is used at the outer level of a generic package, the string
   --  returned will be the name of the instance, not the generic package itself. This is
   --  useful in identifying and logging information from within generic templates.
   --
   function Entity return String renames GNAT.Source_Info.Enclosing_Entity;

   ---------------------------------------------------------------------------
   --
   --  Renamed to keep dependencies to GNAT specific libraries to a minimum.
   --
   --  Return a string literal of the form "name:line", where name is the current source file
   --  name without path information, and line is the current line number. In the event that
   --  instantiations are involved, additional suffixes of the same form are appended after
   --  the separating string " instantiated at ". The result is considered to be a static
   --  string constant.
   --
   function Source return String renames GNAT.Source_Info.Source_Location;

   ---------------------------------------------------------------------------
   --
   --  Functrace is not quite as usefull as the C++ version. The reason are the missing
   --  constructors and destructors in Ada. With Controlled types you can't limit to just one
   --  call to Initialize and one to Finalize There are allways some extra Adjust with
   --  matching. Finalize.
   --
   --  Name of the function calls to be traced.
   --
   function Function_Trace (Name : in String) return Object;

   pragma Inline (Function_Trace);

   ---------------------------------------------------------------------------
   --
   --  Trace the given exeption details and then raise the exception.
   --
   --  Raising : Exeption which is raised Message : Free form Message Entity : Location
   --  destriptor. Suggested content: AdaCL.Trace.Entity Source : Location destriptor.
   --  Suggested content: AdaCL.Trace.Source
   --
   procedure Raise_Exception
     (Raising : in Ada.Exceptions.Exception_Id;
      Message : in String := "No Message given";
      Entity  : in String := "No Entity given.";
      Source  : in String := "No Source given.");

   pragma No_Return (Raise_Exception);

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
      Source    : in String := "No Source given.");

   pragma Inline (Assert);

   ---------------------------------------------------------------------------
   --
   --  Initialize_Plugin: Read initial setup from environment
   --
   procedure Initialize_Plugin;

   ---------------------------------------------------------------------------
   --
   --  Shutdown Plugin: close trace file - of open
   --
   procedure Shutdown_Plugin;

   ---------------------------------------------------------------------------
   --
   --  Enable or Disable Write Line numbers
   --
   function C_Write_Line_Number (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   X_Write_Line_Number : aliased constant Win32.WCHAR_Array := "TRACEWRITELINENUMBER";

   pragma Export
     (Convention => Stdcall,
      Entity => C_Write_Line_Number,
      External_Name => "TRACEWRITELINENUMBER");

   ---------------------------------------------------------------------------
   --
   --  check if Line numbers are written
   --
   function V_Write_Line_Number
     (Arguments : access TakeCmd.Plugin.Buffer)
      return      Interfaces.C.int;

   pragma Export
     (Convention => Stdcall,
      Entity => V_Write_Line_Number,
      External_Name => "_TRACEWRITELINENUMBER");

   ---------------------------------------------------------------------------
   --
   --  Enable or Disable Trace
   --
   function C_Enable (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   X_Enable : aliased constant Win32.WCHAR_Array := "TRACEENABLE";

   pragma Export (Convention => Stdcall, Entity => C_Enable, External_Name => "TRACEENABLE");

   ---------------------------------------------------------------------------
   --
   --  check is trace is Enabled
   --
   function V_Enable (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int;
   pragma Export
     (Convention => Stdcall,
      Entity => V_Enable,
      External_Name => "_TRACEENABLE");

   ---------------------------------------------------------------------------
   --
   --  Enable or Disable Verbose Output
   --
   function C_Verbose (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   X_Verbose : aliased constant Win32.WCHAR_Array := "TRACEVERBOSE";

   pragma Export
     (Convention => Stdcall,
      Entity => C_Verbose,
      External_Name => "TRACEVERBOSE");

   ---------------------------------------------------------------------------
   --
   --  check if Verbose Output is enabled
   --
   function V_Verbose (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int;

   pragma Export
     (Convention => Stdcall,
      Entity => V_Verbose,
      External_Name => "_TRACEVERBOSE");

   ---------------------------------------------------------------------------
   --
   --  Set Trace Destination
   --
   function C_To (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   X_To : aliased constant Win32.WCHAR_Array := "TRACETO";

   pragma Export (Convention => Stdcall, Entity => C_To, External_Name => "TRACETO");

   ---------------------------------------------------------------------------
   --
   --  Check the Trace Destination
   --
   function V_To (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int;

   pragma Export (Convention => Stdcall, Entity => V_To, External_Name => "_TRACETO");

   ---------------------------------------------------------------------------
   --
   --  Set Filename for Trace File
   --
   function C_File (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   X_File : aliased constant Win32.WCHAR_Array := "TRACEFILE";

   pragma Export (Convention => Stdcall, Entity => C_File, External_Name => "TRACEFILE");

   ---------------------------------------------------------------------------
   --
   --  Get Filename for Trace File
   --
   function V_File (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int;

   pragma Export (Convention => Stdcall, Entity => V_File, External_Name => "_TRACEFILE");

   ---------------------------------------------------------------------------
   --
   --  Enable the write prefix
   --
   function C_Write_Prefix (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   X_Write_Prefix : aliased constant Win32.WCHAR_Array := "TRACEWRITEPREFIX";

   pragma Export
     (Convention => Stdcall,
      Entity => C_Write_Prefix,
      External_Name => "TRACEWRITEPREFIX");

   ---------------------------------------------------------------------------
   --
   --  Check the write prefix flag
   --
   function V_Write_Prefix
     (Arguments : access TakeCmd.Plugin.Buffer)
      return      Interfaces.C.int;

   pragma Export
     (Convention => Stdcall,
      Entity => V_Write_Prefix,
      External_Name => "_TRACEWRITEPREFIX");

   ---------------------------------------------------------------------------
   --
   --  Write an Wide_String using writeFormattedString after adding the appropriate padding
   --  for indentation.
   --
   --  A_String : String to be written
   --
   procedure Write (A_String : in Wide_String);
   procedure Write (A_String : in Win32.PCWSTR);

   pragma Inline (Write);

   procedure Write (A_String : in Win32.WCHAR_Array);

   pragma Inline (Write);

   procedure Write (A_String : in String);

   pragma Inline (Write);

   function C_Write (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   X_Write : aliased constant Win32.WCHAR_Array := "TRACEWRITE";

   pragma Export (Convention => Stdcall, Entity => C_Write, External_Name => "TRACEWRITE");

   ---------------------------------------------------------------------------
   --
   --  Write an Address.
   --
   --  A_String : String to be written
   --
   procedure Write (A_String : in Wide_String; An_Address : in System.Address);

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   procedure Write (An_Exception : in Ada.Exceptions.Exception_Occurrence);

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
      A_Source     : in String);

   ---------------------------------------------------------------------------
   --
   --  Create a memory dump, S
   --
   --  An_Address :  String to be written
   --  A_Size     :  Size in Storage_Elements.
   --
   procedure Write_Dump
     (Address       : in System.Address;
      Element_Count : in System.Storage_Elements.Storage_Count);

   ---------------------------------------------------------------------------
   --
   --  Create a memory dump. This Dump takes size in bits.
   --
   --  An_Address : String to be written
   --  A_Size     : Size in Bits - i.E. for 'Size.
   --
   procedure Write_Dump (Address : in System.Address; Size : in Integer);

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the appropriate padding for
   --  indentation.
   --
   --  A_String : String to be written
   --
   procedure Write_Error (A_String : in Wide_String);

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   --
   procedure Write_Error (An_Exception : in Ada.Exceptions.Exception_Occurrence);

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
      A_Source     : in String);

   ---------------------------------------------------------------------------
   --
   --  When verbose is aktivated then an empty line is written to Standart_Output
   --
   procedure Write_Info;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the appropriate padding for
   --  indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output as well.
   --
   --  A_String : String to be written
   --
   procedure Write_Info (A_String : in Wide_String);

   ---------------------------------------------------------------------------
   --
   --  Write Help for Commandline Options parsed from Trace
   --
   function C_Help (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   X_Help : aliased constant Win32.WCHAR_Array := "TRACEHELP";

   pragma Export (Convention => Stdcall, Entity => C_Help, External_Name => "TRACEHELP");

private

   package Inherited renames Ada.Finalization;

   ---------------------------------------------------------------------------
   --
   --  Ada Class Library
   --  Trace
   --
   --  Instanz Data
   --
   --  Name_Length : Lenght of trace String
   --
   type Object (Name_Length : Positive) is new Inherited.Limited_Controlled with record
      Trace_Name : String (1 .. Name_Length);
   end record;

   ---------------------------------------------------------------------------
   --
   --  Trace Copy.
   --
   --  This : Object itself.
   --
   --  procedure Adjust (This : in out Object);

   ---------------------------------------------------------------------------
   --
   --  Trace end of function
   --
   --  This : Object itself.
   --
   procedure Finalize (This : in out Object);

end TakeCmd.Trace;

----------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
