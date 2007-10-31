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

with System;
with System.Storage_Elements;

with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Finalization;

with GNAT.Source_Info;

package TakeCmd.Trace is

   ---------------------------------------------------------------------------
   --  Name_Length : Lenght of trace String
   type Object (Name_Length : Positive) is new Ada.Finalization.Controlled
   with private;

   ---------------------------------------------------------------------------
   --
   --  Trace Destination
   --
   type Destination is (Queue, Standard_Error, Standard_Output, File);

   ---------------------------------------------------------------------------
   --
   --  Renamed to keep dependencies to GNAT specific libraries to a minimum.
   --
   --  Return the name of the current subprogram, package, task, entry or
   --  protected subprogram. The string is in exactly the form used for the
   --  declaration of the entity (casing and encoding conventions), and is
   --  considered to be a static string constant.
   --
   --  Note: if this function is used at the outer level of a generic package,
   --  the string returned will be the name of the instance, not the generic
   --  package itself. This is useful in identifying and logging information
   --  from within generic templates.
   --
   function Entity return String renames GNAT.Source_Info.Enclosing_Entity;

   ---------------------------------------------------------------------------
   --
   --  Renamed to keep dependencies to GNAT specific libraries to a minimum.
   --
   --  Return a string literal of the form "name:line", where name is the
   --  current source file name without path information, and line is the
   --  current line number. In the event that instantiations are involved,
   --  additional suffixes of the same form are appended after the separating
   --  string " instantiated at ". The result is considered to be a static
   --  string constant.
   function Source return String renames GNAT.Source_Info.Source_Location;

   ---------------------------------------------------------------------------
   --
   --  Functrace is not quite as usefull as the C++ version. The reason are
   --  the missing constructors and destructors in Ada. With Controlled types
   --  you can't limit to just one call to Initialize and one to Finalize
   --  There are allways some extra Adjust with matching. Finalize.
   --
   --  Name of the function calls to be traced.
   function Function_Trace (Name : in String) return Object;

   ---------------------------------------------------------------------------
   --
   --  Trace the given exeption details and then raise the exception.
   --
   --  Raising : Exeption which is raised Message : Free form Message Entity :
   --  Location destriptor. Suggested content: AdaCL.Trace.Entity Source :
   --  Location destriptor. Suggested content: AdaCL.Trace.Source
   --
   procedure Raise_Exception
     (Raising : in Ada.Exceptions.Exception_Id;
      Message : in String := "No Message given";
      Entity  : in String := "No Entity given.";
      Source  : in String := "No Source given.");

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
      Source    : in String := "No Source given.");

   ---------------------------------------------------------------------------
   --
   --  Write Line numbers
   --
   procedure Enable_Write_Line_Number;

   ---------------------------------------------------------------------------
   --
   --  Don't Write Line numbers
   --
   procedure Disable_Write_Line_Number;

   ---------------------------------------------------------------------------
   --
   --  check if Line numbers are written
   --
   function Is_Write_Line_Number_Enabled return Boolean;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Enable_Trace;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Disable_Trace;

   ---------------------------------------------------------------------------
   --
   --  check is trace is Enabled
   --
   function Is_Trace_Enabled return Boolean;

   ---------------------------------------------------------------------------
   --
   --  Enable Verbose Output
   --
   procedure Enable_Verbose;

   ---------------------------------------------------------------------------
   --
   --  Disable Verbose Output
   --
   procedure Disable_Verbose;

   ---------------------------------------------------------------------------
   --
   --  check is trace is Enabled
   --
   function Is_Verbose_Enabled return Boolean;

   ---------------------------------------------------------------------------
   --
   --  Write to queue - not supported yet.
   --
   procedure Write_To_Queue;

   ---------------------------------------------------------------------------
   --
   --  Write to Standart Error
   --
   procedure Write_To_Standard_Error;

   ---------------------------------------------------------------------------
   --
   --  Write to Standart Error
   --
   procedure Write_To_Standard_Output;

   ---------------------------------------------------------------------------
   --
   --  Write to queue - not supported yet.
   --
   procedure Write_To_File;

   ---------------------------------------------------------------------------
   --
   --  Set Filename for Trace File
   --
   procedure Write_To_File (New_Filename : in String);

   ---------------------------------------------------------------------------
   --
   --  Check the Trace Destination
   --
   function Trace_Destination return Destination;

   ---------------------------------------------------------------------------
   --
   --  Enable the write prefix
   --
   procedure Enable_Write_Prefix;

   ---------------------------------------------------------------------------
   --
   --  Disable_ the write prefix
   --
   procedure Disable_Write_Prefix;

   ---------------------------------------------------------------------------
   --
   --  Check the write prefix flag
   --
   function Is_Write_Prefix_Enabled return Boolean;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   procedure Write (A_String : in String);

   ---------------------------------------------------------------------------
   --
   --  Write an Address.
   --
   --  A_String : String to be written
   procedure Write (A_String : in String; An_Address : in System.Address);

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_Unbounded : String to be written
   procedure Write
     (A_Unbounded : in Ada.Strings.Unbounded.Unbounded_String);

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
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   procedure Write_Wide (A_String : in Wide_String);

   ---------------------------------------------------------------------------
   --
   --  Create a memory dump, S
   --
   --  An_Address :  String to be written
   --  A_Size     :  Size in Storage_Elements.
   procedure Write_Dump
     (An_Address : in System.Address;
      A_Size     : in System.Storage_Elements.Storage_Count);

   ---------------------------------------------------------------------------
   --
   --  Create a memory dump. This Dump takes size in bits.
   --
   --  An_Address : String to be written
   --  A_Size     : Size in Bits - i.E. for 'Size.
   procedure Write_Dump
     (An_Address : in System.Address;
      A_Size     : in Integer);

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   procedure Write_Error (A_String : in String);

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_Unbounded : String to be written
   procedure Write_Error
     (A_Unbounded : in Ada.Strings.Unbounded.Unbounded_String);

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   procedure Write_Error
     (An_Exception : in Ada.Exceptions.Exception_Occurrence);

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
      A_Source     : in String);

   ---------------------------------------------------------------------------
   --
   --  When verbose is aktivated then an empty line is written to
   --  Standart_Output
   --
   procedure Write_Info;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output
   --  as well.
   --
   --  A_String : String to be written
   procedure Write_Info (A_String : in String);

   ---------------------------------------------------------------------------
   --
   --  When verbose is aktivated then the character is written to
   --  Standart_Output.
   --
   --  A_Character : String to be written
   procedure Write_Info (A_Character : in Character);

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output
   --  as well.
   --
   --  A_Unbounded : String to be written
   procedure Write_Info
     (A_Unbounded : in Ada.Strings.Unbounded.Unbounded_String);

   ---------------------------------------------------------------------------
   --
   --  Write Help for Commandline Options parsed from Trace
   --
   procedure Write_Commandline_Help;

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
   type Object (Name_Length : Positive) is new Inherited.Controlled with
      record
         Trace_Name : String (1 .. Name_Length);
      end record;

   ---------------------------------------------------------------------------
   --
   --  Instanz Access. Please Note that controlled types are never atomic
   --  since finalisation use a double linked list.
   --
   type Object_Class is access Object'Class;

   ---------------------------------------------------------------------------
   --
   --  Trace Copy.
   --
   --  This : Object itself.
   procedure Adjust (This : in out Object);

   ---------------------------------------------------------------------------
   --
   --  Trace end of function
   --
   --  This : Object itself.
   procedure Finalize (This : in out Object);

   pragma Inline (Disable_Verbose);
   pragma Inline (Enable_Verbose);
   pragma Inline (Disable_Trace);
   pragma Inline (Enable_Trace);
   pragma Inline (Is_Write_Line_Number_Enabled);
   pragma Inline (Disable_Write_Line_Number);
   pragma Inline (Enable_Write_Line_Number);
   pragma Inline (Assert);
   pragma Inline (Function_Trace);
   pragma Inline (Is_Trace_Enabled);
   pragma Inline (Is_Verbose_Enabled);
   pragma Inline (Write_To_Queue);
   pragma Inline (Write_To_Standard_Error);
   pragma Inline (Write_To_Standard_Output);
   pragma Inline (Write_To_File);
   pragma Inline (Trace_Destination);
   pragma Inline (Enable_Write_Prefix);
   pragma Inline (Disable_Write_Prefix);
   pragma Inline (Is_Write_Prefix_Enabled);

   pragma No_Return (Raise_Exception);

   for Destination'Size use 8;

end TakeCmd.Trace;
