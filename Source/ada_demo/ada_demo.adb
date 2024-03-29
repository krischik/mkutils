-------------------------------------------------------------- {{{1 ----------
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
--         Help: http://www.jpsoftwiki.com/wiki/index.php?title=Ada_Demo_%28plugin%29
------------------------------------------------------------------------------
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
-------------------------------------------------------------- }}}1 ----------

pragma License (Gpl);

with Ada.Calendar.Formatting;
with Ada.Calendar;
with Ada.Characters.Wide_Latin_1;
with Ada.Exceptions;
with Ada.Strings.Wide_Fixed;
with Ada.Unchecked_Deallocation;

with TakeCmd.Strings;
with TakeCmd.Trace;
with TakeCmd;

with Win32.Winbase;

pragma Elaborate_All (TakeCmd.Trace);

package body Ada_Demo is
   use type Win32.WCHAR_Array;
   use type Interfaces.C.int;

   DLL_Name       : aliased constant Win32.WCHAR_Array := "Ada_Demo" & Win32.Wide_Nul;
   Author         : aliased constant Win32.WCHAR_Array := "Martin Krischik" & Win32.Wide_Nul;
   Author_Email   : aliased constant Win32.WCHAR_Array :=
      "krischik@users.sourceforge.net" & Win32.Wide_Nul;
   Author_WebSite : aliased constant Win32.WCHAR_Array :=
      "http://mkutils.googlecode.com" & Win32.Wide_Nul;
   Description    : aliased constant Win32.WCHAR_Array :=
      "A demonstration Plugin for 4NT/TC, written with Ada." & Win32.Wide_Nul;
   Implements     : aliased constant Win32.WCHAR_Array :=
      "DIR" &
      "," &
      "REMARK" &
      "," &
      "TASKREMARK" &
      "," &
      "USEBUFFER" &
      "," &
      TakeCmd.Trace.X_Enable &
      "," &
      TakeCmd.Trace.X_Help &
      "," &
      TakeCmd.Trace.X_To &
      "," &
      TakeCmd.Trace.X_File &
      "," &
      TakeCmd.Trace.X_Verbose &
      "," &
      TakeCmd.Trace.X_Write_Line_Number &
      "," &
      TakeCmd.Trace.X_Write_Prefix &
      "," &
      TakeCmd.Trace.X_Write &
      ",*" &
      "KEY" &
      ",@" &
      "REVERSE" &
      ",_" &
      "HELLO" &
      ",_" &
      "TASKREMARK" &
      ",_" &
      TakeCmd.Trace.X_Enable &
      ",_" &
      TakeCmd.Trace.X_To &
      ",_" &
      TakeCmd.Trace.X_File &
      ",_" &
      TakeCmd.Trace.X_Verbose &
      ",_" &
      TakeCmd.Trace.X_Write_Line_Number &
      ",_" &
      TakeCmd.Trace.X_Write_Prefix &
      Win32.Wide_Nul;

   Plugin_Info : TakeCmd.Plugin.LP_Plugin_Info := null;
   My_Remark   : Remark_Access                 := null;

   ---------------------------------------------------------------------------
   --  You can use Ada tasking facilities inside a plug-in. Just be aware that library level
   --  task won't work and that most - if not all - command from the TakeCmd library can not be
   --  used.
   --
   task body Remark_Task is
   begin
      Remark_Value.Set_Remark ("The Task has started.");
      loop
         select
            accept Execute do
               TakeCmd.Trace.Write (Wide_String'("Accept Execute"));
            end Execute;
            TakeCmd.Trace.Write (Wide_String'("Start Execute"));
            Remark_Value.Set_Remark ("The Task has run.");
            TakeCmd.Trace.Write (Wide_String'("Done Execute"));
         or
            terminate;
         end select;
      end loop;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
   end Remark_Task;

   protected body Remark_Value is
      function Get_Remark return Wide_String is
      begin
         return Ada.Strings.Wide_Fixed.Trim (Remark, Ada.Strings.Both);
      end Get_Remark;

      procedure Set_Remark (New_Remark : in Wide_String) is
         Trace : TakeCmd.Trace.Object := TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
         pragma Unreferenced (Trace);
      begin
         Ada.Strings.Wide_Fixed.Move
           (Source  => New_Remark,
            Target  => Remark,
            Drop    => Ada.Strings.Right,
            Justify => Ada.Strings.Left,
            Pad     => Ada.Strings.Wide_Space);
         return;
      end Set_Remark;
   end Remark_Value;

   ---------------------------------------------------------------------------
   --  This function shows how you can modify the behaviour of a 4NT/TC command. If you use the
   --  DIR command this function will be called, and a check is made of the current time. If the
   --  value of the "Minutes" is even then a message will be displayed telling you that you
   --  can't use DIR at the moment. If the value is odd the "DID_NOT_PROCESS" value is returned
   --  and 4NT/TC will execute the DIR command as normal.
   --
   function C_Dir (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
      Trace : TakeCmd.Trace.Object := TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
      pragma Unreferenced (Trace);

      Minute : constant Ada.Calendar.Formatting.Minute_Number :=
         Ada.Calendar.Formatting.Minute (Ada.Calendar.Clock);
      Result : Interfaces.C.int                               :=
         TakeCmd.Plugin.Did_Not_Process;
   begin
      TakeCmd.Trace.Write (Arguments);

      if Minute mod 2 = 0 then
         TakeCmd.Q_Put_String
           (Win32.WCHAR_Array'("Sorry, the Minutes are even, " &
                               "so no DIR listing is available!"));
         TakeCmd.CrLf;
         Result := 0;
      end if;

      return Result;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return -2;
   end C_Dir;

   ---------------------------------------------------------------------------
   --
   --  This is an Internal Command called from 4NT/TC
   --
   function C_Remark (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
      Trace : TakeCmd.Trace.Object := TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
      pragma Unreferenced (Trace);
   begin
      TakeCmd.Trace.Write (Arguments);

      TakeCmd.Q_Put_String (Win32.WCHAR_Array'("What a trivial Ada generated Plugin!"));
      TakeCmd.CrLf;
      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return -2;
   end C_Remark;

   ---------------------------------------------------------------------------
   --
   --  This is an Internal Command called from 4NT/TC which is executed in another task.
   --
   function C_Task_Remark (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
      Trace : TakeCmd.Trace.Object := TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
      pragma Unreferenced (Trace);
   begin
      TakeCmd.Trace.Write (Arguments);

      My_Remark.all.Execute;
      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return -2;
   end C_Task_Remark;

   ---------------------------------------------------------------------------
   --  This function illustrates how to call TakeCmd.dll functions which require a buffer in
   --  which to manipulate the supplied string. A number is supplied as a parameter to this
   --  function and it uses "Addcommas" to insert the thousands separator into the number. If
   --  the supplied parameter is not a valid number it is left unchanged. For example:
   --
   --    c:\>usebuffer 123456789
   --    123,456,789
   --
   function C_Use_Buffer (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
      Trace : TakeCmd.Trace.Object := TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
      pragma Unreferenced (Trace);

      pragma Warnings (Off, "variable ""Dummy"" is never read and never assigned");

      Arguments_Length : constant Natural                                               :=
         Natural (Win32.Winbase.lstrlenW (Arguments));
      Buffer           : Wide_String (1 .. Arguments_Length + Arguments_Length / 3 + 1) :=
        (others => ' ');
      Dummy            : Win32.PWSTR;

      pragma Warnings (On, "variable ""Dummy"" is never read and never assigned");
   begin
      TakeCmd.Trace.Write (Arguments);

      TakeCmd.Strings.To_Ada
        (Arguments   => Arguments,
         Buffer      => Buffer,
         Keep_Null   => True,
         To_Upper    => False,
         Trim_Spaces => True);

      TakeCmd.AddCommas (Win32.Addr (Buffer));
      TakeCmd.Q_Put_String (Buffer);
      TakeCmd.CrLf;

      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return -2;
   end C_Use_Buffer;

   ---------------------------------------------------------------------------
   --  This is a Variable Function called from 4NT/TC
   --
   function F_Reverse (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int is
      Trace : TakeCmd.Trace.Object := TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
      pragma Unreferenced (Trace);

      Arguments_Length : constant Natural           :=
         Natural (Win32.Winbase.lstrlenW (Win32.Addr (Arguments.all)));
      Reverse_String   : constant Win32.WCHAR_Array :=
         Arguments.all (Arguments'First .. Arguments'First + Arguments_Length - 1);
      Result           : Interfaces.C.int           := TakeCmd.Plugin.Did_Not_Process;
   begin
      TakeCmd.Trace.Write (Arguments.all);

      if Arguments_Length = 0 then
         Result := 1;
      else
         for I in reverse Reverse_String'Range loop
            Arguments.all (Arguments_Length + 1 - I) := Reverse_String (I);
         end loop;

         Result := 0;
      end if;

      return Result;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return -2;
   end F_Reverse;

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC (after the call to "InitializePlugin") to get information from the
   --  plugin, primarily for the names of functions, variables & commands. All that is necessary
   --  is to return a pointer to the PluginInfo structure that was populated when the Plugin
   --  loaded.
   --
   function Get_Plugin_Info return  TakeCmd.Plugin.LP_Plugin_Info is
      use type TakeCmd.Plugin.LP_Plugin_Info;
   begin
      if Plugin_Info = null then
         Plugin_Info :=
            new TakeCmd.Plugin.Plugin_Info'
           (pszDll         => Win32.Addr (DLL_Name),
            pszAuthor      => Win32.Addr (Author),
            pszEmail       => Win32.Addr (Author_Email),
            pszWWW         => Win32.Addr (Author_WebSite),
            pszDescription => Win32.Addr (Description),
            pszFunctions   => Win32.Addr (Implements),
            nMajor         => 2,
            nMinor         => 2,
            nBuild         => 1,
            hModule        => 0,
            pszModule      => null);
         TakeCmd.Trace.Write_Info ("Ada_Demo: Plugin Info created!");
      else
         TakeCmd.Trace.Write_Info ("Ada_Demo: Plugin Info recycled!");
      end if;
      return Plugin_Info;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return null;
   end Get_Plugin_Info;

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC after loading the plugin. The API requires a return of 0, but as the
   --  function is declared as a boolean we must, somewhat counter-intuitively, return "false".
   --
   function Initialize_Plugin return  Win32.BOOL is
   begin
      TakeCmd.Trace.Initialize_Plugin;

      if My_Remark = null then
         My_Remark := new Remark_Task;
      end if;

      TakeCmd.Trace.Write_Info (Wide_String'("Ada_Demo: DLL initialized OK!"));

      return Win32.FALSE;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return Win32.TRUE;
   end Initialize_Plugin;

   ---------------------------------------------------------------------------
   --  This function illustrates how to use keystroke monitoring and modification. This function
   --  is prefixed with a "*" in the "Implements" field of the PLUGININFO record, and so 4NT/TC
   --  calls it every time a keystroke is entered. This function simply replaces any lower case
   --  letter "a" with an upper case letter "A".
   --
   function K_Key (Arguments : access TakeCmd.Plugin.Key_Info) return Interfaces.C.int is
   begin
      if Arguments.all.nKey = 97 then   --  97 is the ASCII value for "a"
         Arguments.all.nKey := 65;      --  65 is the ASCII value for "A"
      end if;

      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return -2;
   end K_Key;

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC when shutting down, if EndProcess = 0, only the plugin is being closed;
   --  if EndProcess = 1, then 4NT/TC is shutting down. The API requires a return of 0, but as
   --  the function is declared as a boolean we must, somewhat counter-intuitively, return
   --  "false".
   function Shutdown_Plugin (End_Process : in Win32.BOOL) return Win32.BOOL is
      use type TakeCmd.Plugin.LP_Plugin_Info;

      procedure Deallocate is new Ada.Unchecked_Deallocation (
         Object => TakeCmd.Plugin.Plugin_Info,
         Name => TakeCmd.Plugin.LP_Plugin_Info);
      procedure Deallocate is new Ada.Unchecked_Deallocation (
         Object => Remark_Task,
         Name => Remark_Access);

      pragma Unreferenced (End_Process);
   begin
      Deallocate (My_Remark);
      Deallocate (Plugin_Info);

      TakeCmd.Trace.Write_Info (Wide_String'("Ada_Demo: DLL shut down OK!"));
      TakeCmd.Trace.Shutdown_Plugin;

      return Win32.FALSE;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return Win32.TRUE;
   end Shutdown_Plugin;

   ---------------------------------------------------------------------------
   --  This is an Internal Variable called from 4NT/TC
   --
   function V_Hello (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int is
      Trace : TakeCmd.Trace.Object := TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
      pragma Unreferenced (Trace);

      Response : constant Win32.WCHAR_Array :=
         "Hello from an Ada generated Plugin!" & Win32.Wide_Nul;
   begin
      Arguments.all (Arguments'First .. Arguments'First + Response'Last) := Response;

      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return -2;
   end V_Hello;

   ---------------------------------------------------------------------------
   --  This is an Internal Variable called from 4NT/TC
   --
   function V_Task_Remark (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int is
      Trace : TakeCmd.Trace.Object := TakeCmd.Trace.Function_Trace (TakeCmd.Trace.Entity);
      pragma Unreferenced (Trace);

      pragma Warnings (Off, "variable ""Dummy"" is assigned but never read");

      Response : aliased constant Wide_String :=
         Remark_Value.Get_Remark & Ada.Characters.Wide_Latin_1.NUL;
      Dummy    : Win32.PWSTR;

      pragma Warnings (On, "variable ""Dummy"" is assigned but never read");
   begin
      pragma Warnings (Off, "useless assignment to ""Dummy"", value never referenced");

      Dummy :=
         Win32.Winbase.lstrcpynW
           (lpString1  => Win32.Addr (Arguments.all),
            lpString2  => Win32.Addr (Response),
            iMaxLength => Arguments'Length);

      pragma Warnings (On, "useless assignment to ""Dummy"", value never referenced");
      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Trace.Write_Error (An_Exception);
         return -2;
   end V_Task_Remark;

------------------------------------------------------------------------------
--  Start of code that will be executed when the plug-in is first loaded.
--
begin
   TakeCmd.Q_Put_String (Win32.WCHAR_Array'("Ada_Demo: DLL loaded OK!"));
   TakeCmd.CrLf;
exception
   when An_Exception : others =>
      TakeCmd.Q_Put_String (Ada.Exceptions.Exception_Information (An_Exception));
      TakeCmd.CrLf;
end Ada_Demo;

------------------------------------------------------------- {{{1 ----------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=96 filetype=ada foldmethod=expr spell spelllang=en_GB:
