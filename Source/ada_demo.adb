------------------------------------------------------------- {{{1 ----------
--  Description: Options setable by the Ada plugin
--          $Id: ada_options.vim 774 2007-09-17 09:11:59Z krischik $
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author: krischik $
--        $Date: 2007-09-17 11:11:59 +0200 (Mo, 17 Sep 2007) $
--      Version: 4.5
--    $Revision: 774 $
--     $HeadURL: https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ada_options.vim $
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

with Win32.Winbase;
with TakeCmd;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Exceptions;

package body Ada_Demo is

   use type Win32.WCHAR_Array;
   use type Interfaces.C.int;

   Plugin_Info : TakeCmd.Plugin.LP_Plugin_Info := null;

   DLL_Name       : aliased Win32.WCHAR_Array := "AdaDemo" & Win32.Wide_Nul;
   Author         : aliased Win32.WCHAR_Array :=
      "Martin Krischik" & Win32.Wide_Nul;
   Author_Email   : aliased Win32.WCHAR_Array :=
      "krischik@users.sourceforge.net" & Win32.Wide_Nul;
   Author_WebSite : aliased Win32.WCHAR_Array :=
      "http://ada.krischik.com" & Win32.Wide_Nul;
   Description    : aliased Win32.WCHAR_Array :=
      "A demonstration Plugin for 4NT/TC, written with Ada." &
      Win32.Wide_Nul;
   Implements     : aliased Win32.WCHAR_Array :=
      "@REVERSE,_HELLO,REMARK,DIR,*KEY,USEBUFFER" & Win32.Wide_Nul;

   ------------------------------------------------------------------------
   --  This function shows how you can modify the behaviour of a 4NT/TC
   --  command. If you use the DIR command this function will be called, and a
   --  check is made of the current time. If the value of the "Minutes" is
   --  even then a message will be displayed telling you that you can't use
   --  DIR at the moment. If the value is odd the "DID_NOT_PROCESS" value is
   --  returned and 4NT/TC will execute the DIR command as normal.
   --
   function C_Dir (Arguments : Win32.PCWSTR) return Interfaces.C.int is
      Time   : Ada.Calendar.Time                     := Ada.Calendar.Clock;
      Minute : Ada.Calendar.Formatting.Minute_Number :=
         Ada.Calendar.Formatting.Minute (Time);
      Result : Interfaces.C.int                      :=
         TakeCmd.Plugin.Did_Not_Process;
   begin
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
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         return -2;
   end C_Dir;

   ------------------------------------------------------------------------
   --  This is an Internal Command called from 4NT/TC
   --
   function C_Remark (Arguments : Win32.PCWSTR) return Interfaces.C.int is
   begin
      TakeCmd.Q_Put_String
        (Win32.WCHAR_Array'("What a trivial Ada generated Plugin!"));
      TakeCmd.CrLf;
      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         return -2;
   end C_Remark;

   ------------------------------------------------------------------------
   --  This function illustrates how to call TakeCmd.dll functions which
   --  require a buffer in which to manipualte the supplied string. A number
   --  is supplied as a parameter to this function and it uses "Addcommas" to
   --  insert the thousands separator into the number. If the supplied
   --  parameter is not a valid number it is left unchanged. For example:
   --
   --    c:\>usebuffer 123456789
   --    123,456,789
   --
   function C_Use_Buffer
     (Arguments : Win32.PCWSTR)
      return      Interfaces.C.int
   is
      Argument_Length   : Win32.INT := Win32.Winbase.lstrlenW (Arguments);
      Arguments_String  : Win32.WCHAR_Array (
         1 .. Natural (Argument_Length + Argument_Length / 3 + 1));
      Arguments_Pointer : Win32.PWSTR;

      pragma Warnings (Off, Arguments_String);
   begin
      Arguments_Pointer :=
         Win32.Winbase.lstrcpynW
           (lpString1  => Win32.Addr (Arguments_String),
            lpString2  => Arguments,
            iMaxLength => Arguments_String'Length);

      --  ArgumentString:= Trim(WideCharToString(arguments));
      TakeCmd.AddCommas (Win32.Addr (Arguments_String));
      TakeCmd.Q_Put_String (Arguments_String);
      TakeCmd.CrLf;

      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         return -2;
   end C_Use_Buffer;

   ------------------------------------------------------------------------
   --  This is a Variable Function called from 4NT/TC
   --
   function F_Reverse
     (Arguments : in Win32.PCWSTR)
      return      Interfaces.C.int
   is
      Argument_Length  : Win32.INT := Win32.Winbase.lstrlenW (Arguments);
      Arguments_String : Win32.WCHAR_Array (1 .. Natural (Argument_Length));

      --  Your now learning the most dangerous type convertion available in
      --  Ada. Use your new knowlege wisely!
      pragma Import (Convention => C, Entity => Arguments_String);
      for Arguments_String'Address use Arguments.all'Address;

      Reverse_String : Win32.WCHAR_Array (1 .. Natural (Argument_Length)) :=
         Arguments_String;
      Result         : Interfaces.C.int                                    :=
         TakeCmd.Plugin.Did_Not_Process;
   begin
      if Argument_Length = 0 then
         Result := 1;
      else
         for I in reverse Reverse_String'Range loop
            Arguments_String (Arguments_String'Last + 1 - I) :=
               Reverse_String (I);
            null;
         end loop;
         Result := 0;
      end if;
      return Result;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         return -2;
   end F_Reverse;

   ------------------------------------------------------------------------
   --  Called by 4NT/TC (after the call to "InitializePlugin") to get
   --  information from the plugin, primarily for the names of functions,
   --  variables & commands. All that is necessary is to return a pointer to
   --  the PluginInfo structure that was populated when the Plugin loaded.
   function Get_Plugin_Info return  TakeCmd.Plugin.LP_Plugin_Info is
      use type TakeCmd.Plugin.LP_Plugin_Info;
   begin
      if Plugin_Info = null then
         Plugin_Info :=
            new TakeCmd.Plugin.Plugin_Info'
           (DLLName       => Win32.Addr (DLL_Name),
            Author        => Win32.Addr (Author),
            AuthorEmail   => Win32.Addr (Author_Email),
            AuthorWebSite => Win32.Addr (Author_WebSite),
            Description   => Win32.Addr (Description),
            Implements    => Win32.Addr (Implements),
            MajorVer      => 0,
            MinorVer      => 0,
            BuildNum      => 0,
            ModuleHandle  => 0,
            ModuleName    => null);
         TakeCmd.Q_Put_String
           (Win32.WCHAR_Array'("Ada_Demo: Plugin Info created!"));
         TakeCmd.CrLf;
      else
         TakeCmd.Q_Put_String
           (Win32.WCHAR_Array'("Ada_Demo: Plugin Info recycled!"));
         TakeCmd.CrLf;
      end if;
      return Plugin_Info;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         return null;
   end Get_Plugin_Info;

   ------------------------------------------------------------------------
   --  Called by 4NT/TC after loading the plugin. The API requires a return of
   --  0, but as the function is declared as a boolean we must, somewhat
   --  counter-intuitively, return "false".
   function Initialize_Plugin return  Win32.BOOL is
   begin
      TakeCmd.Q_Put_String
        (Win32.WCHAR_Array'("Ada_Demo: DLL initialized OK!"));
      TakeCmd.CrLf;
      return Win32.FALSE;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         return Win32.TRUE;
   end Initialize_Plugin;

   ------------------------------------------------------------------------
   --  This function illustrates how to use keystroke monitoring and
   --  modification. This function is prefixed with a "*" in the "Implements"
   --  field of the PLUGININFO record, and so 4NT/TC calls it every time a
   --  keystroke is entered. This function simply replaces any lower case
   --  letter "a" with an upper case letter "A".
   function K_Key
     (arguments : in TakeCmd.Plugin.LP_Key_Info)
      return      Interfaces.C.int
   is
   begin
      if arguments.KeyName = 97 then   --  97 is the ASCII value for "a"
         arguments.KeyName := 65;      --  65 is the ASCII value for "A"
      end if;
      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         return -2;
   end K_Key;

   ------------------------------------------------------------------------
   --  Called by 4NT/TC when shutting down, if EndProcess = 0, only the plugin
   --  is being closed; if EndProcess = 1, then 4NT/TC is shutting down. The
   --  API requires a return of 0, but as the function is declared as a
   --  boolean we must, somewhat counter-intuitively, return "false".
   function Shutdown_Plugin
     (End_Process : in Win32.BOOL)
      return        Win32.BOOL
   is
   begin
      TakeCmd.Q_Put_String
        (Win32.WCHAR_Array'("Ada_Demo: DLL shut down OK!"));
      TakeCmd.CrLf;
      return Win32.FALSE;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         return Win32.TRUE;
   end Shutdown_Plugin;

   ------------------------------------------------------------------------
   --  This is an Internal Variable called from 4NT/TC
   function V_Hello (Arguments : in Win32.PCWSTR) return Interfaces.C.int is
      Response : constant Win32.WCHAR_Array :=
         "Hello from a Ada generated Plugin!";
   begin
      Arguments_Pointer :=
         Win32.Winbase.lstrcpynW
           (lpString1  => Arguments,
            lpString2  => Response,
            iMaxLength => Arguments_String'Length);
      --  arguments.all := Response;
      return 0;
   exception
      when An_Exception : others =>
         TakeCmd.Q_Put_String
           (Ada.Exceptions.Exception_Information (An_Exception));
         return -2;
   end V_Hello; --  function _HELLO

---------------------------------------------------------------------------
--  Start of code that will be executed when the plugin is first loaded.
--
begin
   TakeCmd.Q_Put_String (Win32.WCHAR_Array'("Ada_Demo: DLL loaded OK!"));
   TakeCmd.CrLf;
exception
   when An_Exception : others =>
      TakeCmd.Q_Put_String
        (Ada.Exceptions.Exception_Information (An_Exception));
end Ada_Demo;

------------------------------------------------------------- {{{1 ----------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab     :
--  vim: set textwidth=0 filetype=ada foldmethod=expr nospell          :
