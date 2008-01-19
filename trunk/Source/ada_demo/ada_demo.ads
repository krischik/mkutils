------------------------------------------------------------- {{{1 ----------
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
----------------------------------------------------------------------------
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
------------------------------------------------------------- }}}1 ----------

------------------------------------------------------------------------------
--  A demonstration Plugin for 4NT and Take Command 8.0 written with Ada {{{1
--  ==========================================================================
--
------------------------------------------------------------------------------
--  Plug-in changes introduced with version 8.0 of 4NT and Take Command
------------------------------------------------------------------------------
--  In version 8.0 of 4NT and Take Command the PLUGININFO structure has changed; an extra
--  pointer has been added to the end of the structure. If you have written any plugins for
--  version 7 they may fail when run under verison 8. It would be sensible to recomplie them
--  with the new PLUGININFO structure that can be found in this demonstration plugin.
------------------------------------------------------------------------------
--
--  Introduction                                                        {{{2
--  ----------------
--
--  This file demonstrates how to produce a 4NT/TC plugin with Ada. Most of the information in
--  these notes on the 4NT/TC API is based on the plugin.h and plugin.cpp files contained in the
--  4NT/TC SDK.
--
--  The code in this file shows how to produce an Internal Command, a Variable Function and an
--  Internal Variable, how to modify an existing 4NT/TC command and how to call functions in
--  Takecmd.dll. It also shows how to use keystroke monitoring/modification from within a
--  plugin.
--
--  These notes also cover some specifics of using Ada to produce a DLL to run with 4NT and Take
--  Command.
--
--  Using the 4NT/Take Command API with Ada                             {{{2
--  ------------------------------------------
--
--  Functions that are to be called by 4NT / TC need to be in the general format:
--
--     function MyFunctionName (Arguments: Win32.PCWSTR) return Interfaces.C.int;
--
--  Every plugin must implement and export the following three functions:
--
--    function InitializePlugin return Win32.BOOL;
--    pragma Export
--      (Convention => Stdcall,
--       Entity => Initialize_Plugin,
--       External_Name => "InitializePlugin");
--
--    function GetPluginInfo return TakeCmd.Plugin.LP_Plugin_Info;
--    pragma Export
--      (Convention => Stdcall,
--       Entity => Get_Plugin_Info,
--       External_Name => "GetPluginInfo");
--
--    function ShutdownPlugin(EndProcess: Win32.BOOL) return Win32.BOOL;
--    pragma Export
--      (Convention => Stdcall,
--       Entity => Shutdown_Plugin,
--       External_Name => "ShutdownPlugin");
--
--  For more detail on these three functions see the comments in the actual functions later in
--  this file.
--
--  Every plugin must define the PLUGININFO structure and return a pointer to it via the
--  GetPluginInfo function. Amongst other things the PLUGININFO structure identifies to 4NT/TC
--  the Internal Commands, Internal Variables and Variable Functions that the plugin implements.
--  More comments on PLUGININFO can be found later in this file in the Type definitions.
--
--  Internal Variable names in the "Implements" field of the PLUGININFO structure (and their
--  corresponding function names) must begin with an underscore ('_').
--
--  Variable Function names in the "Implements" field of the PLUGININFO structure must begin
--  with an @; the corresponding function must be prefixed by "f_". (This allows variable
--  functions to have the same name as internal commands.)
--
--    For example:
--
--       Implements:= 'reverse,@reverse';
--
--  Entering the name "reverse" on the command line will invoke the command reverse()
--
--  Entering the name "@reverse[]" on the command line will invoke the variable function
--  f_reverse()
--
--  Variable function names are limited to a maximum of 31 characters.
--
--  Internal command names may be any combination of alphanumeric characters up to a maximum of
--  12 characters.
--
--  The case of function names (for Internal Commands, Variable Functions and Internal
--  Variables) in the "Implements" field of PLUGININFO and the corresponding function names must
--  match exactly or 4NT/TC will not recognise them (e.g. if you name a function "Sample" but
--  put "sample" in the Implements field of PLUGININFO it will not work).
--
--  4NT and TC call Plugin functions as follows:
--
--      (a) Internal Commands are passed a pointer to a null terminated string
--          containing the command line minus the name of the internal
--          command.
--
--      (b) Variable Functions are passed a pointer to a null terminated
--          string containing the argument(s) to the plugin function.
--
--      (c) Internal Variables are passed a pointer to an empty null
--          terminated string which is used for output only.
--
--  Returning from the Plugin:
--
--      (a) For Internal Commands, return the integer result (anything left in
--          the arguments string will be ignored).
--
--      (b) For Variable Functions, copy the result string over the arguments
--          string and return the integer result.  The maximum length for the
--          result string is 2K (2047 characters + null byte).  The integer
--          return can be:
--
--                0 = success
--                < 0 = failure; error message already displayed by the Plugin
--                      function
--                > 0 = failure; error value should be interpreted as a system
--                      error and displayed by 4NT / TC
--
--      (c) For Internal Variables, copy the result string over the arguments
--          string.  There is no meaningful integer return value for Internal
--          Variables.  The maximum length for the result string is 2K
--          (2047 characters + null byte).
--
--      (d) There is a special return value ($FEDCBA98) that tells the parser
--          to assume that the plugin decided not to handle the
--          variable/function/ command. The parser then continues looking for
--          a matching internal, then external.  Note that you can use this
--          return value to have your plugin modify the command line and then
--          pass it on to an existing internal variable/function/command.  An
--          example of using this return value can be found in the "DIR"
--          command later in this plugin.
--
--  4NT & Take Command will trap any exceptions occurring in the plugin, to prevent the plugin
--  from crashing the command processor. An error message will be displayed and the plugin will
--  return an exit code = 2.
--
--  Note that all strings passed between 4NT/TC and the plugin are null terminated and are
--  UNICODE.
--
--  Version 8.0 of 4NT and Take Command introduced keystroke monitoring. A keystroke monitoring
--  function will be called every time a key is pressed, and is passed a pointer to a structure
--  containing information about the command line and the key that was pressed. Thus a plugin
--  can watch for specific keystrokes being pressed. A keystroke monitoring function's name must
--  be prefixed by "*" in the "Implements" field of the PLUGININFO record to identify it to 4NT
--  and Take Command. This demonstration plugin includes a keystroke monitoring function.
--
--  GNAT specific issues                                                {{{2
--  ------------------------
--
--  This demonstration plugin does not implement a DLLMain function as it is
--  not necessary.  When the plugin is loaded the code between the first
--  "begin" and "end." is executed and any initialisation can be performed
--  there.  In most cases this is likely to be adequate and a DLLMain function
--  is not required.  If you write a  plugin that needs to execute specific
--  statements every time a process or thread attaches or detaches to or from
--  the plugin it can be done by writing a DLLMain function and assigning its
--  address to the DLLProc variable when the plugin is first loaded.  See the
--  GNAT documentation for further information on this technique.
--
--  4NT Plugins use sdcall - but without the added "@nn" postfix. GNAT however will add "@nn"
--  postfixes to all stdcall functions and you will need to remove them at link time using the
--  "-Wl,--kill-at" option. The Demo GNAT-Project-File already contains the needed flag.
--
--  As mentioned all strings passed between 4NT/TC and the plugin are null terminated and are
--  UNICODE. Since Both Ada 95 and Ada 2005 have support for Wide_Characters you might be the
--  better option.
--
--  The Plugin catches all exceptions before returning to 4NT / TakeCommand.
--
--  Notes about this Demo                                               {{{2
--  ------------------------
--
--  This Demo writes some debugging output - which you should not do in a real plugin.
--
----------------------------------------------------------- }}}1--------------

pragma License (Gpl);

with Interfaces.C;
with Win32;
with TakeCmd.Plugin;

package Ada_Demo is
   ---------------------------------------------------------------------------
   --  You can use Ada tasking facilities inside a plug-in. Just be aware that library level
   --  task won't work and that most - if not all - command from the TakeCmd library can not be
   --  used.
   --
   task type Remark_Task is
      entry Execute;
   end Remark_Task;

   protected Remark_Value is
      function Get_Remark return Wide_String;
      procedure Set_Remark (New_Remark : in Wide_String);
   private
      Remark : Wide_String (1 .. 80) := (others => ' ');
   end Remark_Value;

   type Remark_Access is access Remark_Task;

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC after loading the plugin. The API requires a return of 0, but as the
   --  function is declared as a boolean we must, somewhat counter-intuitively, return "false".
   --
   function Initialize_Plugin return  Win32.BOOL;

   pragma Export
     (Convention => Stdcall,
      Entity => Initialize_Plugin,
      External_Name => "InitializePlugin");

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC when shutting down, if EndProcess = 0, only the plugin is being closed;
   --  if EndProcess = 1, then 4NT/TC is shutting down. The API requires a return of 0, but as
   --  the function is declared as a boolean we must, somewhat counter-intuitively, return
   --  "false".
   --
   function Shutdown_Plugin (End_Process : in Win32.BOOL) return Win32.BOOL;

   pragma Export
     (Convention => Stdcall,
      Entity => Shutdown_Plugin,
      External_Name => "ShutdownPlugin");

   ---------------------------------------------------------------------------
   --  Called by 4NT/TC (after the call to "InitializePlugin") to get information from the
   --  plugin, primarily for the names of functions, variables & commands. All that is necessary
   --  is to return a pointer to the PluginInfo structure that was populated when the Plugin
   --  loaded.
   --
   function Get_Plugin_Info return  TakeCmd.Plugin.LP_Plugin_Info;

   pragma Export
     (Convention => Stdcall,
      Entity => Get_Plugin_Info,
      External_Name => "GetPluginInfo");

   ---------------------------------------------------------------------------
   --  This is a Variable Function called from 4NT/TC
   --
   function F_Reverse (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int;

   pragma Export (Convention => Stdcall, Entity => F_Reverse, External_Name => "f_REVERSE");

   ---------------------------------------------------------------------------
   --  This is an Internal Variable called from 4NT/TC
   --
   function V_Hello (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int;

   pragma Export (Convention => Stdcall, Entity => V_Hello, External_Name => "_HELLO");

   ---------------------------------------------------------------------------
   --  This is an Internal Command called from 4NT/TC
   --
   function C_Remark (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   pragma Export (Convention => Stdcall, Entity => C_Remark, External_Name => "REMARK");

   ---------------------------------------------------------------------------
   --  This is an Internal Command called from 4NT/TC
   --
   function C_Task_Remark (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   pragma Export
     (Convention => Stdcall,
      Entity => C_Task_Remark,
      External_Name => "TASKREMARK");

   function V_Task_Remark (Arguments : access TakeCmd.Plugin.Buffer) return Interfaces.C.int;

   pragma Export
     (Convention => Stdcall,
      Entity => V_Task_Remark,
      External_Name => "_TASKREMARK");

   ---------------------------------------------------------------------------
   --  This function shows how you can modify the behaviour of a 4NT/TC command. If you use the
   --  DIR command this function will be called, and a check is made of the current time. If the
   --  value of the "Minutes" is even then a message will be displayed telling you that you
   --  can't use DIR at the moment. If the value is odd the "DID_NOT_PROCESS" value is returned
   --  and 4NT/TC will execute the DIR command as normal.

   function C_Dir (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   pragma Export (Convention => Stdcall, Entity => C_Dir, External_Name => "DIR");

   ---------------------------------------------------------------------------
   --  This function illustrates how to use keystroke monitoring and modification. This function
   --  is prefixed with a "*" in the "Implements" field of the PLUGININFO record, and so 4NT/TC
   --  calls it every time a keystroke is entered. This function simply replaces any lower case
   --  letter "a" with an upper case letter "A".
   --
   function K_Key (Arguments : access TakeCmd.Plugin.Key_Info) return Interfaces.C.int;

   pragma Export (Convention => Stdcall, Entity => K_Key, External_Name => "KEY");

   ---------------------------------------------------------------------------
   --  This function illustrates how to call TakeCmd.dll functions which require a buffer in
   --  which to manipualte the supplied string. A number is supplied as a parameter to this
   --  function and it uses "Addcommas" to insert the thousands separator into the number. If
   --  the supplied parameter is not a valid number it is left unchanged. For example:
   --
   --    c:\>usebuffer 123456789
   --    123,456,789
   --
   function C_Use_Buffer (Arguments : in Win32.PCWSTR) return Interfaces.C.int;

   pragma Export (Convention => Stdcall, Entity => C_Use_Buffer, External_Name => "USEBUFFER");

private

end Ada_Demo;

------------------------------------------------------------- {{{1 ----------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
