------------------------------------------------------------------------------
--  Description: Options setable by the Ada plugin
--          $Id: mk_utils.ads 58 2007-12-02 15:06:05Z krischik@users.sourceforge.net $
--    Copyright: Copyright (C) 2007 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik
--      $Author: krischik@users.sourceforge.net $
--        $Date: 2007-12-02 16:06:05 +0100 (So, 02 Dez 2007) $
--      Version: 4.5
--    $Revision: 58 $
--     $HeadURL: https://mkutils.googlecode.com/svn/trunk/Source/mk_utils/mk_utils.ads $
--      History: 25.10.2007 MK Initial Release
--               29.10.2007 MK Added Threading, parameter names closer to
--                             C original
--         Help:
--  http://www.jpsoftwiki.com/wiki/index.php?title=MK_Utils_%28plugin%29
----------------------------------------------------------------------------
--  Copyright (C) 2007 Martin Krischik
--
--  This file is part of MK_Utils.
--
--  MK_Utils is free software: you can redistribute it and/or modify it under the terms of the
--  GNU General Public License as published by the Free Software Foundation, either version 3
--  of the License, or (at your option) any later version.
--
--  MK_Utils is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
--  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along with MK_Utils. If
--  not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------------

pragma License (Gpl);
pragma Ada_05;

with Ada.Finalization;
with Win32.Winnt;

package Win32.Set_Privilege is

   type Object is tagged limited private;

   Win32_Error : exception;

   function Create (Privilege : in Win32.Winnt.TEXT) return Object;

private

   type Object is new Ada.Finalization.Limited_Controlled with record
      Token : aliased Win32.Winnt.HANDLE;
      Luid  : aliased Win32.Winnt.ULARGE_INTEGER;
   end record;

   procedure Finalize (This : in out Object);

end Win32.Set_Privilege;

------------------------------------------------------------------------------
--  vim: set nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab          :
--  vim: set textwidth=78 filetype=ada foldmethod=expr spell spelllang=en_GB:
