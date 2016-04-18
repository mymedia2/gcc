/* Implementation of diagnostics subroutines which produce XML output.
   Copyright (C) 2016 Free Software Foundation, Inc.
   Contributed by Nicholas Guriev <guriev-ns@ya.ru> and Michael Aksenov <miaks1511@mail.ru>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include <string>

#include "diagnostic-xml.h"

/* Replace special characters in XML with their predefined entities, doing the
 * string suitable for use in XML. */
static std::string
xmlspecialchars (std::string str)
{
  std::string::size_type index = 0;
  while ((index = str.find_first_of ("<>&\"'", index)) != std::string::npos)
    {
      const std::string entity = str[index] == '<' ? "&lt;" :
                                 str[index] == '>' ? "&gt;" :
                                 str[index] == '&' ? "&amp;" :
                                 str[index] == '"' ? "&quot;" :
                                 str[index] == '\'' ? "&apos;" :
                                 str.substr (index, 1);
      str.replace (index, 1, entity);
      index += entity.length ();
    }
  return str;
}

void output_xml_diagnositc (diagnostic_context *context,
                            diagnostic_info *diagnostic)
{
  std::puts("Flag of XML diagnostics works in other module!");
}
