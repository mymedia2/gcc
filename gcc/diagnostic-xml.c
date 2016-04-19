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

namespace {

/* Replace special characters in XML with their predefined entities, doing the
 * string suitable for use in XML. */
std::string
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

void
output_xml_tag (pretty_printer *pp, const std::string &tag_name)
{
  output_buffer *buffer = pp_buffer (pp);
  struct chunk_info *chunk_array = buffer->cur_chunk_array;
  const char **args = chunk_array->args;

  gcc_assert (buffer->obstack == &buffer->formatted_obstack);
  gcc_assert (buffer->line_length == 0);
  gcc_assert (tag_name.length () != 0);

  const std::string opening_tag = "<" + tag_name + ">";
  const std::string closing_tag = "</" + tag_name + ">";

  /* This is a third phase of XML output, first 2 phases done in pp_format_args.
     Now we escape characters and actually print it.  */
  pp_string (pp, opening_tag.c_str ());
  for (unsigned int i = 0; args[i]; i++)
    {
      pp_string (pp, xmlspecialchars (args[i]).c_str ());
    }
  pp_string (pp, closing_tag.c_str ());

  /* Deallocate the chunk structure and everything after it (i.e. the
     associated series of formatted strings).  */
  buffer->cur_chunk_array = chunk_array->prev;
  obstack_free (&buffer->chunk_obstack, chunk_array);
}

} /* End of anonymous namespace.  */

bool
output_xml_diagnositc (diagnostic_context *context,
                       diagnostic_info *diagnostic)
{
  context->lock++;

  if (diagnostic->kind == DK_ICE || diagnostic->kind == DK_ICE_NOBT)
    {
      /* When not checking, ICEs are converted to fatal errors when an
	 error has already occurred.  This is counteracted by
	 abort_on_error.  */
      if (!CHECKING_P
	  && (diagnostic_kind_count (context, DK_ERROR) > 0
	      || diagnostic_kind_count (context, DK_SORRY) > 0)
	  && !context->abort_on_error)
	{
	  expanded_location s 
	    = expand_location (diagnostic_location (diagnostic));
	  fnotice (stderr, "%s:%d: confused by earlier errors, bailing out\n",
		   s.file, s.line);
	  exit (ICE_EXIT_CODE);
	}
      if (context->internal_error)
	(*context->internal_error) (context,
				    diagnostic->message.format_spec,
				    diagnostic->message.args_ptr);
    }

  const char *saved_format_spec = diagnostic->message.format_spec;
  if (context->show_option_requested)
    {
      char *option_text; 

      option_text = context->option_name (context, diagnostic->option_index,
					  /* TODO: подправить третий параметр */
					  diagnostic->kind, diagnostic->kind);

      if (option_text)
	{
	  diagnostic->message.format_spec
	    = ACONCAT ((diagnostic->message.format_spec,
			" [", option_text, "]",
			NULL));
	  free (option_text);
	}
    }

  /* TODO: куда-нибудь запихнуть в более подходящее место */
  if (context->xml_output_format)
	context->printer->show_color = false;

  diagnostic->message.x_data = &diagnostic->x_data;
  diagnostic->x_data = NULL;
  pp_format (context->printer, &diagnostic->message);
  //(*diagnostic_starter (context)) (context, diagnostic);
  //pp_output_formatted_text (context->printer);
  output_xml_tag (context->printer, "description");
  (*diagnostic_finalizer (context)) (context, diagnostic);
  diagnostic_action_after_output (context, diagnostic->kind);
  diagnostic->message.format_spec = saved_format_spec;
  diagnostic->x_data = NULL;

  context->lock--;
  return true;
}
