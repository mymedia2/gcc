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
#include <ctime>
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
      /* NB: quadratic complexity */
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

std::string
get_current_datetime ()
{
  std::time_t current_time = std::time (NULL);
  if (current_time != -1)
    return "";
  std::tm *st = std::gmtime (&current_time);

  char buffer[26];
  sprintf(buffer, "%4d-%2d-%2dT%2d:%2d:%2d%+2d:%2d", st->tm_year + 1900,
	  st->tm_mon + 1, st->tm_mday, st->tm_hour, st->tm_min, st->tm_sec,
	  // TODO
	  0, 0);

  return buffer;
}

std::string
get_work_directory ()
{
  return "";
}

void
print_close_tag ()
{
  fprintf (stderr, "</gcc>");
}

#define START_XML_DIAGNOSTIC \
"<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" \
"<gcc xmlns=\"https://gcc.gnu.org/diagnostics/0.1.1\">\n"

#define XML_META_PROGRAM_NAME "<programName>%s</programName>\n"
#define XML_META_VERSION "<version>%s</version>\n"
#define XML_META_COMMAND_LINE_PARAMETERS "<parameters>%s</parameters>\n"
#define XML_META_COMPILATION_DATETIME "<datetime>%s</datetime>\n"
#define XML_META_WORK_DIRECTORY "<workDirectory>%s</workDirectory>\n"

void
print_root_tag ()
{
  gcc_assert (!atexit (print_close_tag));
  fprintf (stderr, START_XML_DIAGNOSTIC);
  fprintf (stderr, "<meta>\n");

  fprintf (stderr, XML_META_PROGRAM_NAME, progname);
  fprintf (stderr, XML_META_VERSION, "версия\n");

  std::string datetime = get_current_datetime ();
  if (datetime != "")
    fprintf (stderr, XML_META_COMPILATION_DATETIME, datetime.c_str ());
  std::string work_directory = get_work_directory ();
  if (work_directory != "")
    fprintf (stderr, XML_META_WORK_DIRECTORY, work_directory.c_str ());

  fprintf (stderr, "</meta>\n\n");
}

} /* End of anonymous namespace.  */

void
xml_printer::output_xml_tag (const std::string &tag_name)
{
  struct chunk_info *chunk_array = buffer->cur_chunk_array;
  const char **args = chunk_array->args;

  gcc_assert (buffer->obstack == &buffer->formatted_obstack);
  gcc_assert (buffer->line_length == 0);
  gcc_assert (tag_name.length () != 0);

  const std::string opening_tag = "<" + tag_name + ">";
  const std::string closing_tag = "</" + tag_name + ">";

  /* This is a third phase of XML output, first 2 phases done in pp_format_args.
     Now we escape characters and actually print it.  */
  string (opening_tag.c_str ());
  for (unsigned int i = 0; args[i]; i++)
    {
      string (xmlspecialchars (args[i]).c_str ());
    }
  string (closing_tag.c_str ());

  /* Deallocate the chunk structure and everything after it (i.e. the
     associated series of formatted strings).  */
  buffer->cur_chunk_array = chunk_array->prev;
  obstack_free (&buffer->chunk_obstack, chunk_array);
}

xml_printer::xml_printer ()
{
}

xml_printer::~xml_printer ()
{
}
/*Copy-paste from diagnostic_report_current_module*/
void
report_includedFrom_tag (diagnostic_context *context, location_t where)
{
  const line_map_ordinary *map = NULL;

  if (pp_needs_newline (context->printer))
    {
      pp_newline (context->printer);
      pp_needs_newline (context->printer) = false;
    }

  if (where <= BUILTINS_LOCATION)
    return;

  linemap_resolve_location (line_table, where,
			    LRK_MACRO_DEFINITION_LOCATION,
			    &map);

  if (map && diagnostic_last_module_changed (context, map))
    {
      diagnostic_set_last_module (context, map);
      if (! MAIN_FILE_P (map))
	{
	  map = INCLUDED_FROM (line_table, map);
	  pp_verbatim(context->printer, "<includedFrom>");
	  if (context->show_column)
	    pp_verbatim (context->printer,
			 "<file><filename>%s</filename><locations><mark caret=\"%d,%d\" /></locations></file>",
			 xmlspecialchars(LINEMAP_FILE (map)).c_str(),
			 LAST_SOURCE_LINE (map), LAST_SOURCE_COLUMN (map));
	  else
	    pp_verbatim (context->printer,
			 "<file><filename>%s</filename><locations><mark caret=\"%d\" /></locations></file>",
			 xmlspecialchars(LINEMAP_FILE (map)).c_str(), LAST_SOURCE_LINE (map));
	  while (! MAIN_FILE_P (map))
	    {
	      map = INCLUDED_FROM (line_table, map);
	      pp_verbatim (context->printer,
			   "<file><filename>%s</filename><locations><mark caret=\"%d\" /></locations></file>\n",
			   xmlspecialchars(LINEMAP_FILE (map)).c_str(), LAST_SOURCE_LINE (map));
	    }
	  pp_verbatim (context->printer, "</includedFrom>");
	  pp_newline (context->printer);
	}
    }
}

void
xml_diagnostic_starter (diagnostic_context *context,
			    diagnostic_info *diagnostic)
{
  report_includedFrom_tag (context, diagnostic_location (diagnostic));
  pp_set_prefix (context->printer, diagnostic_build_prefix (context,
							    diagnostic));
}

void
initialize_xml_output (diagnostic_context *context, bool value)
{
  if (!value)
    return;
  context->xml_output_format = true;
  context->printer->set_verbatim_wrapping ();
  diagnostic_starter(context) = xml_diagnostic_starter;

  print_root_tag ();
}

bool
output_xml_diagnostic (diagnostic_context *context, diagnostic_info *diagnostic)
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

  context->printer->string (("<description>" +
			     xmlspecialchars (diagnostic->message.format_spec) +
			     "</description>").c_str ());

  if (context->show_option_requested)
    {
      char *option_text;

      option_text = context->option_name (context, diagnostic->option_index,
					  /* TODO: подправить третий параметр */
					  diagnostic->kind, diagnostic->kind);

      if (option_text)
	{
	  context->printer->string (("<option>" +
				     xmlspecialchars (option_text) +
				     "</option>").c_str ());
	  free (option_text);
	}
    }

  /* TODO: куда-нибудь запихнуть в более подходящее место */
  //if (context->xml_output_format)
  //  context->printer->show_color = false;

  diagnostic->message.x_data = &diagnostic->x_data;
  diagnostic->x_data = NULL;
  pp_format (context->printer, &diagnostic->message);
  (*diagnostic_starter (context)) (context, diagnostic);
  pp_output_formatted_text (context->printer);
  (*diagnostic_finalizer (context)) (context, diagnostic);
  diagnostic_action_after_output (context, diagnostic->kind);
  diagnostic->message.format_spec = saved_format_spec;
  diagnostic->x_data = NULL;

  context->lock--;
  return true;
}
