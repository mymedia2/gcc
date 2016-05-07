/* Various declarations for language-independent pretty-print subroutines.
   Copyright (C) 2002-2016 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

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

#ifndef GCC_PRETTY_PRINT_H
#define GCC_PRETTY_PRINT_H

#include "obstack.h"
#include "wide-int-print.h"

/* Maximum number of format string arguments.  */
#define PP_NL_ARGMAX   30

/* The type of a text to be formatted according a format specification
   along with a list of things.  */
struct text_info
{
  const char *format_spec;
  va_list *args_ptr;
  int err_no;  /* for %m */
  void **x_data;
  rich_location *m_richloc;

  void set_location (unsigned int idx, location_t loc, bool caret_p);
  location_t get_location (unsigned int index_of_location) const;
};

/* How often diagnostics are prefixed by their locations:
   o DIAGNOSTICS_SHOW_PREFIX_NEVER: never - not yet supported;
   o DIAGNOSTICS_SHOW_PREFIX_ONCE: emit only once;
   o DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE: emit each time a physical
   line is started.  */
enum diagnostic_prefixing_rule_t
{
  DIAGNOSTICS_SHOW_PREFIX_ONCE       = 0x0,
  DIAGNOSTICS_SHOW_PREFIX_NEVER      = 0x1,
  DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE = 0x2
};

/* The chunk_info data structure forms a stack of the results from the
   first phase of formatting (pp_format) which have not yet been
   output (pp_output_formatted_text).  A stack is necessary because
   the diagnostic starter may decide to generate its own output by way
   of the formatter.  */
struct chunk_info
{
  /* Pointer to previous chunk on the stack.  */
  struct chunk_info *prev;

  /* Array of chunks to output.  Each chunk is a NUL-terminated string.
     In the first phase of formatting, even-numbered chunks are
     to be output verbatim, odd-numbered chunks are format specifiers.
     The second phase replaces all odd-numbered chunks with formatted
     text, and the third phase simply emits all the chunks in sequence
     with appropriate line-wrapping.  */
  const char *args[PP_NL_ARGMAX * 2];
};

/* The output buffer datatype.  This is best seen as an abstract datatype
   whose fields should not be accessed directly by clients.  */
struct output_buffer
{
  output_buffer ();
  ~output_buffer ();

  /* Obstack where the text is built up.  */
  struct obstack formatted_obstack;

  /* Obstack containing a chunked representation of the format
     specification plus arguments.  */
  struct obstack chunk_obstack;

  /* Currently active obstack: one of the above two.  This is used so
     that the text formatters don't need to know which phase we're in.  */
  struct obstack *obstack;

  /* Stack of chunk arrays.  These come from the chunk_obstack.  */
  struct chunk_info *cur_chunk_array;

  /* Where to output formatted text.  */
  FILE *stream;

  /* The amount of characters output so far.  */
  int line_length;

  /* This must be large enough to hold any printed integer or
     floating-point value.  */
  char digit_buffer[128];

  /* Nonzero means that text should be flushed when
     appropriate. Otherwise, text is buffered until either
     pp_really_flush or pp_clear_output_area are called.  */
  bool flush_p;
};

/* Finishes constructing a NULL-terminated character string representing
   the buffered text.  */
static inline const char *
output_buffer_formatted_text (output_buffer *buff)
{
  obstack_1grow (buff->obstack, '\0');
  return (const char *) obstack_base (buff->obstack);
}

/* Append to the output buffer a string specified by its
   STARTing character and LENGTH.  */
static inline void
output_buffer_append_r (output_buffer *buff, const char *start, int length)
{
  gcc_checking_assert (start);
  obstack_grow (buff->obstack, start, length);
  for (int i = 0; i < length; i++)
    if (start[i] == '\n')
      buff->line_length = 0;
    else
      buff->line_length++;
}

/*  Return a pointer to the last character emitted in the
    output_buffer.  A NULL pointer means no character available.  */
static inline const char *
output_buffer_last_position_in_text (const output_buffer *buff)
{
  const char *p = NULL;
  struct obstack *text = buff->obstack;

  if (obstack_base (text) != obstack_next_free (text))
    p = ((const char *) obstack_next_free (text)) - 1;
  return p;
}


extern const char *identifier_to_locale (const char *);
extern void *(*identifier_to_locale_alloc) (size_t);
extern void (*identifier_to_locale_free) (void *);

struct base_printer
{
  /* The type of a hook that formats client-specific data onto a pretty_printer.
     A client-supplied formatter returns true if everything goes well,
     otherwise it returns false.  */
  typedef bool (*printer_fn) (pretty_printer *, text_info *, const char *,
			      int, bool, bool, bool);

  explicit base_printer ();

  virtual ~base_printer ();

  /* Where we print external representation of ENTITY.  */
  output_buffer *buffer;

  /* If non-NULL, this function formats a TEXT into the BUFFER.  When called,
     TEXT->format_spec points to a format code.  FORMAT_DECODER should call
     pp_string (and related functions) to add data to the BUFFER.
     FORMAT_DECODER can read arguments from *TEXT->args_pts using VA_ARG.
     If the BUFFER needs additional characters from the format string, it
     should advance the TEXT->format_spec as it goes.  When FORMAT_DECODER
     returns, TEXT->format_spec should point to the last character processed.
  */
  printer_fn format_decoder;

  /* Nonzero means identifiers are translated to the locale character
     set on output.  */
  bool translate_identifiers;
};

/* If we haven't already defined a front-end-specific diagnostics
   style, use the generic one.  */
#ifdef GCC_DIAG_STYLE
#define GCC_PPDIAG_STYLE GCC_DIAG_STYLE
#else
#define GCC_PPDIAG_STYLE __gcc_diag__
#endif

#if GCC_VERSION >= 3005
#define ATTRIBUTE_GCC_PPDIAG(m, n) __attribute__ ((__format__ (GCC_PPDIAG_STYLE, m ,n))) ATTRIBUTE_NONNULL(m)
#else
#define ATTRIBUTE_GCC_PPDIAG(m, n) ATTRIBUTE_NONNULL(m)
#endif

/* The data structure that contains the bare minimum required to do
   proper pretty-printing.  Clients may derived from this structure
   and add additional fields they need.  */
struct pretty_printer
  : base_printer
{
  /* The type of pretty-printer flags passed to clients.  */
  typedef unsigned int flags_t;

  enum padding_t { pp_none, pp_before, pp_after };

  /* Structure for switching in and out of verbatim mode in a convenient
     manner.  */
  struct wrapping_mode_t
  {
    /* Current prefixing rule.  */
    diagnostic_prefixing_rule_t rule;

    /* The ideal upper bound of number of characters per line, as suggested
       by front-end.  */
    int line_cutoff;
  };

  // Default construct a pretty printer with specified prefix
  // and a maximum line length cut off limit.
  explicit pretty_printer (const char* = NULL, int = 0);

  virtual ~pretty_printer ();

  const char *get_prefix () const { return prefix; }
  void set_prefix (const char *);
  void destroy_prefix ();
  void emit_prefix ();

  /* True if colors should be shown.  */
  bool get_color_state () { return show_color; }

  /* Maybe initialize the color support. */
  void initialize_color (int value = -1);

  void set_line_maximum_length (int);
  int remaining_character_count_for_line ();
  void clear_output_area ();
  const char *formatted_text ();
  const char *last_position_in_text () const;
  void append_text (const char *, const char *);
  void newline_and_flush ();
  void newline_and_indent (int);
  void separate_with (char);
  void printf (const char *, ...) ATTRIBUTE_GCC_PPDIAG(2,3);
  void verbatim (const char *, ...) ATTRIBUTE_GCC_PPDIAG(2,3);
  void flush (bool force = false);
  void format (text_info *);
  void output_formatted_text ();
  void format_verbatim (text_info *);
  void indent ();
  void newline ();
  void character (char);
  void string (const char *);
  void write_text_to_stream ();
  void write_text_as_dot_label_to_stream (bool);
  void maybe_space ();
  wrapping_mode_t set_verbatim_wrapping ();

  void space ()			{ character (' '); }
  void left_paren ()		{ character ('('); }
  void right_paren ()		{ character (')'); }
  void left_bracket ()		{ character ('['); }
  void right_bracket ()		{ character (']'); }
  void left_brace ()		{ character ('{'); }
  void right_brace ()		{ character ('}'); }
  void semicolon ()		{ character (';'); }
  void comma ()			{ character (','); }
  void dot ()			{ character ('.'); }
  void colon ()			{ character (':'); }
  void double_colon ()		{ string ("::"); }
  void arrow ()			{ string ("->"); }
  void equal ()			{ character ('='); }
  void question ()		{ character ('?'); }
  void bar ()			{ character ('|'); }
  void double_bar ()		{ string ("||"); }
  void carret ()		{ character ('^'); }
  void ampersand ()		{ character ('&'); }
  void double_ampersand ()	{ string ("&&"); }
  void less ()			{ character ('<'); }
  void less_equal ()		{ string ("<="); }
  void greater ()		{ character ('>'); }
  void greater_equal ()		{ string (">="); }
  void plus ()			{ character ('+'); }
  void minus ()			{ character ('-'); }
  void star ()			{ character ('*'); }
  void slash ()			{ character ('/'); }
  void modulo ()		{ character ('%'); }
  void exclamation ()		{ character ('!'); }
  void complement ()		{ character ('~'); }
  void quote ()			{ character ('\''); }
  void backquote ()		{ character ('`'); }
  void doublequote ()		{ character ('"'); }
  void underscore ()		{ character ('_'); }

  void
  maybe_newline_and_indent (int n)
    {
      if (need_newline)
	newline_and_indent (n);
    }

  template <typename T>
  void
  scalar (const char* format, T scalar)
    {
      sprintf (buffer->digit_buffer, format, scalar);
      string (buffer->digit_buffer);
    }

  void
  wide_int (const wide_int_ref &wi, signop sgn)
    {
      print_dec (wi, buffer->digit_buffer, sgn);
      string (buffer->digit_buffer);
    }

  void decimal_int (int i) { scalar ("%d", i); }
  void wide_integer (unsigned HOST_WIDE_INT i) { scalar (HOST_WIDE_INT_PRINT_UNSIGNED, i); }
  void wide_integer (HOST_WIDE_INT i) { scalar (HOST_WIDE_INT_PRINT_DEC, i); }
  void pointer (void *ptr) { scalar ("%p", ptr); }

  void
  identifier (const char* id)
    {
      string (translate_identifiers ? identifier_to_locale (id) : id);
    }

  /* True if PRETTY-PRINTER is in line-wrapping mode.  */
  bool is_wrapping_line () { return wrapping.line_cutoff > 0; }

  /* Where to put whitespace around the entity being formatted.  */
  padding_t padding;

  /* The real upper bound of number of characters per line, taking into
     account the case of a very very looong prefix.  */
  int maximum_length;

  /* Indentation count.  */
  int indent_skip;

  /* Current wrapping mode.  */
  wrapping_mode_t wrapping;

  /* Nonzero if current PREFIX was emitted at least once.  */
  bool emitted_prefix;

  /* Nonzero means one should emit a newline before outputting anything.  */
  bool need_newline;

private:
  void clear_state ();
  void wrap_text (const char *start, const char *end);
  void maybe_wrap_text (const char *start, const char *end);
  void append_r (const char *start, int length);
  void set_real_maximum_length ();

  /* The prefix for each new line.  */
  const char *prefix;

  /* Nonzero means that text should be colorized.  */
  bool show_color;
};


/* Deprecated types for compatibility with old code. */
typedef base_printer::printer_fn printer_fn;
typedef pretty_printer::flags_t pp_flags;
typedef pretty_printer::padding_t pp_padding;
namespace
{
  const pretty_printer::padding_t pp_none = pretty_printer::pp_none;
  const pretty_printer::padding_t pp_before = pretty_printer::pp_before;
  const pretty_printer::padding_t pp_after =  pretty_printer::pp_after;
}
typedef pretty_printer::wrapping_mode_t pp_wrapping_mode_t;

/* Deprecated macros for compatibility with old code */
#define pp_get_prefix(PP) (PP)->get_prefix ()

/* Maximum characters per line in automatic line wrapping mode.
   Zero means don't wrap lines.  */
#define pp_line_cutoff(PP)  (PP)->wrapping.line_cutoff

/* Prefixing rule used in formatting a diagnostic message.  */
#define pp_prefixing_rule(PP)  (PP)->wrapping.rule

/* Get or set the wrapping mode as a single entity.  */
#define pp_wrapping_mode(PP) (PP)->wrapping

/* Client supplied function used to decode formats.  */
#define pp_format_decoder(PP) (PP)->format_decoder

/* TRUE if a newline character needs to be added before further
   formatting.  */
#define pp_needs_newline(PP)  (PP)->need_newline

/* True if PRETTY-PRINTER is in line-wrapping mode.  */
#define pp_is_wrapping_line(PP) (PP)->is_wrapping_line ()

/* The amount of whitespace to be emitted when starting a new line.  */
#define pp_indentation(PP) (PP)->indent_skip

/* True if identifiers are translated to the locale character set on
   output.  */
#define pp_translate_identifiers(PP) (PP)->translate_identifiers

/* True if colors should be shown.  */
#define pp_show_color(PP) (PP)->get_color_state ()

#define pp_space(PP)			(PP)->space ()
#define pp_left_paren(PP)		(PP)->left_paren ()
#define pp_right_paren(PP)		(PP)->right_paren ()
#define pp_left_bracket(PP)		(PP)->left_bracket ()
#define pp_right_bracket(PP)		(PP)->right_bracket ()
#define pp_left_brace(PP)		(PP)->left_brace ()
#define pp_right_brace(PP)		(PP)->right_brace ()
#define pp_semicolon(PP)		(PP)->semicolon ()
#define pp_comma(PP)			(PP)->comma ()
#define pp_dot(PP)			(PP)->dot ()
#define pp_colon(PP)			(PP)->colon ()
#define pp_colon_colon(PP)		(PP)->double_colon ()
#define pp_arrow(PP)			(PP)->arrow ()
#define pp_equal(PP)			(PP)->equal ()
#define pp_question(PP)			(PP)->question ()
#define pp_bar(PP)			(PP)->bar ()
#define pp_bar_bar(PP)			(PP)->double_bar ()
#define pp_carret(PP)			(PP)->carret ()
#define pp_ampersand(PP)		(PP)->ampersand ()
#define pp_ampersand_ampersand(PP)	(PP)->double_ampersand ()
#define pp_less(PP)			(PP)->less ()
#define pp_less_equal(PP)		(PP)->less_equal ()
#define pp_greater(PP)			(PP)->greater ()
#define pp_greater_equal(PP)		(PP)->greater_equal ()
#define pp_plus(PP)			(PP)->plus ()
#define pp_minus(PP)			(PP)->minus ()
#define pp_star(PP)			(PP)->star ()
#define pp_slash(PP)			(PP)->slash ()
#define pp_modulo(PP)			(PP)->modulo ()
#define pp_exclamation(PP)		(PP)->exclamation ()
#define pp_complement(PP)		(PP)->complement ()
#define pp_quote(PP)			(PP)->quote ()
#define pp_backquote(PP)		(PP)->backquote ()
#define pp_doublequote(PP)		(PP)->doublequote ()
#define pp_underscore(PP)		(PP)->underscore ()

#define pp_maybe_newline_and_indent(PP, N) (PP)->maybe_newline_and_indent (N)
#define pp_scalar(PP, FORMAT, SCALAR) (PP)->scalar (FORMAT, SCALAR)
#define pp_decimal_int(PP, I)  (PP)->decimal_int (I)
#define pp_unsigned_wide_integer(PP, I) (PP)->wide_integer ((unsigned HOST_WIDE_INT) I)
#define pp_wide_int(PP, W, SGN) (PP)->wide_int (W, SGN)
#define pp_wide_integer(PP, I) (PP)->wide_integer ((HOST_WIDE_INT) I)
#define pp_pointer(PP, P) (PP)->pointer (P)
#define pp_identifier(PP, ID) (PP)->identifier (ID)

#define pp_buffer(PP) (PP)->buffer

#define pp_set_line_maximum_length(PP, length) (PP)->set_line_maximum_length (length)
#define pp_set_prefix(PP, prefix) (PP)->set_prefix (prefix)
#define pp_destroy_prefix(PP) (PP)->destroy_prefix ()
#define pp_remaining_character_count_for_line(PP) (PP)->remaining_character_count_for_line ()
#define pp_clear_output_area(PP) (PP)->clear_output_area ()
#define pp_formatted_text(PP) (PP)->formatted_text ()
#define pp_last_position_in_text(PP) (PP)->last_position_in_text ()
#define pp_emit_prefix(PP) (PP)->emit_prefix ()
#define pp_append_text(PP, start, end) (PP)->append_text (start, end)
#define pp_newline_and_flush(PP) (PP)->newline_and_flush ()
#define pp_newline_and_indent(PP, n) (PP)->newline_and_indent (n)
#define pp_separate_with(PP, c) (PP)->separate_with (c)

/* This header may be included before diagnostics-core.h, hence the duplicate
   definitions to allow for GCC-specific formats.  */
#define pp_printf(PP, ...) (PP)->printf (__VA_ARGS__)
#define pp_verbatim(PP, ...) (PP)->verbatim (__VA_ARGS__)
#define pp_flush(PP) (PP)->flush ()
#define pp_really_flush(PP) (PP)->flush (true)
#define pp_format(PP, text) (PP)->format (text)
#define pp_output_formatted_text(PP) (PP)->output_formatted_text ()
#define pp_format_verbatim(PP, text) (PP)->format_verbatim(text)

#define pp_indent(PP) (PP)->indent ()
#define pp_newline(PP) (PP)->newline ()
#define pp_character(PP, c) (PP)->character (c)
#define pp_string(PP, str) (PP)->string(str)
#define pp_write_text_to_stream(PP) (PP)->write_text_to_stream ()
#define pp_write_text_as_dot_label_to_stream(PP, for_record) (PP)->write_text_as_dot_label_to_stream (for_record)
#define pp_maybe_space(PP) (PP)->maybe_space ()
#define pp_set_verbatim_wrapping(PP) (PP)->set_verbatim_wrapping ()

#define pp_clear_state(PP) (PP)->clear_state ()
#define pp_wrap_text(PP, start, end) (PP)->wrap_text (start, end)
#define pp_maybe_wrap_text(PP, start, end) (PP)->maybe_wrap_text (start, end)
#define pp_append_r(PP, start, length) (PP)->append_r (start, length)
#define pp_set_real_maximum_length(PP) (PP)->set_real_maximum_length ()

#endif /* GCC_PRETTY_PRINT_H */
