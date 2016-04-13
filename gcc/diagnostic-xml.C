#include <string>

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
