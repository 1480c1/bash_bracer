using System;
using System.Collections.Generic;
using System.Text;

namespace Examples
{
    public class BashBraceParser
    {
        private string _expression;
        private int _nextCharIndex;

        /// <summary>
        /// Parse the specified BASH brace expression and return the result string list.
        /// </summary>
        public IList<string> Parse(string expression)
        {
            _expression = expression;
            _nextCharIndex = 0;

            return ParseExpression();
        }

        private List<string> ParseExpression()
        {
            var result = new List<string>();

            while (!Eof())
            {
                // Parsing a component will produce a list of strings,
                // they are added to the final string list

                var items = ParseComponent();

                result.AddRange(items);

                // If next char is ',' simply skip it and parse next component
                if (Peek() == ',')
                {
                    // Skip comma
                    ReadNextChar();
                }
                else
                {
                    break;
                }
            }

            return result;
        }

        private List<string> ParseComponent()
        {
            List<string> leftItems = null;

            while (!Eof())
            {
                // Parse a component part will produce a list of strings (rightItems)
                // We need to combine already parsed string list (leftItems) in this component
                // with the newly parsed 'rightItems'
                var rightItems = ParseComponentPart();
                if (rightItems == null)
                {
                    // No more parts, return current result (leftItems) to the caller
                    break;
                }

                if (leftItems == null)
                {
                    leftItems = rightItems;
                }
                else
                {
                    leftItems = Combine(leftItems, rightItems);
                }
            }

            return leftItems;
        }

        private List<string> ParseComponentPart()
        {
            var nextChar = Peek();

            if (nextChar == '(')
            {
                // Skip '('
                ReadNextChar();

                // Recursively parse the inner expression
                var items = ParseExpression();

                // Skip ')'
                ReadNextChar();

                return items;
            }
            else if (char.IsLetter(nextChar))
            {
                var letters = ReadLetters();

                return new List<string> { letters };
            }
            else
            {
                // Fail to parse a part, it means a component is ended
                return null;
            }
        }

        // Combine two lists of strings and return the combined string list
        private List<string> Combine(List<string> leftItems, List<string> rightItems)
        {
            var result = new List<string>();

            foreach (var leftItem in leftItems)
            {
                foreach (var rightItem in rightItems)
                {
                    result.Add(leftItem + rightItem);
                }
            }

            return result;
        }

        // Peek next char without moving the cursor
        private char Peek()
        {
            if (Eof())
            {
                return '\0';
            }

            return _expression[_nextCharIndex];
        }

        // Read next char and move the cursor to next char
        private char ReadNextChar()
        {
            return _expression[_nextCharIndex++];
        }

        private void UnreadChar()
        {
            _nextCharIndex--;
        }

        // Check if the whole expression string is scanned.
        private bool Eof()
        {
            return _nextCharIndex == _expression.Length;
        }

        // Read a continuous sequence of letters.
        private string ReadLetters()
        {
            if (!char.IsLetter(Peek()))
            {
                return null;
            }

            var str = new StringBuilder();

            while (!Eof())
            {
                var ch = ReadNextChar();
                if (char.IsLetter(ch))
                {
                    str.Append(ch);
                }
                else
                {
                    UnreadChar();
                    break;
                }
            }

            return str.ToString();
        }
        static void Main()
        {
            var parser = new BashBraceParser();
            var result = parser.Parse("((abb,b)o(m,n)p,b)");

            var output = String.Join(" ", result);

            // Result: aomp - aonp - bomp - bonp - b
            Console.WriteLine(output);
        }
    }


}
