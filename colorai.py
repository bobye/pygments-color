from pygments import highlight
from pygments.lexers import PythonLexer
from pygments.formatters import HtmlFormatter
from pygments.filters import VisibleWhitespaceFilter

from pygments.formatter import Formatter
from pygments.util import OptionError, get_choice_opt
from pygments.token import Token
from pygments.console import colorize

## Taken from source code of pygments, change RawTokenFormatter() to AITokenFormatter()
class AITokenFormatter(Formatter):
    r"""
    Format tokens as a raw representation for storing token streams.

    The format is ``tokentype<TAB>repr(tokenstring)\n``. The output can later
    be converted to a token stream with the `RawTokenLexer`, described in the
    :doc:`lexer list <lexers>`.

    Only two options are accepted:

    `compress`
        If set to ``'gz'`` or ``'bz2'``, compress the output with the given
        compression algorithm after encoding (default: ``''``).
    `error_color`
        If set to a color name, highlight error tokens using that color.  If
        set but with no value, defaults to ``'red'``.

        .. versionadded:: 0.11

    """
    name = 'Raw tokens'
    aliases = ['raw', 'tokens']
    filenames = ['*.raw']

    unicodeoutput = False

    def __init__(self, **options):
        Formatter.__init__(self, **options)
        if self.encoding:
            raise OptionError('the raw formatter does not support the '
                              'encoding option')
        self.encoding = 'ascii'  # let pygments.format() do the right thing
        self.compress = get_choice_opt(options, 'compress',
                                       ['', 'none', 'gz', 'bz2'], '')
        self.error_color = options.get('error_color', None)
        if self.error_color is True:
            self.error_color = 'red'
        if self.error_color is not None:
            try:
                colorize(self.error_color, '')
            except KeyError:
                raise ValueError("Invalid color %r specified" %
                                 self.error_color)

    def format(self, tokensource, outfile):
        try:
            outfile.write(b'')
        except TypeError:
            raise TypeError('The raw tokens formatter needs a binary '
                            'output file')
        if self.compress == 'gz':
            import gzip
            outfile = gzip.GzipFile('', 'wb', 9, outfile)
            def write(text):
                outfile.write(text.encode())
            flush = outfile.flush
        elif self.compress == 'bz2':
            import bz2
            compressor = bz2.BZ2Compressor(9)
            def write(text):
                outfile.write(compressor.compress(text.encode()))
            def flush():
                outfile.write(compressor.flush())
                outfile.flush()
        else:
            def write(text):
                outfile.write(text.encode())
            flush = outfile.flush

        if self.error_color:
            for ttype, value in tokensource:
                line = "%s\t%r\n" % (ttype, value)
                if ttype is Token.Error:
                    write(colorize(self.error_color, line))
                else:
                    write(line)
        else:
            for ttype, value in tokensource:
                write("%s\t%r\n" % (ttype, value))
        flush()

import random
def rand():
    return '#%02x%02x%02x' % (random.randint(0,255), random.randint(0,255), random.randint(0,255))


from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic, Text, STANDARD_TYPES

class YourStyle(Style):
    background_color = "#000"
    default_style = ""
    styles = {
        Comment:                'italic ' + rand(),
        Keyword:                'bold ' + rand(),
        Name:                   rand(),
        Name.Function:          rand(),
        Name.Class:             'bold ' + rand(),
        String:                 rand(),
        String.Doc:             'italic ' + rand(),
        Generic:                rand(),
        Text:                   rand()
    }


import os
from pygments.lexers import guess_lexer, guess_lexer_for_filename
def main():
    filename = "codeSample.py"
    codeSampleFile = open(filename,'r')
    codeSample = codeSampleFile.read()
    codeSampleFile.close()

##    lexer = PythonLexer()
    lexer = guess_lexer_for_filename(filename, codeSample)
    lexer2 = guess_lexer_for_filename(filename, codeSample)

    lexer.add_filter(VisibleWhitespaceFilter())
    print highlight(codeSample, lexer, AITokenFormatter())


    renderHtmlFile = open('out.html','w')
    print highlight(codeSample, lexer2, HtmlFormatter(full="True", style=YourStyle), renderHtmlFile)
    renderHtmlFile.close()
    os.system("open out.html")

if __name__ == "__main__":
    main()


