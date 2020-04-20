python
import sys, os.path
sys.path.insert(0, os.path.expanduser('~/.gdb'))
sys.path.insert(0, '/usr/share/gcc-10/python')
import qt5printers
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers(None)
qt5printers.register_printers(gdb.current_objfile())
end

set print pretty on
set print object on
set print static-members on
set print vtbl on
set print demangle on
set demangle-style gnu-v3
set print sevenbit-strings off
