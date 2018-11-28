set -x PATH /usr/local/opt/ruby/bin $PATH
# For compilers to find ruby you may need to set:
set -x LDFLAGS "-L/usr/local/opt/ruby/lib"
set -x CPPFLAGS "-I/usr/local/opt/ruby/include"

# For pkg-config to find ruby you may need to set:
set -x PKG_CONFIG_PATH "/usr/local/opt/ruby/lib/pkgconfig"
