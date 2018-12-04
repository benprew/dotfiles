set -x PATH /usr/local/opt/ruby/bin /usr/local/lib/ruby/gems/2.5.0/bin/ $PATH
# For compilers to find ruby you may need to set:
set -x LDFLAGS "-L/usr/local/opt/ruby/lib"
set -x CPPFLAGS "-I/usr/local/opt/ruby/include"

# For pkg-config to find ruby you may need to set:
set -x PKG_CONFIG_PATH "/usr/local/opt/ruby/lib/pkgconfig"

if test -d ~/.rbenv
    set -x PATH ~/.rbenv/shims $PATH
end
