#!/bin/bash

cat >/tmp/unsafe-openssl.conf <<EOF
openssl_conf = openssl_init
[openssl_init]
ssl_conf = ssl_sect
[ssl_sect]
system_default = system_default_sect
[system_default_sect]
Options = UnsafeLegacyRenegotiation
EOF

OPENSSL_CONF=/tmp/unsafe-openssl.conf /bin/curl --insecure $*

