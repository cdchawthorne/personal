#!/bin/sed -nf

#/foo/,/bar/ ! s_baz_quz_
#/foo/,/bar/ { 3,8 s_baz_quz_ }
#N; N; s_\n_NEWLINE_g
#x
#/save/ h; /load/ { s_load__g; G; }
H
$ { g
    s_^\n__g
    p
}
