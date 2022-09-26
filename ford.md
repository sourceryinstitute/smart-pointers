project:
summary: A library for tracking references to shared resources such as memory.
src_dir: src
output_dir: doc/html
preprocess: true
macro: FORD
preprocessor: gfortran -E
display: public
         protected
         private
source: true
graph: true
md_extensions: markdown.extensions.toc
coloured_edges: true
sort: permission-alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html#ISO_005fC_005fBINDING
project_github: https://github.com/sourceryinstitute/dag
author: Sourcery Institute
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
project_github: https://github.com/sourceryinstitute/reference-counter
project_download: https://github.com/sourceryinstitute/reference-counter/releases
github: https://github.com/sourceryinstitute
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !

{!README.md!}
