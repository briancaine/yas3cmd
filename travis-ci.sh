# OCaml version to install
export OCAML_VERSION=4.03.0
# OPAM packages needed to build tests
export OPAM_PACKAGES='ocamlfind ounit oasis core ppx_deriving ppx_fields_conv ppx_sexp_conv cohttp base64 nocrypto oasis bisect_ppx ocveralls'

# install ocaml from apt
#sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-findlib libgmp-dev aspcud rsync

# install opam
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sudo sh -s /usr/local/bin

opam init -a --compiler=$OCAML_VERSION
opam switch $OCAML_VERSION
eval `opam config env`

# install packages from opam
opam install -q -y ${OPAM_PACKAGES}

oasis setup

# compile & run tests (here assuming OASIS DevFiles)
./configure --enable-tests
BISECT_FILE=$PWD/_coverage/bisect BISECT_COVERAGE=YES make test
