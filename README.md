grisp_updater_tools [WiP]
===================

GRiSP Software Update Tooling


Build
-----

    $ rebar3 escriptize


Create Software Package
-----------------------

    $ ./scripts/gen_update.sh -sz -t /opt/grisp/grisp2-rtems-toolchain -a ../grisp_demo -n grisp_demo -v 0.1.0
