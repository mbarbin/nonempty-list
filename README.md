# nonempty-list

[![CI Status](https://github.com/mbarbin/nonempty-list/workflows/ci/badge.svg)](https://github.com/mbarbin/nonempty-list/actions/workflows/ci.yml)
[![Deploy odoc Status](https://github.com/mbarbin/nonempty-list/workflows/deploy-odoc/badge.svg)](https://github.com/mbarbin/nonempty-list/actions/workflows/deploy-odoc.yml)

This library offers a repackaging of `Core_kernel`'s `Nonempty_list`, as a
standalone opam package for using with Base.

Original code at https://github.com/janestreet/core_kernel/tree/master/nonempty_list

The code was slightly modified in order to remove dependencies into `Core` and
solely depend on `Base` instead, making it available in more contexts, without
requiring to add a dependency into `Core` and `Core_kernel`.

For a repackaging of the original code, including the `Core` bits,
see: https://github.com/mbarbin/nonempty-list-core
