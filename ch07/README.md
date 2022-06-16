#### Exception handling in IO & GHC runtime

- The GHC runtime system implements an exception-throwing and -handling mechanism.
- The main rule is that an exception can be thrown anywhere (including pure code), but it can be caught only in an IO computation.

To make using this API in monad stacks easier, we use the `Control.Monad.Catch` module from the `exceptions` package.
This module reexports the `Control.Exception` API and adds several type classes that can be used in monad stacks that
support throwing and catching GHC exceptions. The utility functions are also redefined in terms of monad stacks, as opposed to IO.
