defmodule Quark.Partial do
  @moduledoc ~S"""
  Provide curried functions, that can also be partially bound without
  dot notation. Partially applying a function will always return a
  fully-curried function.

  Please note that these will use all of the arities up to the defined function.

  For instance:

      defpartial foo(a, b, c), do: a + b + c
      #=> foo/0, foo/1, foo/2, and foo/3

  If you need to use an arity in the range below the original
  function, fall back to `defcurry` and partially apply manually.
  """

  use Quark.Curry
  require Quark.Curry

  defmacro __using__(_) do
    quote do
      import Quark.Partial, only: [defpartial: 2, defpartialp: 2, defpartialx: 2]
    end
  end

  @doc """
  "x"  as   in  experimental.  Curried   clauses  call
  the   defined  functions   in   the  module   (using
  `Kernel.apply/2`)   instead   of   a   parameterized
  `defcurry`ed  anonymous   function  as  `defpartial`
  does. The  reason for  this is  so that  the defined
  curried functions  can be  overridden if  needed (to
  add constraints, etc.).

  Modeled after:

  ```elixir
  defmodule A do
    defmacro __using__(_) do
      quote do
        def new(), do: fn(a)    -> apply(__MODULE__, :new, [a]) end
        def new(a), do: fn(b)   -> apply(__MODULE__, :new, [a, b]) end
        def new(a,b), do: fn(c) -> apply(__MODULE__, :new, [a, b, c]) end
        def new(a, b, c), do: a - b - c

        defoverridable new: 0, new: 1, new: 2
      end
    end
  end

  defmodule B do
    use A
    def new(a) when is_integer(a), do: super(a)
    def new(_), do: raise(ArgumentError, "not integer")
  end
  ```

  Example usage:
  ```elixir
  defmodule D do
    defmacro __using__(_) do
      quote do
        use Quark

        defpartialx m(a,b,c), do: a-b-c

        defoverridable [m: 0, m: 1, m: 2, m: 3]
      end
    end
  end

  defmodule E do
    use D

    def m(a) when is_integer(a), do: super(a)
    def m(_), do: raise(ArgumentError, "not int")

    def m(a,b) when is_integer(a) and is_integer(b), do: super(a,b)
    def m(_,_), do: raise(ArgumentError, "not int")

    def m(a,b,c) when is_integer(a) and is_integer(b) and is_integer(c), do: super(a,b,c)
    def m(_,_,_), do: raise(ArgumentError, "not int")
  end
  ```

  (**Sidenote**: the constraints verbosity could be abstracted, for example in Algae when adding constraints.)
  """

  defmacro defpartialx({fun_name, ctx, args}, do: body) do
    scanned_args = [[]] ++ args_scan(args)
    quote do
      unquote do: make_curried_clauses(scanned_args, {fun_name, ctx, body})
    end
  end

  defp make_curried_clauses([args], {fun_name, ctx, body}) do
    quote do
      def unquote({fun_name, ctx, args}), do: unquote(body)
    end
  end

  defp make_curried_clauses([args|rest], {fun_name, ctx, _} = fun_attrs) do
    quote do
      def unquote({fun_name, ctx, args}) do
        &apply(__MODULE__, unquote(fun_name), unquote(args) ++ [&1])
      end
      unquote do: make_curried_clauses(rest, fun_attrs)
    end
  end

  @doc ~S"""
  A convenience on `defcurry`. Generates a series of partially-bound
  applications of a fully-curried function, for all arities _at and below_
  the user-specified arity.

  For instance:

      defpartial add(a,b), do: a + b
      #=> add/0, add/1, add/2.

  ## Examples

      defpartial minus(a, b, c), do: a - b - c
      minus(3, 2, 1)
      0

      minus.(3).(2).(1)
      0

      below_ten = minus(5)
      below_ten.(2, 1)
      7

      below_five = minus(20, 15)
      below_five.(2)
      3

  """
  defmacro defpartial({fun_name, ctx, args}, do: body) do
    quote do
      defcurry unquote({fun_name, ctx, args}), do: unquote(body)
      unquote do: Enum.map(args_scan(args), &rehydrate(fun_name, ctx, &1))
    end
  end

  defp rehydrate(fun_name, ctx, args) do
    quote do
      def unquote({fun_name, ctx, args}) do
        unquote(partial_apply(fun_name, args))
      end
    end
  end

  @doc ~S"""
  `defpartial`, but generates private functions

  ## Examples

      defpartialp minus(a, b, c), do: a - b - c
      minus(3, 2, 1)
      0

      minus.(3).(2).(1)
      0
      below10 = minus(5)
      below10.(2, 1)
      7

      below5 = minus(10, 5)
      below5.(2)
      3

  """
  defmacro defpartialp({fun_name, ctx, args}, do: body) do
    quote do
      defcurryp unquote({fun_name, ctx, args}), do: unquote(body)
      unquote do: Enum.map(args_scan(args), &rehydratep(fun_name, ctx, &1))
    end
  end

  defp rehydratep(fun_name, ctx, args) do
    quote do
      defp unquote({fun_name, ctx, args}) do
        unquote(partial_apply(fun_name, args))
      end
    end
  end

  defp args_scan(args), do: Enum.scan(args, [], &(&2 ++ [&1]))

  defp partial_apply(fun_name, args) do
    {as, [a]} = Enum.split(args, -1)
    quote do
      unquote(fun_name)(unquote_splicing(as)).(unquote(a))
    end
  end
end
