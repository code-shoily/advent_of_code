defmodule AdventOfCode.Helpers.Arithmetics do
  @moduledoc """
  This module implements arithmetics functions and algorithms
  """

  @doc """
  Returns a list of divisors of a number.

  ## Example

    iex> Arithmetics.divisors(0)
    :error

    iex> Arithmetics.divisors(1)
    [1]

    iex> Arithmetics.divisors(12)
    [1, 12, 2, 6, 3, 4]

    iex> Arithmetics.divisors(13)
    [1, 13]

  """
  def divisors(0), do: :error
  def divisors(n) do
    1..trunc(:math.sqrt(n))
    |> Enum.flat_map(fn
      x when rem(n, x) != 0 -> []
      x when x != div(n, x) -> [x, div(n, x)]
      x -> [x]
    end)
  end

  @doc """
  Chinese Remainder Theorem.

  ## Example

    iex> Arithmetics.crt([{11, 10}, {12, 4}, {13, 12}])
    1000

    iex> Arithmetics.crt([{11, 10}, {22, 4}, {19, 9}])
    nil

    iex> Arithmetics.crt([{3, 2}, {5, 3}, {7, 2}])
    23

  """
  def crt(congruences) do
    {modulii, residues} = Enum.unzip(congruences)
    mod_pi = Enum.reduce(modulii, 1, &Kernel.*/2)
    crt_modulii = Enum.map(modulii, &div(mod_pi, &1))

    case calculate_inverses(crt_modulii, modulii) do
      nil ->
        nil

      inverses ->
        crt_modulii
        |> Enum.zip(
          residues
          |> Enum.zip(inverses)
          |> Enum.map(&Tuple.product/1)
        )
        |> Enum.map(&Tuple.product/1)
        |> Enum.sum()
        |> mod(mod_pi)
    end
  end

  defp mod_inverse(a, b) do
    {_, x, y} = Integer.extended_gcd(a, b)
    (a * x + b * y == 1 && x) || nil
  end

  defp mod(a, m) do
    x = rem(a, m)
    (x < 0 && x + m) || x
  end

  defp calculate_inverses([], []), do: []

  defp calculate_inverses([n | ns], [m | ms]) do
    case mod_inverse(n, m) do
      nil -> nil
      inv -> [inv | calculate_inverses(ns, ms)]
    end
  end
end
