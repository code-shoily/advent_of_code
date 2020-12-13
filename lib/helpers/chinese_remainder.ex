defmodule AdventOfCode.Helpers.ChineseRemainder do
  @moduledoc """
  This module implements Chinese Remainder Theorem
  """

  @doc """
  Chinese Remainder Theorem.

  ## Example

    iex> AdventOfCode.Helpers.ChineseRemainder.chinese_remainder([{11, 10}, {12, 4}, {13, 12}])
    1000

    iex> AdventOfCode.Helpers.ChineseRemainder.chinese_remainder([{11, 10}, {22, 4}, {19, 9}])
    nil

    iex> AdventOfCode.Helpers.ChineseRemainder.chinese_remainder([{3, 2}, {5, 3}, {7, 2}])
    23

  """
  def chinese_remainder(congruences) do
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
          |> Enum.map(fn {a, b} -> a * b end)
        )
        |> Enum.map(fn {a, b} -> a * b end)
        |> Enum.sum()
        |> mod(mod_pi)
    end
  end

  @doc """
  Calculates extended GCD

  ## Example

    iex> AdventOfCode.Helpers.ChineseRemainder.egcd(1914, 899)
    {8, -17}

    iex> AdventOfCode.Helpers.ChineseRemainder.egcd(1432, 123211)
    {-22973, 267}

  """
  def egcd(_, 0), do: {1, 0}

  def egcd(a, b) do
    {s, t} = egcd(b, rem(a, b))
    {t, s - div(a, b) * t}
  end

  defp mod_inverse(a, b) do
    {x, y} = egcd(a, b)
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
