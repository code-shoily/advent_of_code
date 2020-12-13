defmodule AdventOfCode.Helpers.ChineseRemainder do
  def chinese_remainder(congruences) do
    {modulii, residues} = Enum.unzip(congruences)
    mod_pi = List.foldl(modulii, 1, &Kernel.*/2)
    crt_modulii = Enum.map(modulii, &div(mod_pi, &1))

    case calc_inverses(crt_modulii, modulii) do
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

  def egcd(_, 0), do: {1, 0}

  def egcd(a, b) do
    {s, t} = egcd(b, rem(a, b))
    {t, s - div(a, b) * t}
  end

  defp mod_inv(a, b) do
    {x, y} = egcd(a, b)
    (a * x + b * y == 1 && x) || nil
  end

  defp mod(a, m) do
    x = rem(a, m)
    (x < 0 && x + m) || x
  end

  defp calc_inverses([], []), do: []

  defp calc_inverses([n | ns], [m | ms]) do
    case mod_inv(n, m) do
      nil -> nil
      inv -> [inv | calc_inverses(ns, ms)]
    end
  end
end
